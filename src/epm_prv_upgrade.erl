%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_prv_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, upgrade).
-define(DEPS, [lock]).
%% Also only upgrade top-level (0) deps. Transitive deps shouldn't be
%% upgradable -- if the user wants this, they should declare it at the
%% top level and then upgrade.

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 =
        epm_state:add_provider(State,
                                 providers:create([{name, ?PROVIDER},
                                                   {module, ?MODULE},
                                                   {bare, true},
                                                   {deps, ?DEPS},
                                                   {example, "epm upgrade [cowboy[,ranch]]"},
                                                   {short_desc, "Upgrade dependencies."},
                                                   {desc, "Upgrade project dependencies. Use the -a/--all option to "
                                                          "upgrade all dependencies. To upgrade specific dependencies, "
                                                          "their names can be listed in the command."},
                                                   {opts, [{all, $a, "all", undefined, "Upgrade all dependencies."},
                                                          {package, undefined, undefined, string,
                                                           "List of packages to upgrade."}
                                                          ]}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    Cwd = epm_state:dir(State),
    Providers = epm_state:providers(State),
    epm_hooks:run_project_and_app_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    case do_(State) of
        {ok, NewState} ->
            epm_hooks:run_project_and_app_hooks(Cwd, post, ?PROVIDER, Providers, NewState),
            {ok, NewState};
        Other ->
            epm_hooks:run_project_and_app_hooks(Cwd, post, ?PROVIDER, Providers, State),
            Other
    end.

do_(State) ->
    Locks = epm_state:get(State, {locks, default}, []),
    %% We have 3 sources of dependencies to upgrade from:
    %% 1. the top-level epm.config (in `deps', dep name is an atom)
    %% 2. the app-level epm.config in umbrella apps (in `{deps, default}',
    %%    where the dep name is an atom)
    %% 3. the formatted sources for all after app-parsing (in `{deps, default}',
    %%    where the reprocessed app name is a binary)
    %%
    %% The gotcha with these is that the selection of dependencies with a
    %% binary name (those that are stable and usable internally) is done with
    %% in the profile deps only, but while accounting for locks.
    %% Because our job here is to unlock those that have changed, we must
    %% instead use the atom-based names, both in `deps' and `{deps, default}',
    %% as those are the dependencies that may have changed but have been
    %% disregarded by locks.
    %%
    %% As such, the working set of dependencies is the addition of
    %% `deps' and `{deps, default}' entries with an atom name, as those
    %% disregard locks and parsed values post-selection altogether.
    %% Packages without versions can of course be a single atom.
    TopDeps = epm_state:get(State, deps, []),
    ProfileDeps = epm_state:get(State, {deps, default}, []),
    Deps = [Dep || Dep <- TopDeps ++ ProfileDeps, % TopDeps > ProfileDeps
                   is_atom(Dep) orelse is_atom(element(1, Dep))],
    Names = case handle_args(State) of
        {false, undefined} -> throw(?PRV_ERROR(no_arg));
        {true, _} -> [Name || {Name, _, 0} <- Locks];
        {false, Packages} -> Bin = epm_utils:to_binary(Packages),
                             lists:usort(re:split(Bin, <<" *, *">>, [trim, unicode]))
    end,
    DepsDict = deps_dict(epm_state:all_deps(State)),
    AltDeps = find_non_default_deps(Deps, State),
    FilteredNames = cull_default_names_if_profiles(Names, Deps, State),
    Checkouts = [epm_app_info:name(Dep) || Dep <- epm_state:all_checkout_deps(State)],
    case prepare_locks(FilteredNames, Deps, Locks, [], DepsDict, AltDeps, Checkouts) of
        {error, Reason} ->
            {error, Reason};
        {Locks0, Unlocks0} ->
            Deps0 = top_level_deps(Deps, Locks),
            State1 = epm_state:set(State, {deps, default}, Deps0),
            DepsDir = epm_prv_install_deps:profile_dep_dir(State, default),
            D = epm_app_utils:parse_deps(root, DepsDir, Deps0, State1, Locks0, 0),

            %% first update the package index for the packages to be upgraded
            update_pkg_deps(Unlocks0, D, State1),

            State2 = epm_state:set(State1, {parsed_deps, default}, D),
            State3 = epm_state:set(State2, {locks, default}, Locks0),
            State4 = epm_state:set(State3, upgrade, true),
            UpdatedLocks = [L || L <- epm_state:lock(State4),
                                 lists:keymember(epm_app_info:name(L), 1, Locks0)],
            Res = epm_prv_install_deps:do_(epm_state:lock(State4, UpdatedLocks)),
            case Res of
                {ok, State5} ->
                    epm_utils:info_useless(
                      [element(1,Lock) || Lock <- Locks],
                      [epm_app_info:name(App) || App <- epm_state:lock(State5)]
                     ),
                    epm_prv_lock:do(State5);
                _ ->
                    Res
            end
    end.

-spec format_error(any()) -> iolist().
format_error({unknown_dependency, Name}) ->
    io_lib:format("Dependency ~ts not found", [Name]);
format_error({transitive_dependency, Name}) ->
    io_lib:format("Dependency ~ts is transitive and cannot be safely upgraded. "
                 "Promote it to your top-level epm.config file to upgrade it.",
                 [Name]);
format_error({checkout_dependency, Name}) ->
    io_lib:format("Dependency ~ts is a checkout dependency under _checkouts/ and checkouts cannot be upgraded.",
                  [Name]);
format_error(no_arg) ->
    "Specify a list of dependencies to upgrade, or --all to upgrade them all";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

handle_args(State) ->
    {Args, _} = epm_state:command_parsed_args(State),
    All = proplists:get_value(all, Args, false),
    Package = proplists:get_value(package, Args),
    {All, Package}.

%% fetch updates for package deps that have been unlocked for upgrade
update_pkg_deps([], _, _) ->
    ok;
update_pkg_deps([{Name, _, _} | Rest], AppInfos, State) ->
    case epm_app_utils:find(Name, AppInfos) of
        {ok, AppInfo} ->
            Source = epm_app_info:source(AppInfo),
            case element(1, Source) of
                pkg ->
                    Resources = epm_state:resources(State),
                    #{repos := RepoConfigs} = epm_resource_v2:find_resource_state(pkg, Resources),
                    PkgName = element(2, Source),
                    [update_package(PkgName, RepoConfig, State) || RepoConfig <- RepoConfigs];
                _ ->
                    skip
            end;
        _ ->
            %% this should be impossible...
            skip
    end,
    update_pkg_deps(Rest, AppInfos, State).

update_package(Name, RepoConfig, State) ->
    case epm_packages:update_package(Name, RepoConfig, State) of
        fail ->
            ?WARN("Failed to fetch updates for package ~ts from repo ~ts", [Name, maps:get(name, RepoConfig)]);
        _ ->
            ok
    end.

%% Find alternative deps in non-default profiles since they may
%% need to be passed through (they are never locked)
find_non_default_deps(Deps, State) ->
    AltProfiles = epm_state:current_profiles(State) -- [default],
    AltProfileDeps = lists:append([
        epm_state:get(State, {deps, Profile}, []) || Profile <- AltProfiles]
    ),
    [Dep || Dep <- AltProfileDeps,
            is_atom(Dep) orelse is_atom(element(1, Dep))
            andalso not lists:member(Dep, Deps)].

%% If any alt profiles are used, remove the default profiles from
%% the upgrade list and warn about it.
cull_default_names_if_profiles(Names, Deps, State) ->
    case epm_state:current_profiles(State) of
        [default] ->
            Names;
        _ ->
            ?INFO("Dependencies in the default profile will not be upgraded", []),
            lists:filter(fun(Name) ->
                AtomName = binary_to_atom(Name, utf8),
                epm_utils:tup_find(AtomName, Deps) == false
            end, Names)
    end.

prepare_locks([], _, Locks, Unlocks, _Dict, _AltDeps, _Checkouts) ->
    {Locks, Unlocks};
prepare_locks([Name|Names], Deps, Locks, Unlocks, Dict, AltDeps, Checkouts) ->
    AtomName = binary_to_atom(Name, utf8),
    case lists:keyfind(Name, 1, Locks) of
        {_, _, 0} = Lock ->
            case epm_utils:tup_find(AtomName, Deps) of
                false ->
                    ?WARN("Dependency ~ts has been removed and will not be upgraded", [Name]),
                    prepare_locks(Names, Deps, Locks, Unlocks, Dict, AltDeps, Checkouts);
                Dep ->
                    {Source, NewLocks, NewUnlocks} = prepare_lock(Dep, Lock, Locks, Dict),
                    prepare_locks(Names, Deps, NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks], Dict, AltDeps, Checkouts)
            end;
        {_, _, Level} = Lock when Level > 0 ->
            case epm_utils:tup_find(AtomName, Deps) of
                false ->
                    ?PRV_ERROR({transitive_dependency, Name});
                Dep -> % Dep has been promoted
                    {Source, NewLocks, NewUnlocks} = prepare_lock(Dep, Lock, Locks, Dict),
                    prepare_locks(Names, Deps, NewLocks,
                                  [{Name, Source, 0} | NewUnlocks ++ Unlocks], Dict, AltDeps, Checkouts)
            end;
        false ->
            case lists:member(atom_to_binary(AtomName, utf8), Checkouts) of
                true ->
                    ?PRV_ERROR({checkout_dependency, Name});
                false ->
                    case epm_utils:tup_find(AtomName, AltDeps) of
                        false ->
                            ?PRV_ERROR({unknown_dependency, Name});
                        _ -> % non-default profile dependency found, pass through
                            prepare_locks(Names, Deps, Locks, Unlocks, Dict, AltDeps, Checkouts)
                    end
            end
    end.

prepare_lock(Dep, Lock, Locks, Dict) ->
    {Name1, Source} = case Dep of
        {Name, SrcOrVsn} -> {Name, SrcOrVsn};
        {Name, _, Src} -> {Name, Src};
        _ when is_atom(Dep) ->
            %% version-free package. Must unlock whatever matches in locks
            {_, Vsn, _} = lists:keyfind(epm_utils:to_binary(Dep), 1, Locks),
            {Dep, Vsn}
    end,
    Children = all_children(Name1, Dict),
    {NewLocks, NewUnlocks} = unlock_children(Children, Locks -- [Lock]),
    {Source, NewLocks, NewUnlocks}.

top_level_deps(Deps, Locks) ->
    [Dep || Dep <- Deps, lists:keymember(0, 3, Locks)].

unlock_children(Children, Locks) ->
    unlock_children(Children, Locks, [], []).

unlock_children(_, [], Locks, Unlocks) ->
    {Locks, Unlocks};
unlock_children(Children, [App = {Name,_,_} | Apps], Locks, Unlocks) ->
    case lists:member(epm_utils:to_binary(Name), Children) of
        true ->
            unlock_children(Children, Apps, Locks, [App | Unlocks]);
        false ->
            unlock_children(Children, Apps, [App | Locks], Unlocks)
    end.

deps_dict(Deps) ->
    lists:foldl(fun(App, Dict) ->
                        Name = epm_app_info:name(App),
                        Parent = epm_app_info:parent(App),
                        dict:append_list(Parent, [Name], Dict)
                end, dict:new(), Deps).

all_children(Name, Dict) ->
    lists:flatten(all_children_(Name, Dict)).

all_children_(Name, Dict) ->
    case dict:find(epm_utils:to_binary(Name), Dict) of
        {ok, Children} ->
            [Children | [all_children_(Child, Dict) || Child <- Children]];
        error ->
            []
    end.
