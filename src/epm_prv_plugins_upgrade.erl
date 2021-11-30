-module(epm_prv_plugins_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, upgrade).
-define(NAMESPACE, plugins).
-define(DEPS, []).

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 = epm_state:add_provider(
            State,
            providers:create([
                    {name, ?PROVIDER},
                    {module, ?MODULE},
                    {namespace, ?NAMESPACE},
                    {bare, true},
                    {deps, ?DEPS},
                    {example, "epm plugins upgrade <plugin>"},
                    {short_desc, "Upgrade plugins"},
                    {desc, "List or upgrade plugins"},
                    {opts, [{plugin, undefined, undefined, string,
                             "Plugin to upgrade"}]}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = epm_state:command_parsed_args(State),
    case proplists:get_value(plugin, Args, none) of
        none ->
            {_, LocalPluginsNames} = epm_prv_plugins:list_local_plugins(State),
            lists:foldl(
                fun (LocalPluginName, {ok, StateAcc}) ->
                    upgrade(atom_to_list(LocalPluginName), StateAcc)
                end,
                {ok, State},
                LocalPluginsNames);
        Plugin ->
            upgrade(Plugin, State)
    end.

-spec format_error(any()) -> iolist().
format_error({not_found, Plugin}) ->
    io_lib:format("Plugin to upgrade not found: ~ts", [Plugin]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

upgrade(Plugin, State) ->
    Profiles = epm_state:current_profiles(State),
    case find_plugin(Plugin, Profiles, State) of
        not_found ->
            Dep = find_plugin(Plugin, [global], State);
        Dep ->
            Dep
    end,

    case Dep of
        not_found ->
            ?PRV_ERROR({not_found, Plugin});
        {ok, P, Profile} ->
            State1 = epm_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
            maybe_update_pkg(P, State1),
            {Apps, State2} = epm_prv_install_deps:handle_deps_as_profile(Profile, State1, [P], true),

            {no_cycle, Sorted} = epm_prv_install_deps:find_cycles(Apps),
            ToBuild = epm_prv_install_deps:cull_compile(Sorted, []),

            %% Add already built plugin deps to the code path
            CodePaths = [epm_app_info:ebin_dir(A) || A <- Apps -- ToBuild],
            code:add_pathsa(CodePaths),

            %% Build plugin and its deps
            _ = build_plugin(ToBuild, State2),

            {ok, State}
    end.

find_plugin(Plugin, Profiles, State) ->
    ec_lists:search(fun(Profile) ->
                        Plugins = epm_state:get(State, {plugins, Profile}, []) ++
                            epm_state:get(State, {project_plugins, Profile}, []),
                        case epm_utils:tup_find(list_to_atom(Plugin), Plugins) of
                            false ->
                                not_found;
                            P ->
                                {ok, P}
                        end
                    end, Profiles).

build_plugin(ToBuild, State) ->
    Providers = epm_state:providers(State),
    epm_prv_compile:compile(State, Providers, ToBuild, plugins).

maybe_update_pkg(Tup, State) when is_tuple(Tup) ->
    maybe_update_pkg(element(1, Tup), State);
maybe_update_pkg(Name, State) ->
    try epm_app_utils:parse_dep(Name, root, unicode:characters_to_list(?DEFAULT_PLUGINS_DIR), State, [], 0) of
        AppInfo ->
            Source = epm_app_info:source(AppInfo),
            case element(1, Source) of
                pkg ->
                    Resources = epm_state:resources(State),
                    #{repos := RepoConfigs} = epm_resource_v2:find_resource_state(pkg, Resources),
                    PkgName = element(2, Source),
                    [update_package(PkgName, RepoConfig, State) || RepoConfig <- RepoConfigs];
                _ ->
                    skip
            end
    catch
        throw:?PRV_ERROR({parse_dep, _}) ->
            skip
    end.

update_package(Name, RepoConfig, State) ->
    case epm_packages:update_package(Name, RepoConfig, State) of
        fail ->
            ?WARN("Failed to fetch updates for package ~ts from repo ~ts", [Name, maps:get(name, RepoConfig)]);
        _ ->
            ok
    end.
