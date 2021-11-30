%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_prv_clean).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").

-define(PROVIDER, clean).
-define(DEPS, [app_discovery, install_deps]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 = epm_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "epm clean"},
                                                               {short_desc, "Remove compiled beam files from apps."},
                                                               {desc, "Remove compiled beam files from apps."},
                                                               {opts, [{all, $a, "all", undefined, "Clean all apps include deps"},
                                                                       {apps, undefined, "apps", string, "Clean a specific list of apps or dependencies"},
                                                                       {profile, $p, "profile", string, "Clean under profile. Equivalent to `epm as <profile> clean`"}]}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    Providers = epm_state:providers(State),
    {All, Profiles, Specific} = handle_args(State),

    State1 = epm_state:apply_profiles(State, [list_to_atom(X) || X <- Profiles]),

    Cwd = epm_dir:get_cwd(),
    epm_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State1),

    if All; Specific =/= [] ->
        DepsDir = epm_dir:deps_dir(State1),
        DepsDirs = filelib:wildcard(filename:join(DepsDir, "*")),
        AllApps = epm_app_discover:find_apps(DepsDirs, all, State),
        Filter = case All of
            true -> fun(_) -> true end;
            false -> fun(AppInfo) -> filter_name(AppInfo, Specific) end
        end,
        clean_apps(State1, Providers, AllApps, Filter);
       true ->
        ProjectApps = epm_state:project_apps(State1),
        clean_apps(State1, Providers, ProjectApps, fun(_) -> true end)
    end,

    clean_extras(State1),

    epm_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, State1),

    {ok, State1}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

clean_apps(State, Providers, Apps, Filter) ->
    Compilers = epm_state:compilers(State),
    [begin
         ?INFO("Cleaning out ~ts...", [epm_app_info:name(AppInfo)]),
         AppDir = epm_app_info:dir(AppInfo),
         AppInfo1 = epm_hooks:run_all_hooks(AppDir, pre, ?PROVIDER, Providers, AppInfo, State),
         epm_compiler:clean(Compilers, AppInfo1),
         epm_hooks:run_all_hooks(AppDir, post, ?PROVIDER, Providers, AppInfo1, State)
     end || AppInfo <- Apps, Filter(AppInfo)].

clean_extras(State) ->
    BaseDir = epm_dir:base_dir(State),
    epm_file_utils:rm_rf(filename:join([BaseDir, "extras"])).

handle_args(State) ->
    {Args, _} = epm_state:command_parsed_args(State),
    All = proplists:get_value(all, Args, false),
    Profiles = proplists:get_all_values(profile, Args),
    DepsRaw = proplists:get_value(apps, Args),
    Deps = parse_deps(DepsRaw),
    {All, Profiles, Deps}.

parse_deps(undefined) -> [];
parse_deps(Bin) ->
    case lists:usort(re:split(Bin, <<" *, *">>, [trim, unicode])) of
        [<<"">>] -> []; % nothing submitted
        Other -> Other
    end.

filter_name(AppInfo, Names) ->
    Name = epm_app_info:name(AppInfo),
    lists:member(Name, Names).
