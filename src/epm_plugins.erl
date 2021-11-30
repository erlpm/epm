%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_plugins).

-export([project_plugins_install/1
        ,top_level_install/1
        ,project_apps_install/1
        ,install/2
        ,handle_plugins/3
        ,handle_plugins/4]).

-include("epm.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec project_plugins_install(epm_state:t()) -> epm_state:t().
project_plugins_install(State) ->
    Profiles = epm_state:current_profiles(State),
    State1 = epm_state:allow_provider_overrides(State, true),
    State2 = lists:foldl(fun(Profile, StateAcc) ->
                             Plugins = epm_state:get(State, {project_plugins, Profile}, []),
                             handle_plugins(Profile, Plugins, StateAcc)
                         end, State1, Profiles),
    epm_state:allow_provider_overrides(State2, false).

-spec top_level_install(epm_state:t()) -> epm_state:t().
top_level_install(State) ->
    Profiles = epm_state:current_profiles(State),
    lists:foldl(fun(Profile, StateAcc) ->
                        Plugins = epm_state:get(State, {plugins, Profile}, []),
                        handle_plugins(Profile, Plugins, StateAcc)
                end, State, Profiles).

-spec project_apps_install(epm_state:t()) -> epm_state:t().
project_apps_install(State) ->
    Profiles = epm_state:current_profiles(State),
    ProjectApps = epm_state:project_apps(State),
    lists:foldl(fun(Profile, StateAcc) ->
                        StateAcc1 = case Profile of
                                        default ->
                                            %% default profile top level plugins
                                            %% are installed in run_aux
                                            StateAcc;
                                        _ ->
                                            Plugins = epm_state:get(State, {plugins, Profile}, []),
                                            handle_plugins(Profile, Plugins, StateAcc)
                                    end,

                        lists:foldl(fun(AppInfo, StateAcc2) ->
                                            Plugins2 = epm_app_info:get(AppInfo, {plugins, Profile}, []),
                                            handle_plugins(Profile, Plugins2, StateAcc2)
                                    end, StateAcc1, ProjectApps)
                end, State, Profiles).

-spec install(epm_state:t(), epm_app_info:t()) -> epm_state:t().
install(State, AppInfo) ->
    Profiles = epm_state:current_profiles(State),

    %% don't lose the overrides of the dep we are processing plugins for
    Overrides = epm_app_info:get(AppInfo, overrides, []),
    StateOverrides = epm_state:get(State, overrides, []),
    AllOverrides = Overrides ++ StateOverrides,
    State1 = epm_state:set(State, overrides, AllOverrides),

    State2 = lists:foldl(fun(Profile, StateAcc) ->
                             Plugins = epm_app_info:get(AppInfo, {plugins, Profile}, []),
                             Plugins1 = filter_existing_plugins(Plugins, StateAcc),
                             handle_plugins(Profile, Plugins1, StateAcc)
                         end, State1, Profiles),

    %% Reset the overrides after processing the dep
    epm_state:set(State2, overrides, StateOverrides).

filter_existing_plugins(Plugins, State) ->
    PluginNames = lists:zip(Plugins, epm_state:deps_names(Plugins)),
    AllPlugins = epm_state:all_plugin_deps(State),
    lists:filtermap(fun({Plugin, PluginName}) ->
                            case epm_app_utils:find(PluginName, AllPlugins) of
                                {ok, _} ->
                                    false;
                                _ ->
                                    {true, Plugin}
                            end
                    end, PluginNames).

handle_plugins(Profile, Plugins, State) ->
    handle_plugins(Profile, Plugins, State, false).

handle_plugins(Profile, Plugins, State, Upgrade) ->
    %% Set deps dir to plugins dir so apps are installed there
    Locks = epm_state:lock(State),
    DepsDir = epm_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = epm_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
    %% Install each plugin individually so if one fails to install it doesn't effect the others
    {_PluginProviders, State2} =
        lists:foldl(fun(Plugin, {PluginAcc, StateAcc}) ->
                            {NewPlugins, NewState} = handle_plugin(Profile, Plugin, StateAcc, Upgrade),
                            NewState1 = epm_state:create_logic_providers(NewPlugins, NewState),
                            {PluginAcc++NewPlugins, NewState1}
                      end, {[], State1}, Plugins),

    %% reset deps dir
    State3 = epm_state:set(State2, deps_dir, DepsDir),
    epm_state:lock(State3, Locks).

handle_plugin(Profile, Plugin, State, Upgrade) ->
    try
        {Apps, State2} = epm_prv_install_deps:handle_deps_as_profile(Profile, State, [Plugin], Upgrade),
        {no_cycle, Sorted} = epm_prv_install_deps:find_cycles(Apps),
        ToBuild = epm_prv_install_deps:cull_compile(Sorted, []),

        %% Add already built plugin deps to the code path
        ToBuildPaths = [epm_app_info:ebin_dir(A) || A <- ToBuild],
        PreBuiltPaths = [Ebin || A <- Apps,
                                 Ebin <- [epm_app_info:ebin_dir(A)],
                                 not lists:member(Ebin, ToBuildPaths)],
        code:add_pathsa(PreBuiltPaths),

        %% Build plugin and its deps
        build_plugins(ToBuild, Apps, State2),

        %% Add newly built deps and plugin to code path
        State3 = epm_state:update_all_plugin_deps(State2, Apps),
        NewCodePaths = [epm_app_info:ebin_dir(A) || A <- ToBuild],

        %% Store plugin code paths so we can remove them when compiling project apps
        State4 = epm_state:update_code_paths(State3, all_plugin_deps, PreBuiltPaths++NewCodePaths),
        epm_paths:set_paths([plugins], State4),

        {plugin_providers(Plugin), State4}
    catch
        ?WITH_STACKTRACE(C,T,S)
            ?DEBUG("~p ~p ~p", [C, T, S]),
            ?WARN("Errors loading plugin ~p. Run epm with DEBUG=1 set to see errors.", [Plugin]),
            {[], State}
    end.

build_plugins(MustBuildApps, AllApps, State) ->
    State1 = epm_state:deps_to_build(State, MustBuildApps),
    State2 = epm_state:all_deps(State1, AllApps),
    State3 = epm_state:set(State2, deps_dir, ?DEFAULT_PLUGINS_DIR),
    {Args, Extra} = epm_state:command_parsed_args(State),
    State4 = epm_state:command_parsed_args(State3, {[{deps_only, true}|Args], Extra}),
    epm_prv_compile:do(State4),
    ok.

plugin_providers({Plugin, _, _, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers({Plugin, _, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers({Plugin, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers(Plugin) when is_atom(Plugin) ->
    validate_plugin(Plugin).

validate_plugin(Plugin) ->
    _ = application:load(Plugin),
    case application:get_env(Plugin, providers) of
        {ok, Providers} ->
            Providers;
        undefined ->
            Exports = Plugin:module_info(exports),
            case lists:member({init,1}, Exports) of
                false ->
                    ?WARN("Plugin ~p does not export init/1. It will not be used.", [Plugin]),
                    [];
                true ->
                    [Plugin]
            end
    end.

