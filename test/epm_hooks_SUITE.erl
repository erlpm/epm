-module(epm_hooks_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    meck:unload().

init_per_testcase(_, Config) ->
    epm_test_utils:init_epm_state(Config).

end_per_testcase(_, _Config) ->
    catch meck:unload().

all() ->
    [build_and_clean_app, run_hooks_once, run_hooks_once_profiles,
     escriptize_artifacts, run_hooks_for_plugins, deps_hook_namespace,
     bare_compile_hooks_default_ns, deps_clean_hook_namespace, eunit_app_hooks,
     sub_app_hooks, root_hooks, drop_hook_args].

%% Test post provider hook cleans compiled project app, leaving it invalid
build_and_clean_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name, valid}]}),
    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [{provider_hooks, [{post, [{compile, clean}]}]}]),
    {ok, RConf} = file:consult(RConfFile),

    epm_test_utils:run_and_check(Config, RConf,
                                  ["compile"], {ok, [{app, Name, invalid}]}).

escriptize_artifacts(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Artifact = "{{profile_dir}}/bin/"++Name,
    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [
                                       {escript_name, list_to_atom(Name)}
                                       ,{artifacts, [Artifact]}
                                       ]),
    {ok, RConf} = file:consult(RConfFile),

    try epm_test_utils:run_and_check(Config, RConf, ["compile"], return)
    catch
        {error,
         {epm_prv_compile,
          {missing_artifact, Artifact}}} ->
            ok
    end,
    RConfFile1 =
        epm_test_utils:create_config(AppDir,
                                       [
                                       {escript_name, list_to_atom(Name)}
                                       ,{artifacts, [Artifact]}
                                       ,{provider_hooks, [{post, [{compile, escriptize}]}]}
                                       ]),
    {ok, RConf1} = file:consult(RConfFile1),

    epm_test_utils:run_and_check(Config, RConf1,
                                  ["compile"], {ok, [{app, Name, valid}
                                                    ,{file, filename:join([AppDir, "_build/default/bin", Name])}]}).

run_hooks_once(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    EpmConfig = [{pre_hooks, [{compile, "mkdir  \"$EPM_ROOT_DIR/blah\""}]}],
    epm_test_utils:create_config(AppDir, EpmConfig),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], {ok, [{app, Name, valid}]}).

%% test that even if a hook is defined at the project level in a used profile
%% the hook is not run for each application in the project umbrella
run_hooks_once_profiles(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    EpmConfig = [{profiles, [{hooks, [{pre_hooks, [{compile, "mkdir \"$EPM_ROOT_DIR/blah\""}]}]}]}],
    epm_test_utils:create_config(AppDir, EpmConfig),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:run_and_check(Config, EpmConfig, ["as", "hooks", "compile"], {ok, [{app, Name, valid}]}).

deps_hook_namespace(Config) ->
    mock_git_resource:mock([{deps, [{some_dep, "0.0.1"}]}]),
    Deps = epm_test_utils:expand_deps(git, [{"some_dep", "0.0.1", []}]),
    TopDeps = epm_test_utils:top_level_deps(Deps),

    EpmConfig = [
        {deps, TopDeps},
        {overrides, [
            {override, some_dep, [
                {provider_hooks, [
                    {pre, [
                        {compile, clean}
                    ]}
                ]}
            ]}
        ]}
    ],
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["compile"],
        {ok, [{dep, "some_dep"}]}
    ).

%% tests that hooks to compile when running bare compile run in the default namespace
bare_compile_hooks_default_ns(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    HookFile = filename:join([?config(priv_dir, Config), "bare-post.hook"]),

    Cmd = case os:type() of
        {win32, _} -> "dir";
        _ -> "ls"
    end,
    ConfOpts = [{provider_hooks, [{post, [{compile, clean}]}]},
                {post_hooks, [{compile, Cmd ++ " > " ++ HookFile}]}],
    RConfFile = epm_test_utils:create_config(AppDir, ConfOpts),
    {ok, RConf} = file:consult(RConfFile),
    epm_test_utils:run_and_check(
        Config, RConf, ["bare", "compile", "--paths", "."],
        {ok, []}
    ),
    %% check that hooks did actually run
    ?assertMatch({ok, _}, file:read_file(HookFile)),
    ok.

deps_clean_hook_namespace(Config) ->
    mock_git_resource:mock([{deps, [{some_dep, "0.0.1"}]}]),
    Deps = epm_test_utils:expand_deps(git, [{"some_dep", "0.0.1", []}]),
    TopDeps = epm_test_utils:top_level_deps(Deps),

    EpmConfig = [
        {deps, TopDeps},
        {overrides, [
            {override, some_dep, [
                {provider_hooks, [
                    {pre, [
                        {compile, clean}
                    ]}
                ]}
            ]}
        ]}
    ],
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["clean"],
        {ok, [{dep, "some_dep"}]}
    ).

%% Checks that a hook that is defined on an app (not a top level hook of a project with subapps) is run
eunit_app_hooks(Config) ->
    AppDir = ?config(apps, Config),
    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [
                                       {escript_name, list_to_atom(Name)}
                                       ,{provider_hooks, [{post, [{eunit, escriptize}]}]}
                                       ]),
    {ok, RConf} = file:consult(RConfFile),

    epm_test_utils:run_and_check(Config, RConf,
                                  ["eunit"], {ok, [{app, Name, valid}
                                               ,{file, filename:join([AppDir, "_build/test/bin", Name])}]}).

run_hooks_for_plugins(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),

    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    PluginName = epm_test_utils:create_random_name("plugin1_"),
    mock_git_resource:mock([{config, [{pre_hooks, [{compile, "echo whatsup > randomfile"}]}]}]),

    RConfFile = epm_test_utils:create_config(AppDir,
                                               [{plugins, [
                                                          {list_to_atom(PluginName),
                                                          {git, "http://site.com/user/"++PluginName++".git",
                                                          {tag, Vsn}}}
                                                          ]}]),
    {ok, RConf} = file:consult(RConfFile),

    epm_test_utils:run_and_check(Config, RConf, ["compile"], {ok, [{app, Name, valid},
                                                                     {plugin, PluginName},
                                                                     {file, filename:join([AppDir, "_build", "default", "plugins", PluginName, "randomfile"])}]}).

%% test that a subapp of a project keeps its hooks
sub_app_hooks(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("sub_app1_"),
    Vsn = epm_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, "apps", Name]),

    epm_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:create_config(SubAppsDir, [{provider_hooks, [{post, [{compile, clean}]}]}]),

    RConfFile = epm_test_utils:create_config(AppDir, []),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
      Config, RConf, ["compile"],
      {ok, [{app, Name, invalid}]}
     ).

%% test that hooks at the top level don't run in the subapps
root_hooks(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("sub_app1_"),
    Vsn = epm_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, "apps", Name]),

    epm_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:create_config(SubAppsDir, [{provider_hooks, [{post, [{compile, clean}]}]}]),

    RConfFile = epm_test_utils:create_config(AppDir, [{pre_hooks, [{compile, "mkdir \"$EPM_ROOT_DIR/blah\""}]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
      Config, RConf, ["compile"],
      {ok, [{app, Name, invalid}]}
     ).

drop_hook_args(Config) ->
    EpmConfig = [
        {provider_hooks, [
            {pre, [{eunit, path}]}
        ]}
    ],
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["eunit", "--cover=false"],
        {ok, []}
    ).
