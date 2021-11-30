-module(epm_plugins_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         compile_plugins/1,
         compile_global_plugins/1,
         complex_plugins/1,
         list/1,
         upgrade/1,
         upgrade_project_plugin/1,
         upgrade_no_args/1,
         sub_app_plugins/1,
         sub_app_plugin_overrides/1,
         project_plugins/1,
         use_checkout_plugins/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    epm_test_utils:init_epm_state(Config).

end_per_testcase(_, _Config) ->
    catch meck:unload().

all() ->
    [compile_plugins, compile_global_plugins, complex_plugins, list, upgrade, upgrade_project_plugin,
     sub_app_plugins, sub_app_plugin_overrides, project_plugins, use_checkout_plugins].

%% Tests that compiling a project installs and compiles the plugins of deps
compile_plugins(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = epm_test_utils:create_random_name("dep1_"),
    PluginName = epm_test_utils:create_random_name("plugin1_"),

    Plugins = epm_test_utils:expand_deps(git, [{PluginName, Vsn, []}]),
    {SrcDeps, _} = epm_test_utils:flat_deps(Plugins),
    mock_git_resource:mock([{deps, SrcDeps}]),

    mock_pkg_resource:mock([{pkgdeps, [{{list_to_binary(DepName), list_to_binary(Vsn)}, []}]},
                            {config, [{plugins, [
                                                {list_to_atom(PluginName),
                                                 {git, "http://site.com/user/"++PluginName++".git",
                                                 {tag, Vsn}}}]}]}]),

    RConfFile =
        epm_test_utils:create_config(AppDir,
                                      [{deps, [
                                              list_to_atom(DepName)
                                              ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {plugin, PluginName}, {dep, DepName}]}
    ).

%% Tests that compiling a project installs and compiles the global plugins
compile_global_plugins(Config) ->
    AppDir = ?config(apps, Config),
    GlobalDir = filename:join(AppDir, "global"),
    GlobalConfigDir = filename:join([GlobalDir, ".config", "epm"]),
    GlobalConfig = filename:join([GlobalDir, ".config", "epm", "epm.config"]),

    meck:new(epm_dir, [passthrough]),
    meck:expect(epm_dir, global_config, fun() -> GlobalConfig end),
    meck:expect(epm_dir, global_cache_dir, fun(_) -> GlobalDir end),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    Vsn2 = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = epm_test_utils:create_random_name("dep1_"),
    PluginName = epm_test_utils:create_random_name("plugin1_"),

    mock_git_resource:mock([{deps, [{list_to_atom(PluginName), Vsn},
                                    {list_to_atom(PluginName), Vsn2},
                                    {{iolist_to_binary(DepName), iolist_to_binary(Vsn)}, []}]}]),


    epm_test_utils:create_config(GlobalConfigDir,
                                   [{plugins, [
                                              {list_to_atom(PluginName), {git, "http://site.com/user/"++PluginName++".git", {tag, Vsn}}}
                                              ]}]),
    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [{deps, [
                                               {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}}
                                               ]},
                                       {plugins, [
                                                 {list_to_atom(PluginName), {git, "http://site.com/user/"++PluginName++".git", {tag, Vsn2}}}
                                                 ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Runs global plugin install
    epm:init_config(),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name},
             {global_plugin, PluginName, Vsn},
             {plugin, PluginName, Vsn2},
             {dep, DepName}]}
     ),

    meck:unload(epm_dir).

%% Tests installing of plugin with transitive deps
complex_plugins(Config) ->
    AppDir = ?config(apps, Config),

    meck:new(epm_dir, [passthrough]),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    Vsn2 = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = epm_test_utils:create_random_name("dep1_"),
    DepName2 = epm_test_utils:create_random_name("dep2_"),
    DepName3 = epm_test_utils:create_random_name("dep3_"),
    PluginName = epm_test_utils:create_random_name("plugin1_"),

    Deps = epm_test_utils:expand_deps(git, [{PluginName, Vsn2, [{DepName2, Vsn,
                                                                  [{DepName3, Vsn, []}]}]}
                                             ,{DepName, Vsn, []}]),
    {SrcDeps, _} = epm_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [{deps, [
                                               {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}}
                                               ]},
                                       {plugins, [
                                                 {list_to_atom(PluginName), {git, "http://site.com/user/"++PluginName++".git", {tag, Vsn2}}}
                                                 ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name},
              {plugin, PluginName, Vsn2},
              {plugin, DepName2},
              {plugin, DepName3},
              {dep, DepName}]}
     ),

    meck:unload(epm_dir).

list(Config) ->
    epm_test_utils:run_and_check(
        Config, [], ["plugins", "list"],
        {ok, []}
     ).

upgrade(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    PkgName = epm_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]}
    ]),

    %% beam file to verify plugin is acutally compiled
    PluginBeam = filename:join([AppDir, "_build", "default", "plugins",
                                PkgName, "ebin", [PkgName, ".beam"]]),

    RConfFile = epm_test_utils:create_config(AppDir, [{plugins, [list_to_atom(PkgName)]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name, valid}, {file, PluginBeam}, {plugin, PkgName, <<"0.1.1">>}]}
     ),

    catch mock_pkg_resource:unmock(),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.3">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]},
        {upgrade, [PkgName]}
    ]),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["plugins", "upgrade", PkgName],
        {ok, [{app, Name, valid}, {file, PluginBeam}, {plugin, PkgName, <<"0.1.3">>}]}
     ),

    epm_test_utils:run_and_check(
        Config, RConf, ["plugins", "upgrade", "--all"],
        {ok, [{app, Name, valid}, {file, PluginBeam}, {plugin, PkgName}]}
     ).

upgrade_project_plugin(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    PkgName = epm_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]}
    ]),

    RConfFile = epm_test_utils:create_config(AppDir, [{project_plugins, [list_to_atom(PkgName)]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {plugin, PkgName, <<"0.1.1">>}]}
     ),

    catch mock_pkg_resource:unmock(),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.3">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]},
        {upgrade, [PkgName]}
    ]),

    %% Build with deps.
    epm_test_utils:run_and_check(
        Config, RConf, ["plugins", "upgrade", PkgName],
        {ok, [{app, Name}, {plugin, PkgName, <<"0.1.3">>}]}
     ).

upgrade_no_args(Config) ->
    try epm_test_utils:run_and_check(Config, [], ["plugins", "upgrade"], return)
    catch {error, {epm_prv_plugins_upgrade, no_arg}} ->
        ok
    end,
    ok.

sub_app_plugins(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("sub_app1_"),
    Vsn = epm_test_utils:create_random_vsn(),

    DepName = epm_test_utils:create_random_name("dep1_"),
    PluginName = epm_test_utils:create_random_name("plugin1_"),

    mock_pkg_resource:mock([{pkgdeps, [{{list_to_binary(DepName), list_to_binary(Vsn)}, []},
                                       {{list_to_binary(PluginName), list_to_binary(Vsn)}, []}]}]),

    SubAppsDir = filename:join([AppDir, "apps", Name]),

    epm_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:create_config(SubAppsDir, [{deps, [{list_to_binary(DepName), list_to_binary(Vsn)}]},
                                                {plugins, [list_to_atom(PluginName)]}]),

    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [{deps, [
                                               list_to_atom(DepName)
                                               ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
      Config, RConf, ["compile"],
      {ok, [{app, Name}, {dep, DepName}, {plugin, PluginName}]}
     ).

%% Tests that overrides in a dep that includes a plugin are applied to plugin fetching
sub_app_plugin_overrides(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("sub_app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    Dep2Name = epm_test_utils:create_random_name("dep2_"),

    DepName = epm_test_utils:create_random_name("dep1_"),
    PluginName = epm_test_utils:create_random_name("plugin1_"),
    Vsn2 = epm_test_utils:create_random_vsn(),

    Deps = epm_test_utils:expand_deps(git, [{PluginName, Vsn, [{DepName, Vsn, []}]},
                                              {DepName, Vsn, []}]),
    {SrcDeps, _} = epm_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    mock_pkg_resource:mock([{pkgdeps, [{{list_to_binary(Dep2Name), list_to_binary(Vsn)}, []}]},
                            {config, [{plugins, [{list_to_atom(PluginName),
                                                 {git, "http://site.com/user/"++PluginName++".git",
                                                  {tag, Vsn}}}]},
                                      %% Dep2 overrides the plugin's deps to have vsn2 of dep1
                                      {overrides, [{override, list_to_atom(PluginName),
                                                    [{deps, [{list_to_atom(DepName),
                                                              {git, "http://site.com/user/"++DepName++".git",
                                                               {tag, Vsn2}}}]}]}]}]}]),

    SubAppsDir = filename:join([AppDir, "apps", Name]),

    epm_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),

    RConfFile = epm_test_utils:create_config(AppDir, [{deps, [{list_to_binary(Dep2Name), list_to_binary(Vsn)}]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    epm_test_utils:run_and_check(
      Config, RConf, ["compile"],
      {ok, [{app, Name}, {dep, Dep2Name, Vsn}, {plugin, DepName, Vsn2}, {plugin, PluginName}]}
     ).

%% Check that project plugins are first in providers even if they override defaults but that
%% normal plugins do not
project_plugins(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = epm_test_utils:create_random_name("dep1_"),
    PluginName = "compile_plugin",
    PluginName2 = "release",

    Plugins = epm_test_utils:expand_deps(git, [{PluginName, Vsn, []}, {PluginName2, Vsn, []}]),
    {SrcDeps, _} = epm_test_utils:flat_deps(Plugins),
    mock_git_resource:mock([{deps, SrcDeps}], create_plugin),

    mock_pkg_resource:mock([{pkgdeps, [{{list_to_binary(DepName), list_to_binary(Vsn)}, []}]},
                            {config, [{plugins, [
                                                {list_to_atom(PluginName),
                                                 {git, "http://site.com/user/"++PluginName++".git",
                                                 {tag, Vsn}}}]}]}]),

    RConfFile =
        epm_test_utils:create_config(AppDir,
                                      [{deps, [
                                              list_to_atom(DepName)
                                              ]},
                                      {project_plugins, [
                                                        {list_to_atom(PluginName2),
                                                         {git, "http://site.com/user/"++PluginName2++".git",
                                                          {tag, Vsn}}}]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    {ok, State} = epm_test_utils:run_and_check(
                    Config, RConf, ["compile"],
                    {ok, [{app, Name}, {plugin, PluginName}, {plugin, PluginName2}, {dep, DepName}]}
                   ),

    %% Should have 2 release providers but only 1 compile provider
    Release = [P || P <- epm_state:providers(State), providers:impl(P) =:= release, providers:namespace(P) =:= default],
    Compile = [P || P <- epm_state:providers(State), providers:impl(P) =:= compile, providers:namespace(P) =:= default],

    ?assertEqual(length(Release), 2),
    ?assertEqual(length(Compile), 1).

use_checkout_plugins(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    PluginName = "checkedout",
    CheckoutsDir = filename:join(AppDir, "_checkouts/checkedout"),
    epm_test_utils:create_plugin(CheckoutsDir, PluginName, "1.0.0", []),

    RConfFile =
        epm_test_utils:create_config(AppDir,
                                       [{deps, []},
                                        {plugins, [list_to_atom(PluginName)]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Verify we can run the plugin
    ?assertMatch({ok, _}, epm_test_utils:run_and_check(
                            Config, RConf, ["checkedout"],
                            {ok, []}
                           )).
