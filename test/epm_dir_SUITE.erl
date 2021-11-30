-module(epm_dir_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([default_src_dirs/1, default_extra_src_dirs/1, default_all_src_dirs/1
    ,src_dirs/1, alt_src_dir_nested/1, src_dirs_with_opts/1, extra_src_dirs/1, all_src_dirs/1
    ,src_dir_opts/1, recursive/1
    ,top_src_dirs/1
    ,profile_src_dirs/1, profile_extra_src_dirs/1, profile_all_src_dirs/1
    ,profile_src_dir_opts/1
    ,retarget_path/1, alt_base_dir_abs/1, alt_base_dir_env_variable_abs/1, alt_base_dir_rel/1
    ,global_cache_dir/1, default_global_cache_dir/1, overwrite_default_global_cache_dir/1
    ,default_global_config/1, overwrite_default_global_config/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


all() -> [default_src_dirs, default_extra_src_dirs, default_all_src_dirs,
          src_dirs, alt_src_dir_nested, extra_src_dirs, all_src_dirs, src_dir_opts, recursive,
          profile_src_dirs, profile_extra_src_dirs, profile_all_src_dirs,
          profile_src_dir_opts, top_src_dirs,
          retarget_path, alt_base_dir_abs, alt_base_dir_env_variable_abs, alt_base_dir_rel,
          global_cache_dir, default_global_cache_dir, overwrite_default_global_cache_dir,
          default_global_config, overwrite_default_global_config].

init_per_testcase(default_global_cache_dir, Config) ->
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = epm_test_utils:init_epm_state(Config),
    NewState = epm_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(overwrite_default_global_cache_dir, Config) ->
    os:putenv("EPM_CACHE_DIR", ?config(priv_dir, Config)),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = epm_test_utils:init_epm_state(Config),
    NewState = epm_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(default_global_config, Config) ->
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = epm_test_utils:init_epm_state(Config),
    NewState = epm_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(overwrite_default_global_config, Config) ->
    ConfDir = filename:join([?config(priv_dir, Config), "custom"]),
    ok = file:make_dir(ConfDir),
    os:putenv("EPM_GLOBAL_CONFIG_DIR", ConfDir),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = epm_test_utils:init_epm_state(Config),
    NewState = epm_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(_, Config) ->
    C = epm_test_utils:init_epm_state(Config),
    AppDir = ?config(apps, C),

    Name1 = epm_test_utils:create_random_name("app1_"),
    Vsn1 = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = epm_test_utils:create_random_name("app2_"),
    Vsn2 = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2, [kernel, stdlib]),

    [{app_one, Name1}, {app_two, Name2}] ++ C.

end_per_testcase(_, _Config) -> ok.

default_src_dirs(Config) ->
    {ok, State} = epm_test_utils:run_and_check(Config, [], ["compile"], return),

    [] = epm_dir:src_dirs(epm_state:opts(State)),
    ["src"] = epm_dir:src_dirs(epm_state:opts(State), ["src"]).

default_extra_src_dirs(Config) ->
    {ok, State} = epm_test_utils:run_and_check(Config, [], ["compile"], return),

    [] = epm_dir:extra_src_dirs(epm_state:opts(State)),
    ["src"] = epm_dir:extra_src_dirs(epm_state:opts(State), ["src"]).

default_all_src_dirs(Config) ->
    {ok, State} = epm_test_utils:run_and_check(Config, [], ["compile"], return),

    [] = epm_dir:all_src_dirs(epm_state:opts(State)),
    ["src", "test"] = epm_dir:all_src_dirs(epm_state:opts(State), ["src"], ["test"]).

src_dirs(Config) ->
    EpmConfig = [{erl_opts, [{src_dirs, ["foo", "./bar", "bar", "bar/", "./bar/", "baz",
                                           "./", ".", "../", "..", "./../", "../.", ".././../"]}]}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    [".", "..", "../..", "bar", "baz", "foo"] = epm_dir:src_dirs(epm_state:opts(State)).

alt_src_dir_nested(Config) ->
    EpmConfig = [{src_dirs, ["src", "alt/nested"]}],
    AppsDir = ?config(apps, Config),
    Name1 = ?config(app_one, Config),
    ModDir = filename:join([AppsDir, "apps", Name1, "alt", "nested"]),
    Mod = "-module(altmod). -export([main/0]). main() -> ok.",

    ec_file:mkdir_path(ModDir),
    ok = file:write_file(filename:join([ModDir, "altmod.erl"]), Mod),

    Ebin = filename:join([AppsDir, "_build", "default", "lib", Name1, "ebin", "altmod.beam"]),
    {ok, State} = epm_test_utils:run_and_check(
           Config, EpmConfig, ["compile"],
           {ok, [{file, Ebin}]}
    ),
    ["alt/nested", "src"] = epm_dir:src_dirs(epm_state:opts(State)).

src_dirs_with_opts(Config) ->
    EpmConfig = [{erl_opts, [{src_dirs, ["foo", "bar", "baz"]},
                               {src_dirs, [{"foo",[{recursive,false}]}, "qux"]}]}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    ["bar", "baz", "foo", "qux"] = epm_dir:src_dirs(epm_state:opts(State)).

extra_src_dirs(Config) ->
    EpmConfig = [{erl_opts, [{extra_src_dirs, ["foo", "bar", "baz"]}]}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    ["bar", "baz", "foo"] = epm_dir:extra_src_dirs(epm_state:opts(State)).

all_src_dirs(Config) ->
    EpmConfig = [{erl_opts, [{src_dirs, ["foo", "bar"]}, {extra_src_dirs, ["baz", "qux"]}, {src_dirs, [{"foo", [{recursive,false}]}]}]}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    ["bar", "baz", "foo", "qux"] = epm_dir:all_src_dirs(epm_state:opts(State)).

src_dir_opts(Config) ->
    EpmConfig =
        [{erl_opts, [{src_dirs, [{"foo",[{recursive,true}]}, "bar"]},
                     {extra_src_dirs, ["baz", {"foo", [{recursive,false}]}]},
                     {src_dirs, [{"foo", [{recursive,false}]}]}]}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig,
                                                 ["compile"], return),
    [{recursive,true}] = epm_dir:src_dir_opts(epm_state:opts(State), "foo"),
    [] = epm_dir:src_dir_opts(epm_state:opts(State), "bar"),
    [] = epm_dir:src_dir_opts(epm_state:opts(State), "nonexisting").

recursive(Config) ->
    EpmConfig1 =
        [{erl_opts, [{src_dirs, ["foo", "bar"]},
                     {extra_src_dirs, ["baz", {"foo", [{recursive,true}]}]},
                     {src_dirs, [{"foo", [{recursive,false}]}]}]}],
    {ok, State1} = epm_test_utils:run_and_check(Config, EpmConfig1,
                                                 ["compile"], return),
    false = epm_dir:recursive(epm_state:opts(State1), "foo"),
    true = epm_dir:recursive(epm_state:opts(State1), "bar"),

    EpmConfig2 = [{erlc_compiler,[{recursive,false}]},
                    {erl_opts,[{src_dirs,["foo",{"bar",[{recursive,true}]}]}]}],
    {ok, State2} = epm_test_utils:run_and_check(Config, EpmConfig2,
                                                 ["compile"], return),
    false = epm_dir:recursive(epm_state:opts(State2), "foo"),
    true = epm_dir:recursive(epm_state:opts(State2), "bar"),

    ok.

top_src_dirs(Config) ->
    %% We can get the same result out of specifying src_dirs from the config root,
    %% not just the erl_opts
    EpmConfig = [{src_dirs, ["foo", "./bar", "bar", "bar/", "./bar/", "baz",
                               "./", ".", "../", "..", "./../", "../.", ".././../"]}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    [".", "..", "../..", "bar", "baz", "foo"] = epm_dir:src_dirs(epm_state:opts(State)).

profile_src_dirs(Config) ->
    EpmConfig = [
        {erl_opts, [{src_dirs, ["foo", "bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, ["baz", "qux"]}]}]}
        ]}
    ],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = epm_dir:src_dirs(epm_state:opts(State)).

profile_extra_src_dirs(Config) ->
    EpmConfig = [
        {erl_opts, [{extra_src_dirs, ["foo", "bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{extra_src_dirs, ["baz", "qux"]}]}]}
        ]}
    ],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = epm_dir:extra_src_dirs(epm_state:opts(State)).

profile_all_src_dirs(Config) ->
    EpmConfig = [
        {erl_opts, [{src_dirs, ["foo"]}, {extra_src_dirs, ["bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, ["baz"]}, {extra_src_dirs, ["qux"]}]}]}
        ]}
    ],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = epm_dir:all_src_dirs(epm_state:opts(State)).

profile_src_dir_opts(Config) ->
    EpmConfig = [
        {erl_opts, [{src_dirs, ["foo"]},
                    {extra_src_dirs, [{"bar",[recursive]}]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, [{"bar",[{recursive,false}]}]},
                                {extra_src_dirs, ["qux"]}]}]}
        ]}
    ],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig,
                                                 ["as", "more", "compile"],
                                                 return),

    [{recursive,false}] = epm_dir:src_dir_opts(epm_state:opts(State),"bar"),

    {ok, State1} = epm_test_utils:run_and_check(Config, EpmConfig,
                                                 ["compile"], return),

    [{recursive,true}] = epm_dir:src_dir_opts(epm_state:opts(State1),"bar").

retarget_path(Config) ->
    {ok, State} = epm_test_utils:run_and_check(Config, [], ["compile"], return),

    BaseDir = epm_dir:base_dir(State),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assertEqual(filename:join([BaseDir, "lib", Name1, "test"]),
                 epm_dir:retarget_path(State, filename:join([epm_dir:root_dir(State), "apps", Name1, "test"]))),
    ?assertEqual(filename:join([BaseDir, "lib", Name2, "test"]),
                 epm_dir:retarget_path(State, filename:join([epm_dir:root_dir(State), "apps", Name2, "test"]))),
    ?assertEqual(filename:join([BaseDir, "lib", Name1, "more_test"]),
                 epm_dir:retarget_path(State, filename:join([epm_dir:root_dir(State), "apps", Name1, "more_test"]))),
    ?assertEqual(filename:join([BaseDir, "test"]),
                 epm_dir:retarget_path(State, filename:join([epm_dir:root_dir(State), "test"]))),
    ?assertEqual(filename:join([BaseDir, "some_other_dir"]),
                 epm_dir:retarget_path(State, filename:join([epm_dir:root_dir(State), "some_other_dir"]))),
    ?assertEqual("/somewhere/outside/the/project",
                 epm_dir:retarget_path(State, "/somewhere/outside/the/project")).

alt_base_dir_abs(Config) ->
    AltName = lists:flatten(io_lib:format("~p", [os:timestamp()])),
    AltBaseDir = filename:join(?config(priv_dir, Config), AltName),
    EpmConfig = [{base_dir, AltBaseDir}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    BaseDir = epm_dir:base_dir(State),
    ?assertEqual(filename:join(AltBaseDir, "default"), BaseDir),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name1, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".beam"]))),
    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name2, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".beam"]))).

alt_base_dir_env_variable_abs(Config) ->
    AltName = lists:flatten(io_lib:format("~p", [os:timestamp()])),
    AltBaseDir = filename:join(?config(priv_dir, Config), AltName),
    EpmConfig = [],

    true = os:putenv("EPM_BASE_DIR", AltBaseDir),
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),
    true = os:unsetenv("EPM_BASE_DIR"),

    BaseDir = epm_dir:base_dir(State),
    ?assertEqual(filename:join(AltBaseDir, "default"), BaseDir),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name1, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".beam"]))),
    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name2, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".beam"]))).

alt_base_dir_rel(Config) ->
    AltName = lists:flatten(io_lib:format("~p", [os:timestamp()])),
    AltBaseDir = filename:join("..", AltName),
    EpmConfig = [{base_dir, AltBaseDir}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),

    BaseDir = epm_dir:base_dir(State),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name1, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".beam"]))),
    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name2, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".beam"]))).

global_cache_dir(Config) ->
    EpmConfig = [{erl_opts, []}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),
    DataDir = ?config(priv_dir, Config),
    Expected = filename:join([DataDir, "cache"]),
    ?assertEqual(Expected, epm_dir:global_cache_dir(epm_state:opts(State))).

default_global_cache_dir(Config) ->
    EpmConfig = [{erl_opts, []}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),
    Expected = filename:join([epm_dir:home_dir(), ".cache", "epm"]),
    ?assertEqual(Expected, epm_dir:global_cache_dir(epm_state:opts(State))).

overwrite_default_global_cache_dir(Config) ->
    EpmConfig = [{erl_opts, []}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),
    Expected = ?config(priv_dir, Config),
    ?assertEqual(Expected, epm_dir:global_cache_dir(epm_state:opts(State))).

default_global_config(Config) ->
    EpmConfig = [{erl_opts, []}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),
    ConfDir = ?config(priv_dir, Config),
    Expected = filename:join([ConfDir, ".config", "epm", "epm.config"]),
    ?assertEqual(Expected, epm_dir:global_config(State)).

overwrite_default_global_config(Config) ->
    EpmConfig = [{erl_opts, []}],
    {ok, State} = epm_test_utils:run_and_check(Config, EpmConfig, ["compile"], return),
    Expected = filename:join([os:getenv("EPM_GLOBAL_CONFIG_DIR"), ".config", "epm", "epm.config"]),
    epm_dir:global_config(State),
    ?assertEqual(Expected, epm_dir:global_config(State)).
