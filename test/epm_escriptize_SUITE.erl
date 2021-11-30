-module(epm_escriptize_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         escriptize_with_name/1,
         escriptize_with_bad_name/1,
         escriptize_with_bad_dep/1,
         build_and_clean_app/1,
         escriptize_with_ebin_subdir/1]).

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

all() ->
    [
     build_and_clean_app,
     escriptize_with_name,
     escriptize_with_bad_name,
     escriptize_with_bad_dep,
     escriptize_with_ebin_subdir
    ].

%% Test escriptize builds and runs the app's escript
build_and_clean_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:run_and_check(Config, [], ["escriptize"],
                                   {ok, [{app, Name, valid}]}).

escriptize_with_name(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:run_and_check(Config, [{escript_main_app, Name}], ["escriptize"],
                                   {ok, [{app, Name, valid}]}).

escriptize_with_bad_name(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    epm_test_utils:run_and_check(Config, [{escript_main_app, boogers}], ["escriptize"],
                                   {error,{epm_prv_escriptize, {bad_name, boogers}}}).

escriptize_with_bad_dep(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib, boogers]),
    epm_test_utils:run_and_check(Config, [{escript_main_app, Name}], ["escriptize"],
                                   {error,{epm_prv_escriptize, {bad_app, boogers}}}).

escriptize_with_ebin_subdir(Config) ->
    AppDir = ?config(apps, Config),
    Name = epm_test_utils:create_random_name("app1_"),
    Vsn = epm_test_utils:create_random_vsn(),

    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    filelib:ensure_dir(filename:join([AppDir, "ebin", "subdir", "subdirfile"])),

    %% To work, this test must run from the AppDir itself. To avoid breaking
    %% other tests, be careful with cwd
    Cwd = file:get_cwd(),
    try
        file:set_cwd(AppDir),
        {ok, _} = epm:run(epm_state:new(?config(state,Config), [], AppDir),
                             ["escriptize"])
    after
        file:set_cwd(Cwd) % reset always
    end,
    ok.
