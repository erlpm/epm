%%% This suite currently only tests for options parsing since we do
%%% not know if epmd will be running to actually boot nodes.
-module(epm_dist_utils_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [from_config, from_cli, overlap, from_config_profile].

init_per_testcase(_, Config0) ->
    Config = epm_test_utils:init_epm_state(Config0),
    AppDir = ?config(apps, Config),
    Name = epm_test_utils:create_random_name("app_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(filename:join([AppDir,"apps",Name]), Name, Vsn, [kernel, stdlib]),
    Config.


end_per_testcase(_, _) ->
    ok.

from_config(Config) ->
    ShortConfig = [{dist_node, [{sname, 'a@localhost'}, {setcookie, abc}]}],
    LongConfig = [{dist_node, [{name, 'a@localhost.x'}, {setcookie, abc}]}],
    BothConfig = [{dist_node, [{sname, 'a@localhost'}, {name, 'a@localhost.x'}, {setcookie,abc}]}],
    NoConfig = [],
    CookieConfig = [{dist_node, [{setcookie, def}]}],
    NoCookie = [{dist_node, [{sname, 'a@localhost'}]}],
    {ok, State0} = epm_test_utils:run_and_check(Config, ShortConfig, ["version"], return),
    {undefined, 'a@localhost', [{setcookie, abc}]} = epm_dist_utils:find_options(State0),
    {ok, State1} = epm_test_utils:run_and_check(Config, LongConfig, ["version"], return),
    {'a@localhost.x', undefined, [{setcookie, abc}]} = epm_dist_utils:find_options(State1),
    %% only support the first name found, side-effect of wanting profile support
    {ok, State2} = epm_test_utils:run_and_check(Config, BothConfig, ["version"], return),
    {undefined, 'a@localhost', [{setcookie, abc}]} = epm_dist_utils:find_options(State2),
    {ok, State3} = epm_test_utils:run_and_check(Config, NoConfig, ["version"], return),
    {undefined, undefined, []} = epm_dist_utils:find_options(State3),
    {ok, State4} = epm_test_utils:run_and_check(Config, CookieConfig, ["version"], return),
    {undefined, undefined, [{setcookie, def}]} = epm_dist_utils:find_options(State4),
    {ok, State5} = epm_test_utils:run_and_check(Config, NoCookie, ["version"], return),
    {undefined, 'a@localhost', []} = epm_dist_utils:find_options(State5),
    ok.

from_cli(Config) ->
    {ok, State0} = epm_test_utils:run_and_check(Config, [], ["version"], return),
    {undefined, undefined, []} = epm_dist_utils:find_options(State0),
    State1 = epm_state:command_parsed_args(State0, {[{sname, 'a@localhost'}, {setcookie,abc}], []}),
    {undefined, 'a@localhost', [{setcookie, abc}]} = epm_dist_utils:find_options(State1),
    State2 = epm_state:command_parsed_args(State0, {[{name, 'a@localhost.x'}, {setcookie,abc}], []}),
    {'a@localhost.x', undefined, [{setcookie, abc}]} = epm_dist_utils:find_options(State2),
    State3 = epm_state:command_parsed_args(State0, {[{sname, 'a@localhost'}, {name, 'a@localhost.x'}, {setcookie,abc}], []}),
    {'a@localhost.x', 'a@localhost', [{setcookie, abc}]} = epm_dist_utils:find_options(State3),
    State4 = epm_state:command_parsed_args(State0, {[{setcookie,def}], []}),
    {undefined, undefined, [{setcookie, def}]} = epm_dist_utils:find_options(State4),
    State5 = epm_state:command_parsed_args(State0, {[{sname, 'a@localhost'}], []}),
    {undefined, 'a@localhost', []} = epm_dist_utils:find_options(State5),
    ok.

overlap(Config) ->
    %% Make sure that CLI config takes over epm config without clash for names, though
    %% cookies can pass through
    EpmConfig = [{dist_node, [{sname, 'a@localhost'}, {setcookie, abc}]}],
    {ok, State0} = epm_test_utils:run_and_check(Config, EpmConfig, ["version"], return),
    State1 = epm_state:command_parsed_args(State0, {[{name, 'b@localhost.x'}], []}),
    {'b@localhost.x', undefined, [{setcookie, abc}]} = epm_dist_utils:find_options(State1),
    ok.

from_config_profile(Config) ->
    %% running as a profile does not create name clashes
    EpmConfig = [{dist_node, [{sname, 'a@localhost'}, {setcookie, abc}]},
                   {profiles, [ {fake, [{dist_node, [{name, 'a@localhost.x'}]}]} ]}],
    {ok, State0} = epm_test_utils:run_and_check(Config, EpmConfig, ["as","fake","version"], return),
    {'a@localhost.x', undefined, [{setcookie, abc}]} = epm_dist_utils:find_options(State0),
    ok.
