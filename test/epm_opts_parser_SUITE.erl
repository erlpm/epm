-module(epm_opts_parser_SUITE).

-export([all/0, init_per_testcase/2
    ,bad_arg_to_flag/1, missing_arg_to_flag/1]).

-include_lib("common_test/include/ct.hrl").


all() -> [bad_arg_to_flag, missing_arg_to_flag].

init_per_testcase(_, Config) ->
    epm_test_utils:init_epm_state(Config, "opts_parser_").

bad_arg_to_flag(Config) ->
    ok = meck:new(getopt),
    ok = meck:expect(getopt,
                     parse,
                     fun(_, _) -> {error, {invalid_option_arg, {foo, "null"}}} end),

    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("bad_arg_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    {error, Error} = epm_test_utils:run_and_check(Config,
                                                    [],
                                                    ["compile", "--foo=null"],
                                                    return),

    true = meck:validate(getopt),
    ok = meck:unload(getopt),

    "Invalid argument null to option foo" = lists:flatten(Error).

missing_arg_to_flag(Config) ->
    ok = meck:new(getopt),
    ok = meck:expect(getopt, parse, fun(_, _) -> {error, {missing_option_arg, foo}} end),

    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("missing_arg_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    {error, Error} = epm_test_utils:run_and_check(Config,
                                                    [],
                                                    ["compile", "--foo"],
                                                    return),

    true = meck:validate(getopt),
    ok = meck:unload(getopt),

    "Missing argument to option foo" = lists:flatten(Error).
