-module(epm_utils_SUITE).

-export([all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         empty_arglist/1,
         single_task/1,
         single_task_with_immediate_comma/1,
         single_task_with_trailing_comma/1,
         multiple_task/1,
         multiple_task_no_spaces/1,
         multiple_task_with_immediate_comma/1,
         multiple_task_with_trailing_comma/1,
         task_with_arg/1,
         task_with_arg_with_immediate_comma/1,
         task_with_arg_with_trailing_comma/1,
         task_with_multiple_args/1,
         task_with_flag/1,
         task_with_flag_with_immediate_comma/1,
         task_with_flag_with_trailing_comma/1,
         task_with_flag_with_commas/1,
         task_with_multiple_flags/1,
         special_task_do/1,
         valid_otp_version/1,
         valid_old_format_otp_version/1,
         valid_otp_version_equal/1,
         invalid_otp_version/1,
         nonblacklisted_otp_version/1,
         blacklisted_otp_version/1,
         sh_does_not_miss_messages/1,
         tup_merge/1,
         proxy_auth/1,
        is_list_of_strings/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

init_per_testcase(_, Config) ->
    epm_test_utils:init_epm_state(Config).

end_per_testcase(_, _Config) ->
    catch meck:unload().

all() ->
    [{group, args_to_tasks},
     sh_does_not_miss_messages,
     tup_merge,
     proxy_auth, is_list_of_strings].

groups() ->
    [{args_to_tasks, [], [empty_arglist,
                          single_task,
                          single_task_with_immediate_comma,
                          single_task_with_trailing_comma,
                          multiple_task,
                          multiple_task_no_spaces,
                          multiple_task_with_immediate_comma,
                          multiple_task_with_trailing_comma,
                          task_with_arg,
                          task_with_arg_with_immediate_comma,
                          task_with_arg_with_trailing_comma,
                          task_with_multiple_args,
                          task_with_flag,
                          task_with_flag_with_immediate_comma,
                          task_with_flag_with_trailing_comma,
                          task_with_flag_with_commas,
                          task_with_multiple_flags,
                          special_task_do,
                          valid_otp_version,
                          valid_old_format_otp_version,
                          valid_otp_version_equal,
                          invalid_otp_version,
                          nonblacklisted_otp_version,
                          blacklisted_otp_version
    ]}].

init_per_group(_, Config) -> Config.
end_per_group(_, Config) -> Config.

empty_arglist(_Config) ->
    [] = epm_utils:args_to_tasks([]).

single_task(_Config) ->
    [{"foo", []}] = epm_utils:args_to_tasks(["foo"]).

single_task_with_immediate_comma(_Config) ->
    [{"foo", []}] = epm_utils:args_to_tasks(["foo,"]).

single_task_with_trailing_comma(_Config) ->
    [{"foo", []}] = epm_utils:args_to_tasks(["foo", ","]).

multiple_task(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = epm_utils:args_to_tasks(["foo,",
                                                                         "bar,",
                                                                         "baz"]).

multiple_task_no_spaces(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = epm_utils:args_to_tasks(["foo,bar,baz"]).

multiple_task_with_immediate_comma(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = epm_utils:args_to_tasks(["foo,",
                                                                         "bar,",
                                                                         "baz,"]).

multiple_task_with_trailing_comma(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = epm_utils:args_to_tasks(["foo",
                                                                         ",",
                                                                         "bar",
                                                                         ",",
                                                                         "baz",
                                                                         ","]).
task_with_arg(_Config) ->
    [{"foo", ["bar"]}] = epm_utils:args_to_tasks(["foo", "bar"]).

task_with_arg_with_immediate_comma(_Config) ->
    [{"foo", ["bar"]}, {"baz", []}] = epm_utils:args_to_tasks(["foo", "bar,", "baz"]).

task_with_arg_with_trailing_comma(_Config) ->
    [{"foo", ["bar"]}, {"baz", []}] = epm_utils:args_to_tasks(["foo", "bar", ",", "baz"]).

task_with_multiple_args(_Config) ->
    [{"foo", ["bar", "baz"]}] = epm_utils:args_to_tasks(["foo", "bar", "baz"]).

task_with_flag(_Config) ->
    [{"foo", ["--bar"]}] = epm_utils:args_to_tasks(["foo", "--bar"]).

task_with_flag_with_immediate_comma(_Config) ->
    [{"foo", ["--bar"]}, {"baz", []}] = epm_utils:args_to_tasks(["foo", "--bar,", "baz"]).

task_with_flag_with_trailing_comma(_Config) ->
    [{"foo", ["--bar"]}, {"baz", []}] = epm_utils:args_to_tasks(["foo", "--bar", ",", "baz"]).

task_with_flag_with_commas(_Config) ->
    [{"foo", ["--bar=baz,qux"]}] = epm_utils:args_to_tasks(["foo", "--bar=baz,qux"]).

task_with_multiple_flags(_Config) ->
    [{"foo", ["--bar", "--baz"]}] = epm_utils:args_to_tasks(["foo", "--bar", "--baz"]).

special_task_do(_Config) ->
    [{"foo", []}, {"do", ["bar,", "baz"]}] = epm_utils:args_to_tasks(["foo,",
                                                                        "do",
                                                                        "bar,",
                                                                        "baz"]).

valid_otp_version(_Config) ->
    meck:new(epm_utils, [passthrough]),
    meck:expect(epm_utils, otp_release, fun() -> "42.4" end),
    epm_utils:check_min_otp_version("42.3"),
    meck:unload(epm_utils).

valid_old_format_otp_version(_Config) ->
    meck:new(epm_utils, [passthrough]),
    meck:expect(epm_utils, otp_release, fun() -> "R15B03-1" end),
    epm_utils:check_min_otp_version("14"),

    meck:expect(epm_utils, otp_release, fun() -> "R16B03" end),
    epm_utils:check_min_otp_version("16.0"),

    meck:expect(epm_utils, otp_release, fun() -> "18.0.1" end),
    epm_utils:check_min_otp_version("17.5.4"),

    meck:expect(epm_utils, otp_release, fun() -> "18.0-rc1" end),
    ?assertException(throw, epm_abort, epm_utils:check_min_otp_version("19")),

    meck:unload(epm_utils).

valid_otp_version_equal(_Config) ->
    meck:new(epm_utils, [passthrough]),
    meck:expect(epm_utils, otp_release, fun() -> "42.3" end),
    epm_utils:check_min_otp_version("42.3"),
    meck:unload(epm_utils).

invalid_otp_version(_Config) ->
    meck:new(epm_utils, [passthrough]),
    meck:expect(epm_utils, otp_release, fun() -> "17.4" end),
    ?assertException(throw, epm_abort, epm_utils:check_min_otp_version("42.3")),
    meck:unload(epm_utils).

nonblacklisted_otp_version(_Config) ->
    meck:new(epm_utils, [passthrough]),
    meck:expect(epm_utils, otp_release, fun() -> "42.4" end),
    epm_utils:check_blacklisted_otp_versions(["1\\.2", "42\\.3"]),
    meck:unload(epm_utils).

blacklisted_otp_version(_Config) ->
    meck:new(epm_utils, [passthrough]),
    meck:expect(epm_utils, otp_release, fun() -> "42.4" end),
    ?assertException(throw, epm_abort, epm_utils:check_blacklisted_otp_versions(["1\\.2", "42\\.[1-4]"])),
    meck:unload(epm_utils).

sh_does_not_miss_messages(_Config) ->
    Source = "~nmain(_) ->~n io:format(\"donotmissme\").~n",
    file:write_file("do_not_miss_messages", io_lib:format(Source,[])),
    {ok, "donotmissme"} = epm_utils:sh("escript do_not_miss_messages", []),
    AnyMessageRemained =
        receive
            What -> What
        after 100 ->
            false
        end,
    AnyMessageRemained = false.

tup_merge(_Config) ->
    ?assertEqual(
       [a,{a,a},{a,a,a},{a,b},{a,b,b},b,{b,a},{b,a,a},{b,b},{b,b,b},z,{z,a},{z,a,a},{z,b},{z,b,b}],
       epm_utils:tup_umerge(
        epm_utils:tup_sort([a,{a,a},{a,a,a},b,{b,a},{b,a,a},z,{z,a},{z,a,a}]),
        epm_utils:tup_sort([a,{a,b},{a,b,b},b,{b,b},{b,b,b},z,{z,b},{z,b,b}])
       )
    ),
    ?assertEqual(
       [a,{a,b},{a,b,b},{a,a},{a,a,a},b,{b,b},{b,b,b},{b,a},{b,a,a},z,{z,b},{z,b,b},{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([a,{a,b},{a,b,b},b,{b,b},{b,b,b},z,{z,b},{z,b,b}]),
         epm_utils:tup_sort([a,{a,a},{a,a,a},b,{b,a},{b,a,a},z,{z,a},{z,a,a}])
       )
    ),
    ?assertEqual(
       [a,{a,b},{a,b,b},{a,a},{a,a,a},b,{b,b},{b,b,b},{b,a},{b,a,a},z,{z,b},{z,b,b},{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([a,b,z,{a,b},{b,b},{z,b},{a,b,b},{b,b,b},{z,b,b}]),
         epm_utils:tup_sort([a,{a,a},{a,a,a},b,{b,a},{b,a,a},z,{z,a},{z,a,a}])
       )
    ),
    ?assertEqual(
       [{a,b},a,{a,b,b},{a,a},{a,a,a},{b,b},b,{b,b,b},{b,a},{b,a,a},{z,b},z,{z,b,b},{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([{a,b},{b,b},{z,b},a,b,z,{a,b,b},{b,b,b},{z,b,b}]),
         epm_utils:tup_sort([a,{a,a},{a,a,a},b,{b,a},{b,a,a},z,{z,a},{z,a,a}])
       )
    ),
    ?assertEqual(
       [a,{a,b},{a,b,b},{a,a},{a,a,a},b,{b,b},{b,b,b},{b,a},{b,a,a},z,{z,b},{z,b,b},{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([a,{a,b},{a,b,b},b,{b,b},{b,b,b},z,{z,b},{z,b,b}]),
         epm_utils:tup_sort([{a,a},a,{a,a,a},{b,a},b,{b,a,a},{z,a},z,{z,a,a}])
       )
    ),
    ?assertEqual(
       [{a,b},a,{a,b,b},{a,a},{a,a,a},{b,b},b,{b,b,b},{b,a},{b,a,a},{z,b},z,{z,b,b},{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([{a,b},{b,b},{z,b},a,b,z,{a,b,b},{b,b,b},{z,b,b}]),
         epm_utils:tup_sort([{a,a},a,{a,a,a},{b,a},b,{b,a,a},{z,a},z,{z,a,a}])
       )
    ),
    ?assertEqual(
       [{a,b},{a,b,b},a,{a,a},{a,a,a},{b,b},{b,b,b},b,{b,a},{b,a,a},{z,b},{z,b,b},z,{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([{a,b},{a,b,b},{b,b},{b,b,b},{z,b},{z,b,b},a,b,z]),
         epm_utils:tup_sort([{a,a},{a,a,a},a,{b,a},{b,a,a},b,{z,a},{z,a,a},z])
       )
    ),
    ?assertEqual(
       [{a,b},{a,b,b},a,{a,a},{a,a,a},{b,b},{b,b,b},b,{b,a},{b,a,a},{z,b},{z,b,b},z,{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([{a,b},{a,b,b},{b,b},{b,b,b},{z,b},{z,b,b},a,b,z]),
         epm_utils:tup_sort([{a,a},{a,b},{a,a,a},{a,b,b},a,{b,a},{b,a,a},b,{z,a},{z,a,a},z])
       )
    ),
    ?assertEqual(
       [{l, a}, {r, a, b}, {s, a}, {s, b}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([{r, a, b}, {s, a}, {l, a}]),
         epm_utils:tup_sort([{s, b}])
       )
    ),
    ?assertEqual(
       [{a,b,b},{a,b},a,{a,a},{a,a,a},{b,b},{b,b,b},b,{b,a,a},{b,a},{z,b},{z,b,b},z,{z,a},{z,a,a}],
       epm_utils:tup_umerge(
         epm_utils:tup_sort([{a,b,b},{b,b},{a,b},{b,b,b},{z,b},{z,b,b},a,b,z]),
         epm_utils:tup_sort([{a,a},{a,a,a},a,{b,a,a},b,{z,a},{z,a,a},{b,a},z])
       )
    ).

proxy_auth(Config) ->
    proxy_auth(Config, "http://", "http_proxy"),
    proxy_auth(Config, "https://", "https_proxy"),
    proxy_auth(Config, "", "http_proxy"),
    proxy_auth(Config, "", "https_proxy").

proxy_auth(_Config, Schema, ProxyEnvKey) ->
	Host = "host:",
	Port = "1234",

	%% remember current proxy specification
	OldProxySpec = os:getenv(ProxyEnvKey),

	%% proxy auth not set
	application:unset_env(epm, proxy_auth),
	?assertEqual([], epm_utils:get_proxy_auth()),

	%% proxy auth with regular username/password
	os:putenv(ProxyEnvKey, Schema++"Username:Password@" ++ Host ++ Port),
	epm_utils:set_httpc_options(),
	?assertEqual([{proxy_auth, {"Username", "Password"}}],
				 epm_utils:get_proxy_auth()),

	%% proxy auth with username missing and url encoded password
	os:putenv(ProxyEnvKey, Schema++":%3F!abc%23%24@" ++ Host ++ Port),
	epm_utils:set_httpc_options(),
	?assertEqual([{proxy_auth, {"", "?!abc#$"}}],
				 epm_utils:get_proxy_auth()),

	%% restore original proxy specification if any
	restore_proxy_env(ProxyEnvKey, OldProxySpec),
	application:unset_env(epm, proxy_auth).

restore_proxy_env(ProxyEnvKey, false) ->
    os:putenv(ProxyEnvKey, "");
restore_proxy_env(ProxyEnvKey, ProxySpec) ->
    os:putenv(ProxyEnvKey, ProxySpec).

is_list_of_strings(_Config) ->
    ?assert(epm_utils:is_list_of_strings(["foo"])),
    ?assert(epm_utils:is_list_of_strings([])),
    ?assert(epm_utils:is_list_of_strings("")),
    ?assert(epm_utils:is_list_of_strings("foo") == false).
