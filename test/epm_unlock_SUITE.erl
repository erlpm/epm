-module(epm_unlock_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [pkgunlock, unlock, unlock_all].

init_per_testcase(pkgunlock, Config0) ->
    Config = epm_test_utils:init_epm_state(Config0, "pkgunlock"),
    Lockfile = filename:join(?config(apps, Config), "epm.lock"),
    ec_file:copy(filename:join(?config(data_dir, Config), "pkg.epm.lock"),
                 Lockfile),
    [{lockfile, Lockfile} | Config];
init_per_testcase(Case, Config0) ->
    Config = epm_test_utils:init_epm_state(Config0, atom_to_list(Case)),
    Lockfile = filename:join(?config(apps, Config), "epm.lock"),
    ec_file:copy(filename:join(?config(data_dir, Config), "epm.lock"),
                 Lockfile),
    [{lockfile, Lockfile} | Config].

end_per_testcase(_, Config) ->
    Config.

pkgunlock(Config) ->
    Locks = read_locks(Config),
    Hashes = read_hashes(Config),
    epm_test_utils:run_and_check(Config, [], ["unlock", "fakeapp"], {ok, []}),
    Locks = read_locks(Config),
    Hashes = read_hashes(Config),
    epm_test_utils:run_and_check(Config, [], ["unlock", "bbmustache"], {ok, []}),
    ?assertEqual(Locks -- ["bbmustache"], read_locks(Config)),
    ?assertEqual(Hashes -- ["bbmustache"], read_hashes(Config)),
    epm_test_utils:run_and_check(Config, [], ["unlock", "cf,certifi"], {ok, []}),
    ?assertEqual(Locks -- ["bbmustache","cf","certifi"], read_locks(Config)),
    ?assertEqual(Hashes -- ["bbmustache","cf","certifi"], read_hashes(Config)),
    epm_test_utils:run_and_check(Config, [], ["unlock", epm_string:join(Locks,",")], {ok, []}),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ?assertEqual({error, enoent}, read_hashes(Config)),
    ok.

unlock(Config) ->
    Locks = read_locks(Config),
    epm_test_utils:run_and_check(Config, [], ["unlock", "fakeapp"], {ok, []}),
    Locks = read_locks(Config),
    {ok, State} = epm_test_utils:run_and_check(Config, [], ["unlock", "uuid"], return),
    ?assertEqual(Locks -- ["uuid"], read_locks(Config)),
    ?assert(false =:= lists:keyfind(<<"uuid">>, 1, epm_state:get(State, {locks, default}))),
    ?assert(false =/= lists:keyfind(<<"itc">>, 1, epm_state:get(State, {locks, default}))),
    epm_test_utils:run_and_check(Config, [], ["unlock", "gproc,itc"], {ok, []}),
    ?assertEqual(Locks -- ["uuid","gproc","itc"], read_locks(Config)),
    epm_test_utils:run_and_check(Config, [], ["unlock", epm_string:join(Locks,",")], {ok, []}),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ok.

unlock_all(Config) ->
    [_|_] = read_locks(Config),
    {ok, State} = epm_test_utils:run_and_check(Config, [], ["unlock"], return),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ?assertEqual([], epm_state:get(State, {locks, default})),
    ok.

read_locks(Config) ->
    case file:consult(?config(lockfile, Config)) of
        {ok, _} ->
            Locks = epm_config:consult_lock_file(?config(lockfile, Config)),
            [binary_to_list(element(1,Lock)) || Lock <- Locks];
        Other ->
            Other
    end.

read_hashes(Config) ->
    case file:consult(?config(lockfile, Config)) of
        {ok, [{_Vsn, _Locks},Props|_]} ->
            Hashes = proplists:get_value(pkg_hash, Props, []),
            [binary_to_list(element(1,Hash)) || Hash <- Hashes];
        {ok, [{_Vsn, _Locks}]} ->
            [];
        Other ->
            Other
    end.
