-module(epm_pkg_alias_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("epm.hrl").

all() -> [same_alias, diff_alias, diff_alias_vsn, transitive_alias%% ,
          %% transitive_hash_mismatch
         ].

%% {uuid, {pkg, uuid}} = uuid
%% {uuid, {pkg, alias}} = uuid on disk
%% another run should yield the same lock file without error
init_per_suite(Config) ->
    Config.
    %% mock_config(?MODULE, Config).

end_per_suite(Config) ->
    Config.
    %% unmock_config(Config).

init_per_testcase(same_alias, Config0) ->
    mock_config(?MODULE, Config0),
    Config = epm_test_utils:init_epm_state(Config0,"same_alias_"),
    AppDir = ?config(apps, Config),
    epm_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    EpmConf = epm_test_utils:create_config(AppDir, [{deps, [{fakelib, {pkg, fakelib}}]}]),
    [{epmconfig, EpmConf} | Config];
init_per_testcase(diff_alias, Config0) ->
    mock_config(?MODULE, Config0),
    Config = epm_test_utils:init_epm_state(Config0,"diff_alias_"),
    AppDir = ?config(apps, Config),
    epm_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    EpmConf = epm_test_utils:create_config(AppDir, [{deps, [{fakelib, {pkg, goodpkg}}]}]),
    [{epmconfig, EpmConf} | Config];
init_per_testcase(diff_alias_vsn, Config0) ->
    mock_config(?MODULE, Config0),
    Config = epm_test_utils:init_epm_state(Config0,"diff_alias_vsn_"),
    AppDir = ?config(apps, Config),
    epm_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    EpmConf = epm_test_utils:create_config(AppDir, [{deps, [{fakelib, "1.0.0", {pkg, goodpkg}}]}]),
    [{epmconfig, EpmConf} | Config];
init_per_testcase(transitive_alias, Config0) ->
    mock_config(?MODULE, Config0),
    Config = epm_test_utils:init_epm_state(Config0,"transitive_alias_vsn_"),
    AppDir = ?config(apps, Config),
    epm_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    EpmConf = epm_test_utils:create_config(AppDir, [{deps, [{topdep, "1.0.0", {pkg, topdep}}]}]),
    [{epmconfig, EpmConf} | Config];
init_per_testcase(transitive_hash_mismatch, Config0) ->
    mock_config(?MODULE, Config0),
    Config = epm_test_utils:init_epm_state(Config0,"transitive_alias_vsn_"),
    AppDir = ?config(apps, Config),
    epm_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    EpmConf = epm_test_utils:create_config(AppDir, [{deps, [{topdep, "1.0.0", {pkg, topdep}}]}]),
    [{epmconfig, EpmConf} | Config].

end_per_testcase(_, Config) ->
    unmock_config(Config),
    Config.

same_alias(Config) ->
    {ok, EpmConfig} = file:consult(?config(epmconfig, Config)),
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["lock"],
        {ok, [{lock, "fakelib"}, {dep, "fakelib"}]}
    ).

diff_alias(Config) ->
    %% even though the dep is 'fakelib' aliased as 'goodpkg' all
    %% internal records use 'fakelib' as a value. Just make sure
    %% the lock actually maintains the proper source as 'goodpkg'
    AppDir = ?config(apps, Config),
    Lockfile = filename:join([AppDir, "epm.lock"]),
    {ok, EpmConfig} = file:consult(?config(epmconfig, Config)),
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["lock"],
        {ok, [{lock, "fakelib"},{dep, "fakelib"}]}
    ),
    {ok, [{Vsn, LockData}|_]} = file:consult(Lockfile),
    ?assert(lists:any(fun({<<"fakelib">>,{pkg,<<"goodpkg">>,_},_}) -> true
                      ;  (_) -> false end, LockData)),
    %% An second run yields the same
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["lock"],
        {ok, [{lock, "fakelib"},{dep, "fakelib"}]}
    ),
    {ok, [{Vsn, LockData}|_]} = file:consult(Lockfile),
    %% So does an upgrade
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["upgrade", "--all"],
        {ok, [{lock, "fakelib"},{dep, "fakelib"}]}
    ),
    {ok, [{Vsn, LockData}|_]} = file:consult(Lockfile).

diff_alias_vsn(Config) -> diff_alias(Config).

transitive_alias(Config) ->
    %% ensure that the apps fetched under transitive aliases are
    %% locked properly, but also that they are stored in the right
    %% directory in the build dir to avoid breaking includes and
    %% static analysis tools that rely on the location to work
    AppDir = ?config(apps, Config),
    Lockfile = filename:join([AppDir, "epm.lock"]),
    {ok, EpmConfig} = file:consult(?config(epmconfig, Config)),
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["lock"],
        {ok, [{lock, "topdep"},{dep, "topdep"},
              {lock,"transitive_app"},{dep,"transitive_app"}]}
    ),
    {ok, [{_Vsn, LockData}|_]} = file:consult(Lockfile),
    ?assert(lists:any(fun({<<"transitive_app">>,{pkg,<<"transitive">>,_},_}) -> true
                      ;  (_) -> false end, LockData)),
    AppDir = ?config(apps, Config),
    AliasedName = filename:join([AppDir, "_build", "default", "lib", "transitive_app"]),
    PkgName = filename:join([AppDir, "_build", "default", "lib", "transitive"]),
    ?assert(filelib:is_dir(AliasedName)),
    ?assertNot(filelib:is_dir(PkgName)),
    %% An second run yields the same
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["lock"],
        {ok, [{lock, "topdep"},{dep, "topdep"},
              {lock,"transitive_app"},{dep,"transitive_app"}]}
    ),
    {ok, [{Vsn, LockData}|_]} = file:consult(Lockfile),
    ?assert(filelib:is_dir(AliasedName)),
    ?assertNot(filelib:is_dir(PkgName)),
    %% So does an upgrade
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["upgrade", "--all"],
        {ok, [{lock, "topdep"},{dep, "topdep"},
              {lock,"transitive_app"},{dep,"transitive_app"}]}
    ),
    {ok, [{Vsn, LockData}|_]} = file:consult(Lockfile),
    ?assert(filelib:is_dir(AliasedName)),
    ?assertNot(filelib:is_dir(PkgName)),
    ok.

transitive_hash_mismatch(Config) ->
    %% ensure that the apps fetched under transitive aliases are
    %% locked properly, but also that they are stored in the right
    %% directory in the build dir to avoid breaking includes and
    %% static analysis tools that rely on the location to work
    AppDir = ?config(apps, Config),
    Lockfile = filename:join([AppDir, "epm.lock"]),
    {ok, EpmConfig} = file:consult(?config(epmconfig, Config)),
    epm_test_utils:run_and_check(
        Config, EpmConfig, ["lock"],
        {ok, [{lock, "topdep"},{dep, "topdep"},
              {lock,"transitive_app"},{dep,"transitive_app"}]}
    ),
    {ok, [LockData|Attrs]} = file:consult(Lockfile),
    %% Change Lock hash data to cause a failure next time, but on transitive
    %% deps only
    NewLock = [LockData|lists:map(
        fun([{pkg_hash, Hashes}|Rest]) ->
                [{pkg_hash, [{<<"transitive_app">>, <<"fakehash">>}
                             | lists:keydelete(<<"transitive_app">>, 1, Hashes)]}
                 | Rest]
        ;  (Attr) ->
                Attr
        end, Attrs)],
    {ok, Io} = file:open(Lockfile, [write]),
    [io:format(Io, "~p.~n", [Attr]) || Attr <- NewLock],
    file:close(Io),
    ct:pal("lock: ~p", [file:consult(Lockfile)]),
    ec_file:remove(filename:join([AppDir, "_build"]), [recursive]),
    ?assertMatch(
       {error, {epm_fetch, {unexpected_hash, _, _, _}}},
       epm_test_utils:run_and_check(Config, EpmConfig, ["lock"], return)
    ),
    ok.

parse_deps(Deps) ->
    [{maps:get(app, D, Name), {pkg, Name, Constraint, undefined}} || D=#{package := Name,
                                                                         requirement := Constraint} <- Deps].

mock_config(Name, Config) ->
    {ChkFake, Etag} = create_lib(Name, Config, "fakelib"),
    {ChkTop, _} = create_lib(Name, Config, "topdep"),
    {ChkTrans, _} = create_lib(Name, Config, "transitive_app", "transitive"),
    ct:pal("{~p, _}",[ChkTop]),
    ct:pal("{~p, _}",[ChkTrans]),
    Priv = ?config(priv_dir, Config),
    TmpDir = filename:join([Priv, "tmp", atom_to_list(Name)]),
    %% Add an alias for goodpkg -> fakelib by hand
    AppDir = filename:join([Priv, "fakelib"]),
    CacheRoot = filename:join([Priv, "cache", atom_to_list(Name)]),
    CacheDir = filename:join([CacheRoot, "hex", "com", "test", "packages"]),
    epm_test_utils:create_app(AppDir, "fakelib", "1.0.0", [kernel, stdlib]),
    ct:pal("{~p, ~p}",[ChkFake, Etag]),
    {ChkGood, EtagGood} = epm_test_utils:package_app(AppDir, CacheDir, "goodpkg", "1.0.0"),

    AllDeps = [
     {<<"fakelib">>,[[<<"1.0.0">>]]},
     {<<"goodpkg">>,[[<<"1.0.0">>]]},
     {<<"topdep">>,[[<<"1.0.0">>]]},
     {<<"transitive">>, [[<<"1.0.0">>]]},
     {{<<"fakelib">>,<<"1.0.0">>}, [[], ChkFake, [<<"epm">>]]},
     {{<<"goodpkg">>,<<"1.0.0">>}, [[], ChkGood, [<<"epm">>]]},
     {{<<"topdep">>,<<"1.0.0">>},
      [[
        {<<"transitive">>, <<"1.0.0">>, false, <<"transitive_app">>}
       ], ChkTop, [<<"epm">>]]},
     {{<<"transitive">>,<<"1.0.0">>}, [[], ChkTrans, [<<"epm">>]]}
    ],
    Tid = ets:new(registry_table, [public]),
    ets:insert_new(Tid, AllDeps),
    ok = ets:tab2file(Tid, filename:join([CacheDir, "registry"])),
    %% ets:delete(Tid),
    %% The state returns us a fake registry
    meck:new(epm_dir, [passthrough, no_link]),
    meck:expect(epm_dir, global_cache_dir, fun(_) -> CacheRoot end),

    meck:new(epm_packages, [passthrough, no_link]),
    meck:expect(epm_packages, registry_dir, fun(_) -> {ok, CacheDir} end),
    meck:expect(epm_packages, package_dir, fun(_, _) -> {ok, CacheDir} end),

    %% TODO: is something else wrong that we need this for transitive_alias to pass
    meck:expect(epm_packages, update_package, fun(_, _, _) -> ok end),

    meck:new(epm_prv_update, [passthrough]),
    meck:expect(epm_prv_update, do, fun(State) -> {ok, State} end),

    catch ets:delete(?PACKAGE_TABLE),
    epm_packages:new_package_table(),

    lists:foreach(fun({{N, Vsn}, [Deps, Checksum, _]}) ->
                          case ets:member(?PACKAGE_TABLE, {ec_cnv:to_binary(N), Vsn, <<"hexpm">>}) of
                              false ->
                                  ets:insert(?PACKAGE_TABLE, #package{key={ec_cnv:to_binary(N), ec_semver:parse(Vsn), <<"hexpm">>},
                                                                      dependencies=[{DAppName, {pkg, DN, DV, undefined}} || {DN, DV, _, DAppName} <- Deps],
                                                                      retired=false,
                                                                      outer_checksum=Checksum});
                              true ->
                                  ok
                          end;
                     ({_N, _Vsns}) ->
                          ok

                  end, AllDeps),

    meck:new(r3_hex_repo, [passthrough]),
    meck:expect(r3_hex_repo, get_package,
                fun(_Config, PkgName) ->
                        Matches = ets:match_object(Tid, {{PkgName,'_'}, '_'}),
                        Releases =
                            [#{outer_checksum => Checksum,
                               version => Vsn,
                               dependencies => [{DAppName, {pkg, DN, DV, undefined}} ||
                                                   {DN, DV, _, DAppName} <- Deps]} ||
                                {{_, Vsn}, [Deps, Checksum, _]} <- Matches],
                        {ok, {200, #{}, Releases}}
                end),

    meck:expect(r3_hex_repo, get_tarball, fun(_, _, _) ->
                                               {ok, {304, #{<<"etag">> => EtagGood}, <<>>}}
                                       end),

    %% Move all packages to cache
    NewConf = [{cache_root, CacheRoot},
               {cache_dir, CacheDir},
               {tmp_dir, TmpDir},
               {mock_table, Tid} | Config],
    NewConf.

unmock_config(Config) ->
    meck:unload(),
    Config.

create_lib(Name, Config, PkgName) ->
    create_lib(Name, Config, PkgName, PkgName).

create_lib(Name, Config, AppName, PkgName) ->
    Priv = ?config(priv_dir, Config),
    AppDir = filename:join([Priv, PkgName]),
    CacheRoot = filename:join([Priv, "cache", atom_to_list(Name)]),
    CacheDir = filename:join([CacheRoot, "hex", "com", "test", "packages"]),
    filelib:ensure_dir(filename:join([CacheDir, "registry"])),
    epm_test_utils:create_app(AppDir, AppName, "1.0.0", [kernel, stdlib]),
    epm_test_utils:package_app(AppDir, CacheDir, PkgName, "1.0.0").
