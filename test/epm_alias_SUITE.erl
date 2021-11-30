-module(epm_alias_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_testcase(_, Config) ->
    epm_test_utils:init_epm_state(Config, "alias_").

end_per_testcase(_, _Config) ->
    ok.

all() -> [command, args, many, override_default, no_circular, release,
          check_namespaces, create_lib].

command() ->
    [{doc, "Runs multiple regular commands as one alias"}].
command(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("alias_command_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{test, [compile, unlock]}]}],

    %% compile job ran
    epm_test_utils:run_and_check(Config, EpmConfig,
                                   ["test"], {ok, [{app, Name}]}),
    %% unlock job also ran
    Lockfile = filename:join(?config(apps, Config), "epm.lock"),
    ?assertNot(filelib:is_file(Lockfile)),
    ok.

args() ->
    [{doc, "Runs multiple regular commands as one alias, some of "
           "which have default arguments"}].
args(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("alias_args_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{test, [{eunit,"-c"}, cover]}]}],

    %% test job ran (compiled and succeeded)
    epm_test_utils:run_and_check(Config, EpmConfig,
                                   ["test"], {ok, [{app, Name}]}),
    %% cover job also ran, meaning eunit had coverage on, otherwise
    %% the index file is not generated.
    CoverFile = filename:join([?config(apps, Config),
                               "_build", "test", "cover", "index.html"]),
    ?assert(filelib:is_file(CoverFile)),
    ok.

many() ->
    [{doc, "Multiple aliases may be registered"}].
many(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("alias_args_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{test, [{eunit,"-c"}, cover]},
                            {nolock, [compile, unlock]}]}],

    %% test job ran (compiled and succeeded)
    epm_test_utils:run_and_check(Config, EpmConfig,
                                   ["test"], {ok, [{app, Name}]}),
    epm_test_utils:run_and_check(Config, EpmConfig,
                                   ["nolock"], {ok, [{app, Name}]}),
    %% both jobs ran (see args/1 and command/1)
    CoverFile = filename:join([?config(apps, Config),
                               "_build", "test", "cover", "index.html"]),
    ?assert(filelib:is_file(CoverFile)),
    Lockfile = filename:join(?config(apps, Config), "epm.lock"),
    ?assertNot(filelib:is_file(Lockfile)),
    ok.

override_default() ->
    [{doc, "An alias cannot take over a default provider"}].
override_default(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("alias_override_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{compile, [help]}]}],

    %% App compiles anyway
    epm_test_utils:run_and_check(Config, EpmConfig, ["compile"],
                                   {ok, [{app, Name}]}),
    ok.

no_circular() ->
    [{doc, "An alias cannot define itself as itself"},
     {timetrap, 2000}].
no_circular(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("alias_circular_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{test, [help, {test,"-a"}, compile]}]}],

    %% Code does not deadlock forever and errors by not knowing
    %% the command
    epm_test_utils:run_and_check(Config, EpmConfig, ["test"],
                {error, [$C,$o,$m,$m,$a,$n,$d,$ ,"test",$ ,$n,$o,$t,$ ,
                         $f,$o,$u,$n,$d]}),
    ok.

release() ->
    [{doc, "An alias for a release command"}].
release(Config) ->
    AppDir = ?config(apps, Config),

    Name = epm_test_utils:create_random_name("alias_release_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{relx,
                    [{release, {skipped_release, Vsn}, []},
                     {release, {the_release, Vsn}, [list_to_atom(Name)]},
                     {lib_dirs, [AppDir]}]},
                   {alias,
                    [{the_rel1, [clean, {release, "-n the_release"}]},
                     {the_rel2, [clean, {release, "--relname=the_release"}]}]}],
    epm_test_utils:run_and_check(
      Config, EpmConfig,
      ["the_rel1"],
      {ok, [{release, the_release, Vsn, false}]}),
    epm_test_utils:run_and_check(
      Config, EpmConfig,
      ["the_rel2"],
      {ok, [{release, the_release, Vsn, false}]}),
    ok.

check_namespaces() ->
    [{doc, "Test calling commands with namespaces from epm"}].
check_namespaces(Config) ->
    AppDir = ?config(apps, Config),
    Name = epm_test_utils:create_random_name("alias_args_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{test, [{eunit,"-c"}, {plugins, list}]}]}],
    epm_test_utils:run_and_check(Config, EpmConfig,
                                   ["test"], {ok, [{app, Name}]}),
    ok.

create_lib() ->
    [{doc, "Test calling commands with namespaces from epm"}].
create_lib(Config) ->
    AppDir = ?config(apps, Config),
    Name = epm_test_utils:create_random_name("create_lib_"),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    EpmConfig = [{alias, [{test, [compile, {do, "new lib shouldexist"}]}]}],
    epm_test_utils:run_and_check(Config, EpmConfig,
                                   ["test"], {ok, [{app, Name}]}),
    AppFile = filename:join(?config(apps, Config),
                            "../../../../shouldexist/src/shouldexist.app.src"),
    ?assert(filelib:is_file(AppFile)),
    ok.
