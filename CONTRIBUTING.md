# Contributing to Epm

1. [License](#license)
2. [Submitting a bug](#submitting-a-bug)
3. [Requesting or implementing a feature](#requesting-or-implementing-a-feature)
4. [Project Structure](#project-structure)
5. [Tests](#tests)
6. [Submitting your changes](#submitting-your-changes)
   1. [Code Style](#code-style)
   2. [Committing your changes](#committing-your-changes)
   3. [Pull Requests and Branching](#pull-requests-and-branching)
   4. [Credit](#credit)

## License ##

Epm is licensed under the [Apache License 2.0](LICENSE) for all new code.
However, since it is built from older code bases, some files still hold other
free licenses (such as BSD). Where it is the case, the license is added in
comments.

All files without specific headers can safely be assumed to be under Apache
2.0.

## Submitting a Bug

Bugs can be submitted to the [Github issue page](https://github.com/erlpm/epm/issues).

Epm is not perfect software and will be buggy. When submitting a bug, be
careful to know the following:

- The Erlang version you are running
- The Epm version you are using
- The command you were attempting to run

This information can be automatically generated to put into your bug report
by calling `epm report "my command"`.

You may be asked for further information regarding:

- Your environment, including the Erlang version used to compile epm,
  details about your operating system, where your copy of Erlang was installed
  from, and so on;
- Your project, including its structure, and possibly to remove build
  artifacts to start from a fresh build
- What it is you are trying to do exactly; we may provide alternative
  means to do so.

If you can provide an example code base to reproduce the issue on, we will
generally be able to provide more help, and faster.

All contributors and epm maintainers are generally unpaid developers
working on the project in their own free time with limited resources. We
ask for respect and understanding and will try to provide the same back.

## Requesting or implementing a feature

Before requesting or implementing a new feature, please do the following:

- Take a look at our [list of plugins](https://www.erldoc.com/erlpm/configuration/plugins#recommended-plugins)
  to know if the feature isn't already supported by the community.
- Verify in existing [tickets](https://github.com/erlpm/epm/issues) whether
  the feature might already is in the works, has been moved to a plugin, or
  has already been rejected.

If this is done, open up a ticket. Tell us what is the feature you want,
why you need it, and why you think it should be in epm itself.

We may discuss details with you regarding the implementation, its inclusion
within the project or as a plugin. Depending on the feature, we may provide
full support for it, or ask you to help implement and/or commit to maintaining
it in the future. We're dedicated to providing a stable build tool, and may
also ask features to exist as a plugin before being included in core epm --
the migration path from one to the other is fairly simple and little to no code
needs rewriting.

## Project Structure

Epm is an escript built around the concept of providers. Providers are the
modules that do the work to fulfill a user's command. They are documented in
[the official documentation website](http://www.erldoc.com/erlpm/plugins#section-provider-interface).

Example provider:

```erlang
-module(epm_prv_something).

-behaviour(epm_provider).

-export([init/1,
    do/1,
    format_error/1]).

-define(PROVIDER, something).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:state()) -> {ok, epm_state:state()}.
init(State) ->
    State1 = epm_state:add_provider(State, epm_provider:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "epm dummy"},
        {short_desc, "dummy plugin."},
        {desc, ""},
        {opts, []}
    ])),
    {ok, State1}.

-spec do(epm_state:state()) -> {ok, epm_state:state()}.
do(State) ->
    %% Do something
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
```

Providers are then listed in `epm.app.src`, and can be called from
the command line or as a programmatically API.

All commands are therefore implemented in standalone modules. If you call
`epm <task>`, the module in charge of it is likely located in
`src/epm_prv_<task>.erl`.

Templates are included in `priv/templates/`

The official test suite is Common Test, and tests are located in `test/`.

Useful modules include:
- `epm_api`, providing an interface for plugins to call into core epm
  functionality
- `epm_core`, for initial boot and setup of a project
- `epm_config`, handling the configuration of each project.
- `epm_app_info`, giving access to the metadata of a specific OTP application
  in a project.
- `epm_base_compiler`, giving a uniform interface to compile `.erl` files.
- `epm_dir` for directory handling and management
- `epm_file_util` for cross-platform file handling
- `epm_state`, the glue holding together a specific build or task run;
  includes canonical versions of the configuration, profiles, applications,
  dependencies, and so on.
- `epm_utils` for generic tasks and functionality required across
  multiple providers or modules.

## Tests

Epm tries to have as many of its features tested as possible. Everything
that a user can do and should be repeatable in any way should be tested.

Tests are written using the Common Test framework. Tests for epm can be run
by calling:

```bash
$ epm escriptize # or bootstrap
$ ./epm ct
```

Most tests are named according to their module name followed by the `_SUITE`
suffix. Providers are made shorter, such that `epm_prv_new` is tested in
`epm_new_SUITE`.

Most tests in the test suite will rely on calling Epm in its API form,
then investigating the build output. Because most tests have similar
requirements, the `test/epm_test_utils` file contains common code
to set up test projects, run tasks, and verify artifacts at once.

A basic example can look like:

```erlang
-module(epm_some_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [checks_success, checks_failure].

init_per_testcase(Case, Config0) ->
    %% Create a project directory in the test run's priv_dir
    Config = epm_test_utils:init_epm_state(Config0),
    %% Create toy applications
    AppDir = ?config(apps, Config),
    Name = epm_test_utils:create_random_name("app1_"++atom_to_list(Case)),
    Vsn = epm_test_utils:create_random_vsn(),
    epm_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    %% Add the data to the test config
    [{name, Name} | Config].

end_per_testcase(_, Config) ->
    Config.

checks_success(Config) ->
    %% Validates that the application in `name' is successfully compiled
    Name = ?config(name, Config),
    epm_test_utils:run_and_check(Config, [],
                                   ["compile"],
                                   {ok, [{app, Name}]}).

checks_failure(Config) ->
    %% Checks that a result fails
    Command = ["fakecommand", "fake-arg"],
    epm_test_utils:run_and_check(
      Config, [], Command,
      {error, io_lib:format("Command ~p not found", [fakecommand])}
    ).
```

The general interface to `epm_test_utils:run_and_check` is
`run_and_check(CTConfig, EpmConfig, Command, Expect)` where `Expect` can
be any of:

```erlang
{ok, OKRes}
{ok, OKRes, ProfilesUsed}
{error, Reason}

% where:
ProfilesUsed :: string() % matching the profiles to validate (defaults to "*")
OKRes :: {app, Name}           % name of an app that is in the build directory
       | {app, Name, valid}    % name of an app that is in the build directory and compiled properly
       | {app, Name, invalid}  % name of an app that didn't compile properly
       | {dep, Name}           % name of a dependency in the build directory
       | {dep, Name, Vsn}      % name of a dependency in the build directory with a specific version
       | {dep_not_exist, Name} % name of a dependency missing from the build directory
       | {checkout, Name}      % name of an app that is a checkout dependency
       | {plugin, Name}        % name of a plugin in the build directory
       | {plugin, Name, Vsn}   % name of a plugin in the build directory with a specific version
       | {global_plugin, Name} % name of a global plugin in the build directory
       | {global_plugin, Name, Vsn} % name of a global plugin in the build directory with a specific version
       | {lock, Name}          % name of a locked dependency
       | {lock, Name, Vsn}     % name of a locked dependency of a specific version
       | {lock, pkg, Name, Vsn}% name of a locked package of a specific version
       | {lock, src, Name, Vsn}% name of a locked source dependency of a specific version
       | {release, Name, Vsn, ExpectedDevMode} % validates a release
       | {tar, Name, Vsn}      % validates a tarball's existence
       | {file, Filename}      % validates the presence of a given file
       | {dir, Dirname}        % validates the presence of a given directory
Reason :: term() % the exception thrown by epm
```

This generally lets most features be tested fine. Ask for help if you cannot
figure out how to write tests for your feature or patch.

## Submitting your changes

While we're not too formal when it comes to pull requests to the project,
we do appreciate users taking the time to conform to the guidelines that
follow.

We do expect all pull requests submitted to come with [tests](#tests) before
they are merged. If you cannot figure out how to write your tests properly, ask
in the pull request for guidance.

### Code Style

 * Do not introduce trailing whitespace
 * Indentation is 4 spaces wide, no tabs.
 * Try not to introduce lines longer than 80 characters
 * Write small functions whenever possible, and use descriptive names for
   functions and variables.
 * Avoid having too many clauses containing clauses.
   Basically, avoid deeply nested `case ... of` or `try ... catch` expressions.
   Break them out into functions if possible.
 * Comment tricky or non-obvious decisions made to explain their rationale.

### Committing your changes

It helps if your commits are structured as follows:

- Fixing a bug is one commit.
- Adding a feature is one commit.
- Adding two features is two commits.
- Two unrelated changes is two commits (and likely two Pull requests)

If you fix a (buggy) commit, squash (`git rebase -i`) the changes as a fixup
commit into the original commit, unless the patch was following a
maintainer's code review. In such cases, it helps to have separate commits.

The reviewer may ask you to later squash the commits together to provide
a clean commit history before merging in the feature.

It's important to write a proper commit title and description. The commit title
should be no more than 50 characters; it is the first line of the commit text. The
second line of the commit text must be left blank. The third line and beyond is
the commit message. You should write a commit message. If you do, wrap all
lines at 72 characters. You should explain what the commit does, what
references you used, and any other information that helps understanding your
changes.

### Pull Requests and Branching

All fixes to epm end up requiring a +1 from one or more of the project's
maintainers. When opening a pull request, explain what the patch is doing
and if it makes sense, why the proposed implementation was chosen.

Try to use well-defined commits (one feature per commit) so that reading
them and testing them is easier for reviewers and while bisecting the code
base for issues.

During the review process, you may be asked to correct or edit a few things
before a final rebase to merge things. Do send edits as individual commits
to allow for gradual and partial reviews to be done by reviewers. Once the +1s
are given, rebasing is appreciated but not mandatory.

Please work in feature branches, and do not commit to `master` in your fork.

Provide a clean branch without merge commits.

If you can, pick a descriptive title for your pull request. When we generate
changelogs before cutting a release, a script uses the pull request names
to populate the entries.


### Credit

To give everyone proper credit in addition to the git history, please feel free to append
your name to `THANKS` in your first contribution.
