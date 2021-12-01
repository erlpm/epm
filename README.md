# Epm

[![Build Status](https://github.com/erlpm/epm/workflows/Common%20Test/badge.svg)](https://github.com/erlpm/epm/actions?query=branch%3Amaster+workflow%3A"Common+Test") [![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-22.0%20to%2024.0-blue)](http://www.erlang.org)

1. [What is Epm?](#what-is-epm)
2. [Why Epm?](#why-epm)
3. [Should I Use Epm?](#should-i-use-epm)
4. [Getting Started](#getting-started)
5. [Documentation](#documentation)
6. [Features](#features)
7. [Migrating from rebar3](#migrating-from-rebar3)
8. [Additional Resources](#additional-resources)

## What is Epm

Epm is an Erlang tool that makes it easy to create, develop, and
release Erlang libraries, applications, and systems in a repeatable manner.

Epm will:
- respect and enforce standard Erlang/OTP conventions for project
  structure so they are easily reusable by the community;
- manage source dependencies and Erlang [packages](https://erl.pm)
  while ensuring repeatable builds;
- handle build artifacts, paths, and libraries such that standard
  development tools can be used without a headache;
- adapt to projects of all sizes on almost any platform;
- treat [documentation](https://www.erldoc.com/erlpm/) as a feature,
  and errors or lack of documentation as a bug.

Epm is also a self-contained Erlang script. It is easy to distribute or
embed directly in a project. Tasks or behaviours can be modified or expanded
with a [plugin system](https://www.erldoc.com/erlpm/configuration/plugins)
[flexible enough](https://github.com/lfe-rebar3/rebar3_lfe) that even other languages
on the Erlang VM will use it as a build tool.

## Why Epm

Epm is the spiritual successor to [rebar 2.x](https://github.com/rebar/rebar), which was the first usable build tool
for Erlang that ended up seeing widespread community adoption. It however
had several shortcomings that made it difficult to use with larger projects
or with teams with users new to Erlang.

Epm was our attempt at improving over the legacy of Epm 2.x, providing the
features we felt it was missing, and to provide a better environment in which
newcomers joining our teams could develop.

## Should I use Epm?

If your main language for your system is Erlang, that you value repeatable builds
and want your various tools to integrate together, we do believe Epm is the
best experience you can get.

## Getting Started

A [getting started guide is maintained on the official documentation website](https://www.erldoc.com/erlpm/getting-started),
but installing epm can be done by any of the ways described below

Latest stable compiled version:
```bash
$ wget https://www.erl.pm/dl/epm && chmod +x epm
```

From Source (assuming you have a full Erlang install):

```bash
$ git clone https://github.com/erlpm/epm.git
$ cd epm
$ ./bootstrap
```

Stable versions can also be obtained from the [releases page](https://github.com/erlpm/epm/releases).

The epm escript can also extract itself with a run script under the user's home directory:

```bash
$ ./epm local install
===> Extracting epm libs to ~/.cache/epm/lib...
===> Writing epm run script ~/.cache/epm/bin/epm...
===> Add to $PATH for use: export PATH=~/.cache/epm/bin:$PATH
```

To keep it up to date after you've installed epm this way you can use `epm local upgrade` which
fetches the latest stable release and extracts to the same place as above. A [nightly version can
also be obtained](https://www.erl.pm/dl/epm) if desired.

Epm may also be available on various OS-specific package managers such as
FreeBSD Ports. Those are maintained by the community and Epm maintainers
themselves are generally not involved in that process.

If you do not have a full Erlang install, we recommend using [erln8](https://erln8.github.io/erln8/)
or [kerl](https://github.com/yrashk/kerl). For binary packages, use those provided
by [Erlang Solutions](https://www.erlang-solutions.com/resources/download.html),
but be sure to choose the "Standard" download option or you'll have issues building
projects.

Do note that if you are planning to work with multiple Erlang versions on the same machine, you will want to build Epm with the oldest one of them. The 3 newest major Erlang releases are supported at any given time: if the newest version is OTP-24, building with versions as old as OTP-22 will be supported, and produce an executable that will work with those that follow.

## Documentation

Epm documentation is maintained on [https://www.erldoc.com/erlpm](https://www.erldoc.com/erlpm)

## Features

Epm supports the following features or tools by default, and may provide many
others via the plugin ecosystem:

| features             | Description |
|--------------------- |------------ |
| Command composition  | Epm allows multiple commands to be run in sequence by calling `epm do <task1>,<task2>,...,<taskN>`. |
| Command dependencies | Epm commands know their own dependencies. If a test run needs to fetch dependencies and build them, it will do so. |
| Command namespaces   | Allows multiple tools or commands to share the same name. |
| Compiling            | Build the project, including fetching all of its dependencies by calling `epm compile` |
| Clean up artifacts   | Remove the compiled beam files from a project with `epm clean` or just remove the `_build` directory to remove *all* compilation artifacts |
| Code Coverage        | Various commands can be instrumented to accumulate code coverage data (such as `eunit` or `ct`). Reports can be generated with `epm cover` |
| Common Test          | The test framework can be run by calling `epm ct` |
| Dependencies         | Epm maintains local copies of dependencies on a per-project basis. They are fetched deterministically, can be locked, upgraded, fetched from source, packages, or from local directories. See [Dependencies on the documentation website](http://www.erldoc.com/erlpm/configuration/dependencies/). Call `epm tree` to show the whole dependency tree. |
| Documentation        | Print help for epm itself (`epm help`) or for a specific task (`epm help <task>`). Full reference at [erl.pm](http://www.erldoc.com/erlpm). |
| Dialyzer             | Run the Dialyzer analyzer on the project with `epm dialyzer`. Base PLTs for each version of the language will be cached and reused for faster analysis |
| Edoc                 | Generate documentation using edoc with `epm edoc` |
| Escript generation   | Epm can be used to generate [escripts](http://www.erldoc.com/doc/man/escript.html) providing an easy way to run all your applications on a system where Erlang is installed |
| Eunit                | The test framework can be run by calling `epm eunit` |
| Locked dependencies  | Dependencies are going to be automatically locked to ensure repeatable builds. Versions can be changed with `epm upgrade` or `epm upgrade <app>`, or locks can be released altogether with `epm unlock`. |
| Packages             | A given [Erlang package](https://www.erl.pm) can be inspected `epm pkgs <name>`. This will output its description and available versions |
| Path                 | While paths are managed automatically, you can print paths to the current build directories with `epm path`. |
| Plugins              | Epm can be fully extended with [plugins](https://www.erldoc.com/doc/configuration/plugins/). List or upgrade plugins by using the plugin namespace (`epm plugins`). |
| Profiles             | Epm can have configuration options for different profiles, such as `test` or `prod`. These allow specific dependencies or compile options to be used in specific contexts. See [Profiles](http://www.erldoc.com/erlpm/configuration/profiles) in the docs. |
| Releases             | Epm supports [building releases](https://www.erldoc.com/doc/deployment/releases) with the `relx` tool, providing a way to ship fully self-contained Erlang systems. Release update scripts for live code updates can also be generated. |
| Shell                | A full shell with your applications available can be started with `epm shell`. From there, call tasks as `r3:do(compile)` to automatically recompile and reload the code without interruption |
| Tarballs             | Releases can be packaged into tarballs ready to be deployed. |
| Templates            | Configurable templates ship out of the box (try `epm new` for a list or `epm new help <template>` for a specific one). [Custom templates](http://www.erldoc.com/erlpm/tutorials/templates) are also supported, and plugins can also add their own. |
| Xref                 | Run cross-reference analysis on the project with [xref](https://www.erldoc.com/doc/apps/tools/xref_chapter.html) by calling `epm xref`. |

## Migrating From rebar3

The grievances we had with Epm 2.x were not fixable without breaking
compatibility in some very important ways.

A full guide titled [From Epm to Epm](https://www.erldoc.com/doc/tutorials/from_rebar3_to_epm/)
is provided on the documentation website.

Notable modifications include mandating a more standard set of directory
structures, changing the handling of dependencies, moving some compilers (such
as C, Diameter, ErlyDTL, or ProtoBuffs) to
[plugins](https://www.erldoc.com/doc/configuration/plugins) rather than
maintaining them in core epm, and moving release builds from reltool to
relx.

## Additional Resources

In the case of problems that cannot be solved through documentation or examples, you
may want to try to contact members of the community for help. The community is
also where you want to go for questions about how to extend epm, fill in bug
reports, and so on.

If you need
quick feedback, you can try the #epm channel on
[irc.freenode.net](https://freenode.net) or the #epm channel on
[erlanger.slack.com](https://erlanger.slack.com/). Be sure to check the
[documentation](http://www.erldoc.com/erlpm) first, just to be sure you're not
asking about things with well-known answers.

For bug reports, roadmaps, and issues, visit the [github issues page](https://github.com/erlpm/epm/issues).

General epm community resources and links can be found at
[erldoc.com/erlpm/about-us/#community](http://www.erldoc.com/erlpm/about/about-us/#community)

To contribute to epm, please refer to [CONTRIBUTING](参与.md).

