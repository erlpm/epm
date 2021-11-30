-module(epm_prv_deps_tree).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").

-define(PROVIDER, tree).
-define(DEPS, [lock]).

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 = epm_state:add_provider(
               State,
               providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "epm tree"},
                                 {short_desc, "Print dependency tree."},
                                 {desc, ""},
                                 {opts, [{verbose, $v, "verbose", undefined, "Print repo and branch/tag/ref for git and hg deps"}]}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = epm_state:command_parsed_args(State),
    Verbose = proplists:get_value(verbose, Args, false),
    print_deps_tree(epm_state:all_deps(State), Verbose, State),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

print_deps_tree(SrcDeps, Verbose, State) ->
    D = lists:foldl(fun(App, Dict) ->
                            Name = epm_app_info:name(App),
                            Vsn = epm_app_info:original_vsn(App),
                            Vsn1 = epm_utils:vcs_vsn(App, Vsn, State),
                            Source  = epm_app_info:source(App),
                            Parent = epm_app_info:parent(App),
                            dict:append_list(Parent, [{Name, Vsn1, Source}], Dict)
                    end, dict:new(), SrcDeps),
    ProjectAppNames = [{epm_app_info:name(App)
                       , epm_utils:vcs_vsn(App, epm_app_info:original_vsn(App), State)
                       ,project} || App <- epm_state:project_apps(State)],
    case dict:find(root, D) of
        {ok, Children} ->
            print_children("", lists:keysort(1, ProjectAppNames), D, Verbose),
            print_children("   ", lists:keysort(1, Children), D, Verbose);
        error ->
            print_children("", lists:keysort(1, ProjectAppNames), D, Verbose)
    end.

print_children(_, [], _, _) ->
    ok;
print_children(Prefix, [{Name, Vsn, Source} | Rest], Dict, Verbose) ->
    Prefix1 = case Rest of
                  [] ->
                      io:format("~ts~ts", [Prefix, <<226,148,148,226,148,128,32>>]), %Binary for └─ utf8%
                      [Prefix, "   "];
                  _ ->
                      io:format("~ts~ts", [Prefix, <<226,148,156,226,148,128,32>>]), %Binary for ├─ utf8%
                      [Prefix, <<226,148,130,32,32>>] %Binary for │  utf8%
              end,
    io:format("~ts~ts~ts (~ts)~n", [Name, <<226,148,128>>, Vsn, type(Source, Verbose)]), %Binary for ─ utf8%
    case dict:find(Name, Dict) of
        {ok, Children} ->
            print_children(Prefix1, lists:keysort(1, Children), Dict, Verbose),
            print_children(Prefix, Rest, Dict, Verbose);
        error ->
            print_children(Prefix, Rest, Dict, Verbose)
    end.

type(project, _) ->
    "project app";
type(checkout, _) ->
    "checkout app";
type(Source, Verbose) when is_tuple(Source) ->
    case {element(1, Source), Verbose} of
        {pkg, _} ->
            "hex package";
        {Other, false} ->
            io_lib:format("~ts repo", [Other]);
        {_, true} ->
            io_lib:format("~ts", [element(2, Source)])
    end.
