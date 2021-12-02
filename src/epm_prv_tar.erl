%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_prv_tar).

-behaviour(provider).

-export([init/1,
    do/1,
    format_error/1]).

-include("epm.hrl").

-define(PROVIDER, tar).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 = epm_state:add_provider(State, providers:create([{name, ?PROVIDER},
        {module, ?MODULE},
        {bare, true},
        {deps, ?DEPS},
        {example, "epm tar"},
        {short_desc, "Tar archive of release built of project."},
        {desc, "Tar archive of release built of project."},
        {opts, epm_relx:opt_spec_list()}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    epm_relx:do(?PROVIDER, State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
