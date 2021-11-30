%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_prv_state).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").

-define(PROVIDER, state).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    Provider = providers:create(
        [{name, ?PROVIDER},
         {module, ?MODULE},
         {bare, false},
         {deps, ?DEPS},
         {example, "epm state"},
         {short_desc, "Print current configuration state"},
         {desc, "Display epm configuration for debugging purpose"},
         {opts, []}]),
    State1 = epm_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    L = epm_state:to_list(State),
    ?CONSOLE("State:", []),
    [?CONSOLE("  ~w: ~p", [K, V]) || {K,V} <- L],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
