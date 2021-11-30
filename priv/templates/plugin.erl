-module({{name}}).

-export([init/1]).

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    {ok, State1} = {{name}}_prv:init(State),
    {ok, State1}.
