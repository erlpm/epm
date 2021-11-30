%% Vendor from epm_core v0.7.1, do not edit manually

-module(erlpm_api_key).
-export([
    list/1, get/2, add/3, delete/2,
    delete_all/1
]).

-export_type([permission/0]).

-type permission() :: api_permission() | repo_permission() | repos_permission().
-ifdef(OTP_19).
-type api_permission() :: #{domain := api, resource => read | write}.
-type repo_permission() :: #{domain := repository, resource := binary()}.
-type repos_permission() :: #{domain := repositories}.
-else.
-type api_permission() :: #{domain => api, resource => read | write}.
-type repo_permission() :: #{domain => repository, resource => binary()}.
-type repos_permission() :: #{domain => repositories}.
-endif.

%% @doc
%% Lists the user's or organization's API and repository keys.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_key:list(erlpm_core:default_config()).
%% {ok, {200, ..., [#{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"epm_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"epm_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://erl.pm/api/keys/test">>},
%%     }]}}
%% '''
%% @end
-spec list(erlpm_core:config()) -> erlpm_api:response().
list(Config) when is_map(Config) ->
    Path = erlpm_api:build_organization_path(Config, ["keys"]),
    erlpm_api:get(Config, Path).

%% @doc
%% Gets an API or repository key by name.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_key:get(erlpm_core:default_config(), <<"test">>).
%% {ok, {200, ..., #{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"epm_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"epm_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://erl.pm/api/keys/test">>},
%%     }}}
%% '''
%% @end
-spec get(erlpm_core:config(), binary()) -> erlpm_api:response().
get(Config, Name) when is_map(Config) and is_binary(Name) ->
    Path = erlpm_api:build_organization_path(Config, ["keys", Name]),
    erlpm_api:get(Config, Path).

%% @doc
%% Adds a new API or repository key.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_key:add(erlpm_core:default_config(), <<"test">>, [...]).
%% {ok, {200, ..., #{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"epm_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"epm_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://erl.pm/api/keys/test">>},
%%     }}}
%% '''
%% @end
-spec add(erlpm_core:config(), binary(), [permission()]) -> erlpm_api:response().
add(Config, Name, Permissions) when is_map(Config) and is_binary(Name) and is_list(Permissions) ->
    Path = erlpm_api:build_organization_path(Config, ["keys"]),
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    erlpm_api:post(Config, Path, Params).

%% @doc
%% Deletes an API or repository key.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_key:delete(erlpm_core:default_config(), <<"test">>).
%% {ok, {200, ..., #{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"epm_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"epm_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://erl.pm/api/keys/test">>},
%%     }}}
%% '''
%% @end
-spec delete(erlpm_core:config(), binary()) -> erlpm_api:response().
delete(Config, Name) when is_map(Config) and is_binary(Name) ->
    Path = erlpm_api:build_organization_path(Config, ["keys", Name]),
    erlpm_api:delete(Config, Path).

%% @doc
%% Deletes all API and repository keys associated with the account.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_key:delete_all(erlpm_core:default_config()).
%% {ok, {200, ..., [#{
%%     <<"authing_key">> => true,
%%     <<"inserted_at">> => <<"2019-02-27T11:15:32Z">>,
%%     <<"last_use">> =>
%%         #{<<"ip">> => <<"1.2.3.4">>,
%%           <<"used_at">> => <<"2019-02-27T14:38:54Z">>,
%%           <<"user_agent">> => <<"epm_core/0.5.0 (httpc) (OTP/21) (erts/10.2)">>},
%%     <<"name">> => <<"epm_core">>,
%%     <<"permissions">> => [#{<<"domain">> => <<"api">>,<<"resource">> => <<"read">>}],
%%     <<"revoked_at">> => nil,
%%     <<"updated_at">> => <<"2019-02-27T14:38:54Z">>,
%%     <<"url">> => <<"https://erl.pm/api/keys/test">>},
%%     }]}}
%% '''
%% @end
-spec delete_all(erlpm_core:config()) -> erlpm_api:response().
delete_all(Config) when is_map(Config) ->
    Path = erlpm_api:build_organization_path(Config, ["keys"]),
    erlpm_api:delete(Config, Path).
