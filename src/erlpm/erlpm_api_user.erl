%% Vendor from epm_core v0.7.1, do not edit manually

-module(erlpm_api_user).
-export([
    create/4, get/2, me/1, reset_password/2
]).

%% @doc
%% Gets the authenticated user.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_user:me(erlpm_core:default_config()).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"full_name">> => <<"John Doe">>,
%%      <<"handles">> => #{...},
%%      <<"inserted_at">> => <<"2014-04-21T17:20:12Z">>,
%%      <<"level">> => <<"full">>,
%%      <<"updated_at">> => <<"2019-08-04T19:28:05Z">>,
%%      <<"url">> => <<"https://erl.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec me(erlpm_core:config()) -> erlpm_api:response().
me(Config) when is_map(Config) ->
    erlpm_api:get(Config, ["users", "me"]).

%% @doc
%% Creates a new user account.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_user:create(erlpm_core:default_config(), <<"user">>, <<"hunter42">>, <<"user@example.com">>).
%% {ok, {201, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"full_name">> => <<"John Doe">>,
%%      <<"handles">> => #{...},
%%      <<"inserted_at">> => <<"2014-04-21T17:20:12Z">>,
%%      <<"level">> => <<"full">>,
%%      <<"updated_at">> => <<"2019-08-04T19:28:05Z">>,
%%      <<"url">> => <<"https://erl.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec create(erlpm_core:config(), binary(), binary(), binary()) -> erlpm_api:response().
create(Config, Username, Password, Email)
    when is_map(Config) and is_binary(Username) and is_binary(Password) and is_binary(Email) ->
    Params = #{
        <<"username">> => Username,
        <<"password">> => Password,
        <<"email">> => Email
    },
    erlpm_api:post(Config, ["users"], Params).

%% @doc
%% Resets the user's password.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_user:reset_password(erlpm_core:default_config(), <<"user">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec reset_password(erlpm_core:config(), binary()) -> erlpm_api:response().
reset_password(Config, Username) when is_map(Config) and is_binary(Username) ->
    erlpm_api:post(Config, ["users", Username, "reset"], #{}).

%% @doc
%% Gets a user.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_user:get(erlpm_core:default_config()).
%% {ok, {200, ..., #{
%%      <<"email">> => <<"user@example.com">>,
%%      <<"full_name">> => <<"John Doe">>,
%%      <<"handles">> => #{...},
%%      <<"inserted_at">> => <<"2014-04-21T17:20:12Z">>,
%%      <<"level">> => <<"full">>,
%%      <<"updated_at">> => <<"2019-08-04T19:28:05Z">>,
%%      <<"url">> => <<"https://erl.pm/api/users/user">>,
%%      <<"username">> => <<"user">>
%%      }}}
%% '''
%% @end
-spec get(erlpm_core:config(), binary()) -> erlpm_api:response().
get(Config, Username) when is_map(Config) and is_binary(Username) ->
    erlpm_api:get(Config, ["users", Username]).
