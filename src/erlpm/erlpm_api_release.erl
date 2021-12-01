%% Vendor from epm_core v0.7.1, do not edit manually

-module(erlpm_api_release).
-export([
    delete/3, get/3, publish/2, publish/3, retire/4, unretire/3
]).

-export_type([publish_params/0, retirement_params/0, retirement_reason/0]).

-type publish_params() :: [{replace, boolean()}].

-type retirement_reason() :: other | invalid | security | deprecated | renamed.

-ifdef(OTP_19).
-type retirement_params() :: #{reason := retirement_reason(), message => binary()}.
-else.
-type retirement_params() :: #{reason => retirement_reason(), message => binary()}.
-endif.
%% @doc
%% Gets a package release.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_release:get(erlpm_core:default_config(), <<"package">>, <<"1.0.0">>).
%% {ok, {200, ..., #{
%%      <<"checksum">> => <<"540d210d81f56f17f64309a4896430e727972499b37bd59342dc08d61dff74d8">>,
%%      <<"docs_html_url">> => <<"https://hexdocs.pm/package/1.0.0/">>,
%%      <<"downloads">> => 740,<<"has_docs">> => true,
%%      <<"html_url">> => <<"https://erl.pm/packages/package/1.0.0">>,
%%      <<"inserted_at">> => <<"2014-12-09T18:32:03Z">>,
%%      <<"meta">> =>
%%          #{<<"app">> => <<"package">>,
%%            <<"build_tools">> => [<<"mix">>]},
%%      <<"package_url">> => <<"https://erl.pm/api/packages/package">>,
%%      <<"publisher">> => nil,<<"requirements">> => #{},
%%      <<"retirement">> => nil,
%%      <<"updated_at">> => <<"2019-07-28T21:12:11Z">>,
%%      <<"url">> => <<"https://erl.pm/api/packages/package/releases/1.0.0">>,
%%      <<"version">> => <<"1.0.0">>
%%      }}}
%% '''
%% @end
-spec get(erlpm_core:config(), binary(), binary()) -> erlpm_api:response().
get(Config, Name, Version) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = erlpm_api:build_repository_path(Config, ["packages", Name, "releases", Version]),
    erlpm_api:get(Config, Path).

%% @doc
%% Publishes a new package release.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_release:publish(erlpm_core:default_config(), Tarball).
%% {ok, {200, ..., #{
%%      <<"checksum">> => <<"540d210d81f56f17f64309a4896430e727972499b37bd59342dc08d61dff74d8">>,
%%      <<"docs_html_url">> => <<"https://hexdocs.pm/package/1.0.0/">>,
%%      <<"downloads">> => 740,<<"has_docs">> => true,
%%      <<"html_url">> => <<"https://erl.pm/packages/package/1.0.0">>,
%%      <<"inserted_at">> => <<"2014-12-09T18:32:03Z">>,
%%      <<"meta">> =>
%%          #{<<"app">> => <<"package">>,
%%            <<"build_tools">> => [<<"mix">>]},
%%      <<"package_url">> => <<"https://erl.pm/api/packages/package">>,
%%      <<"publisher">> => nil,<<"requirements">> => #{},
%%      <<"retirement">> => nil,
%%      <<"updated_at">> => <<"2019-07-28T21:12:11Z">>,
%%      <<"url">> => <<"https://erl.pm/api/packages/package/releases/1.0.0">>,
%%      <<"version">> => <<"1.0.0">>
%%      }}}
%% '''
%% @end
-spec publish(erlpm_core:config(), binary()) -> erlpm_api:response().
publish(Config, Tarball) -> publish(Config, Tarball, []).


%% @doc
%% Publishes a new package release with query parameters.
%%
%% Supported query params :
%%  - replace : boolean
%%
%% Examples:
%%
%% ```
%% > erlpm_api_release:publish(erlpm_core:default_config(), Tarball, [{replace, true}]).
%% {ok, {201, ..., #{
%%      <<"checksum">> => <<"540d210d81f56f17f64309a4896430e727972499b37bd59342dc08d61dff74d8">>,
%%      <<"docs_html_url">> => <<"https://hexdocs.pm/package/1.0.0/">>,
%%      <<"downloads">> => 740,<<"has_docs">> => true,
%%      <<"html_url">> => <<"https://erl.pm/packages/package/1.0.0">>,
%%      <<"inserted_at">> => <<"2014-12-09T18:32:03Z">>,
%%      <<"meta">> =>
%%          #{<<"app">> => <<"package">>,
%%            <<"build_tools">> => [<<"mix">>]},
%%      <<"package_url">> => <<"https://erl.pm/api/packages/package">>,
%%      <<"publisher">> => nil,<<"requirements">> => #{},
%%      <<"retirement">> => nil,
%%      <<"updated_at">> => <<"2019-07-28T21:12:11Z">>,
%%      <<"url">> => <<"https://erl.pm/api/packages/package/releases/1.0.0">>,
%%      <<"version">> => <<"1.0.0">>
%%      }}}
%% '''
%% @end
-spec publish(erlpm_core:config(), binary(), publish_params()) -> erlpm_api:response().
publish(Config, Tarball, Params) when is_map(Config) andalso is_binary(Tarball) andalso is_list(Params) ->
    QueryString = erlpm_api:encode_query_string([{replace, proplists:get_value(replace, Params, false)}]),
    Path = erlpm_api:join_path_segments(erlpm_api:build_repository_path(Config, ["publish"])),
    PathWithQuery = <<Path/binary, "?", QueryString/binary>>,
    TarballContentType = "application/octet-stream",
    Config2 = put_header(<<"content-length">>, integer_to_binary(byte_size(Tarball)), Config),
    Body = {TarballContentType, Tarball},
    erlpm_api:post(Config2, PathWithQuery, Body).

%% @doc
%% Deletes a package release.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_release:delete(erlpm_core:default_config(), <<"package">>, <<"1.0.0">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec delete(erlpm_core:config(), binary(), binary()) -> erlpm_api:response().
delete(Config, Name, Version) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = erlpm_api:build_repository_path(Config, ["packages", Name, "releases", Version]),
    erlpm_api:delete(Config, Path).

%% @doc
%% Retires a package release.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_release:retire(erlpm_core:default_config(), <<"package">>, <<"1.0.0">>, Params).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec retire(erlpm_core:config(), binary(), binary(), retirement_params()) -> erlpm_api:response().
retire(Config, Name, Version, Params) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = erlpm_api:build_repository_path(Config, ["packages", Name, "releases", Version, "retire"]),
    erlpm_api:post(Config, Path, Params).

%% @doc
%% Unretires a package release.
%%
%% Examples:
%%
%% ```
%% > erlpm_api_release:unretire(erlpm_core:default_config(), <<"package">>, <<"1.0.0">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec unretire(erlpm_core:config(), binary(), binary()) -> erlpm_api:response().
unretire(Config, Name, Version) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = erlpm_api:build_repository_path(Config, ["packages", Name, "releases", Version, "retire"]),
    erlpm_api:delete(Config, Path).

%%====================================================================
%% Internal functions
%%====================================================================

put_header(Name, Value, Config) ->
    Headers = maps:get(http_headers, Config, #{}),
    Headers2 = maps:put(Name, Value, Headers),
    maps:put(http_headers, Headers2, Config).
