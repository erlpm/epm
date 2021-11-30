%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_prv_local_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").
-include_lib("providers/include/providers.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROVIDER, upgrade).
-define(NAMESPACE, local).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 =
        epm_state:add_provider(State,
                                providers:create([{name, ?PROVIDER},
                                                  {module, ?MODULE},
                                                  {bare, true},
                                                  {namespace, ?NAMESPACE},
                                                  {deps, ?DEPS},
                                                  {example, "epm unstable upgrade"},
                                                  {short_desc, "Download latest epm escript and extract."},
                                                  {desc, ""},
                                                  {opts, []}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    case os:type() of
        {win32, _} ->
            ?ERROR("Sorry, this feature is not yet available on Windows.", []),
            {ok, State};
        _ ->
            Md5 = case epm_state:escript_path(State) of
                      undefined ->
                          false;
                      ScriptPath ->
                          get_md5(ScriptPath)
                  end,

            case maybe_fetch_epm(Md5) of
                {saved, TmpEpm} ->
                    {Vsn, Archive} =
                        try
                            {ok, Escript} = escript:extract(TmpEpm, []),
                            {comment, "Epm " ++ EpmVsn} = lists:keyfind(comment, 1, Escript),
                            {archive, FullArchive} = lists:keyfind(archive, 1, Escript),
                            {EpmVsn, FullArchive}
                        catch
                            C:T:S ->
                                ?DIAGNOSTIC("local upgrade version extraction exception: ~p:~p:~p", [C, T, S]),
                                error(?PRV_ERROR(failed_vsn_lookup))
                        end,
                    epm_prv_local_install:install_escript(State, Vsn, Archive);
                up_to_date ->
                    {ok, State};
                Error ->
                    Error
            end
    end.

-spec format_error(any()) -> iolist().
format_error(failed_vsn_lookup) ->
    "Failed to extract the version from the downloaded epm escript.\n     Try downloading https://www.erl.pm/dl/epm manually and running `chmod +x epm && ./epm local install`";
format_error(bad_checksum) ->
    "Not updating epm, the checksum of download did not match the one provided by s3.";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal

get_md5(EpmPath) ->
    {ok, EpmFile} = file:read_file(EpmPath),
    Digest = crypto:hash(md5, EpmFile),
    DigestHex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Digest)]),
    epm_string:lowercase(DigestHex).

maybe_fetch_epm(EpmMd5) ->
    TmpDir = ec_file:insecure_mkdtemp(),
    TmpFile = filename:join(TmpDir, "epm"),
    case request("https://www.erl.pm/erlpm/epm", EpmMd5) of
        {ok, Binary, ETag} ->
            file:write_file(TmpFile, Binary),
            case etag(TmpFile) of
                ETag ->
                    {saved, TmpFile};
                _ ->
                    ?PRV_ERROR(bad_checksum)
            end;
        error ->
            ?ERROR("Unable to fetch latest epm escript. Please try again later.", []);
        _ ->
            ?CONSOLE("No upgrade available", []),
            up_to_date
    end.

etag(Path) ->
     case file:read_file(Path) of
         {ok, Binary} ->
             <<X:128/big-unsigned-integer>> = crypto:hash(md5, Binary),
             epm_string:lowercase(lists:flatten(io_lib:format("~32.16.0b", [X])));
         {error, _} ->
             false
     end.

-spec request(Url, ETag) -> Res when
      Url :: string(),
      ETag :: false | string(),
      Res :: 'error' | {ok, cached} | {ok, any(), string()}.
request(Url, ETag) ->
    HttpOptions = [{ssl, epm_utils:ssl_opts(Url)},
                   {relaxed, true} | epm_utils:get_proxy_auth()],
    case httpc:request(get, {Url, [{"if-none-match", "\"" ++ ETag ++ "\""}
                                   || ETag =/= false] ++
                                 [{"User-Agent", epm_utils:user_agent()}]},
                       HttpOptions, [{body_format, binary}], epm) of
        {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
            ?DEBUG("Successfully downloaded ~ts", [Url]),
            {"etag", ETag1} = lists:keyfind("etag", 1, Headers),
            {ok, Body, epm_string:trim(ETag1, both, [$"])};
        {ok, {{_Version, 304, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Cached copy of ~ts still valid", [Url]),
            {ok, cached};
        {ok, {{_Version, Code, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Request to ~p failed: status code ~p", [Url, Code]),
            error;
        {error, Reason} ->
            ?DEBUG("Request to ~p failed: ~p", [Url, Reason]),
            error
    end.
