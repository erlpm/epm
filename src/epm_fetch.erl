%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% epm: Erlang Build Tools
%%
%% -------------------------------------------------------------------
-module(epm_fetch).

-export([lock_source/2,
         download_source/2,
         needs_update/2]).

-export([format_error/1]).

-include("epm.hrl").
-include_lib("providers/include/providers.hrl").

-spec lock_source(epm_app_info:t(), epm_state:t())
                 -> epm_resource_v2:source() | {error, string()}.
lock_source(AppInfo, State)      ->
    epm_resource_v2:lock(AppInfo, State).

-spec download_source(epm_app_info:t(), epm_state:t())
                     -> epm_app_info:t() | {error, any()}.
download_source(AppInfo, State)  ->
    AppDir = epm_app_info:dir(AppInfo),
    try download_source_(AppInfo, State) of
        ok ->
            %% freshly downloaded, update the app info opts to reflect the new config
            Config = epm_config:consult(AppDir),
            AppInfo1 = epm_app_info:update_opts(AppInfo, epm_app_info:opts(AppInfo), Config),
            case epm_app_discover:find_app(AppInfo1, AppDir, all, State) of
                {true, AppInfo2} ->
                    epm_app_info:is_available(AppInfo2, true);
                false ->
                    throw(?PRV_ERROR({dep_app_not_found, epm_app_info:name(AppInfo1)}))
            end;
        {error, Reason} ->
            throw(?PRV_ERROR(Reason))
    catch
        %% if already a PRV_ERROR format just re-raise it
        ?WITH_STACKTRACE(error, {error, {Module, Reason}}, S)
            erlang:raise(error, {error, {Module, Reason}}, S);
        throw:{no_resource, Type, Location} ->
            throw(?PRV_ERROR({no_resource, Location, Type}));
        ?WITH_STACKTRACE(C,T,S)
            ?DIAGNOSTIC("epm_fetch exception ~p ~p ~p", [C, T, S]),
            throw(?PRV_ERROR({fetch_fail, epm_app_info:source(AppInfo)}))
    end.

download_source_(AppInfo, State) ->
    AppDir = epm_app_info:dir(AppInfo),
    TmpDir = ec_file:insecure_mkdtemp(),
    AppDir1 = epm_utils:to_list(AppDir),
    case epm_resource_v2:download(TmpDir, AppInfo, State) of
        ok ->
            ec_file:mkdir_p(AppDir1),
            code:del_path(filename:absname(filename:join(AppDir1, "ebin"))),
            FetchDir = epm_app_info:fetch_dir(AppInfo),
            ok = epm_file_utils:rm_rf(filename:absname(FetchDir)),
            ?DIAGNOSTIC("Moving checkout ~p to ~p", [TmpDir, filename:absname(FetchDir)]),
            epm_file_utils:mv(TmpDir, filename:absname(FetchDir));
        Error ->
            Error
    end.

-spec needs_update(epm_app_info:t(), epm_state:t())
                  -> boolean() | {error, string()}.
needs_update(AppInfo, State) ->
    try
        epm_resource_v2:needs_update(AppInfo, State)
    catch
        _:_ ->
            true
    end.

format_error({fetch_fail, Name, Vsn}) ->
    io_lib:format("Failed to fetch and copy dep: ~ts-~ts", [Name, Vsn]);
format_error({fetch_fail, Source}) ->
    io_lib:format("Failed to fetch and copy dep: ~p", [Source]);
format_error({dep_app_not_found, AppName}) ->
    io_lib:format("Dependency failure: source for ~ts does not contain a "
                  "recognizable project and can not be built", [AppName]).
