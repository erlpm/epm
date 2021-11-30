-module(epm_compiler_yrl).

-behaviour(epm_compiler).

-export([context/1,
         needed_files/4,
         dependencies/3,
         compile/4,
         clean/2]).

context(AppInfo) ->
    Dir = epm_app_info:dir(AppInfo),
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".yrl",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, Mappings, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles,
                           not lists:member(Source, FirstFiles),
                           epm_compiler:needs_compile(Source, ".erl", Mappings)],

    Opts = epm_opts:get(epm_app_info:opts(AppInfo), yrl_opts, []),
    Opts1 = epm_compiler_xrl:update_opts(Opts, AppInfo),

    {{FirstFiles, Opts1}, {RestFiles, Opts1}}.

dependencies(_, _, _) ->
    [].

compile(Source, [{_, OutDir}], _, Opts0) ->
    Opts = case proplists:get_value(parserfile, Opts0) of
        undefined ->
            BaseName = filename:basename(Source, ".yrl"),
            Target = filename:join([OutDir, BaseName]),
            [{parserfile, Target} | Opts0];
        _ ->
            Opts0
    end,
    AllOpts = [{return, true} | Opts],
    case yecc:file(Source, AllOpts) of
        {ok, _} ->
            ok;
        {ok, _Mod, Ws} ->
            epm_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            epm_compiler:error_tuple(Source, Es, Ws, AllOpts)
    end.

clean(YrlFiles, _AppInfo) ->
    epm_file_utils:delete_each(
      [epm_utils:to_list(re:replace(F, "\\.yrl$", ".erl", [unicode]))
       || F <- YrlFiles]).
