%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(epm_prv_as).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("epm.hrl").

-define(PROVIDER, as).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(epm_state:t()) -> {ok, epm_state:t()}.
init(State) ->
    State1 = epm_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "epm as <profile1>,<profile2>,... <task1>, <task2>, ..."},
                                                               {short_desc, "Higher order provider for running multiple tasks in a sequence as a certain profiles."},
                                                               {desc, "Higher order provider for running multiple tasks in a sequence as a certain profiles."},
                                                               {opts, [{profile, undefined, undefined, string, "Profiles to run as."}]}])),
    {ok, State1}.

-spec do(epm_state:t()) -> {ok, epm_state:t()} | {error, string()}.
do(State) ->
    {Profiles, Tasks} = args_to_profiles_and_tasks(epm_state:command_args(State)),
    case {Profiles, Tasks} of
        {[], _} ->
            {error, "At least one profile must be specified when using `as`"};
        {_, []} ->
            {error, "At least one task must be specified when using `as`"};
        _  ->
            warn_on_empty_profile(Profiles, State),
            State1 = epm_state:apply_profiles(State, [list_to_atom(X) || X <- Profiles]),
            State2 = epm_plugins:project_apps_install(State1),
            {FirstTask, FirstTaskArgs} = hd(Tasks),
            FirstTaskAtom = list_to_atom(FirstTask),
            case epm_core:process_namespace(State2, FirstTaskAtom) of
                {ok, State3, NewTask} ->
                    epm_prv_do:do_tasks(
                        [{atom_to_list(NewTask),FirstTaskArgs}|tl(Tasks)],
                        State3
                    );
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

args_to_profiles_and_tasks(Args) ->
    first_profile(Args).

first_profile([]) -> {[], []};
first_profile([ProfileList|Rest]) ->
    case re:split(ProfileList, ",", [{return, list}, {parts, 2}, unicode]) of
        %% `foo, bar`
        [P, ""]   -> profiles(Rest, [P]);
        %% `foo,bar`
        [P, More] -> profiles([More] ++ Rest, [P]);
        %% `foo`
        [P]       -> comma_or_end(Rest, [P])
    end.

profiles([], Acc) -> {lists:reverse(Acc), epm_utils:args_to_tasks([])};
profiles([ProfileList|Rest], Acc) ->
    case re:split(ProfileList, ",", [{return, list}, {parts, 2}, unicode]) of
        %% `foo, bar`
        [P, ""]   -> profiles(Rest, [P|Acc]);
        %% `foo,bar`
        [P, More] -> profiles([More] ++ Rest, [P|Acc]);
        %% `foo`
        [P]       -> comma_or_end(Rest, [P|Acc])
    end.

%% `, foo...`
comma_or_end([","|Rest], Acc) ->
    profiles(Rest, Acc);
%% `,foo...`
comma_or_end(["," ++ Profile|Rest], Acc) ->
    profiles([Profile|Rest], Acc);
comma_or_end(Tasks, Acc) ->
    {lists:reverse(Acc), epm_utils:args_to_tasks(Tasks)}.

%% If a profile is used by 'as' but has no entry under `profile` within
%% the top level epm.config or any project app's epm.config print a warning.
%% This is just to help developers, in case they forgot to define a profile but
%% thought it was being used.
warn_on_empty_profile(Profiles, State) ->
    ProjectApps = epm_state:project_apps(State),
    DefinedProfiles = epm_state:get(State, profiles, []) ++
        lists:flatten([epm_app_info:get(AppInfo, profiles, []) || AppInfo <- ProjectApps]),
    [?WARN("No entry for profile ~ts in config.", [Profile])
     || Profile <- Profiles,
        not lists:keymember(list_to_atom(Profile), 1, DefinedProfiles),
        Profile =/= "global", Profile =/= "default"],
    ok.
