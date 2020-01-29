%%
%% Title  : CS 511 C - HW Assignment 5
%% Desc   : Modeling a system of sensors/watchers with process monitoring in Erlang
%% Name   : Bobby Georgiou
%% Date   : 12/01/2019
%% Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
%%

-module(main).
-compile(export_all).
-author("Bobby Georgiou").

setup_loop_help(N, Num_watchers, IdStart) ->
    %% spawn needed watchers, watchers will spawn & monitor sensors
    if N > 10 ->
        spawn(watcher, start_sensors, [10, IdStart, []]),
        setup_loop_help(N - 10, Num_watchers - 1, IdStart + 10);
    true ->
        spawn(watcher, start_sensors, [N, IdStart, []])
    end.

setup_loop(N, Num_watchers) ->
    setup_loop_help(N, Num_watchers, 0).

start() ->
    {ok, [ N ]} = io:fread("enter number of sensors> ", "~d"),
    if N =< 1 ->
        io:fwrite("setup: range must be at least 2~n", []);
    true ->
        Num_watchers = 1 + (N div 10),
        setup_loop(N, Num_watchers)
    end.
