%%
%% Title  : CS 511 C - HW Assignment 5
%% Desc   : Modeling a system of sensors/watchers with process monitoring in Erlang
%% Name   : Bobby Georgiou
%% Date   : 12/01/2019
%% Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
%%

-module(sensor).
-compile(export_all).
-author("Bobby Georgiou").

measure(Id, WatcherPid) ->
    Measurement = rand:uniform(11),
    case Measurement of
        11 -> exit(anomalous_reading);
        M -> WatcherPid ! {Id, M} % send measurement to watcher
    end,
    timer:sleep(rand:uniform(10000)),
    measure(Id, WatcherPid).
