%%
%% Title  : CS 511 C - HW Assignment 5
%% Desc   : Modeling a system of sensors/watchers with process monitoring in Erlang
%% Name   : Bobby Georgiou
%% Date   : 12/01/2019
%% Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
%%

-module(watcher).
-compile(export_all).
-author("Bobby Georgiou").

watch_sensors(Sensors) ->
    receive
        {Id, M} -> 
            %% report reading
            io:fwrite("Got ~p from sensor ~p~n", [M, Id]),
            watch_sensors(Sensors);
        {'DOWN', _, _, SensorPid, Reason} -> 
            %% lookup sensor id
            SensorId = case lists:filter(fun ({_, Pid}) -> Pid==SensorPid end, Sensors) of
                [{Id, _}] -> Id
            end,
            io:fwrite("Sensor ~p died b/c ~p~n", [SensorId, Reason]),
            %% respawn sensor
            {Pid, _} = spawn_monitor(sensor, measure, [SensorId, self()]),
            NewSensors = lists:delete({SensorId, SensorPid}, Sensors)++[{SensorId, Pid}],
            io:fwrite("Sensor ~p restarted. Sensors: ~p~n", [SensorId, NewSensors]),
            watch_sensors(NewSensors)
    end.

start_sensors(0, _, Sensors) ->
    %% start monitoring
    io:fwrite("Watcher started. Sensors: ~p~n", [Sensors]),
    watch_sensors(Sensors);
start_sensors(N, CurId, Sensors) ->
    %% spawn and monitor a sensor
    {Pid, _} = spawn_monitor(sensor, measure, [CurId, self()]),
    start_sensors(N - 1, CurId + 1, Sensors++[{CurId, Pid}]).
