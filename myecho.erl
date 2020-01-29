-module(myecho).
-compile(export_all).

echoserver() ->
    receive
        {From, Msg} -> 
            io:format("Got '~p' from ~p", [Msg, From]),
            echoserver()
    end.

start() ->
    spawn(?MODULE, echoserver, []).
