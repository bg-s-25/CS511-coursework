-module(gg).
-compile(export_all).

% EB8 #8 - guessing game
%% Notes:
%%% When client asks to play guessing game, server spawns a servlet to handle requests from that particular client
%% Usage: 
%%% Spawn 10 clients to guess game: S = gg:start(), [spawn(gg, client,[S]) || _ <- lists:seq(1, 10)]

start() ->
    spawn(fun server/0).

server() ->
    receive
        {start, From, Ref} -> 
            S = spawn(?MODULE, servlet, [rand:uniform(20)]), % pass arguments to servlet
            From ! {servlet, self(), Ref, S},
            server() % call server again to continue receiving, tail recursive
    end.

servlet(Client, N) ->
    receive % servlet should only process messages from original client
        {guess, Client, Ref, N} -> % pattern match received N with target N; maybe change to if..else
            Client ! {gotIt, self(), Ref};
        {guess, Client, Ref, _} -> 
            Client ! {tryAgain, self(), Ref},
            servlet(Client, N)
    end.

client(S) -> % argument S is the server
    R = make_ref(),
    S ! {start, self(), R},
    receive
        {servlet, S, R, Servlet} -> client_loop(Servlet, 0)
    end.

client_loop(Servlet, C) -> % arguments: servlet pid, guess count
    R = make_ref(),
    Servlet ! {guess, self(), R, rand:uniform(20)},
    receive
        {servlet, From, Ref, ServletPid} -> ServletPid;
        {gotIt, From, Ref} -> io:format("Client ~p guessed in ~w attempts~n", [self(), C]);
        {tryAgain, From, Ref} -> client_loop(Servlet, C+1)
    end.
