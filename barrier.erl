-module(barrier).
-compile(export_all).

%% Barrier example
%% Should always print a permutation of letters followed by numbers

%% L is a list of client pids that have reached barrier
barrier(0, N, L) -> % all processes have reached the barrier
    lists:foreach(fun (R) -> R ! {self(), ok} end, L), % notify all threads that they can proceed
    barrier(3, N, []); % restart barrier
barrier(M, N, L) when M>0 -> % some processes have not yet reached barrier
    receive
        {From, reached} -> 
            barrier(M-1, N, [From|L]) % update barrier state & call it
    end.

pass_barrier(B) ->
    B ! {self(), reached},
    receive
        {B, ok} -> ok
    end.

client(B, Letter, Number) ->
    io:format("~p ~s~n", [self(), Letter]),
    pass_barrier(B), % wait for pass_barrier to receive an ok, then print number
    io:format("~p ~w~n", [self(), Number]),

    %% introduce loop to illustrate different permutations:
    pass_barrier(B),
    client(B, Letter, Number).

start() ->
    B = spawn(?MODULE, barrier, [3, 3, []]),
    spawn(?MODULE, client, [B, "a", 1]),
    spawn(?MODULE, client, [B, "b", 2]),
    spawn(?MODULE, client, [B, "c", 3]).
