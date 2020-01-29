%%
%% Title  : CS 511 C - HW Assignment 4
%% Desc   : TinyChat, a simple chat application written in Erlang
%% Name   : Bobby Georgiou
%% Date   : 11/17/2019
%% Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
%%

-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) -> 
	RoomPid = case maps:find(ChatName, State#serv_st.chatrooms) of
		{ok, R} -> R; % chatroom exists
		_ -> spawn(chatroom, start_chatroom, [ChatName]) % spawn new chatroom
	end,
	ClientNick = case maps:find(ClientPID, State#serv_st.nicks) of {ok, Nick} -> Nick end,
	RoomPid ! {self(), Ref, register, ClientPID, ClientNick}, % register with chatroom
	RoomClients = case maps:find(ChatName, State#serv_st.registrations) of 
		{ok, RC} -> RC;
		_ -> []
	end,
	State#serv_st{nicks=State#serv_st.nicks, registrations=maps:put(ChatName, [ClientPID]++RoomClients, State#serv_st.registrations), 
		chatrooms=maps:put(ChatName, RoomPid, State#serv_st.chatrooms)}.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	RoomPid = case maps:find(ChatName, State#serv_st.chatrooms) of {ok, R} -> R end,
	RoomPid ! {self(), Ref, unregister, ClientPID},
	ClientPID ! {self(), Ref, ack_leave},
	NewRoomClients = case maps:find(ChatName, State#serv_st.registrations) of {ok, RC} -> lists:delete(ClientPID, RC) end,
	State#serv_st{nicks=State#serv_st.nicks, 
		registrations=maps:put(ChatName, NewRoomClients, State#serv_st.registrations), 
		chatrooms=State#serv_st.chatrooms}.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
	case lists:member(NewNick, maps:values(State#serv_st.nicks)) of
		true -> ClientPID ! {self(), Ref, err_nick_used}, State;
		_ -> 
			ClientRooms = maps:filter(fun (_, Clients) -> lists:member(ClientPID, Clients) end, State#serv_st.registrations),
			lists:foreach(fun (ChatName) ->
				RPid = maps:get(ChatName, State#serv_st.chatrooms),
				RPid ! {self(), Ref, update_nick, ClientPID, NewNick} % each of client's chatrooms should update the client's nickname
			end, maps:keys(ClientRooms)),
			ClientPID ! {self(), Ref, ok_nick},
			State#serv_st{nicks=maps:put(ClientPID, NewNick, State#serv_st.nicks), registrations=State#serv_st.registrations, chatrooms=State#serv_st.chatrooms}
	end.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
	NewNicks = maps:remove(ClientPID, State#serv_st.nicks),
	ClientRooms = maps:filter(fun (_, Clients) -> lists:member(ClientPID, Clients) end, State#serv_st.registrations),
	lists:foreach(fun (ChatName) ->
		RPid = maps:find(ChatName, State#serv_st.chatrooms),
		RPid ! {self(), Ref, unregister, ClientPID} % each of client's chatrooms should unregister the client
	end, maps:keys(ClientRooms)),
	NewRegistrations = maps:map(fun (_, V) -> lists:delete(ClientPID, V) end, State#serv_st.registrations), % remove client's pid from all of its rooms
	ClientPID ! {self(), Ref, ack_quit},
	State#serv_st{nicks=NewNicks, registrations=NewRegistrations, chatrooms=State#serv_st.chatrooms}.
