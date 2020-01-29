%%
%% Title  : CS 511 C - HW Assignment 3
%% Desc   : Various operations for shipping company ShipCo written in Erlang
%% Name   : Bobby Georgiou
%% Date   : 10/27/2019
%% Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
%%

-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

get_ship(Shipping_State, Ship_ID) ->
    case lists:filter(fun (R) -> R#ship.id==Ship_ID end, Shipping_State#shipping_state.ships) of
        [] -> error; % no ship with given id
        [R] -> R
    end.

get_container(Shipping_State, Container_ID) ->
    case lists:filter(fun (R) -> R#container.id==Container_ID end, Shipping_State#shipping_state.containers) of
        [] -> error; % no container with given id
        [R] -> R
    end.

get_port(Shipping_State, Port_ID) ->
    case lists:filter(fun (R) -> R#port.id==Port_ID end, Shipping_State#shipping_state.ports) of
        [] -> error; % no port with given id"
        [R] -> R
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    case lists:filter(fun ({Port, _, _}) -> Port==Port_ID end, Shipping_State#shipping_state.ship_locations) of
        [] -> error; % no occupied docks
        Docks -> lists:map(fun ({_, Dock, _}) -> Dock end, Docks)
    end.

get_ship_location(Shipping_State, Ship_ID) ->
    case lists:filter(fun ({_, _, Ship}) -> Ship==Ship_ID end, Shipping_State#shipping_state.ship_locations) of
        [] -> error; % ship not found
        [{Port, Dock, _}] -> {Port, Dock}
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    lists:foldl(fun (C, Sum) -> (case get_container(Shipping_State, C) of
        R -> R#container.weight;
        error -> error % some containers not found
    end) + Sum end, 0, Container_IDs).

get_ship_weight(Shipping_State, Ship_ID) ->
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        {ok, Cs} -> lists:foldl(fun (C, Sum) -> get_container_weight(Shipping_State, [C]) + Sum end, 0, Cs);
        error -> error % ship not found
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    P = case get_ship_location(Shipping_State, Ship_ID) of {P, _} -> P end,
    ShipConts = case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end,
    ShipCap = case get_ship(Shipping_State, Ship_ID) of R -> R#ship.container_cap end,
    ShipOk = lists:flatlength(Container_IDs) + lists:flatlength(ShipConts) < ShipCap,
    case Container_IDs of
        [] -> Shipping_State;
        [C|Cs] -> case lists:member(C, case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, X} -> X end) of
            true when ShipOk -> % current container (C) is at ship's port; call load_ship using updated record values
                NewPort = lists:delete(C, case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end),
                NewShip = lists:append(case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end, [C]),
                NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations, ship_inventory=maps:put(Ship_ID, NewShip, Shipping_State#shipping_state.ship_inventory),
                port_inventory=maps:put(P, NewPort, Shipping_State#shipping_state.port_inventory)},
                load_ship(NewSS, Ship_ID, Cs);
            _ -> error % container is not at the same port as the ship OR requested containers exceed container cap
        end
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    P = case get_ship_location(Shipping_State, Ship_ID) of {P, _} -> P end,
    PortConts = case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end,
    ShipConts = case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end,
    PortCap = case get_port(Shipping_State, P) of R -> R#port.container_cap end,
    PortOk = lists:flatlength(ShipConts) + lists:flatlength(PortConts) < PortCap,
    case PortOk of
        true -> % all containers on the ship can be unloaded
            NewPort = lists:append(case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end, ShipConts),
            NewShip = [],
            NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations, ship_inventory=maps:put(Ship_ID, NewShip, Shipping_State#shipping_state.ship_inventory),
                port_inventory=maps:put(P, NewPort, Shipping_State#shipping_state.port_inventory)},
            NewSS;
        _ -> error % ship containers exceed port cap
    end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    P = case get_ship_location(Shipping_State, Ship_ID) of {P, _} -> P end,
    PortConts = case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end,
    PortCap = case get_port(Shipping_State, P) of R -> R#port.container_cap end,
    PortOk = lists:flatlength(Container_IDs) + lists:flatlength(PortConts) < PortCap,
    case Container_IDs of
        [] -> Shipping_State;
        [C|Cs] -> case lists:member(C, case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, X} -> X end) of
            true when PortOk -> % current container (C) is loaded on the ship; call unload_ship using updated record values
                NewPort = lists:append(case maps:find(P, Shipping_State#shipping_state.port_inventory) of {ok, L} -> L end, [C]),
                NewShip = lists:delete(C, case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of {ok, M} -> M end),
                NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
                ship_locations=Shipping_State#shipping_state.ship_locations, ship_inventory=maps:put(Ship_ID, NewShip, Shipping_State#shipping_state.ship_inventory),
                port_inventory=maps:put(P, NewPort, Shipping_State#shipping_state.port_inventory)},
                unload_ship(NewSS, Ship_ID, Cs);
            _ -> error % container is not loaded on the ship OR requested containers exceed port cap
        end
    end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    PortsDocks = lists:map(fun ({P, D, _}) -> {P, D} end, Shipping_State#shipping_state.ship_locations),
    case lists:member({Port_ID, Dock}, PortsDocks) of
        true -> error; % port & dock combination is occupied
        _ -> case lists:filter(fun ({_, _, S}) -> S==Ship_ID end, Shipping_State#shipping_state.ship_locations) of % find location with Ship_ID
            [] -> NewShipLoc = lists:append(Shipping_State#shipping_state.ship_locations, [{Port_ID, Dock, Ship_ID}]);
            [T] -> NewShipLoc = lists:append(lists:delete(T, Shipping_State#shipping_state.ship_locations), [{Port_ID, Dock, Ship_ID}])
        end,
        NewSS = #shipping_state{ships=Shipping_State#shipping_state.ships, containers=Shipping_State#shipping_state.containers, ports=Shipping_State#shipping_state.ports,
        ship_locations=NewShipLoc, ship_inventory=Shipping_State#shipping_state.ship_inventory, port_inventory=Shipping_State#shipping_state.port_inventory},
        NewSS
    end.




%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
