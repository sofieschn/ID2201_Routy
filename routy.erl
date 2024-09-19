-module(routy).
-export([start/2, stop/1, init/1, status/1]).


start(Reg, Name) ->
    Pid = spawn(fun() -> init(Name) end),
    case register(Reg, Pid) of
        true -> {ok, Pid};
        _ -> {error, registration_failed}
    end.
    
stop(Node) ->
    Node ! stop,
    unregister(Node).


init(Name) ->
    Intf = interface:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).


    router(Name, Counter, Hist, Intf, Table, Map) ->
        receive
            % A message has reached its final destination!
            {route, Name, From, Message} ->
                %display message and the sending router
                io:format("~p: received message ~p from ~p~n", [Name, Message, From]),
                router(Name, Counter, Hist, Intf, Table, Map);

            % if the name is not matched, then forward to another hop
            {route, To, From, Message} ->
                % calls dijkstra route function to look for the 'To' router 
                case dijkstra:route(To, Table) of
                    % the gateway found for the route
                    {ok, Gateway} ->
                        % checks the interfaces for the gateway 
                        case interface:lookup(Gateway, Intf) of
                            % if found, sends message to router
                            {ok, Pid} ->
                                Pid ! {route, To, From, Message};
                            % if no gateway, aka no route, is found to the destination router - message is dropped
                            notfound ->
                                io:format("~p: interface for gateway ~p not found. Dropping message.~n", [Name, Gateway])
                        end;
                    % if destination router was not found in table at all
                    notfound ->
                        io:format("~p: no routing entry for ~p. Dropping message.~n", [Name, To])
                end,
                router(Name, Counter, Hist, Intf, Table, Map);

             % Allow users to send a message.
            {send, To, Message} ->
                self() ! {route, To, Name, Message},
                router(Name, Counter, Hist, Intf, Table, Map);

            % Add a new interface/connection to another router or node.
            {add, Node, Pid} ->
                Ref = erlang:monitor(process, Pid),
                Intf1 = interface:add(Node, Ref, Pid, Intf),
                % Update the routing table and map here in order to have immediate updates, not needing to broadcast for changes to be made
                % UpdatedTable = dijkstra:table(interface:list(Intf1), Map),
                router(Name, Counter, Hist, Intf1, Table, Map);
    
            % Remove an existing connection/interface.
            {remove, Node} ->
                {ok, Ref} = interface:ref(Node, Intf),
                erlang:demonitor(Ref),
                Intf1 = interface:remove(Node, Intf),
                % UpdatedTable = dijkstra:table(interface:list(Intf1), Map),
                router(Name, Counter, Hist, Intf1, Table , Map);
    
            % React to the crash or termination of a connected node's process.
            {'DOWN', Ref, process, _, _} ->
                {ok, Down} = interface:name(Ref, Intf),
                io:format("~w: exit received from ~w~n", [Name, Down]),
                Intf1 = interface:remove(Down, Intf),
                % UpdatedTable = dijkstra:table(interface:list(Intf1), Map),
                router(Name, Counter, Hist, Intf1, Table, Map);
    
            %% Handling link-state updates
            {links, Node, MessageNr, Links} ->
                case hist:update(Node, MessageNr, Hist) of
                    {new, UpdatedHist} ->
                        io:format("~p: Received new link-state from ~p. Links: ~p~n", [Name, Node, Links]),
                        UpdatedMap = map:update(Node, Links, Map),
                        UpdatedTable = dijkstra:table(interface:list(Intf), UpdatedMap),
                        % io:format("~p: Updated map: ~p~n", [Name, UpdatedMap]),
                        interface:broadcast({links, Node, MessageNr, Links}, Intf),
                        % Update the routing table here as well
                        router(Name, Counter, UpdatedHist, Intf, UpdatedTable, UpdatedMap);
                    old ->
                        io:format("~p: Received old link-state from ~p. Ignoring...~n", [Name, Node]),
                        router(Name, Counter, Hist, Intf, Table, Map)
                end;
    
            % Respond to a request for the current state of the router.
            {status, From} ->
                From ! {status, {Name, Counter, Hist, Intf, Table, Map}},
                router(Name, Counter, Hist, Intf, Table, Map);

            %updates the routing table based on calls for updates from routers
            update ->
                UpdatedTable = dijkstra:table(interface:list(Intf), Map),
                router(Name, Counter, Hist, Intf, UpdatedTable, Map);
            
            % Broadcasts link-state messages to neighbors
            broadcast ->
                Message = {links, Name, Counter, interface:list(Intf)},
                interface:broadcast(Message, Intf),
                router(Name, Counter + 1, Hist, Intf, Table, Map);
    
        stop -> ok
    end.
    
    
    
    %% Sends a status request to the router and prints the response.
    status(RouterPid) ->
        %% Send the status request to the router.
        RouterPid ! {status, self()},

        %% Wait for the response.
        receive
            {status, {Name, Counter, Hist, Intf, Table, Map}} ->
                %% Pretty-print the state information.
                io:format("Router Status:~n"),
                io:format("  Name: ~p~n", [Name]),
                io:format("  Counter: ~p~n", [Counter]),
                io:format("  History: ~p~n", [Hist]),
                io:format("  Interfaces: ~p~n", [Intf]),
                io:format("  Routing Table: ~p~n", [Table]),
                io:format("  Network Map: ~p~n", [Map])
        after
            5000 -> % Timeout after 5000 milliseconds
                io:format("Failed to receive status response from router.~n")
        end.
    
