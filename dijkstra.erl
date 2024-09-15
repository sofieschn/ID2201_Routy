-module(dijkstra).
-export([table/2, route/2]).

% function helps us iterate through all nodes in the map/list to find the length of the path to a specific node calling the function
entry(Node, []) ->
    % empty list or not found node returns 0
    0; 
entry(Node, [{Node, Length, _Gateway} | _Rest]) ->
    % If the current node in the tuple matches the Node we are looking for, it returns the Length (the path length to that node).
    Length;
entry(Node, [_ | Rest]) ->
    % If the current element in the list doesn’t match the Node, the function recursively calls itself with the Rest of the list.
    entry(Node, Rest).



% replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. 
replace(Node, N, Gateway, Nodes) ->
    % takes the new tuple for Node and inserts to the list Nodes, and deletes the previous tuple connected to this Node from the list, and calls the insert function to sort the list correctly
    insert({Node, N, Gateway}, lists:keydelete(Node, 1, Nodes)).



% helper function for sorting a list of Nodes
insert({Node, NewLength, NewGateway}, []) ->
    % if the list is empty, ut just adds the new tuple and returns the list
    [{Node, NewLength, NewGateway}];

insert({New, NewLength, NewGateway}, [{Node, ExistingLength, ExistingGateway} | Rest]) when NewLength < ExistingLength ->
    % the path length of the new entry calling the function and compares it to th currently shortest path length in the list. When the new one is shorter than the one it is compared to, it is added in the list
    [{New, NewLength, NewGateway}, {Node, ExistingLength, ExistingGateway} | Rest];

insert(New, [Node | Rest]) ->
    % when the new node is not added to the list since it has a longer length than the node it is being checked against in the list, it will iterate through the nodes in the list
    [Node | insert(New, Rest)].



update(Node, N, Gateway, Sorted) ->
    % calls entry function to get the path length of the node which is being updated (maybe)
    CurrentLength = entry(Node, Sorted),
    % checks if the path length actually is shorter than the existing one in the sorted list
    case CurrentLength of 
        0 -> 
            % node not existing in the list, nothing should be updated and the sorted list is returned as is
            Sorted;
        _ when N < CurrentLength -> 
            % when the new pathway is shorter than the length of the pathway in the sorted list, the length is replaced with the new replace function
            replace(Node, N, Gateway, Sorted);
        _ -> 
            % if the path is not shorter than the previous one in the list, nothing is done and the list is returned as is
            Sorted
    end.



% iterates the sorted list of nodes and their connections to manage a routing table

% all reachable nodes in sorted list have been processed and added to routing table, no more in sorted list so table is returned
iterate([], Map, Table) ->
    Table;

% Infinite path case: If the first node has an infinite path, return the table
iterate([{_Node, inf, _Gateway} |_Rest], _Map, Table) ->
    Table;

% Main case: Process the reachable nodes and add to the routing table
iterate([{Node, Length, Gateway} | Rest], Map, Table) ->
    % call the reachable function in map to retrieve directly reachable nodes for this node
    ReachableNodes = map:reachable(Node, Map),

    % Update sorted list for each reachable node using the update function
    % We pass Rest (rest of the list after processing the node), not the full list, so the processed Node is not included
    UpdatedSorted = lists:foldl(fun(ReachableNode, SortedAcc) ->
        dijkstra:update(ReachableNode, Length + 1, Node, SortedAcc)
    end, Rest, ReachableNodes),

    % Add this node to the routing table
    NewTable = [{Node, Gateway} | Table],

    % recursively calls itself with the updated sorted list after each node is added
    iterate(UpdatedSorted, Map, NewTable).




% function takes in a map list and a list of the gateways, and creates a routing table
table(Gateways, Map) ->
    % alls all_nodes fun to add all nodes to list
    AllNodes = map:all_nodes(Map),
    % checks each node in the allNodes list, and sees if its a memeber of the gateways list 
    NodesWithPaths = [case lists:member(Node, Gateways) of  
                    % if case is true, a tuple is created for the node with a path lenth 0
                    true -> {Node, 0, Node};
                    % if case is false, tuple with path lenght inf is created for the node
                    false -> {Node, inf, unknown}
                end || Node <- AllNodes],

    % Sort the list based on the path length (second element of the tuple)
    SortedList = lists:sort(fun({_, Length1, _}, {_, Length2, _}) -> Length1 < Length2 end, NodesWithPaths),


    % the sorted list is send with the map and an empty table to iterate, to create the complete table
    iterate(SortedList, Map, []).
    
% route is used to find the gateway for a specific node in a routing table
route(Node, Table) ->
    % searched the specific node in the table
    case lists:keyfind(Node, 1, Table) of
        % if found it displays ok, gateway
        {Node, Gateway} -> {ok, Gateway};
        % if its not found 'notfound' is displayed 
        false -> notfound
    end.