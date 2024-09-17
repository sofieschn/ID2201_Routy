-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]).

%% Finds the length of the shortest path to a given node from a sorted list of nodes. 
%% If the node is not found in the list, it returns 0.
entry(Node, SortedList) ->
    case lists:keyfind(Node, 1, SortedList) of
        {_Node, Length, _Gateway} ->
            Length;
        false ->
            0
    end. 

%% Replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. The resulting list should of course be sorted.
replace(Node, Length, Gateway, Sorted)->
	%% Keysort sorts by the second element in the tuple, which is the number of hops
	lists:keysort(2, lists:keyreplace(Node, 1, Sorted, {Node, Length, Gateway})). 

%% Update list if the new path is shorter.
update(Node, NewLength, Gateway, Sorted)->
	case entry(Node, Sorted) of

		%% If the new length is shorter than the old length, replace the element in the list.
		OldLength when NewLength<OldLength ->
			replace(Node, NewLength, Gateway, Sorted);
		%% Otherwise, just leave the list as it is.
		_->
			Sorted
	end.
	
% all reachable nodes in sorted list have been processed and added to routing table, no more in sorted list so table is returned
iterate([], _Map, Table)->
	Table;

% Infinite path case: If the first node has an infinite path, return the table
iterate([{_Node, inf, _Gateway}|_Rest], _Map, Table)->
	Table;

% Main case: Process the reachable nodes and add to the routing table
iterate([{Node, Length, Gateway} | T], Map, Table) ->
    % Get nodes directly reachable from the current node in the map.
    Reachables = map:reachable(Node, Map),
    
    % Update the sorted list with shorter paths found through the current node.
    UpdatedSorted = lists:foldl(fun(N, Sorted) -> update(N, Length + 1, Gateway, Sorted) end, T, Reachables),
    
    % Recursively call iterate with the updated sorted list, adding the current node to the routing table.
    iterate(UpdatedSorted, Map, [{Node, Gateway} | Table]).
	
%% Constructs a routing table for the network using Dijkstra's algorithm.
table(Gateways, Map) ->
    % Get all nodes in the map.
    AllNodes = map:all_nodes(Map),
    
    % Initialize a list with all nodes set to an infinite path length.
    IntialList = lists:map(fun(Node) -> {Node, inf, unknown} end, AllNodes),

    % Set the path length to 0 for the gateway nodes and sort the list.
    SortedList = lists:foldl(fun(Node, List) -> update(Node, 0, Node, List) end, IntialList, Gateways),
    
    % Call iterate to generate the complete routing table from the sorted list.
    iterate(SortedList, Map, []).

% route is used to find the gateway for a specific node in a routing table
route(Node, Table)->
	case lists:keyfind(Node, 1, Table) of
		{_, Gateway}->
			{ok, Gateway};
		false->
			notfound
	end.