-module(dijkstra).
-export([entry/2, replace/4, update/4]).

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
insert({Node, X, Gx}, []) ->
    % if the list is empty, ut just adds the new tuple and returns the list
    [{Node, X, Gx}];
insert({New, X, Gx}, [{Node, Y, Gy} | Rest]) when X < Y ->
    % the path length of the new entry calling the function and compares it to th currently shortest path length in the list. When the new one is shorter than the one it is compared to, it is added in the list
    [{New, X, Gx}, {Node, Y, Gy} | Rest];
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



