-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% function new returns an empty list
new() ->
    [].

update(Node, Links, Map) ->
    % removes any existing entries from this Node if any have been made
    % when update is made, the prior update will always be deleted, therefor pos is 1. 
    MapWithoutNode = list:keydelete(Node, 1, Map),
    % adds the new entry for links to this Node
    [{Node, Links} | MapWithoutNode].

% returns a list of the nodes which can be reached directly from current node
reachable(Node, Map) ->
    % case evaluates whether keyfind returns a tuple (with the directly reachable nodes) or false, meaning no reachable nodes. False returns empty list
    case lists:keyfind(Node, 1, Map) of
        false -> [];
    % if result is a tuple of links, it returns the links in the tuple
    {Node, Links} -> Links
end. 

all_nodes(Map) ->
    % Adds all nodes with links to the list NodesWithLinks by going through the map and adding the first element of each tuple to the list.
    NodesWithLinks = [Node || {Node, _} <- Map],
    
    % adds all the reachable (nodes that are reached by the nodes in the 'NodesWithLinks' list) to a list 'ReachableNodes'
    % For each node in NodesWithLinks it calls reachable function to get their connected nodes and adds all these to the new list
    ReachableNodes = lists:flatten([reachable(Node, Map) || Node <- NodesWithLinks]),
    
    % Combine the two created lists and remove duplicates making a list of ALL nodes 
    lists:usort(NodesWithLinks ++ ReachableNodes).



