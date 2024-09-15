-module(history).
-export([new/1, update/3]).

% function makes the first message received from Name with a message number greater than 0 to be considered new.
new(Name) ->
    [{Name, 0}].

% Updates the 'max seen' message number for a node to track the latest messages received.
% This helps in determining whether an incoming message is new or part of a cyclic path.
% If the message number is greater than the previously seen number, it is considered new,
% and the history is updated. Otherwise, the message is marked as old.

update(Node, MessageNr, History) ->
    case lists:keyfind(Node, 1, History) of
        % when message nr is greater than prev max seen messagenr, update history
        {Node, MaxSeen} when MessageNr > MaxSeen ->
            UpdatedHistory = lists:keyreplace(Node, 1, History, {Node, MessageNr}),
            {new, UpdatedHistory};
        % when the node is found, but the messagenr is not bigger than the prev max seen, it is classed as old and no upd is made
        {Node, _} -> 
            old;
        % Node is not found in the history, so a tuple for this node is created with the messagenr and added to history
        false -> 
            {new, [{Node, MessageNr} | History]}
    end.



