-module(interface).
- export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% creates new empty intf list
new() ->
    [].

%Adds a tuple to list of interfaces
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

%Removes an interface from the list given a name
remove(Name, Intf) ->
    % checks for the name in the intf list and removes if found
    lists:keydelete(Name,1,Intf).

% looks up interface based on the name, returns process identifyer
lookup(Name, Intf)->
	case lists:keyfind(Name, 1, Intf) of
		{Name, _, Pid}->
			{ok, Pid};
		false->
			notfound
	end.

% looks up interface based on the name, returns Ref
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {Name, Ref, _} -> {ok, Ref};
        false -> notfound
    end.    

% looks up interface based on the ref, returns name
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        {Name, Ref, _} -> {ok, Name};
        false -> notfound
    end.

% Creates a list including only the names of all interfaces in the inputed list
list(Intf) -> 
    [Name || {Name, _, _} <- Intf].

% Takes in a message, and a list of interfaces, and sends the message to the process identifyer of each interface in the list
broadcast(Message, Intf) ->
    [Pid ! Message || {_, _, Pid} <- Intf],
    ok. 