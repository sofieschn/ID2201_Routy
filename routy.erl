-module(routy).
-export([start/2, stop/1]).


start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).


stop(Node) ->
    Node ! stop,
    unregister(Node).


init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf, Map),
    Hist = hist:new(Name),
