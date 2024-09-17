-module(routytester2).
-export([run/0]).

run() ->
    % Step 1: Start routers
    {ok, R1} = routy:start(r1, stockholm),
    {ok, R2} = routy:start(r2, lund),
    {ok, R3} = routy:start(r3, gothenburg),
    {ok, R4} = routy:start(r4, uppsala),
    {ok, R5} = routy:start(r5, malmo),

    % Step 2: Connect routers
    R1 ! {add, lund, R2},
    R2 ! {add, gothenburg, R3},
    R3 ! {add, uppsala, R4},
    R4 ! {add, malmo, R5},
    R5 ! {add, stockholm, R1},

    % Step 3: Broadcast link-state messages
    R1 ! broadcast,
    timer:sleep(100),
    R2 ! broadcast,
    timer:sleep(100),
    R3 ! broadcast,
    timer:sleep(100),
    R4 ! broadcast,
    timer:sleep(100),
    R5 ! broadcast,
    timer:sleep(100),

    % Step 4: Update routing tables
    R1 ! update,
    timer:sleep(100),
    R2 ! update,
    timer:sleep(100),
    R3 ! update,
    timer:sleep(100),
    R4 ! update,
    timer:sleep(100),
    R5 ! update,
    timer:sleep(100),

    % Step 5: Test routing
    io:format("Testing routing from stockholm to malmo~n"),
    R1 ! {send, malmo, "Hello from stockholm!"},
    timer:sleep(1000),

    io:format("Testing routing from uppsala to gothenburg~n"),
    R4 ! {send, gothenburg, "Hello from uppsala!"},
    timer:sleep(1000),

    io:format("Testing routing from lund to stockholm~n"),
    R2 ! {send, stockholm, "Hello from lund!"}.
