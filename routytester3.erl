-module(routytester3).
-export([run/0]).

run() ->
    % Step 1: Start routers on different nodes
    {ok, R1} = routy:start(r1, stockholm),
    {ok, R2} = routy:start(r2, lund),
    {ok, R3} = routy:start(r3, gothenburg),
    {ok, R4} = routy:start(r4, uppsala),
    {ok, R5} = routy:start(r5, malmo),

    % Step 2: Connect routers using the node names and IPs
    R1 ! {add, lund, {r2, 'sweden@130.229.164.39'}},
    R2 ! {add, gothenburg, {r3, 'sweden@130.229.164.39'}},
    R3 ! {add, uppsala, {r4, 'sweden@130.229.164.39'}},
    R4 ! {add, malmo, {r5, 'sweden@130.229.164.39'}},
    R5 ! {add, stockholm, {r1, 'sweden@130.229.164.39'}},

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
    R2 ! {send, stockholm, "Hello from lund!"},


    % Step 6: Kill a node and test routing again
    io:format("Killing node: gothenburg~n"),
    exit(whereis(r3), kill),
    timer:sleep(1000),
    
    % Step 7: Broadcast and update routing tables again
    R1 ! broadcast,
    timer:sleep(100),
    R2 ! broadcast,
    timer:sleep(100),
    R4 ! broadcast,
    timer:sleep(100),
    R5 ! broadcast,
    timer:sleep(100),

    R1 ! update,
    timer:sleep(100),
    R2 ! update,
    timer:sleep(100),
    R4 ! update,
    timer:sleep(100),
    R5 ! update,
    timer:sleep(100),

    % Step 8: Test routing after killing a node
    io:format("Testing routing from stockholm to malmo after killing gothenburg~n"),
    R1 ! {send, malmo, "Hello again from stockholm after killing gothenburg!"},
    timer:sleep(1000),

    io:format("Testing routing from uppsala to lund after killing gothenburg~n"),
    R4 ! {send, lund, "Hello again from uppsala after killing gothenburg!"}.
