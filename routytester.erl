- module(routytester).
- export([run/0]).

run() ->
    % start routers 
    % reason for {} is to set R1, R2 etc as variables for the pid of each router
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, gothenburg),
    routy:start(r4, uppsala),
    routy:start(r5, malmo),

    % Step 2: Connect routers
    r1 ! {add, lund, {r2, 'sweden@130.229.164.39'}},
    r2 ! {add, gothenburg, {r3, 'sweden@130.229.164.39'}},
    r3 ! {add, uppsala, {r4, 'sweden@130.229.164.39'}},
    r4 ! {add, malmo, {r5, 'sweden@130.229.164.39'}},
    r5 ! {add, stockholm, {r1, 'sweden@130.229.164.39'}},

    % Step 3: Broadcast link-state messages
    r1 ! broadcast,
    timer:sleep(100),
    r2 ! broadcast,
    timer:sleep(100),
    r3 ! broadcast, 
    timer:sleep(100),
    r4 ! broadcast,
    timer:sleep(100),
    r5 ! broadcast,
    timer:sleep(100),

    % Step 4: Update routing tables
    r1 ! update,
    timer:sleep(100),
    r2 ! update,
    timer:sleep(100),
    r3 ! update,
    timer:sleep(100),
    r4 ! update,
    timer:sleep(100),
    r5 ! update,
    timer:sleep(100),

    % Step 5: Test routing
    io:format("Testing routing from stockholm to malmo~n"),
    r1 ! {send, malmo, "Hello from stockholm!"},
    timer:sleep(200),

    io:format("Testing routing from uppsala to gothenburg~n"),
    r4 ! {send, gothenburg, "Hello from uppsala!"},
    timer:sleep(200),

    io:format("Testing routing from lund to stockholm~n"),
    r2 ! {send, stockholm, "Hello from lund!"}.
