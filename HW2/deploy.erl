-module(deploy).
-export([deploy/0]).
deploy() ->
    io:format("Start deploying...~n"),

    routy:start(r1, stockholm),
    routy:start(r2, lund),
    routy:start(r3, malmo),
    routy:start(r4, gothenburg),
    routy:start(r5, helsingborg),
    io:format("Routers started~n"),

    r1 ! {add, lund, r2},
    r1 ! {add, gothenburg, r4},
    r2 ! {add, malmo, r3},
    r3 ! {add, gothenburg, r4},
    r4 ! {add, helsingborg, r5},
    r5 ! {add, stockholm, r1},
    r5 ! {add, malmo, r3},
    timer:sleep(1000), % wait messages to be processed
    io:format("Links added~n"),

    r1 ! broadcast,
    r2 ! broadcast,
    r3 ! broadcast,
    r4 ! broadcast,
    r5 ! broadcast,
    timer:sleep(1000), % wait messages to be processed
    io:format("Routers' neighbors broadcasted~n"),

    r1 ! update,
    r2 ! update,
    r3 ! update,
    r4 ! update,
    r5 ! update,
    timer:sleep(1000), % wait messages to be processed
    io:format("Routing tables updated~n"),

    io:format("Deploy finished!~n").