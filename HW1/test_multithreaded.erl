-module(test_multithreaded).
-export([start/2]).

bench(Host, Port) ->
    Clients = 0,                                % number of clients is +1 to the one defined as the code runs one extra time.
    spawn_many(Clients, Host, Port),
    Start = erlang:system_time(micro_seconds),
    run(100, Host, Port),
    Finish = erlang:system_time(micro_seconds),
    Time = Finish - Start,
    io:format("Finished in: ~w~n ms", [Time]).

run(N, Host, Port) ->
    if N == 0 ->
        ok;
    true ->
        request(Host, Port),
        run(N-1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of {ok, _} ->
        ok;
    {error, Error} ->
        io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).

spawn_many(Clients, Host, Port) ->
    if Clients == 0 ->
        ok;
    true ->
        spawn(test, bench, [Host, Port]),
        spawn_many(Clients-1, Host, Port)
    end.    

start(Host, Port) ->
    register(test, spawn(fun() -> bench(Host, Port) end)).
