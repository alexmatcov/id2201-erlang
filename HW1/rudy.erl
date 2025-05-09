-module(rudy).
-export([init/1]).
-export([start/1, stop/0]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of                   %Open a listening socket.
        {ok, Listen} ->
            handler(Listen);
        {error, Error} ->
            Error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of                       %Listen to the socket.
        {ok, Client} ->
            request(Client),
            handler(Listen);
        {error, Error} ->
            Error
    end.

request (Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
        {ok, Str} ->
            Request = http:parse_request(Str),
            Response = reply(Request),
            gen_tcp:send(Client, Response);
        {error, Error} ->
            io:format("rudy: error: ~w~n", [Error])
        end,
        gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").