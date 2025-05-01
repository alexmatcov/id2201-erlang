-module(rudy_multithreaded).
-export([init/1, handler/2, start/1, stop/0]).

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of                  
        {ok, Listen} ->
            Acceptors = 10,
            spawn_many(Acceptors, Listen, self()), 
            wait_for_threads(Acceptors);    
        {error, Error} ->
            Error
    end.

    wait_for_threads(0) ->
        ok;
    
    wait_for_threads(Acceptors) ->
        receive
            handler_done ->                             %Waits in this stage until handler() closes Client.
                wait_for_threads(Acceptors-1)
        end.


handler(Listen, Pid) ->
    case gen_tcp:accept(Listen) of                
        {ok, Client} ->
            request(Client),
            gen_tcp:close(Client),
            handler(Listen, Pid);
        {error, Error} ->
            Error
    end,
    Pid ! handler_done.

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

spawn_many(0, _,  _) ->
    ok;

spawn_many(Acceptors, Listen, Pid) ->
    spawn(rudy_multithreaded, handler, [Listen, Pid]),
    spawn_many(Acceptors-1, Listen, Pid).

start(Port) ->
    register(rudy_multithreaded, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy_multithreaded), "time to die").