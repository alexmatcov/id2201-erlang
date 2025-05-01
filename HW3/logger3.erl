-module(logger3).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = time:clock(Nodes),
    HoldbackQueue = [],
    loop(Clock, HoldbackQueue).

loop(Clock, HoldbackQueue) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedHoldbackQueue = lists:keysort(2, [{From, Time, Msg} | HoldbackQueue]),
            UpdatedClock = time:update(From, Time, Clock),
            UnsafeHoldbackQueue = safe(UpdatedClock, UpdatedHoldbackQueue),
            loop(UpdatedClock, UnsafeHoldbackQueue);
        stop ->
            io:format("Holdback Queue: ~p ~n", [HoldbackQueue])
    end.

safe(Clock, HoldbackQueue) ->
    if
        HoldbackQueue == [] ->
            [];
        true ->
            [{From, Time, Msg} | Rest] = HoldbackQueue,
            case time:safe(Time, Clock) of
                true ->
                    log(From, Time, Msg),
                    safe(Clock, Rest);
                false ->
                    HoldbackQueue %at the first false when Ti > Tj, we know the rest is false as list sorted, we return the list with messages that are not safe to print
            end
    end.    

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).