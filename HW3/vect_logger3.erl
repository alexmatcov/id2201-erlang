-module(vect_logger3).

-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clock = vect_time:clock(Nodes),
    HoldbackQueue = [],
    loop(Clock, HoldbackQueue).

loop(Clock, HoldbackQueue) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClock = vect_time:update(From, Time, Clock),
            UpdatedHoldbackQueue = lists:keysort(2, [{From, Time, Msg} | HoldbackQueue]),
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
            case vect_time:safe(Time, Clock) of
                true ->
                    log(From, Time, Msg),
                    safe(Clock, Rest);
                false ->
                    HoldbackQueue
            end
    end.    

log(From, Time, Msg) ->
    io:format("log: ~w ~w ~p~n", [Time, From, Msg]).