-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    0.

inc(_, T) ->
    T+1.

merge(Ti, Tj) ->
    lists:max([Ti, Tj]).

leq(Ti, Tj) ->
    if
        Ti =< Tj ->
            true;
        true ->
            false
    end.

clock(Nodes) ->
    case Nodes of
        [] ->
            [];
        [H | T] ->
            [{H, 0} | clock(T)]
    end.

update(Node, Time, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(Time, Clock) ->
    UpdatedClock = lists:keysort(2, Clock),
    [{_, Counter} | _] = UpdatedClock,
    leq(Time, Counter).