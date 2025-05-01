-module(vect_time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
    [].

inc(Name, Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, LC} ->
            lists:keyreplace(Name, 1, Time, {Name, LC+1});
        false ->
            [{Name, 1}|Time]
    end.

merge([], Time) ->
    Time;

merge([{Name, Ti}|Rest], Time) ->
    case lists:keyfind(Name, 1, Time) of
        {Name, Tj} ->
            [{Name, lists:max([Ti, Tj])}|merge(Rest, lists:keydelete(Name, 1, Time))];
        false ->
            [{Name, Ti}|merge(Rest, Time)]
    end.

leq([], _) ->
    true;

leq([{Name, Ti}|Rest], Time) ->
    case lists:keyfind(Name, 1,  Time) of
        {Name, Tj} ->
            if 
                Ti =< Tj ->
                    leq(Rest, Time);
                true ->
                    false
            end;
        false ->
            false
    end.

% change clock
clock(_) ->
    [].

update(From, Time, Clock) ->
    {From, ValueIn} = lists:keyfind(From, 1, Time),
    case lists:keyfind(From, 1, Clock) of
        {From, Value} ->
            lists:keyreplace(From, 1, Clock, {From, lists:max([Value, ValueIn])});
        false ->
            [{From, ValueIn}|Clock]
    end.


safe(Time, Clock) ->
    leq(Time, Clock).

