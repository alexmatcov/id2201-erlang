-module(gms4).
-compile(export_all).

-define(arghh, 100).
-define(timeout, 1000).

leader(Id, Master, N, Slaves, Group, HoldbackQueue) ->
    receive
        {mcast, Msg} ->
            UpdatedHoldbackQueue = lists:keysort(2, [{N, Msg} | HoldbackQueue]),
            UpdatedClock = N+1,
            UnsafeHoldbackQueue = safe(N, Master, Id, Slaves, UpdatedClock, UpdatedHoldbackQueue),
            leader(Id, Master, UpdatedClock, Slaves, Group, UnsafeHoldbackQueue);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2, HoldbackQueue);
        stop ->
            io:format("leader ~w (~w) stoped~n", [Id, self()]) 
    end.

bcast(Id, Msg, Slaves) ->
    lists:foreach(fun(Slave) -> Slave ! Msg, crash(Id) end, Slaves).

crash(Id) ->
    case rand:uniform(?arghh) of
        ?arghh ->
            io:format("leader ~w (~w): crash~n", [Id, self()]),
            exit(no_luck);
        _ ->
            ok
    end.

safe(N, Master, Id, Slaves, Clock, HoldbackQueue) ->
    if
        HoldbackQueue == [] ->
            [];
        true ->
            [{N, Msg} | Rest] = HoldbackQueue,
            case time_safe(N, Clock) of
                true ->
                    bcast(Id, {msg, N, Msg}, Slaves),
                    Master ! Msg,
                    safe(N, Master, Id, Slaves, Clock, Rest);
                false ->
                    HoldbackQueue %at the first false when Ti > Tj, we know the rest is false as list sorted, we return the list with messages that are not safe to print
            end
    end.    

time_safe(N, Clock) ->
    UpdatedClock = lists:keysort(2, Clock),
    [{_, Counter} | _] = UpdatedClock,
    leq(N, Counter).

leq(Ti, Tj) ->
    if
        Ti =< Tj ->
            true;
        true ->
            false
    end.

slave(Id, Master, Leader, N, Last, Slaves, Group, HoldbackQueue) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group, HoldbackQueue);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group, HoldbackQueue);
        {msg, I, _} when I < N ->                                                    %discard msgs that we have seen (seq nr < N)
            slave(Id, Master, Leader, N, Last, Slaves, Group, HoldbackQueue);
        {msg, N, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group, HoldbackQueue);
        {view, I, _, _} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group, HoldbackQueue);
        {view, N, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2, HoldbackQueue);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group, HoldbackQueue);
        stop ->
            io:format("slave ~w (~w): stoped~n", [Id, self()])
    end.    

election(Id, Master, N, Last, Slaves, [_|Group], HoldbackQueue) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("new leader: ~w (~w)~n", [Id, self()]),
            bcast(Id, Last, Rest),
            bcast(Id, {view, N, Slaves, Group}, Rest),                  
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group, HoldbackQueue);
        [Leader|Rest] ->
            io:format("slave: ~w (~w)~n", [Id, self()]),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group, HoldbackQueue)
    end.

start(Id) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    rand:seed(exsss, {Rnd, Rnd, Rnd}),
    N = 1,                                                         %set expected seq nr of the next msg 
    HoldbackQueue = [],
    io:format("leader: ~w (~w)~n", [Id, self()]),
    leader(Id, Master, N, [], [Master], HoldbackQueue).

start(Id, Grp) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Grp, Rnd, Self) end)}.

init(Id, Grp, Rnd, Master) ->
    rand:seed(exsss, {Rnd, Rnd, Rnd}),
    Self = self(),
    Grp ! {join, Master, Self},
    receive
        {view, N, [Leader|Slaves], Group} ->
            Master ! {view, Group},
            erlang:monitor(process, Leader),
            io:format("slave: ~w (~w)~n", [Id, self()]),
            HoldbackQueue = [],
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group, HoldbackQueue)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.