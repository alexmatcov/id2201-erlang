-module(gms3).
-compile(export_all).

-define(arghh, 100).
-define(timeout, 1000).

leader(Id, Master, N, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            bcast(Id, {msg, N, Msg}, Slaves),
            Master ! Msg,
            leader(Id, Master, N+1, Slaves, Group);
        {join, Wrk, Peer} ->
            Slaves2 = lists:append(Slaves, [Peer]),
            Group2 = lists:append(Group, [Wrk]),
            bcast(Id, {view, N, [self()|Slaves2], Group2}, Slaves2),
            Master ! {view, Group2},
            leader(Id, Master, N+1, Slaves2, Group2);
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

slave(Id, Master, Leader, N, Last, Slaves, Group) ->
    receive
        {mcast, Msg} ->
            Leader ! {mcast, Msg},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {join, Wrk, Peer} ->
            Leader ! {join, Wrk, Peer},
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, I, _} when I < N ->                                       %discard msgs that we have seen (seq nr < N)
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {msg, N, Msg} ->
            Master ! Msg,
            slave(Id, Master, Leader, N+1, {msg, N, Msg}, Slaves, Group);
        {view, I, _, _} when I < N ->
            slave(Id, Master, Leader, N, Last, Slaves, Group);
        {view, N, [Leader|Slaves2], Group2} ->
            Master ! {view, Group2},
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves2], Group2}, Slaves2, Group2);
        {'DOWN', _Ref, process, Leader, _Reason} ->
            election(Id, Master, N, Last, Slaves, Group);
        stop ->
            io:format("slave ~w (~w): stoped~n", [Id, self()])
    end.    

election(Id, Master, N, Last, Slaves, [_|Group]) ->
    Self = self(),
    case Slaves of
        [Self|Rest] ->
            io:format("new leader: ~w (~w)~n", [Id, self()]),
            bcast(Id, Last, Rest),                                      %sending the last msg to all the slaves to ensure that all slaves recevived it after last leader crash
            bcast(Id, {view, N, Slaves, Group}, Rest),                  
            Master ! {view, Group},
            leader(Id, Master, N+1, Rest, Group);
        [Leader|Rest] ->
            io:format("slave: ~w (~w)~n", [Id, self()]),
            erlang:monitor(process, Leader),
            slave(Id, Master, Leader, N, Last, Rest, Group)
    end.

start(Id) ->
    Rnd = rand:uniform(1000),
    Self = self(),
    {ok, spawn_link(fun()-> init(Id, Rnd, Self) end)}.

init(Id, Rnd, Master) ->
    rand:seed(exsss, {Rnd, Rnd, Rnd}),
    N = 1,                                                         %set expected seq nr of the next msg 
    io:format("leader: ~w (~w)~n", [Id, self()]),
    leader(Id, Master, N, [], [Master]).                            %master is the application layer process and belongs in group layer

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
            slave(Id, Master, Leader, N+1, {view, N, [Leader|Slaves], Group}, Slaves, Group)
    after ?timeout ->
        Master ! {error, "no reply from leader"}
    end.