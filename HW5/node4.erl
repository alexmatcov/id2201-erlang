-module(node4).
-export([start/1, start/2]).
-define(Stabilize, 1000).
-define(Timeout, 10000).

node(Id, Predecessor, Successor, Nxt, Store, Replica) ->
    receive
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Nxt, Store, Replica);
        {notify, New} ->
            {Pred, Added} = notify(New, Id, Predecessor, Successor, Store),
            node(Id, Pred, Successor, Nxt, Added, Replica);
        {request, Peer} ->
            request(Peer, Predecessor, Successor),
            node(Id, Predecessor, Successor, Nxt, Store, Replica);
        {status, Pred, Nx} ->
            {Succ, Next} = stabilize(Pred, Nx, Id, Successor),
            node(Id, Predecessor, Succ, Next, Store, Replica);
        stabilize ->
            stabilize(Successor),
            node(Id, Predecessor, Successor, Nxt, Store, Replica);
        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Nxt, Added, Replica);
        {replicate, Key, Value} ->
            AddedReplica = storage:add(Key, Value, Replica),
            node(Id, Predecessor, Successor, Nxt, Store, AddedReplica);
        {replace, NewReplica} ->
            node(Id, Predecessor, Successor, Nxt, Store, NewReplica);
        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Nxt, Store, Replica);
        {handover, Elements} ->
            Merged = storage:merge(Store, Elements),
            node(Id, Predecessor, Successor, Nxt, Merged, Replica);
        {'DOWN', Ref, process, _, _} ->
            {Pred, Succ, Next} = down(Ref, Predecessor, Successor, Nxt),
            node(Id, Pred, Succ, Next, Store, Replica);
        stop ->
            io:format("node ~w (~w) stopped~n", [Id, self()]);
        probe ->
            create_probe(Id, Successor),
            node(Id, Predecessor, Successor, Nxt, Store, Replica);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Nxt, Store, Replica);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor),
            node(Id, Predecessor, Successor, Nxt, Store, Replica)
    end.

stabilize(Pred, Nx, Id, Successor) ->
    {Skey, Sref, Spid} = Successor,
    case Pred of
        nil ->
            Spid ! {notify, {Id, self()}},
            {Successor, Nx};
        {Id, _} ->
            {Successor, Nx};
        {Skey, _} ->
            Spid ! {notify, {Id, self()}},
            {Successor, Nx};
        {Xkey, Xpid} ->
            case key:between(Xkey, Id, Skey) of
                true ->
                    Xpid ! {request, self()},
                    drop(Sref),
                    Xref = monitor(Xpid),
                    {{Xkey, Xref, Xpid}, Successor};
                false ->
                    Spid ! {notify, {Id, self()}},
                    {Successor, Nx}
            end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor, Successor) ->
    {Skey, _, Spid} = Successor,
    case Predecessor of
        nil ->
            Peer ! {status, nil, {Skey, Spid}};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}, {Skey, Spid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Successor, Store) ->
    case Predecessor of
        nil ->
            Keep = handover(Id, Store, Nkey, Npid),
            Nref = monitor(Npid),
            {{Nkey, Nref, Npid}, Keep};
        % case to detect pointing to itself? no because Predecessor = {Pkey, Ppid} = {Id, self()}, forming a full loop which includes Nkey.
        {Pkey, Pref, _} ->
            case key:between(Nkey, Pkey, Id) of
                true ->
                    Keep = handover(Id, Store, Nkey, Npid),
                    drop(Pref),
                    Nref = monitor(Npid),
                    {_, _, Spid} = Successor,
                    Spid ! {replace, Keep},
                    {{Nkey, Nref, Npid}, Keep};
                false ->
                    {Predecessor, Store}
            end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok},
            storage:add(Key, Value, Store); %returns Store with added key value pair
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store %needs to return the unmodified Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.

monitor(Pid) ->
    erlang:monitor(process, Pid).

drop(nil) ->
    ok;
drop(Ref) ->
    erlang:demonitor(Ref, [flush]).

down(Ref, {_, Ref, _}, Successor, Next) ->
    {nil, Successor, Next};
down(Ref, Predecessor, {_, Ref, _}, {Nkey, Npid}) ->
    Npid ! {request, self()},
    Nref = monitor(Npid),
    {Predecessor, {Nkey, Nref, Npid}, nil}.

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    Nxt = nil,
    {ok, Successor} = connect(Id, Peer),
    Store = storage:create(),
    Replica = storage:create(),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Nxt, Store, Replica).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive{Qref, Skey} ->
        Ref = monitor(Peer),
        {ok, {Skey, Ref, Peer}}
after ?Timeout ->
    io:format("Time out: no response~n",[])
end.

create_probe(Id, Successor) ->
    Time = erlang:system_time(micro_seconds),
    {_, Spid} = Successor,
    Spid ! {probe, Id, [{Id, self()}], Time}.

remove_probe(T, Nodes) ->
    Time = erlang:system_time(micro_seconds) - T,
    io:format("Around the ring time: ~w ms,~nWith list of nodes: ~w~n", [Time, Nodes]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
    {_, Spid} = Successor,
    Spid ! {probe, Ref, [{Id, self()}|Nodes], T}.