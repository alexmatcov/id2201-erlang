-module(vect_worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    rand:seed(exsss, {Seed, Seed, Seed}),
    LC = vect_time:zero(),
        receive
            {peers, Peers} ->
                loop(Name, Log, LC, Peers, Sleep, Jitter);
            stop ->
                ok 
        end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

% The sleep value will determine how active the worker is sending messages,
% and the jitter value will introduce a rand delay between the sending of a
% message and the sending of a log entry.
loop(Name, Log, LC, Peers, Sleep, Jitter) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, Time, Msg} ->
            TempLC = vect_time:merge(LC, Time),
            IncLC = vect_time:inc(Name, TempLC),
            Log ! {log, Name, IncLC, {received, Msg}},
            loop(Name, Log, IncLC, Peers, Sleep, Jitter);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, time, {error, Error}}
    after Wait -> 
            Selected = select(Peers),
            Time = vect_time:inc(Name, LC),
            Message = {hello, rand:uniform(100)},
            Selected ! {msg, Time, Message},
            jitter(Jitter),
            Log ! {log, Name, Time, {sending, Message}},
            loop(Name, Log, Time, Peers, Sleep, Jitter)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

% Higher jitter makes log entries be printed out of order.
% Smaller jitter helps keeping the ored of log entries and jitter @ 0 eliminates nr of wrong entries.
jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).
