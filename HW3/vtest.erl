-module(vtest).

-export([run/2]).

run(Sleep, Jitter) ->
    AllNodes = [john, paul, ringo, george],
    Log = vect_logger3:start(AllNodes),
    A = vect_worker:start(john, Log, 13, Sleep, Jitter, AllNodes),
    B = vect_worker:start(paul, Log, 23, Sleep, Jitter, AllNodes),
    C = vect_worker:start(ringo, Log, 36, Sleep, Jitter, AllNodes),
    D = vect_worker:start(george, Log, 49, Sleep, Jitter, AllNodes),
    vect_worker:peers(A, [B, C, D]),
    vect_worker:peers(B, [A, C, D]),
    vect_worker:peers(C, [A, B, D]),
    vect_worker:peers(D, [A, B, C]),
    timer:sleep(5000),
    vect_logger3:stop(Log),
    vect_worker:stop(A),
    vect_worker:stop(B),
    vect_worker:stop(C),
    vect_worker:stop(D).