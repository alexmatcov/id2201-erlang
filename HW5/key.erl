-module(key).
-export([generate/0, between/3]).

generate() ->
    rand:uniform(1000000000).

between(Key, From, To) ->
    if 
        Key > From, Key =< To ->
            true;
        Key < From, Key =< To, From > To ->
            true;
        Key > From, Key >= To, From > To ->
            true;
        From == To ->
            true;
        true ->
            false
    end.

