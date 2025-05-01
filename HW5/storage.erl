-module(storage).
-compile(export_all).

create() ->
    [].

add(Key, Value, Store) ->
    lists:keymerge(1, [{Key, Value}], Store).

lookup(Key, Store) ->
    lists:keyfind(Key, 1, Store).

split(From, To, Store) ->
    lists:partition(fun({Key, _}) -> key:between(Key, From, To) end, Store).

merge(Entries, Store) ->
    lists:keymerge(1, Entries, Store).