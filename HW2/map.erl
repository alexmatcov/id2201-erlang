-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() ->
    [].

update(Node, Links, Map) ->
    NewMap = lists:keydelete(Node, 1, Map), %pos_integer 1 because deletes the 2nd element in the tuple
    [{Node, Links} | NewMap].

reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {_, Links} -> 
            Links;
        false ->
            []
    end.

all_nodes(Map) -> 
    AllNodes = lists:flatmap(fun({Node, Links}) -> [Node | Links] end, Map),
    lists:reverse(AllNodes).

