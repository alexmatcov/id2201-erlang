-module(dijkstra).
-export([table/2, route/2]).

entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {_, Length, _} -> 
            Length;
        false ->
            0                               %because in update/4 the guard contidion would not satisfy if no entry found and return the Sorted list.
    end.

replace(Node, N, Gateway, Sorted) ->
    List = lists:keyreplace(Node, 1, Sorted, {Node, N, Gateway}),
    lists:keysort(2, List).

update(Node, N, Gateway, Sorted) ->
    Length = entry(Node, Sorted),
    if
        N < Length ->
            replace(Node, N, Gateway, Sorted);
        true -> Sorted
    end.

iterate(Sorted, Map, Table) ->
    case Sorted of
        [{_, inf, _} | _] ->
            Table;
        [] ->
            Table;
        [Entry | Tail] ->
            {Node, N, Gateway} = Entry,
            ReachableNodes = map:reachable(Node, Map),
            NewList = lists:foldl(fun(Element, AccSorted) -> update(Element, N+1, Gateway, AccSorted) end, Tail, ReachableNodes),
            iterate(NewList, Map, [{Node, Gateway} | Table])
    end.

table(Gateways, Map) ->
    AllNodes = map:all_nodes(Map),
    List = lists:map(fun(Node) ->
                                case lists:member(Node, Gateways) of
                                    true ->
                                        {Node, 0, Node};
                                    false ->
                                        {Node, inf, unknown}
                                end
                    end, AllNodes),
    RoutingTable = lists:keysort(2, List),
    iterate(RoutingTable, Map, []).

route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {_, Gateway} ->
            {ok, Gateway};
        false ->
            notfound
    end.


