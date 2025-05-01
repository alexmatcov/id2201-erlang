-module(hist).
-export([new/1, update/3]).

new(Name) ->
    History = dict:new(),
    dict:append(Name, inf, History).

update(Node, N, History) ->
    case dict:find(Node, History) of
        {ok, [Value | _]} ->
            if 
                N > Value ->
                    D1 = dict:erase(Node, History),
                    D2 = dict:append(Node, N, D1),
                    {new, D2};
                true ->
                    old
            end;
        error ->
            {new, dict:append(Node, N, History)}
    end.
