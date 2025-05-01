-module(test).
-export([sweden/0, netherlands/0, moldova/0, add_links/0, deploy/0]).

-define(NODE_SE,'sweden@192.168.3.7').
-define(NODE_NL,'netherlands@192.168.3.7').
-define(NODE_MD,'moldova@192.168.3.7').


sweden() ->
    routy:start(r1, stockholm),
    routy:start(r2, lund),
    r2 ! {add, stockholm, {r1, ?NODE_SE}},
    r1 ! {add, lund, {r2, ?NODE_SE}}.

netherlands() ->
    routy:start(r3, amsterdam),
    routy:start(r4, enschede),
    r3 ! {add, enschede, {r4, ?NODE_NL}},
    r4 ! {add, amsterdam, {r3, ?NODE_NL}}.

moldova() ->
    routy:start(r5, chisinau).

add_links() ->
    {r2, ?NODE_SE} ! {add, amsterdam,{r3, ?NODE_NL}},
    {r3, ?NODE_NL} ! {add, lund,{r2, ?NODE_SE}},
    {r5, ?NODE_MD} ! {add, stockholm,{r1, ?NODE_SE}},
    {r5, ?NODE_MD} ! {add, enschede,{r4, ?NODE_NL}},
    {r4, ?NODE_NL} ! {add, chisinau,{r5, ?NODE_MD}},
    {r1, ?NODE_SE} ! {add, chisinau,{r5, ?NODE_MD}}.

deploy() ->
    Routers = [{r4, ?NODE_NL},{r3, ?NODE_NL},{r2, ?NODE_SE},{r1, ?NODE_SE},{r5, ?NODE_MD}],
    lists:foreach(fun(Router) -> Router ! broadcast end, Routers),
    timer:sleep(1000),
    lists:foreach(fun(Router) -> Router ! update end, Routers).