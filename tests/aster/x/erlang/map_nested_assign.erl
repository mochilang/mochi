#!/usr/bin/env escript
-module(main).

-export([main/1]).

% Generated by Mochi transpiler v0.10.34 (94dce0814) on 2025-07-22 09:21 +0700
main(_) ->
    Data = #{"outer" => #{"inner" => 1}},
    Tmp = maps:get("outer", Data),
    Tmp2 = maps:put("inner", 2, Tmp),
    Data2 = maps:put("outer", Tmp2, Data),
    io:format("~p~n", [maps:get("inner", maps:get("outer", Data2))])
.
