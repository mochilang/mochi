#!/usr/bin/env escript
-module(main).

-export([main/1]).

% Generated by Mochi transpiler v0.10.34 (3e0d18933) on 2025-07-21 23:54 +0700
main(_) ->
    People = [#{"name" => "Alice", "city" => "Paris"}, #{"name" => "Bob", "city" => "Hanoi"}, #{"name" => "Charlie", "city" => "Paris"}, #{"name" => "Diana", "city" => "Hanoi"}, #{"name" => "Eve", "city" => "Paris"}, #{"name" => "Frank", "city" => "Hanoi"}, #{"name" => "George", "city" => "Paris"}],
    Big = lists:map(fun (P) ->     Key = element(1, P),
    Items = element(2, P),
    #{"city" => Key, "num" => length(Items)}
 end, lists:filter(fun (P) ->     Key = element(1, P),
    Items = element(2, P),
    (length(Items) >= 4)
 end, maps:to_list(lists:foldl(fun (P, Acc) ->     K = element(1, P),
    V = element(2, P),
    maps:put(K, (maps:get(K, Acc, []) ++ [V]), Acc)
 end, #{}, [{maps:get("city", P), P} || P <- People])))),
    io:format("~p~n", [Big])
.
