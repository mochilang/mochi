#!/usr/bin/env escript
-module(main).

-export([main/1]).

% Generated by Mochi transpiler v0.10.35 (86eba7eda) on 2025-07-22 12:58 +0700
main(_) ->
    People = [#{"name" => "Alice", "age" => 30}, #{"name" => "Bob", "age" => 25}],
    lists:foreach(fun (_row) ->     io:format("~p~n", [_row])
 end, People)
.
