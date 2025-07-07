#!/usr/bin/env escript
%% map_nested_assign.erl - manual translation of tests/vm/valid/map_nested_assign.mochi

main(_) ->
    Data0 = #{"outer" => #{"inner" => 1}},
    Inner0 = maps:get("outer", Data0),
    Data1 = Data0#{"outer" => Inner0#{"inner" => 2}},
    io:format("~p~n", [maps:get("inner", maps:get("outer", Data1))]).
