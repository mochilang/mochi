#!/usr/bin/env escript
% map_nested_assign.erl - generated from map_nested_assign.mochi

main(_) ->
    Data0 = #{outer => #{inner => 1}},
    DataInner0 = maps:get("outer", Data0), DataInnerUpd0 = maps:put("inner", 2, DataInner0), Data1 = Data0#{"outer" => DataInnerUpd0},
    io:format("~p~n", [maps:get("inner", maps:get("outer", Data1))]).
