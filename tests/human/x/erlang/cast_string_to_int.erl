#!/usr/bin/env escript
%% cast_string_to_int.erl - manual translation of tests/vm/valid/cast_string_to_int.mochi

main(_) ->
    io:format("~p~n", [list_to_integer("1995")]).
