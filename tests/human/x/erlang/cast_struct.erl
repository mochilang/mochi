#!/usr/bin/env escript
%% cast_struct.erl - manual translation of tests/vm/valid/cast_struct.mochi

main(_) ->
    Todo = #{title => "hi"},
    io:format("~s~n", [maps:get(title, Todo)]).
