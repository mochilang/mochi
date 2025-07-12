#!/usr/bin/env escript
% cast_struct.erl - generated from cast_struct.mochi

main(_) ->
    Todo = #{"title" => "hi"},
    io:format("~p~n", [maps:get(title, Todo)]).
