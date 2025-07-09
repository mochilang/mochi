#!/usr/bin/env escript
% sort_stable.erl - generated from sort_stable.mochi

main(_) ->
    Items = [#{n => 1, v => "a"}, #{n => 1, v => "b"}, #{n => 2, v => "c"}],
    Result = [V || {_, V} <- lists:keysort(1, [{maps:get(n, I), maps:get(v, I)} || I <- Items])],
    io:format("~p~n", [Result]).
