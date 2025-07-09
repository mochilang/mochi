#!/usr/bin/env escript
% in_operator_extended.erl - generated from in_operator_extended.mochi

main(_) ->
    Xs = [1, 2, 3],
    Ys = [X || X <- Xs, ((X rem 2) == 1)],
    io:format("~p~n", [lists:member(1, Ys)]),
    io:format("~p~n", [lists:member(2, Ys)]),
    M = #{a => 1},
    io:format("~p~n", [maps:is_key("a", M)]),
    io:format("~p~n", [maps:is_key("b", M)]),
    S = "hello",
    io:format("~p~n", [string:str(S, "ell") > 0]),
    io:format("~p~n", [string:str(S, "foo") > 0]).
