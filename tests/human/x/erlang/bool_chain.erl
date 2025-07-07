#!/usr/bin/env escript
%% bool_chain.erl - manual translation of tests/vm/valid/bool_chain.mochi

boom() ->
    io:format("boom~n"),
    true.

main(_) ->
    io:format("~p~n", [(1 < 2) andalso (2 < 3) andalso (3 < 4)]),
    io:format("~p~n", [(1 < 2) andalso (2 > 3) andalso boom()]),
    io:format("~p~n", [(1 < 2) andalso (2 < 3) andalso (3 > 4) andalso boom()]).
