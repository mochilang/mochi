#!/usr/bin/env escript
%% list_assign.erl - manual translation of tests/vm/valid/list_assign.mochi

main(_) ->
    Nums0 = [1,2],
    Nums1 = [hd(Nums0), 3],
    io:format("~p~n", [lists:nth(2, Nums1)]).
