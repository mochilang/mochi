#!/usr/bin/env escript
% list_assign.erl - generated from list_assign.mochi

main(_) ->
    Nums0 = [1, 2],
    Nums1 = lists:sublist(Nums0, 1) ++ [3] ++ lists:nthtail((1)+1, Nums0),
    io:format("~p~n", [lists:nth((1)+1, Nums1)]).
