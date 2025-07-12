#!/usr/bin/env escript
% two-sum.erl - generated from two-sum.mochi

twoSum(Nums, Target) ->
    N = length(Nums),
    lists:foreach(fun(I) -> lists:foreach(fun(J) -> (case ((lists:nth((I)+1, Nums) + lists:nth((J)+1, Nums)) == Target) of true -> [I, J]; _ -> ok end) end, lists:seq((I + 1), (N)-1)) end, lists:seq(0, (N)-1)),
    [-1, -1].

main(_) ->
    Result = twoSum([2, 7, 11, 15], 9),
    io:format("~p~n", [lists:nth((0)+1, Result)]),
    io:format("~p~n", [lists:nth((1)+1, Result)]).
