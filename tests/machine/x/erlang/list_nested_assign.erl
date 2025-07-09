#!/usr/bin/env escript
% list_nested_assign.erl - generated from list_nested_assign.mochi

main(_) ->
    Matrix0 = [[1, 2], [3, 4]],
    MatrixInner0 = lists:nth((1)+1, Matrix0), MatrixInnerUpd0 = lists:sublist(MatrixInner0, 0) ++ [5] ++ lists:nthtail((0)+1, MatrixInner0), Matrix1 = lists:sublist(Matrix0, 1) ++ [MatrixInnerUpd0] ++ lists:nthtail((1)+1, Matrix0),
    io:format("~p~n", [lists:nth((0)+1, lists:nth((1)+1, Matrix1))]).
