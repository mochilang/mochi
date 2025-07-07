#!/usr/bin/env escript
%% list_nested_assign.erl - manual translation of tests/vm/valid/list_nested_assign.mochi

main(_) ->
    Matrix0 = [[1,2],[3,4]],
    [Row1, Row2] = Matrix0,
    UpdatedRow2 = [5, lists:nth(2, Row2)],
    Matrix1 = [Row1, UpdatedRow2],
    io:format("~p~n", [hd(lists:nth(2, Matrix1))]).
