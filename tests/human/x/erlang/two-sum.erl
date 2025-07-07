#!/usr/bin/env escript
%% two-sum.erl - manual translation of tests/vm/valid/two-sum.mochi

two_sum(Nums, Target) ->
    two_sum(Nums, Target, 0).

two_sum(Nums, Target, I) when I < length(Nums) ->
    case find_j(Nums, Target, I, I+1) of
        {ok, J} -> [I, J];
        false -> two_sum(Nums, Target, I+1)
    end;
two_sum(_, _, _) -> [-1,-1].

find_j(Nums, Target, I, J) when J < length(Nums) ->
    case lists:nth(I+1, Nums) + lists:nth(J+1, Nums) of
        Target -> {ok, J};
        _ -> find_j(Nums, Target, I, J+1)
    end;
find_j(_, _, _, _) -> false.

main(_) ->
    Result = two_sum([2,7,11,15], 9),
    io:format("~p~n", [lists:nth(1, Result)]),
    io:format("~p~n", [lists:nth(2, Result)]).
