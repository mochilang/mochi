#!/usr/bin/env escript
%% group_by_conditional_sum.erl - manual translation of tests/vm/valid/group_by_conditional_sum.mochi

main(_) ->
    Items = [
        #{cat => "a", val => 10, flag => true},
        #{cat => "a", val => 5, flag => false},
        #{cat => "b", val => 20, flag => true}
    ],
    Groups = lists:foldl(fun(Item,Acc) ->
                Cat = maps:get(cat,Item),
                Val = maps:get(val,Item),
                Flag = maps:get(flag,Item),
                {SumFlag,SumVal} = maps:get(Cat,Acc,{0,0}),
                NewSumFlag = SumFlag + (case Flag of true -> Val; false -> 0 end),
                maps:put(Cat,{NewSumFlag,SumVal+Val},Acc)
            end, #{}, Items),
    Result0 = [{Cat,{SF,SV}} || {Cat,{SF,SV}} <- maps:to_list(Groups)],
    Result1 = lists:map(fun({Cat,{SF,SV}}) ->
                #{cat => Cat, share => SF / SV}
            end, Result0),
    Sorted = lists:sort(fun(A,B) -> maps:get(cat,A) =< maps:get(cat,B) end, Result1),
    io:format("~p~n", [Sorted]).
