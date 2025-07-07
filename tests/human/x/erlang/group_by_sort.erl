#!/usr/bin/env escript
%% group_by_sort.erl - manual translation of tests/vm/valid/group_by_sort.mochi

main(_) ->
    Items = [
        #{cat => "a", val => 3},
        #{cat => "a", val => 1},
        #{cat => "b", val => 5},
        #{cat => "b", val => 2}
    ],
    Groups = lists:foldl(fun(I,Acc) ->
                Cat = maps:get(cat,I),
                Val = maps:get(val,I),
                List = maps:get(Cat,Acc,[]),
                maps:put(Cat,[Val|List],Acc)
            end, #{}, Items),
    Result0 = [#{cat => C, total => lists:sum(Vals)} || {C,Vals} <- maps:to_list(Groups)],
    Result = lists:sort(fun(A,B) -> maps:get(total,B) < maps:get(total,A) end, Result0),
    io:format("~p~n", [Result]).
