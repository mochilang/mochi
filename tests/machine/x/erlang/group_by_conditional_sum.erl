#!/usr/bin/env escript
% group_by_conditional_sum.erl - generated from group_by_conditional_sum.mochi

main(_) ->
    Items = [#{cat => "a", val => 10, flag => true}, #{cat => "a", val => 5, flag => false}, #{cat => "b", val => 20, flag => true}],
    Result = [V || {_, V} <- lists:keysort(1, [{Key0, #{cat => Key0, share => (lists:sum([(case maps:get(flag, X) of true -> maps:get(val, X); _ -> 0 end) || X <- Val0]) / lists:sum([maps:get(val, X) || X <- Val0]))}} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{maps:get(cat, I), #{I=I}} || I <- Items]))])],
    io:format("~p~n", [Result]).
