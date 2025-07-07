#!/usr/bin/env escript
%% group_items_iteration.erl - manual translation of tests/vm/valid/group_items_iteration.mochi

main(_) ->
    Data = [
        #{tag => "a", val => 1},
        #{tag => "a", val => 2},
        #{tag => "b", val => 3}
    ],
    Groups = lists:foldl(fun(Item, Acc) ->
        Tag = maps:get(tag, Item),
        Existing = maps:get(Tag, Acc, []),
        maps:put(Tag, [Item|Existing], Acc)
    end, #{}, Data),
    Tmp = [#{tag => T, total => lists:sum([maps:get(val, I) || I <- lists:reverse(Is)])} || {T, Is} <- maps:to_list(Groups)],
    Result = lists:sort(fun(A,B) -> maps:get(tag,A) < maps:get(tag,B) end, Tmp),
    io:format("~p~n", [Result]).
