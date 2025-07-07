#!/usr/bin/env escript
%% tree_sum.erl - manual translation of tests/vm/valid/tree_sum.mochi

-record(node, {left, value, right}).

type_sum_tree(leaf) -> 0;
type_sum_tree(#node{left=L, value=V, right=R}) ->
    type_sum_tree(L) + V + type_sum_tree(R).

main(_) ->
    T = #node{left=leaf, value=1,
             right=#node{left=leaf, value=2, right=leaf}},
    io:format("~p~n", [type_sum_tree(T)]).
