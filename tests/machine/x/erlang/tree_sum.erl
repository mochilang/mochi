#!/usr/bin/env escript
% tree_sum.erl - generated from tree_sum.mochi

sum_tree(T) ->
    (case T of leaf -> 0; #{"__name" := "Node", left := Left, value := Value, right := Right} -> ((sum_tree(Left) + Value) + sum_tree(Right)) end).

main(_) ->
    T = #{"__name" => "Node", left => leaf, value => 1, right => #{"__name" => "Node", left => leaf, value => 2, right => leaf}},
    io:format("~p~n", [sum_tree(T)]).
