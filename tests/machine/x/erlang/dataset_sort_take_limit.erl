#!/usr/bin/env escript
% dataset_sort_take_limit.erl - generated from dataset_sort_take_limit.mochi

main(_) ->
    Products = [#{name => "Laptop", price => 1500}, #{name => "Smartphone", price => 900}, #{name => "Tablet", price => 600}, #{name => "Monitor", price => 300}, #{name => "Keyboard", price => 100}, #{name => "Mouse", price => 50}, #{name => "Headphones", price => 200}],
    Expensive = lists:sublist(lists:nthtail(1, [V || {_, V} <- lists:keysort(1, [{-maps:get(price, P), P} || P <- Products])]), 3),
    io:format("~p~n", ["--- Top products (excluding most expensive) ---"]),
    lists:foreach(fun(Item) -> io:format("~p ~p ~p~n", [maps:get(name, Item), "costs $", maps:get(price, Item)]) end, Expensive).
