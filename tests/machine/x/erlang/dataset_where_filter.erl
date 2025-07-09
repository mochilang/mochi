#!/usr/bin/env escript
% dataset_where_filter.erl - generated from dataset_where_filter.mochi

main(_) ->
    People = [#{name => "Alice", age => 30}, #{name => "Bob", age => 15}, #{name => "Charlie", age => 65}, #{name => "Diana", age => 45}],
    Adults = [#{name => maps:get(name, Person), age => maps:get(age, Person), is_senior => (maps:get(age, Person) >= 60)} || Person <- People, (maps:get(age, Person) >= 18)],
    io:format("~p~n", ["--- Adults ---"]),
    lists:foreach(fun(Person) -> io:format("~p ~p ~p ~p~n", [maps:get(name, Person), "is", maps:get(age, Person), (case maps:get(is_senior, Person) of true -> " (senior)"; _ -> "" end)]) end, Adults).
