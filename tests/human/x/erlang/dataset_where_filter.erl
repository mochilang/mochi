#!/usr/bin/env escript
%% dataset_where_filter.erl - manual translation of tests/vm/valid/dataset_where_filter.mochi

main(_) ->
    People = [
        #{name => "Alice", age => 30},
        #{name => "Bob", age => 15},
        #{name => "Charlie", age => 65},
        #{name => "Diana", age => 45}
    ],
    Adults = [#{name => Name, age => Age, is_senior => Age >= 60} || #{name := Name, age := Age} <- People, Age >= 18],
    io:format("--- Adults ---~n"),
    lists:foreach(fun(P) ->
        Suffix = case maps:get(is_senior,P) of true -> " (senior)"; false -> "" end,
        io:format("~s is ~p~s~n", [maps:get(name,P), maps:get(age,P), Suffix])
    end, Adults).
