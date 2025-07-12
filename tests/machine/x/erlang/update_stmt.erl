#!/usr/bin/env escript
% update_stmt.erl - generated from update_stmt.mochi

main(_) ->
    People = [#{"__name" => "Person", name => "Alice", age => 17, status => "minor"}, #{"__name" => "Person", name => "Bob", age => 25, status => "unknown"}, #{"__name" => "Person", name => "Charlie", age => 18, status => "unknown"}, #{"__name" => "Person", name => "Diana", age => 16, status => "minor"}],
    People0 = [case (maps:get(age, PeopleItem0) >= 18) of true -> PeopleItem0#{status => "adult", age => (maps:get(age, PeopleItem0) + 1)}; _ -> PeopleItem0 end || PeopleItem0 <- People],
    (case (People == [#{"__name" => "Person", name => "Alice", age => 17, status => "minor"}, #{"__name" => "Person", name => "Bob", age => 26, status => "adult"}, #{"__name" => "Person", name => "Charlie", age => 19, status => "adult"}, #{"__name" => "Person", name => "Diana", age => 16, status => "minor"}]) of true -> ok; _ -> erlang:error(test_failed) end),
    io:format("~p~n", ["ok"]).
