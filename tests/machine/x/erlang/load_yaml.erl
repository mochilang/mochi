#!/usr/bin/env escript
% load_yaml.erl - generated from load_yaml.mochi

main(_) ->
    People = [#{"__name" => "Person", age => 30, email => "alice@example.com", name => "Alice"}, #{"__name" => "Person", name => "Bob", age => 15, email => "bob@example.com"}, #{"__name" => "Person", name => "Charlie", age => 20, email => "charlie@example.com"}],
    Adults = [#{name => maps:get(name, P), email => maps:get(email, P)} || P <- People, (maps:get(age, P) >= 18)],
    lists:foreach(fun(A) -> io:format("~p ~p~n", [maps:get(name, A), maps:get(email, A)]) end, Adults).
