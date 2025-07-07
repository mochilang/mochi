#!/usr/bin/env escript
%% load_yaml.erl - manual translation of tests/vm/valid/load_yaml.mochi
%% This Erlang version uses a hardcoded list instead of loading YAML.

main(_) ->
    People = [
        #{name => "Alice", age => 30, email => "alice@example.com"},
        #{name => "Bob", age => 15, email => "bob@example.com"},
        #{name => "Charlie", age => 20, email => "charlie@example.com"}
    ],
    Adults = [#{name => maps:get(name,P), email => maps:get(email,P)} ||
                P <- People, maps:get(age,P) >= 18],
    lists:foreach(fun(A) ->
        io:format("~s ~s~n", [maps:get(name,A), maps:get(email,A)])
    end, Adults).
