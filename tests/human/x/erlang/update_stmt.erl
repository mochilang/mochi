#!/usr/bin/env escript
%% update_stmt.erl - manual translation of tests/vm/valid/update_stmt.mochi

main(_) ->
    People0 = [
        #{name => "Alice", age => 17, status => "minor"},
        #{name => "Bob", age => 25, status => "unknown"},
        #{name => "Charlie", age => 18, status => "unknown"},
        #{name => "Diana", age => 16, status => "minor"}
    ],
    People = [if maps:get(age,P) >= 18 ->
                    P#{status => "adult", age => maps:get(age,P) + 1};
                true -> P end || P <- People0],
    Expected = [
        #{name => "Alice", age => 17, status => "minor"},
        #{name => "Bob", age => 26, status => "adult"},
        #{name => "Charlie", age => 19, status => "adult"},
        #{name => "Diana", age => 16, status => "minor"}
    ],
    (case People =:= Expected of
        true -> ok;
        false -> erlang:error(unexpected_result)
     end),
    io:format("ok~n").
