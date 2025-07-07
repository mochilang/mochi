#!/usr/bin/env escript
%% save_jsonl_stdout.erl - manual translation of tests/vm/valid/save_jsonl_stdout.mochi

main(_) ->
    People = [#{name => "Alice", age => 30},
              #{name => "Bob", age => 25}],
    lists:foreach(fun(P) ->
        io:format("{\"name\":\"~s\",\"age\":~p}\n",
                  [maps:get(name,P), maps:get(age,P)])
    end, People).
