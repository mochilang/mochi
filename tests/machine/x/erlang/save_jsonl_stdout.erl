#!/usr/bin/env escript
% save_jsonl_stdout.erl - generated from save_jsonl_stdout.mochi

main(_) ->
    People = [#{name => "Alice", age => 30}, #{name => "Bob", age => 25}],
    lists:foreach(fun(Row0) -> io:format("~s\n", [mochi_json_encode(Row0)]) end, People).

mochi_json_value(V) when is_integer(V); is_float(V) -> io_lib:format("~p", [V]);
mochi_json_value(V) when is_list(V) -> io_lib:format("~s", [V]);
mochi_json_value(true) -> "true";
mochi_json_value(false) -> "false";
mochi_json_value(undefined) -> "null".

mochi_json_encode(M) ->
    Pairs = [ io_lib:format("\"~s\":~s", [atom_to_list(K), mochi_json_value(V)]) || {K,V} <- maps:to_list(M) ],
    lists:flatten(["{", string:join(Pairs, ","), "}"]).
