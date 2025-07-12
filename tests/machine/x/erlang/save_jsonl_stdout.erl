#!/usr/bin/env escript
% save_jsonl_stdout.erl - generated from save_jsonl_stdout.mochi

main(_) ->
    People = [#{name => "Alice", age => 30}, #{name => "Bob", age => 25}],
    lists:foreach(fun(Row0) -> io:format("~s\n", [mochi_to_json(Row0)]) end, People).

mochi_escape_json([]) -> [];
mochi_escape_json([H|T]) ->
    E = case H of
        $\ -> "\\";
        $" -> "\"";
        _ -> [H]
    end,
    E ++ mochi_escape_json(T).

mochi_to_json(true) -> "true";
mochi_to_json(false) -> "false";
mochi_to_json(V) when is_integer(V); is_float(V) -> lists:flatten(io_lib:format("~p", [V]));
mochi_to_json(V) when is_binary(V) -> "\"" ++ mochi_escape_json(binary_to_list(V)) ++ "\"";
mochi_to_json(V) when is_list(V), (V =:= [] orelse is_integer(hd(V))) -> "\"" ++ mochi_escape_json(V) ++ "\"";
mochi_to_json(V) when is_list(V) -> "[" ++ lists:join(",", [mochi_to_json(X) || X <- V]) ++ "]";
mochi_to_json(V) when is_map(V) -> "{" ++ lists:join(",", ["\"" ++ atom_to_list(K) ++ "\":" ++ mochi_to_json(Val) || {K,Val} <- maps:to_list(V)]) ++ "}".

mochi_json(V) -> io:format("~s
", [mochi_to_json(V)]).
