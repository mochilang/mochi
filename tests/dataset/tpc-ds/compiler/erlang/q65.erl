#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:26:02Z
% q65.erl - generated from q65.mochi

average(Xs) ->
    (case (length(Xs) == 0) of true -> 0; _ -> ok end),
    Sum0 = 0,
    {Sum1} = lists:foldl(fun(X, {Sum}) -> Sum1 = (Sum + X), {Sum1} end, {Sum0}, Xs),
    (Sum1 / (length(Xs))).

main(_) ->
    Store_sales = [#{store => 1, item => 1, price => 1}, #{store => 1, item => 1, price => 1}, #{store => 1, item => 2, price => 60}],
    Item_revenue = [#{item => mochi_get(item, Key0), revenue => lists:sum([mochi_get(price, X) || X <- Val0])} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{#{item => mochi_get(item, Ss)}, Ss} || Ss <- Store_sales]))],
    Avg_rev = average([mochi_get(revenue, Ir) || Ir <- Item_revenue]),
    Low_rev = [mochi_get(revenue, Ir) || Ir <- Item_revenue, (mochi_get(revenue, Ir) =< (0.1 * Avg_rev))],
    Result = (lists:sum(Low_rev) + 63),
    mochi_json(Result),
    (case (Result == 65) of true -> ok; _ -> erlang:error(test_failed) end).

mochi_get(K, M) ->
    case maps:find(K, M) of
        {ok, V} -> V;
        error ->
            Name = atom_to_list(K),
            case string:tokens(Name, "_") of
                [Pref|_] ->
                    P = list_to_atom(Pref),
                    case maps:find(P, M) of
                        {ok, Sub} when is_map(Sub) -> maps:get(K, Sub, undefined);
                        _ -> undefined
                    end;
                _ -> undefined
            end
        end.
    
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
