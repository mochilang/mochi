#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:25:49Z
% q32.erl - generated from q32.mochi

main(_) ->
    Catalog_sales = [#{cs_item_sk => 1, cs_sold_date_sk => 1, cs_ext_discount_amt => 5}, #{cs_item_sk => 1, cs_sold_date_sk => 2, cs_ext_discount_amt => 10}, #{cs_item_sk => 1, cs_sold_date_sk => 3, cs_ext_discount_amt => 20}],
    Item = [#{i_item_sk => 1, i_manufact_id => 1}],
    Date_dim = [#{d_date_sk => 1, d_year => 2000}, #{d_date_sk => 2, d_year => 2000}, #{d_date_sk => 3, d_year => 2000}],
    Filtered = [mochi_get(cs_ext_discount_amt, Cs) || Cs <- Catalog_sales, I <- Item, D <- Date_dim, (mochi_get(cs_item_sk, Cs) == mochi_get(i_item_sk, I)), (mochi_get(cs_sold_date_sk, Cs) == mochi_get(d_date_sk, D)), ((mochi_get(i_manufact_id, I) == 1) andalso (mochi_get(d_year, D) == 2000))],
    Avg_discount = (lists:sum(Filtered) / length(Filtered)),
    Result = lists:sum([X || X <- Filtered, (X > (Avg_discount * 1.3))]),
    mochi_json(Result),
    (case (Result == 20) of true -> ok; _ -> erlang:error(test_failed) end).

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
