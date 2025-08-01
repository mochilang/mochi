#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:26:05Z
% q71.erl - generated from q71.mochi

main(_) ->
    Item = [#{i_item_sk => 1, i_brand_id => 10, i_brand => "BrandA", i_manager_id => 1}, #{i_item_sk => 2, i_brand_id => 20, i_brand => "BrandB", i_manager_id => 1}],
    Time_dim = [#{t_time_sk => 1, t_hour => 8, t_minute => 30, t_meal_time => "breakfast"}, #{t_time_sk => 2, t_hour => 18, t_minute => 0, t_meal_time => "dinner"}, #{t_time_sk => 3, t_hour => 12, t_minute => 0, t_meal_time => "lunch"}],
    Date_dim = [#{d_date_sk => 1, d_moy => 12, d_year => 1998}],
    Web_sales = [#{ws_ext_sales_price => 100, ws_sold_date_sk => 1, ws_item_sk => 1, ws_sold_time_sk => 1}],
    Catalog_sales = [#{cs_ext_sales_price => 200, cs_sold_date_sk => 1, cs_item_sk => 1, cs_sold_time_sk => 2}],
    Store_sales = [#{ss_ext_sales_price => 150, ss_sold_date_sk => 1, ss_item_sk => 2, ss_sold_time_sk => 1}],
    Union_sales = concat([#{ext_price => mochi_get(ws_ext_sales_price, Ws), item_sk => mochi_get(ws_item_sk, Ws), time_sk => mochi_get(ws_sold_time_sk, Ws)} || Ws <- Web_sales, D <- Date_dim, (mochi_get(d_date_sk, D) == mochi_get(ws_sold_date_sk, Ws)), ((mochi_get(d_moy, D) == 12) andalso (mochi_get(d_year, D) == 1998))], [#{ext_price => mochi_get(cs_ext_sales_price, Cs), item_sk => mochi_get(cs_item_sk, Cs), time_sk => mochi_get(cs_sold_time_sk, Cs)} || Cs <- Catalog_sales, D <- Date_dim, (mochi_get(d_date_sk, D) == mochi_get(cs_sold_date_sk, Cs)), ((mochi_get(d_moy, D) == 12) andalso (mochi_get(d_year, D) == 1998))], [#{ext_price => mochi_get(ss_ext_sales_price, Ss), item_sk => mochi_get(ss_item_sk, Ss), time_sk => mochi_get(ss_sold_time_sk, Ss)} || Ss <- Store_sales, D <- Date_dim, (mochi_get(d_date_sk, D) == mochi_get(ss_sold_date_sk, Ss)), ((mochi_get(d_moy, D) == 12) andalso (mochi_get(d_year, D) == 1998))]),
    Result = [V || {_, V} <- lists:keysort(1, [{[-lists:sum([mochi_get(ext_price, mochi_get(s, X)) || X <- Val0]), mochi_get(brand_id, Key0)], #{i_brand_id => mochi_get(brand_id, Key0), i_brand => mochi_get(brand, Key0), t_hour => mochi_get(t_hour, Key0), t_minute => mochi_get(t_minute, Key0), ext_price => lists:sum([mochi_get(ext_price, mochi_get(s, X)) || X <- Val0])}} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{#{brand_id => mochi_get(i_brand_id, I), brand => mochi_get(i_brand, I), t_hour => mochi_get(t_hour, T), t_minute => mochi_get(t_minute, T)}, #{i => I, s => S, t => T}} || I <- Item, S <- Union_sales, T <- Time_dim, (mochi_get(item_sk, S) == mochi_get(i_item_sk, I)), (mochi_get(t_time_sk, T) == mochi_get(time_sk, S)), ((mochi_get(i_manager_id, I) == 1) andalso (((mochi_get(t_meal_time, T) == "breakfast") orelse (mochi_get(t_meal_time, T) == "dinner"))))]))])],
    mochi_json(Result),
    (case (Result == [#{i_brand_id => 10, i_brand => "BrandA", t_hour => 18, t_minute => 0, ext_price => 200}, #{i_brand_id => 20, i_brand => "BrandB", t_hour => 8, t_minute => 30, ext_price => 150}, #{i_brand_id => 10, i_brand => "BrandA", t_hour => 8, t_minute => 30, ext_price => 100}]) of true -> ok; _ -> erlang:error(test_failed) end).

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
