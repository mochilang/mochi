#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:25:51Z
% q40.erl - generated from q40.mochi

main(_) ->
    Catalog_sales = [#{order => 1, item_sk => 1, warehouse_sk => 1, date_sk => 1, price => 100}, #{order => 2, item_sk => 1, warehouse_sk => 1, date_sk => 2, price => 150}],
    Catalog_returns = [#{order => 2, item_sk => 1, refunded => 150}],
    Item = [#{item_sk => 1, item_id => "I1", current_price => 1.2}],
    Warehouse = [#{warehouse_sk => 1, state => "CA"}],
    Date_dim = [#{date_sk => 1, date => "2020-01-10"}, #{date_sk => 2, date => "2020-01-20"}],
    Records = [#{w_state => mochi_get(state, W), i_item_id => mochi_get(item_id, I), sold_date => mochi_get(date, D), net => (mochi_get(price, Cs) - ((case (Cr == undefined) of true -> 0; _ -> mochi_get(refunded, Cr) end)))} || Cs <- Catalog_sales, {Cs, Cr} <- mochi_left_join_item(Cs, Catalog_returns, fun(Cs, Cr) -> ((mochi_get(order, Cs) == mochi_get(order, Cr)) andalso (mochi_get(item_sk, Cs) == mochi_get(item_sk, Cr))) end), W <- Warehouse, I <- Item, D <- Date_dim, (mochi_get(warehouse_sk, Cs) == mochi_get(warehouse_sk, W)), (mochi_get(item_sk, Cs) == mochi_get(item_sk, I)), (mochi_get(date_sk, Cs) == mochi_get(date_sk, D)), ((mochi_get(current_price, I) >= 0.99) andalso (mochi_get(current_price, I) =< 1.49))],
    Result = [#{w_state => mochi_get(w_state, Key0), i_item_id => mochi_get(i_item_id, Key0), sales_before => lists:sum([(case (mochi_get(sold_date, X) < "2020-01-15") of true -> mochi_get(net, X); _ -> 0 end) || X <- Val0]), sales_after => lists:sum([(case (mochi_get(sold_date, X) >= "2020-01-15") of true -> mochi_get(net, X); _ -> 0 end) || X <- Val0])} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{#{w_state => mochi_get(w_state, R), i_item_id => mochi_get(i_item_id, R)}, R} || R <- Records]))],
    mochi_json(Result),
    (case (Result == [#{w_state => "CA", i_item_id => "I1", sales_before => 100, sales_after => 0}]) of true -> ok; _ -> erlang:error(test_failed) end).

mochi_left_join_item(A, B, Fun) ->
    Matches = [ {A, J} || J <- B, Fun(A, J) ],
    case Matches of
        [] -> [{A, undefined}];
        _ -> Matches
    end.

mochi_left_join(L, R, Fun) ->
    lists:flatmap(fun(X) -> mochi_left_join_item(X, R, Fun) end, L).

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
