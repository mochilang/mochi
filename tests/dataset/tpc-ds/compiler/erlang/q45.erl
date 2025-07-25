#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:25:54Z
% q45.erl - generated from q45.mochi

main(_) ->
    Web_sales = [#{bill_customer_sk => 1, item_sk => 1, sold_date_sk => 1, sales_price => 50}, #{bill_customer_sk => 2, item_sk => 2, sold_date_sk => 1, sales_price => 30}],
    Customer = [#{c_customer_sk => 1, c_current_addr_sk => 1}, #{c_customer_sk => 2, c_current_addr_sk => 2}],
    Customer_address = [#{ca_address_sk => 1, ca_zip => "85669"}, #{ca_address_sk => 2, ca_zip => "99999"}],
    Item = [#{i_item_sk => 1, i_item_id => "I1"}, #{i_item_sk => 2, i_item_id => "I2"}],
    Date_dim = [#{d_date_sk => 1, d_qoy => 1, d_year => 2020}],
    Zip_list = ["85669", "86197", "88274", "83405", "86475", "85392", "85460", "80348", "81792"],
    Item_ids = ["I2"],
    Base = [#{ca_zip => Key0, sum_ws_sales_price => lists:sum([mochi_get(sales_price, mochi_get(ws, X)) || X <- Val0])} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{mochi_get(ca_zip, Ca), #{ws => Ws, c => C, ca => Ca, i => I, d => D}} || Ws <- Web_sales, C <- Customer, Ca <- Customer_address, I <- Item, D <- Date_dim, (mochi_get(bill_customer_sk, Ws) == mochi_get(c_customer_sk, C)), (mochi_get(c_current_addr_sk, C) == mochi_get(ca_address_sk, Ca)), (mochi_get(item_sk, Ws) == mochi_get(i_item_sk, I)), (mochi_get(sold_date_sk, Ws) == mochi_get(d_date_sk, D)), ((((lists:member(substr(mochi_get(ca_zip, Ca), 0, 5), Zip_list) orelse lists:member(mochi_get(i_item_id, I), Item_ids))) andalso (mochi_get(d_qoy, D) == 1)) andalso (mochi_get(d_year, D) == 2020))]))],
    Records = Base,
    mochi_json(Records),
    (case (Records == [#{ca_zip => "85669", sum_ws_sales_price => 50}, #{ca_zip => "99999", sum_ws_sales_price => 30}]) of true -> ok; _ -> erlang:error(test_failed) end).

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
