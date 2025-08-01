#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:26:06Z
% q73.erl - generated from q73.mochi

main(_) ->
    Store_sales = [#{ss_ticket_number => 1, ss_customer_sk => 1, ss_sold_date_sk => 1, ss_store_sk => 1, ss_hdemo_sk => 1}],
    Date_dim = [#{d_date_sk => 1, d_dom => 1, d_year => 1998}],
    Store = [#{s_store_sk => 1, s_county => "A"}],
    Household_demographics = [#{hd_demo_sk => 1, hd_buy_potential => "1001-5000", hd_vehicle_count => 2, hd_dep_count => 3}],
    Customer = [#{c_customer_sk => 1, c_last_name => "Smith", c_first_name => "Alice", c_salutation => "Ms.", c_preferred_cust_flag => "Y"}],
    Groups = [#{key => Key0, cnt => (case Val0 of #{items := It} -> length(It); _ -> length(Val0) end)} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{#{ticket => mochi_get(ss_ticket_number, Ss), cust => mochi_get(ss_customer_sk, Ss)}, #{ss => Ss, d => D, s => S, hd => Hd}} || Ss <- Store_sales, D <- Date_dim, S <- Store, Hd <- Household_demographics, (mochi_get(d_date_sk, D) == mochi_get(ss_sold_date_sk, Ss)), (mochi_get(s_store_sk, S) == mochi_get(ss_store_sk, Ss)), (mochi_get(hd_demo_sk, Hd) == mochi_get(ss_hdemo_sk, Ss)), (((((((mochi_get(d_dom, D) >= 1) andalso (mochi_get(d_dom, D) =< 2)) andalso (((mochi_get(hd_buy_potential, Hd) == "1001-5000") orelse (mochi_get(hd_buy_potential, Hd) == "0-500")))) andalso (mochi_get(hd_vehicle_count, Hd) > 0)) andalso ((mochi_get(hd_dep_count, Hd) / mochi_get(hd_vehicle_count, Hd)) > 1)) andalso ((((mochi_get(d_year, D) == 1998) orelse (mochi_get(d_year, D) == 1999)) orelse (mochi_get(d_year, D) == 2000)))) andalso (mochi_get(s_county, S) == "A"))]))],
    Result = [V || {_, V} <- lists:keysort(1, [{[-mochi_get(cnt, G), mochi_get(c_last_name, C)], #{c_last_name => mochi_get(c_last_name, C), c_first_name => mochi_get(c_first_name, C), c_salutation => mochi_get(c_salutation, C), c_preferred_cust_flag => mochi_get(c_preferred_cust_flag, C), ss_ticket_number => mochi_get(ticket, mochi_get(key, G)), cnt => mochi_get(cnt, G)}} || G <- Groups, C <- Customer, (mochi_get(c_customer_sk, C) == mochi_get(cust, mochi_get(key, G))), ((mochi_get(cnt, G) >= 1) andalso (mochi_get(cnt, G) =< 5))])],
    mochi_json(Result),
    (case (Result == [#{c_last_name => "Smith", c_first_name => "Alice", c_salutation => "Ms.", c_preferred_cust_flag => "Y", ss_ticket_number => 1, cnt => 1}]) of true -> ok; _ -> erlang:error(test_failed) end).

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
