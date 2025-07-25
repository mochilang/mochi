#!/usr/bin/env escript
% Generated by Mochi compiler v0.10.26 on 2025-07-15T07:25:48Z
% q30.erl - generated from q30.mochi

main(_) ->
    Web_returns = [#{wr_returning_customer_sk => 1, wr_returned_date_sk => 1, wr_return_amt => 100, wr_returning_addr_sk => 1}, #{wr_returning_customer_sk => 2, wr_returned_date_sk => 1, wr_return_amt => 30, wr_returning_addr_sk => 2}, #{wr_returning_customer_sk => 1, wr_returned_date_sk => 1, wr_return_amt => 50, wr_returning_addr_sk => 1}],
    Date_dim = [#{d_date_sk => 1, d_year => 2000}],
    Customer_address = [#{ca_address_sk => 1, ca_state => "CA"}, #{ca_address_sk => 2, ca_state => "CA"}],
    Customer = [#{c_customer_sk => 1, c_customer_id => "C1", c_first_name => "John", c_last_name => "Doe", c_current_addr_sk => 1}, #{c_customer_sk => 2, c_customer_id => "C2", c_first_name => "Jane", c_last_name => "Smith", c_current_addr_sk => 2}],
    Customer_total_return = [#{ctr_customer_sk => mochi_get(cust, Key0), ctr_state => mochi_get(state, Key0), ctr_total_return => lists:sum([mochi_get(wr_return_amt, X) || X <- Val0])} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{#{cust => mochi_get(wr_returning_customer_sk, Wr), state => mochi_get(ca_state, Ca)}, #{wr => Wr, d => D, ca => Ca}} || Wr <- Web_returns, D <- Date_dim, Ca <- Customer_address, (mochi_get(wr_returned_date_sk, Wr) == mochi_get(d_date_sk, D)), (mochi_get(wr_returning_addr_sk, Wr) == mochi_get(ca_address_sk, Ca)), ((mochi_get(d_year, D) == 2000) andalso (mochi_get(ca_state, Ca) == "CA"))]))],
    Avg_by_state = [#{state => Key1, avg_return => (lists:sum([mochi_get(ctr_total_return, X) || X <- Val1]) / length([mochi_get(ctr_total_return, X) || X <- Val1]))} || {Key1, Val1} <- maps:to_list(lists:foldl(fun({Key1, Val1}, Acc1) -> L = maps:get(Key1, Acc1, []), maps:put(Key1, [Val1 | L], Acc1) end, #{}, [{mochi_get(ctr_state, Ctr), Ctr} || Ctr <- Customer_total_return]))],
    Result = [#{c_customer_id => mochi_get(c_customer_id, C), c_first_name => mochi_get(c_first_name, C), c_last_name => mochi_get(c_last_name, C), ctr_total_return => mochi_get(ctr_total_return, Ctr)} || Ctr <- Customer_total_return, Avg <- Avg_by_state, C <- Customer, (mochi_get(ctr_state, Ctr) == mochi_get(state, Avg)), (mochi_get(ctr_customer_sk, Ctr) == mochi_get(c_customer_sk, C)), (mochi_get(ctr_total_return, Ctr) > (mochi_get(avg_return, Avg) * 1.2))],
    mochi_json(Result),
    (case (Result == [#{c_customer_id => "C1", c_first_name => "John", c_last_name => "Doe", ctr_total_return => 150}]) of true -> ok; _ -> erlang:error(test_failed) end).

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
