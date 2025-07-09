#!/usr/bin/env escript
% group_by_multi_join_sort.erl - generated from group_by_multi_join_sort.mochi

main(_) ->
    Nation = [#{n_nationkey => 1, n_name => "BRAZIL"}],
    Customer = [#{c_custkey => 1, c_name => "Alice", c_acctbal => 100, c_nationkey => 1, c_address => "123 St", c_phone => "123-456", c_comment => "Loyal"}],
    Orders = [#{o_orderkey => 1000, o_custkey => 1, o_orderdate => "1993-10-15"}, #{o_orderkey => 2000, o_custkey => 1, o_orderdate => "1994-01-02"}],
    Lineitem = [#{l_orderkey => 1000, l_returnflag => "R", l_extendedprice => 1000, l_discount => 0.1}, #{l_orderkey => 2000, l_returnflag => "N", l_extendedprice => 500, l_discount => 0}],
    Start_date = "1993-10-01",
    End_date = "1994-01-01",
    Result = [V || {_, V} <- lists:keysort(1, [{-lists:sum([(maps:get(l_extendedprice, maps:get(l, X)) * ((1 - maps:get(l_discount, maps:get(l, X))))) || X <- Val0]), #{c_custkey => maps:get(c_custkey, Key0), c_name => maps:get(c_name, Key0), revenue => lists:sum([(maps:get(l_extendedprice, maps:get(l, X)) * ((1 - maps:get(l_discount, maps:get(l, X))))) || X <- Val0]), c_acctbal => maps:get(c_acctbal, Key0), n_name => maps:get(n_name, Key0), c_address => maps:get(c_address, Key0), c_phone => maps:get(c_phone, Key0), c_comment => maps:get(c_comment, Key0)}} || {Key0, Val0} <- maps:to_list(lists:foldl(fun({Key0, Val0}, Acc0) -> L = maps:get(Key0, Acc0, []), maps:put(Key0, [Val0 | L], Acc0) end, #{}, [{#{c_custkey => maps:get(c_custkey, C), c_name => maps:get(c_name, C), c_acctbal => maps:get(c_acctbal, C), c_address => maps:get(c_address, C), c_phone => maps:get(c_phone, C), c_comment => maps:get(c_comment, C), n_name => maps:get(n_name, N)}, #{C=C, O=O, L=L, N=N}} || C <- Customer, O <- Orders, L <- Lineitem, N <- Nation, (maps:get(o_custkey, O) == maps:get(c_custkey, C)), (maps:get(l_orderkey, L) == maps:get(o_orderkey, O)), (maps:get(n_nationkey, N) == maps:get(c_nationkey, C)), (((((maps:get(o_orderdate, O) >= Start_date) andalso maps:get(o_orderdate, O)) < End_date) andalso maps:get(l_returnflag, L)) == "R")]))])],
    io:format("~p~n", [Result]).
