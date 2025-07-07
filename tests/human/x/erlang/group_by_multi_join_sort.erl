#!/usr/bin/env escript
%% group_by_multi_join_sort.erl - manual translation of tests/vm/valid/group_by_multi_join_sort.mochi

main(_) ->
    Nation = [#{n_nationkey => 1, n_name => "BRAZIL"}],
    Customer = [#{c_custkey => 1, c_name => "Alice", c_acctbal => 100.0,
                 c_nationkey => 1, c_address => "123 St", c_phone => "123-456",
                 c_comment => "Loyal"}],
    Orders = [#{o_orderkey => 1000, o_custkey => 1, o_orderdate => "1993-10-15"},
              #{o_orderkey => 2000, o_custkey => 1, o_orderdate => "1994-01-02"}],
    Lineitem = [#{l_orderkey => 1000, l_returnflag => "R",
                 l_extendedprice => 1000.0, l_discount => 0.1},
               #{l_orderkey => 2000, l_returnflag => "N",
                 l_extendedprice => 500.0, l_discount => 0.0}],
    Start = "1993-10-01",
    End = "1994-01-01",

    Rows = [#{c => C, o => O, l => L, n => N} ||
              C <- Customer,
              O <- Orders, maps:get(o_custkey,O) =:= maps:get(c_custkey,C),
              L <- Lineitem, maps:get(l_orderkey,L) =:= maps:get(o_orderkey,O),
              N <- Nation, maps:get(n_nationkey,N) =:= maps:get(c_nationkey,C),
              maps:get(o_orderdate,O) >= Start,
              maps:get(o_orderdate,O) < End,
              maps:get(l_returnflag,L) =:= "R"],
    Groups0 = lists:foldl(fun(Row,Acc) ->
                CRow = maps:get(c, Row),
                NRow = maps:get(n, Row),
                Key = {maps:get(c_custkey,CRow),
                       maps:get(c_name,CRow),
                       maps:get(c_acctbal,CRow),
                       maps:get(c_address,CRow),
                       maps:get(c_phone,CRow),
                       maps:get(c_comment,CRow),
                       maps:get(n_name,NRow)},
                List = maps:get(Key,Acc,[]),
                maps:put(Key,[Row|List],Acc)
            end, #{}, Rows),
    Result0 = [group_entry(Key, lists:reverse(Items)) || {Key,Items} <- maps:to_list(Groups0)],
    Result = lists:sort(fun(A,B) -> maps:get(revenue,B) < maps:get(revenue,A) end, Result0),
    io:format("~p~n", [Result]).

group_entry({CustKey,Name,Acct,Addr,Phone,Comment,NName}, Items) ->
    Revenue = lists:sum([
                maps:get(l_extendedprice, L) * (1 - maps:get(l_discount, L))
            || I <- Items,
               L = maps:get(l, I)
           ]),
    #{c_custkey => CustKey,
      c_name => Name,
      revenue => Revenue,
      c_acctbal => Acct,
      n_name => NName,
      c_address => Addr,
      c_phone => Phone,
      c_comment => Comment}.
