# Errors

- append_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	A = [1, 2],
  7: 	mochi_print([append(A, 3)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- avg_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([mochi_avg([1, 2, 3])]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- basic_compare.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	A = (10 - 3),
  7: 	B = (2 + 2),
  8: 	mochi_print([A]),
  9: 	mochi_print([(A == 7)]),
 10: 	mochi_print([(B < 5)]).
- binary_precedence.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([(1 + (2 * 3))]),
  7: 	mochi_print([((1 + 2) * 3)]),
  8: 	mochi_print([((2 * 3) + 1)]),
  9: 	mochi_print([(2 * (3 + 1))]).
 10: 
- bool_chain.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, boom/0]).
  4: 
  5: % line 1
  6: boom() ->
  7: 	try
  8: 		mochi_print(["boom"]),
  9: 		throw({return, true})
 10: 	catch
- break_continue.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9],
  7: 	mochi_foreach(fun(N) ->
  8: 				case ((N % 2) == 0) of
  9: 			true ->
 10: 				throw(mochi_continue);
- cast_string_to_int.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print(["1995"]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- cast_struct.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: -record(todo, {title}).
  6: 
  7: main(_) ->
  8: 	Todo = mochi_cast_todo(#{"title" => "hi"}),
  9: 	mochi_print([Todo#todo.title]).
 10: 
- closure.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, 'makeAdder'/1]).
  4: 
  5: % line 1
  6: 'makeAdder'(N) ->
  7: 	try
  8: 		throw({return, fun(X) ->
  9: 		try
 10: 			throw({return, (X + N)})
- count_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([mochi_count([1, 2, 3])]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- cross_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}],
  7: 	Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}],
  8: 	Result = [#{orderId => maps:get(id, O), orderCustomerId => maps:get(customerId, O), pairedCustomerName => maps:get(name, C), orderTotal => maps:get(total, O)} || O <- Orders, C <- Customers],
  9: 	mochi_print(["--- Cross Join: All order-customer pairs ---"]),
 10: 	mochi_foreach(fun(Entry) ->
- cross_join_filter.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nums = [1, 2, 3],
  7: 	Letters = ["A", "B"],
  8: 	Pairs = [#{n => N, l => L} || N <- [N || N <- Nums, ((N % 2) == 0)], L <- Letters],
  9: 	mochi_print(["--- Even pairs ---"]),
 10: 	mochi_foreach(fun(P) ->
- cross_join_triple.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nums = [1, 2],
  7: 	Letters = ["A", "B"],
  8: 	Bools = [true, false],
  9: 	Combos = [#{n => N, l => L, b => B} || N <- Nums, L <- Letters, B <- Bools],
 10: 	mochi_print(["--- Cross Join of three lists ---"]),
- dataset_sort_take_limit.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Products = [#{name => "Laptop", price => 1500}, #{name => "Smartphone", price => 900}, #{name => "Tablet", price => 600}, #{name => "Monitor", price => 300}, #{name => "Keyboard", price => 100}, #{name => "Mouse", price => 50}, #{name => "Headphones", price => 200}],
  7: 	Expensive = (fun() ->
  8: 	Items = [{-maps:get(price, P), P} || P <- Products],
  9: 	Sorted = begin
 10: 		SPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Items),
- dataset_where_filter.mochi: compile error: unsupported expression
- exists_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Data = [1, 2],
  7: 	Flag = exists([X || X <- [X || X <- Data, (X == 1)]]),
  8: 	mochi_print([Flag]).
  9: 
 10: mochi_print(Args) ->
- for_list_collection.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_foreach(fun(N) ->
  7: 		mochi_print([N])
  8: 	end, [1, 2, 3]).
  9: 
 10: mochi_print(Args) ->
- for_loop.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_foreach(fun(I) ->
  7: 		mochi_print([I])
  8: 	end, lists:seq(1, (4)-1)).
  9: 
 10: mochi_print(Args) ->
- for_map_collection.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{"a" => 1, "b" => 2},
  7: 	mochi_foreach(fun(K) ->
  8: 		mochi_print([K])
  9: 	end, maps:keys(M)).
 10: 
- fun_call.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, add/2]).
  4: 
  5: % line 1
  6: add(A, B) ->
  7: 	try
  8: 		throw({return, (A + B)})
  9: 	catch
 10: 		throw:{return, V} -> V
- fun_expr_in_let.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Square = fun(X) ->
  7: 		try
  8: 			throw({return, (X * X)})
  9: 		catch
 10: 			throw:{return, V} -> V
- fun_three_args.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, sum3/3]).
  4: 
  5: % line 1
  6: sum3(A, B, C) ->
  7: 	try
  8: 		throw({return, ((A + B) + C)})
  9: 	catch
 10: 		throw:{return, V} -> V
- group_by.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	People = [#{name => "Alice", age => 30, city => "Paris"}, #{name => "Bob", age => 15, city => "Hanoi"}, #{name => "Charlie", age => 65, city => "Paris"}, #{name => "Diana", age => 45, city => "Hanoi"}, #{name => "Eve", age => 70, city => "Paris"}, #{name => "Frank", age => 22, city => "Hanoi"}],
  7: 	Stats = [#{city => maps:get(key, G), count => mochi_count(G), avg_age => mochi_avg([maps:get(age, P) || P <- G])} || G <- mochi_group_by(People, fun(Person) -> maps:get(city, Person) end)],
  8: 	mochi_print(["--- People grouped by city ---"]),
  9: 	mochi_foreach(fun(S) ->
 10: 		mochi_print([maps:get(city, S), ": count =", maps:get(count, S), ", avg_age =", maps:get(avg_age, S)])
- group_by_conditional_sum.mochi: compile error: unsupported expression
- group_by_having.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	People = [#{name => "Alice", city => "Paris"}, #{name => "Bob", city => "Hanoi"}, #{name => "Charlie", city => "Paris"}, #{name => "Diana", city => "Hanoi"}, #{name => "Eve", city => "Paris"}, #{name => "Frank", city => "Hanoi"}, #{name => "George", city => "Paris"}],
  7: 	Big = [#{city => maps:get(key, G), num => mochi_count(G)} || G <- mochi_group_by(People, fun(P) -> maps:get(city, P) end)],
  8: 	mochi_json(Big).
  9: 
 10: mochi_count(X) when is_list(X) -> length(X);
- group_by_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
  7: 	Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 1}, #{id => 102, customerId => 2}],
  8: 	Stats = [#{name => maps:get(key, G), count => mochi_count(G)} || O <- Orders, C <- Customers, (maps:get(customerId, O) == maps:get(id, C))],
  9: 	mochi_print(["--- Orders per customer ---"]),
 10: 	mochi_foreach(fun(S) ->
- group_by_left_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}],
  7: 	Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 1}, #{id => 102, customerId => 2}],
  8: 	Stats = [#{name => maps:get(key, G), count => mochi_count([R || R <- [R || R <- G, maps:get(o, R)]])} || C <- Customers, {C, O} <- mochi_left_join_item(C, Orders, fun(C, O) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
  9: 	mochi_print(["--- Group Left Join ---"]),
 10: 	mochi_foreach(fun(S) ->
- group_by_multi_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nations = [#{id => 1, name => "A"}, #{id => 2, name => "B"}],
  7: 	Suppliers = [#{id => 1, nation => 1}, #{id => 2, nation => 2}],
  8: 	Partsupp = [#{part => 100, supplier => 1, cost => 10, qty => 2}, #{part => 100, supplier => 2, cost => 20, qty => 1}, #{part => 200, supplier => 1, cost => 5, qty => 3}],
  9: 	Filtered = [#{part => maps:get(part, Ps), value => (maps:get(cost, Ps) * maps:get(qty, Ps))} || Ps <- Partsupp, S <- Suppliers, N <- Nations, (maps:get(id, S) == maps:get(supplier, Ps)), (maps:get(id, N) == maps:get(nation, S)), (maps:get(name, N) == "A")],
 10: 	Grouped = [#{part => maps:get(key, G), total => mochi_sum([maps:get(value, R) || R <- G])} || G <- mochi_group_by(Filtered, fun(X) -> maps:get(part, X) end)],
- group_by_multi_join_sort.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nation = [#{n_nationkey => 1, n_name => "BRAZIL"}],
  7: 	Customer = [#{c_custkey => 1, c_name => "Alice", c_acctbal => 100, c_nationkey => 1, c_address => "123 St", c_phone => "123-456", c_comment => "Loyal"}],
  8: 	Orders = [#{o_orderkey => 1000, o_custkey => 1, o_orderdate => "1993-10-15"}, #{o_orderkey => 2000, o_custkey => 1, o_orderdate => "1994-01-02"}],
  9: 	Lineitem = [#{l_orderkey => 1000, l_returnflag => "R", l_extendedprice => 1000, l_discount => 0.1}, #{l_orderkey => 2000, l_returnflag => "N", l_extendedprice => 500, l_discount => 0}],
 10: 	Start_date = "1993-10-01",
- group_by_sort.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Items = [#{cat => "a", val => 3}, #{cat => "a", val => 1}, #{cat => "b", val => 5}, #{cat => "b", val => 2}],
  7: 	Grouped = (fun() ->
  8: 	Items = [{-mochi_sum([maps:get(val, X) || X <- G]), #{cat => maps:get(key, G), total => mochi_sum([maps:get(val, X) || X <- G])}} || I <- Items],
  9: 	Sorted = begin
 10: 		SPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Items),
- group_items_iteration.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Data = [#{tag => "a", val => 1}, #{tag => "a", val => 2}, #{tag => "b", val => 3}],
  7: 	Groups = [G || G <- mochi_group_by(Data, fun(D) -> maps:get(tag, D) end)],
  8: 	Tmp = [],
  9: 	mochi_foreach(fun(G) ->
 10: 		Total = 0,
- if_else.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	X = 5,
  7: 		case (X > 3) of
  8: 		true ->
  9: 			mochi_print(["big"]);
 10: 				_ ->
- if_then_else.mochi: compile error: unsupported expression
- if_then_else_nested.mochi: compile error: unsupported expression
- in_operator.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Xs = [1, 2, 3],
  7: 	mochi_print([lists:member(2, Xs)]),
  8: 	mochi_print([not lists:member(5, Xs)]).
  9: 
 10: mochi_print(Args) ->
- in_operator_extended.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Xs = [1, 2, 3],
  7: 	Ys = [X || X <- [X || X <- Xs, ((X % 2) == 1)]],
  8: 	mochi_print([lists:member(1, Ys)]),
  9: 	mochi_print([lists:member(2, Ys)]),
 10: 	M = #{a => 1},
- inner_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}],
  7: 	Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}, #{id => 103, customerId => 4, total => 80}],
  8: 	Result = [#{orderId => maps:get(id, O), customerName => maps:get(name, C), total => maps:get(total, O)} || O <- Orders, C <- Customers, (maps:get(customerId, O) == maps:get(id, C))],
  9: 	mochi_print(["--- Orders with customer info ---"]),
 10: 	mochi_foreach(fun(Entry) ->
- join_multi.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
  7: 	Orders = [#{id => 100, customerId => 1}, #{id => 101, customerId => 2}],
  8: 	Items = [#{orderId => 100, sku => "a"}, #{orderId => 101, sku => "b"}],
  9: 	Result = [#{name => maps:get(name, C), sku => maps:get(sku, I)} || O <- Orders, C <- Customers, I <- Items, (maps:get(customerId, O) == maps:get(id, C)), (maps:get(id, O) == maps:get(orderId, I))],
 10: 	mochi_print(["--- Multi Join ---"]),
- json_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{a => 1, b => 2},
  7: 	mochi_json(M).
  8: 
  9: 
 10: mochi_escape_json([]) -> [];
- left_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}],
  7: 	Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 3, total => 80}],
  8: 	Result = [#{orderId => maps:get(id, O), customer => C, total => maps:get(total, O)} || O <- Orders, {O, C} <- mochi_left_join_item(O, Customers, fun(O, C) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
  9: 	mochi_print(["--- Left Join ---"]),
 10: 	mochi_foreach(fun(Entry) ->
- left_join_multi.mochi: compile error: unsupported join side
- len_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([length([1, 2, 3])]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- len_map.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([length(#{"a" => 1, "b" => 2})]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- len_string.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([length("mochi")]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- let_and_print.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	A = 10,
  7: 	B = 20,
  8: 	mochi_print([(A + B)]).
  9: 
 10: mochi_print(Args) ->
- list_assign.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nums = [1, 2],
  7: 	Nums_1 = 3,
  8: 	mochi_print([mochi_get(Nums_1, 1)]).
  9: 
 10: mochi_print(Args) ->
- list_index.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Xs = [10, 20, 30],
  7: 	mochi_print([mochi_get(Xs, 1)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- list_nested_assign.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Matrix = [[1, 2], [3, 4]],
  7: 	Matrix_1 = 5,
  8: 	mochi_print([mochi_get(mochi_get(Matrix_1, 1), 0)]).
  9: 
 10: mochi_print(Args) ->
- list_set_ops.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([mochi_union([1, 2], [2, 3])]),
  7: 	mochi_print([mochi_except([1, 2, 3], [2])]),
  8: 	mochi_print([mochi_intersect([1, 2, 3], [2, 4])]),
  9: 	mochi_print([length(lists:append([1, 2], [2, 3]))]).
 10: 
- load_yaml.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: -record(person, {name, age, email}).
  6: 
  7: main(_) ->
  8: 	People = mochi_load("../interpreter/valid/people.yaml", #{format => "yaml"}),
  9: 	Adults = [#{name => P#person.name, email => P#person.email} || P <- [P || P <- People, (P#person.age >= 18)]],
 10: 	mochi_foreach(fun(A) ->
- map_assign.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Scores = #{"alice" => 1},
  7: 	Scores_1 = 2,
  8: 	mochi_print([mochi_get(Scores_1, "bob")]).
  9: 
 10: mochi_print(Args) ->
- map_in_operator.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{1 => "a", 2 => "b"},
  7: 	mochi_print([maps:is_key(1, M)]),
  8: 	mochi_print([maps:is_key(3, M)]).
  9: 
 10: mochi_print(Args) ->
- map_index.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{"a" => 1, "b" => 2},
  7: 	mochi_print([maps:get("b", M)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- map_int_key.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{1 => "a", 2 => "b"},
  7: 	mochi_print([maps:get(1, M)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- map_literal_dynamic.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	X = 3,
  7: 	Y = 4,
  8: 	M = #{"a" => X, "b" => Y},
  9: 	mochi_print([maps:get("a", M), maps:get("b", M)]).
 10: 
- map_membership.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{"a" => 1, "b" => 2},
  7: 	mochi_print([maps:is_key("a", M)]),
  8: 	mochi_print([maps:is_key("c", M)]).
  9: 
 10: mochi_print(Args) ->
- map_nested_assign.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Data = #{"outer" => #{"inner" => 1}},
  7: 	Data_1 = 2,
  8: 	mochi_print([mochi_get(mochi_get(Data_1, "outer"), "inner")]).
  9: 
 10: mochi_print(Args) ->
- match_expr.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	X = 2,
  7: 	Label = (fun() ->
  8: 	_t0 = X,
  9: 	case _t0 of
 10: 		1 -> "one";
- match_full.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, classify/1]).
  4: 
  5: % line 32
  6: classify(N) ->
  7: 	try
  8: 		throw({return, (fun() ->
  9: 	_t0 = N,
 10: 	case _t0 of
- math_ops.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([(6 * 7)]),
  7: 	mochi_print([(7 / 2)]),
  8: 	mochi_print([(7 % 2)]).
  9: 
 10: mochi_print(Args) ->
- membership.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nums = [1, 2, 3],
  7: 	mochi_print([lists:member(2, Nums)]),
  8: 	mochi_print([lists:member(4, Nums)]).
  9: 
 10: mochi_print(Args) ->
- min_max_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nums = [3, 1, 4],
  7: 	mochi_print([mochi_min(Nums)]),
  8: 	mochi_print([mochi_max(Nums)]).
  9: 
 10: mochi_print(Args) ->
- nested_function.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, outer/1]).
  4: 
  5: % line 1
  6: outer(X) ->
  7: 	try
  8: 		ok,
  9: 		throw({return, inner(5)})
 10: 	catch
- order_by_map.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Data = [#{a => 1, b => 2}, #{a => 1, b => 1}, #{a => 0, b => 5}],
  7: 	Sorted = (fun() ->
  8: 	Items = [{#{a => maps:get(a, X), b => maps:get(b, X)}, X} || X <- Data],
  9: 	Sorted = begin
 10: 		SPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Items),
- outer_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}, #{id => 4, name => "Diana"}],
  7: 	Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}, #{id => 103, customerId => 5, total => 80}],
  8: 	Result = [#{order => O, customer => C} || {O, C} <- mochi_outer_join(Orders, Customers, fun(O, C) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
  9: 	mochi_print(["--- Outer Join using syntax ---"]),
 10: 	mochi_foreach(fun(Row) ->
- partial_application.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, add/2]).
  4: 
  5: % line 1
  6: add(A, B) ->
  7: 	try
  8: 		throw({return, (A + B)})
  9: 	catch
 10: 		throw:{return, V} -> V
- print_hello.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print(["hello"]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- pure_fold.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, triple/1]).
  4: 
  5: % line 1
  6: triple(X) ->
  7: 	try
  8: 		throw({return, (X * 3)})
  9: 	catch
 10: 		throw:{return, V} -> V
- pure_global_fold.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, inc/1]).
  4: 
  5: % line 2
  6: inc(X) ->
  7: 	try
  8: 		throw({return, (X + K)})
  9: 	catch
 10: 		throw:{return, V} -> V
- query_sum_select.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Nums = [1, 2, 3],
  7: 	Result = [mochi_sum(N) || N <- [N || N <- Nums, (N > 1)]],
  8: 	mochi_print([Result]).
  9: 
 10: mochi_print(Args) ->
- record_assign.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, inc/1]).
  4: 
  5: -record(counter, {n}).
  6: 
  7: % line 3
  8: inc(C) ->
  9: 	try
 10: 		C_1 = (C#counter.n + 1)
- right_join.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Customers = [#{id => 1, name => "Alice"}, #{id => 2, name => "Bob"}, #{id => 3, name => "Charlie"}, #{id => 4, name => "Diana"}],
  7: 	Orders = [#{id => 100, customerId => 1, total => 250}, #{id => 101, customerId => 2, total => 125}, #{id => 102, customerId => 1, total => 300}],
  8: 	Result = [#{customerName => maps:get(name, C), order => O} || {C, O} <- mochi_right_join(Customers, Orders, fun(C, O) -> (maps:get(customerId, O) == maps:get(id, C)) end)],
  9: 	mochi_print(["--- Right Join using syntax ---"]),
 10: 	mochi_foreach(fun(Entry) ->
- save_jsonl_stdout.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	People = [#{name => "Alice", age => 30}, #{name => "Bob", age => 25}],
  7: 	mochi_save(People, "-", #{format => "jsonl"}).
  8: 
  9: 
 10: mochi_load(Path, Opts) ->
- short_circuit.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, boom/2]).
  4: 
  5: % line 1
  6: boom(A, B) ->
  7: 	try
  8: 		mochi_print(["boom"]),
  9: 		throw({return, true})
 10: 	catch
- slice.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([mochi_slice([1, 2, 3], 1, 3)]),
  7: 	mochi_print([mochi_slice([1, 2, 3], 0, 2)]),
  8: 	mochi_print([mochi_slice("hello", 1, 4)]).
  9: 
 10: mochi_print(Args) ->
- sort_stable.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Items = [#{n => 1, v => "a"}, #{n => 1, v => "b"}, #{n => 2, v => "c"}],
  7: 	Result = (fun() ->
  8: 	Items = [{maps:get(n, I), maps:get(v, I)} || I <- Items],
  9: 	Sorted = begin
 10: 		SPairs = lists:sort(fun({A,_},{B,_}) -> A =< B end, Items),
- str_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([mochi_format(123)]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- string_compare.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([("a" < "b")]),
  7: 	mochi_print([("a" =< "a")]),
  8: 	mochi_print([("b" > "a")]),
  9: 	mochi_print([("b" >= "b")]).
 10: 
- string_concat.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([("hello " + "world")]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- string_contains.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	S = "catch",
  7: 	mochi_print([(string:str(S, "cat") =/= 0)]),
  8: 	mochi_print([(string:str(S, "dog") =/= 0)]).
  9: 
 10: mochi_print(Args) ->
- string_in_operator.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	S = "catch",
  7: 	mochi_print([(string:str(S, "cat") =/= 0)]),
  8: 	mochi_print([(string:str(S, "dog") =/= 0)]).
  9: 
 10: mochi_print(Args) ->
- string_index.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	S = "mochi",
  7: 	mochi_print([mochi_get(S, 1)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- string_prefix_slice.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Prefix = "fore",
  7: 	S1 = "forest",
  8: 	mochi_print([(mochi_slice(S1, 0, length(Prefix)) == Prefix)]),
  9: 	S2 = "desert",
 10: 	mochi_print([(mochi_slice(S2, 0, length(Prefix)) == Prefix)]).
- substring_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([substring("mochi", 1, 4)]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- sum_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([mochi_sum([1, 2, 3])]).
  7: 
  8: mochi_print(Args) ->
  9: 	Strs = [ mochi_format(A) || A <- Args ],
 10: 	io:format("~s~n", [lists:flatten(Strs)]).
- tail_recursion.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, sum_rec/2]).
  4: 
  5: % line 1
  6: sum_rec(N, Acc) ->
  7: 	try
  8: 				case (N == 0) of
  9: 			true ->
 10: 				throw({return, Acc});
- test_block.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: 
  6: main(_) ->
  7: 	mochi_print(["ok"])
  8: ,
  9: 	mochi_run_test("addition works", fun() ->
 10: 		X = (1 + 2),
- tree_sum.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, sum_tree/1]).
  4: 
  5: -record(leaf, {}).
  6: -record(node, {left, value, right}).
  7: 
  8: % line 9
  9: sum_tree(T) ->
 10: 	try
- two-sum.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1, 'twoSum'/2]).
  4: 
  5: % line 1
  6: 'twoSum'(Nums, Target) ->
  7: 	try
  8: 		N = length(Nums),
  9: 		mochi_foreach(fun(I) ->
 10: 			mochi_foreach(fun(J) ->
- typed_let.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	Y = undefined,
  7: 	mochi_print([Y]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- typed_var.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	X = undefined,
  7: 	mochi_print([X]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- unary_neg.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	mochi_print([-3]),
  7: 	mochi_print([(5 + -2)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- update_stmt.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: -record(person, {name, age, status}).
  6: 
  7: 
  8: main(_) ->
  9: 	People = [#person{name="Alice", age=17, status="minor"}, #person{name="Bob", age=25, status="unknown"}, #person{name="Charlie", age=18, status="unknown"}, #person{name="Diana", age=16, status="minor"}],
 10: 	People_1 = [ (case (Item#person.age >= 18) of true -> Item#person{status="adult", age=(Item#person.age + 1)}; _ -> Item end) || Item <- People ],
- user_type_literal.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: -record(person, {name, age}).
  6: -record(book, {title, author}).
  7: 
  8: main(_) ->
  9: 	Book = #book{title="Go", author=#person{name="Bob", age=42}},
 10: 	mochi_print([maps:get(name, maps:get(author, Book))]).
- values_builtin.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	M = #{"a" => 1, "b" => 2, "c" => 3},
  7: 	mochi_print([values(M)]).
  8: 
  9: mochi_print(Args) ->
 10: 	Strs = [ mochi_format(A) || A <- Args ],
- var_assignment.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	X = 1,
  7: 	X_1 = 2,
  8: 	mochi_print([X_1]).
  9: 
 10: mochi_print(Args) ->
- while_loop.mochi: convert error: exit status 1
  1: #!/usr/bin/env escript
  2: -module(main).
  3: -export([main/1]).
  4: 
  5: main(_) ->
  6: 	I = 0,
  7: 	Loop = fun Loop(I) ->
  8: 		case (I < 3) of
  9: 			true ->
 10: 				try
