# Go Round-Trip Failures using runtime/vm

## tests/vm/valid/append_builtin.mochi

```
golden mismatch:
-- got --

-- want --
1 2 3
```

## tests/vm/valid/avg_builtin.mochi

```
golden mismatch:
-- got --

-- want --
2
```

## tests/vm/valid/basic_compare.mochi

```
golden mismatch:
-- got --

-- want --
7
true
true
```

## tests/vm/valid/binary_precedence.mochi

```
golden mismatch:
-- got --

-- want --
7
9
7
8
```

## tests/vm/valid/bool_chain.mochi

```
golden mismatch:
-- got --

-- want --
true
false
false
```

## tests/vm/valid/break_continue.mochi

```
golden mismatch:
-- got --

-- want --
odd number: 1
odd number: 3
odd number: 5
odd number: 7
```

## tests/vm/valid/cast_string_to_int.mochi

```
roundtrip parse error: parse error: 3:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/cast_struct.mochi

```
roundtrip parse error: parse error: 6:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/closure.mochi

```
roundtrip parse error: parse error: 2:28: unexpected token "(" (expected "{" Statement* "}")
```

## tests/vm/valid/count_builtin.mochi

```
golden mismatch:
-- got --

-- want --
3
```

## tests/vm/valid/cross_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Cross Join: All order-customer pairs ---
Order 100 (customerId: 1 , total: $ 250 ) paired with Alice
Order 100 (customerId: 1 , total: $ 250 ) paired with Bob
Order 100 (customerId: 1 , total: $ 250 ) paired with Charlie
Order 101 (customerId: 2 , total: $ 125 ) paired with Alice
Order 101 (customerId: 2 , total: $ 125 ) paired with Bob
Order 101 (customerId: 2 , total: $ 125 ) paired with Charlie
Order 102 (customerId: 1 , total: $ 300 ) paired with Alice
Order 102 (customerId: 1 , total: $ 300 ) paired with Bob
Order 102 (customerId: 1 , total: $ 300 ) paired with Charlie
```

## tests/vm/valid/cross_join_filter.mochi

```
golden mismatch:
-- got --

-- want --
--- Even pairs ---
2 A
2 B
```

## tests/vm/valid/cross_join_triple.mochi

```
golden mismatch:
-- got --

-- want --
--- Cross Join of three lists ---
1 A true
1 A false
1 B true
1 B false
2 A true
2 A false
2 B true
2 B false
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/dataset_where_filter.mochi

```
golden mismatch:
-- got --

-- want --
--- Adults ---
Alice is 30
Charlie is 65  (senior)
Diana is 45
```

## tests/vm/valid/exists_builtin.mochi

```
golden mismatch:
-- got --

-- want --
true
```

## tests/vm/valid/for_list_collection.mochi

```
golden mismatch:
-- got --

-- want --
1
2
3
```

## tests/vm/valid/for_loop.mochi

```
golden mismatch:
-- got --

-- want --
1
2
3
```

## tests/vm/valid/for_map_collection.mochi

```
golden mismatch:
-- got --

-- want --
a
b
```

## tests/vm/valid/fun_call.mochi

```
golden mismatch:
-- got --

-- want --
5
```

## tests/vm/valid/fun_expr_in_let.mochi

```
golden mismatch:
-- got --

-- want --
36
```

## tests/vm/valid/fun_three_args.mochi

```
golden mismatch:
-- got --

-- want --
6
```

## tests/vm/valid/group_by.mochi

```
roundtrip parse error: parse error: 4:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
roundtrip parse error: parse error: 3:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_having.mochi

```
golden mismatch:
-- got --

-- want --
[{"city":"Paris","num":4}]
```

## tests/vm/valid/group_by_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Orders per customer ---
Alice orders: 2
Bob orders: 1
```

## tests/vm/valid/group_by_left_join.mochi

```
roundtrip parse error: parse error: 3:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_multi_join.mochi

```
roundtrip parse error: parse error: 3:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
roundtrip parse error: parse error: 3:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_sort.mochi

```
roundtrip parse error: parse error: 3:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_items_iteration.mochi

```
compile error: cannot iterate over type any
```

## tests/vm/valid/if_else.mochi

```
golden mismatch:
-- got --

-- want --
big
```

## tests/vm/valid/if_then_else.mochi

```
golden mismatch:
-- got --

-- want --
yes
```

## tests/vm/valid/if_then_else_nested.mochi

```
golden mismatch:
-- got --

-- want --
medium
```

## tests/vm/valid/in_operator.mochi

```
golden mismatch:
-- got --

-- want --
true
true
```

## tests/vm/valid/in_operator_extended.mochi

```
golden mismatch:
-- got --

-- want --
true
false
true
false
true
false
```

## tests/vm/valid/inner_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Orders with customer info ---
Order 100 by Alice - $ 250
Order 101 by Bob - $ 125
Order 102 by Alice - $ 300
```

## tests/vm/valid/join_multi.mochi

```
golden mismatch:
-- got --

-- want --
--- Multi Join ---
Alice bought item a
Bob bought item b
```

## tests/vm/valid/json_builtin.mochi

```
golden mismatch:
-- got --

-- want --
{"a":1,"b":2}
```

## tests/vm/valid/left_join.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/left_join_multi.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/len_builtin.mochi

```
golden mismatch:
-- got --

-- want --
3
```

## tests/vm/valid/len_map.mochi

```
golden mismatch:
-- got --

-- want --
2
```

## tests/vm/valid/len_string.mochi

```
golden mismatch:
-- got --

-- want --
5
```

## tests/vm/valid/let_and_print.mochi

```
golden mismatch:
-- got --

-- want --
30
```

## tests/vm/valid/list_assign.mochi

```
golden mismatch:
-- got --

-- want --
3
```

## tests/vm/valid/list_index.mochi

```
golden mismatch:
-- got --

-- want --
20
```

## tests/vm/valid/list_nested_assign.mochi

```
golden mismatch:
-- got --

-- want --
5
```

## tests/vm/valid/list_set_ops.mochi

```
golden mismatch:
-- got --

-- want --
1 2 3
1 3
2
4
```

## tests/vm/valid/load_yaml.mochi

```
roundtrip parse error: parse error: 8:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/map_assign.mochi

```
golden mismatch:
-- got --

-- want --
2
```

## tests/vm/valid/map_in_operator.mochi

```
golden mismatch:
-- got --

-- want --
true
false
```

## tests/vm/valid/map_index.mochi

```
golden mismatch:
-- got --

-- want --
2
```

## tests/vm/valid/map_int_key.mochi

```
golden mismatch:
-- got --

-- want --
a
```

## tests/vm/valid/map_literal_dynamic.mochi

```
golden mismatch:
-- got --

-- want --
3 4
```

## tests/vm/valid/map_membership.mochi

```
golden mismatch:
-- got --

-- want --
true
false
```

## tests/vm/valid/map_nested_assign.mochi

```
golden mismatch:
-- got --

-- want --
2
```

## tests/vm/valid/match_expr.mochi

```
golden mismatch:
-- got --

-- want --
two
```

## tests/vm/valid/match_full.mochi

```
golden mismatch:
-- got --

-- want --
two
relaxed
confirmed
zero
many
```

## tests/vm/valid/math_ops.mochi

```
golden mismatch:
-- got --

-- want --
42
3.5
1
```

## tests/vm/valid/membership.mochi

```
golden mismatch:
-- got --

-- want --
true
false
```

## tests/vm/valid/min_max_builtin.mochi

```
golden mismatch:
-- got --

-- want --
1
4
```

## tests/vm/valid/nested_function.mochi

```
golden mismatch:
-- got --

-- want --
8
```

## tests/vm/valid/order_by_map.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/outer_join.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/partial_application.mochi

```
golden mismatch:
-- got --

-- want --
8
```

## tests/vm/valid/print_hello.mochi

```
golden mismatch:
-- got --

-- want --
hello
```

## tests/vm/valid/pure_fold.mochi

```
golden mismatch:
-- got --

-- want --
9
```

## tests/vm/valid/pure_global_fold.mochi

```
golden mismatch:
-- got --

-- want --
5
```

## tests/vm/valid/query_sum_select.mochi

```
golden mismatch:
-- got --

-- want --
5
```

## tests/vm/valid/record_assign.mochi

```
roundtrip parse error: parse error: 8:26: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/right_join.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
roundtrip parse error: parse error: 2:44: unexpected token "[" (expected ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/short_circuit.mochi

```
golden mismatch:
-- got --

-- want --
false
true
```

## tests/vm/valid/slice.mochi

```
golden mismatch:
-- got --

-- want --
2 3
1 2
ell
```

## tests/vm/valid/sort_stable.mochi

```
roundtrip parse error: parse error: 3:3: unexpected token "on" (expected "fun" <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/str_builtin.mochi

```
golden mismatch:
-- got --

-- want --
123
```

## tests/vm/valid/string_compare.mochi

```
golden mismatch:
-- got --

-- want --
true
true
true
true
```

## tests/vm/valid/string_concat.mochi

```
golden mismatch:
-- got --

-- want --
hello world
```

## tests/vm/valid/string_contains.mochi

```
golden mismatch:
-- got --

-- want --
true
false
```

## tests/vm/valid/string_in_operator.mochi

```
golden mismatch:
-- got --

-- want --
true
false
```

## tests/vm/valid/string_index.mochi

```
golden mismatch:
-- got --

-- want --
o
```

## tests/vm/valid/string_prefix_slice.mochi

```
golden mismatch:
-- got --

-- want --
true
false
```

## tests/vm/valid/substring_builtin.mochi

```
golden mismatch:
-- got --

-- want --
och
```

## tests/vm/valid/sum_builtin.mochi

```
golden mismatch:
-- got --

-- want --
6
```

## tests/vm/valid/tail_recursion.mochi

```
golden mismatch:
-- got --

-- want --
55
```

## tests/vm/valid/test_block.mochi

```
roundtrip parse error: parse error: 1:5: unexpected token "expect" (expected <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/tree_sum.mochi

```
roundtrip parse error: parse error: 3:1: unexpected token "}" (expected "{" Statement* "}")
```

## tests/vm/valid/two-sum.mochi

```
golden mismatch:
-- got --

-- want --
0
1
```

## tests/vm/valid/typed_let.mochi

```
golden mismatch:
-- got --

-- want --
<nil>
```

## tests/vm/valid/typed_var.mochi

```
golden mismatch:
-- got --

-- want --
<nil>
```

## tests/vm/valid/unary_neg.mochi

```
golden mismatch:
-- got --

-- want --
-3
3
```

## tests/vm/valid/update_stmt.mochi

```
roundtrip parse error: parse error: 7:5: unexpected token "expect" (expected <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/user_type_literal.mochi

```
golden mismatch:
-- got --

-- want --
Bob
```

## tests/vm/valid/values_builtin.mochi

```
golden mismatch:
-- got --

-- want --
1 2 3
```

## tests/vm/valid/var_assignment.mochi

```
golden mismatch:
-- got --

-- want --
2
```

## tests/vm/valid/while_loop.mochi

```
golden mismatch:
-- got --

-- want --
0
1
2
```

