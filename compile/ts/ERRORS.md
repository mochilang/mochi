# VM Round-trip via TypeScript

## tests/vm/valid/append_builtin.mochi

```
parse converted error: parse error: 5:1: unexpected token "}" (expected ")")
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
type converted error: error[T024]: cannot assign to `a` (immutable)
  --> :4:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/binary_precedence.mochi

```
parse converted error: parse error: 3:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/bool_chain.mochi

```
parse converted error: parse error: 7:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/break_continue.mochi

```
type converted error: error[T024]: cannot assign to `numbers` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/cast_string_to_int.mochi

```
golden mismatch:
-- got --

-- want --
1995
```

## tests/vm/valid/cast_struct.mochi

```
type converted error: error[T024]: cannot assign to `todo` (immutable)
  --> :6:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/closure.mochi

```
parse converted error: parse error: 1:12: unexpected token "(" (expected TypeRef)
```

## tests/vm/valid/count_builtin.mochi

```
parse converted error: parse error: 7:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/cross_join.mochi

```
parse converted error: parse error: 22:7: lexer: invalid input text "\")\n  }\n}\n"
```

## tests/vm/valid/cross_join_filter.mochi

```
type converted error: error[T024]: cannot assign to `nums` (immutable)
  --> :5:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/cross_join_triple.mochi

```
parse converted error: parse error: 15:5: unexpected token "}" (expected "{" Statement* "}")
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
golden mismatch:
-- got --

-- want --
--- Top products (excluding most expensive) ---
Smartphone costs $ 900
Tablet costs $ 600
Monitor costs $ 300
```

## tests/vm/valid/dataset_where_filter.mochi

```
parse converted error: parse error: 13:24: lexer: invalid input text "? \" (senior)\n  }..."
```

## tests/vm/valid/exists_builtin.mochi

```
parse converted error: parse error: 6:16: unexpected token "where" (expected ")")
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
type converted error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
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
parse converted error: parse error: 1:13: unexpected token "(" (expected TypeRef)
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
golden mismatch:
-- got --

-- want --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333332
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
golden mismatch:
-- got --

-- want --
map[cat:a share:0.6666666666666666] map[cat:b share:1]
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
golden mismatch:
-- got --

-- want --
--- Group Left Join ---
Alice orders: 2
Bob orders: 1
Charlie orders: 0
```

## tests/vm/valid/group_by_multi_join.mochi

```
golden mismatch:
-- got --

-- want --
map[part:100 total:20] map[part:200 total:15]
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
golden mismatch:
-- got --

-- want --
map[c_acctbal:100 c_address:123 St c_comment:Loyal c_custkey:1 c_name:Alice c_phone:123-456 n_name:BRAZIL revenue:900]
```

## tests/vm/valid/group_by_sort.mochi

```
golden mismatch:
-- got --

-- want --
map[cat:b total:7] map[cat:a total:4]
```

## tests/vm/valid/group_items_iteration.mochi

```
golden mismatch:
-- got --

-- want --
map[tag:a total:3] map[tag:b total:3]
```

## tests/vm/valid/if_else.mochi

```
type converted error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/if_then_else.mochi

```
parse converted error: parse error: 5:18: lexer: invalid input text "? \"yes\" : \"no\"\n ..."
```

## tests/vm/valid/if_then_else_nested.mochi

```
parse converted error: parse error: 5:18: lexer: invalid input text "? \"big\" : ((x > ..."
```

## tests/vm/valid/in_operator.mochi

```
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/in_operator_extended.mochi

```
parse converted error: parse error: 8:27: unexpected token ")" (expected "}")
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
golden mismatch:
-- got --

-- want --
--- Left Join ---
Order 100 customer map[id:1 name:Alice] total 250
Order 101 customer <nil> total 80
```

## tests/vm/valid/left_join_multi.mochi

```
golden mismatch:
-- got --

-- want --
--- Left Join Multi ---
100 Alice map[orderId:100 sku:a]
101 Bob <nil>
```

## tests/vm/valid/len_builtin.mochi

```
parse converted error: parse error: 6:14: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/len_map.mochi

```
parse converted error: parse error: 6:1: unexpected token "}" (expected ")")
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
type converted error: error[T024]: cannot assign to `a` (immutable)
  --> :4:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_assign.mochi

```
type converted error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_index.mochi

```
type converted error: error[T024]: cannot assign to `xs` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_nested_assign.mochi

```
type converted error: error[T024]: cannot assign to `matrix` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_set_ops.mochi

```
parse converted error: parse error: 9:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/load_yaml.mochi

```
golden mismatch:
-- got --

-- want --
Alice alice@example.com
Charlie charlie@example.com
```

## tests/vm/valid/map_assign.mochi

```
type converted error: error[T024]: cannot assign to `scores` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/map_in_operator.mochi

```
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/map_index.mochi

```
type converted error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/map_int_key.mochi

```
type converted error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/map_literal_dynamic.mochi

```
type converted error: error[T024]: cannot assign to `x` (immutable)
  --> :5:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/map_membership.mochi

```
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/map_nested_assign.mochi

```
type converted error: error[T024]: cannot assign to `data` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
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
parse converted error: parse error: 4:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/membership.mochi

```
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/min_max_builtin.mochi

```
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/nested_function.mochi

```
parse converted error: parse error: 7:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/order_by_map.mochi

```
golden mismatch:
-- got --

-- want --
map[a:0 b:5] map[a:1 b:1] map[a:1 b:2]
```

## tests/vm/valid/outer_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Outer Join using syntax ---
Order 100 by Alice - $ 250
Order 101 by Bob - $ 125
Order 102 by Alice - $ 300
Order 103 by Unknown - $ 80
Customer Charlie has no orders
Customer Diana has no orders
```

## tests/vm/valid/partial_application.mochi

```
parse converted error: parse error: 8:1: unexpected token "}" (expected ")")
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
parse converted error: parse error: 6:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/pure_global_fold.mochi

```
parse converted error: parse error: 8:1: unexpected token "}" (expected ")")
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
type converted error: error[T009]: cannot assign int to `c` (expected Counter)
  --> :6:3

help:
  Make sure the assigned value is compatible with `c`.
```

## tests/vm/valid/right_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Right Join using syntax ---
Customer Alice has order 100 - $ 250
Customer Bob has order 101 - $ 125
Customer Alice has order 102 - $ 300
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
golden mismatch:
-- got --

-- want --
{"age":30,"name":"Alice"}
{"age":25,"name":"Bob"}
```

## tests/vm/valid/short_circuit.mochi

```
parse converted error: parse error: 7:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/slice.mochi

```
parse converted error: parse error: 7:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/sort_stable.mochi

```
golden mismatch:
-- got --

-- want --
a b c
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
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/string_in_operator.mochi

```
parse converted error: parse error: 5:3: unexpected token "print" (expected ")")
```

## tests/vm/valid/string_index.mochi

```
parse converted error: parse error: 5:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/string_prefix_slice.mochi

```
parse converted error: parse error: 8:3: unexpected token "s2" (expected ")")
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
parse converted error: parse error: 9:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/test_block.mochi

```
golden mismatch:
-- got --

-- want --
ok
```

## tests/vm/valid/tree_sum.mochi

```
parse converted error: parse error: 6:3: unexpected token "left" (expected "}")
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
type converted error: error[T002]: undefined variable: undefined
  --> :3:7

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/typed_var.mochi

```
type converted error: error[T002]: undefined variable: undefined
  --> :3:7

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/unary_neg.mochi

```
parse converted error: parse error: 4:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/update_stmt.mochi

```
golden mismatch:
-- got --

-- want --
ok
```

## tests/vm/valid/user_type_literal.mochi

```
type converted error: error[T024]: cannot assign to `book` (immutable)
  --> :11:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/values_builtin.mochi

```
parse converted error: parse error: 5:1: unexpected token "}" (expected ")")
```

## tests/vm/valid/var_assignment.mochi

```
type converted error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/while_loop.mochi

```
type converted error: error[T024]: cannot assign to `i` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

