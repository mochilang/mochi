# Clojure roundtrip VM test failures

## tests/vm/valid/avg_builtin.mochi

```
parse roundtrip error: parse error: 2:56: unexpected token "+" (expected ")")
```

## tests/vm/valid/binary_precedence.mochi

```
output mismatch
-- got --
7
7
7
7
-- want --
7
9
7
8
```

## tests/vm/valid/bool_chain.mochi

```
parse roundtrip error: parse error: 2:22: unexpected token "," (expected PostfixExpr)
```

## tests/vm/valid/break_continue.mochi

```
parse roundtrip error: parse error: 3:72: unexpected token "break" (expected PostfixExpr)
```

## tests/vm/valid/cast_string_to_int.mochi

```
type roundtrip error: error[T003]: unknown function: int
  --> :2:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/cast_struct.mochi

```
parse roundtrip error: parse error: 2:35: lexer: invalid input text "#, get(m, %), fi..."
```

## tests/vm/valid/closure.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/cross_join.mochi

```
parse roundtrip error: parse error: 2:20: unexpected token ":" (expected "}")
```

## tests/vm/valid/cross_join_filter.mochi

```
parse roundtrip error: parse error: 4:19: unexpected token ">" (expected ")")
```

## tests/vm/valid/cross_join_triple.mochi

```
parse roundtrip error: parse error: 5:20: unexpected token ">" (expected ")")
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
parse roundtrip error: parse error: 2:19: unexpected token ":" (expected "}")
```

## tests/vm/valid/dataset_where_filter.mochi

```
parse roundtrip error: parse error: 2:17: unexpected token ":" (expected "}")
```

## tests/vm/valid/exists_builtin.mochi

```
parse roundtrip error: parse error: 3:25: unexpected token ">" (expected ")")
```

## tests/vm/valid/for_list_collection.mochi

```
parse roundtrip error: parse error: 2:74: unexpected token "break" (expected PostfixExpr)
```

## tests/vm/valid/for_loop.mochi

```
parse roundtrip error: parse error: 2:46: unexpected token "break" (expected PostfixExpr)
```

## tests/vm/valid/for_map_collection.mochi

```
parse roundtrip error: parse error: 3:66: unexpected token "break" (expected PostfixExpr)
```

## tests/vm/valid/fun_call.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/fun_expr_in_let.mochi

```
parse roundtrip error: parse error: 2:28: unexpected token "," (expected ")")
```

## tests/vm/valid/fun_three_args.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/group_by.mochi

```
parse roundtrip error: parse error: 9:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
parse roundtrip error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_having.mochi

```
parse roundtrip error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_join.mochi

```
parse roundtrip error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_left_join.mochi

```
parse roundtrip error: parse error: 3:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_multi_join.mochi

```
parse roundtrip error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
parse roundtrip error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_by_sort.mochi

```
parse roundtrip error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/group_items_iteration.mochi

```
parse roundtrip error: parse error: 3:65: lexer: invalid input text "@groups, ks), sw..."
```

## tests/vm/valid/if_else.mochi

```
output mismatch
-- got --

-- want --
big
```

## tests/vm/valid/in_operator.mochi

```
parse roundtrip error: parse error: 3:14: lexer: invalid input text "#, 2 == %, xs))\n..."
```

## tests/vm/valid/in_operator_extended.mochi

```
parse roundtrip error: parse error: 4:14: lexer: invalid input text "#, 1 == %, ys))\n..."
```

## tests/vm/valid/inner_join.mochi

```
parse roundtrip error: parse error: 2:20: unexpected token ":" (expected "}")
```

## tests/vm/valid/join_multi.mochi

```
parse roundtrip error: parse error: 2:20: unexpected token ":" (expected "}")
```

## tests/vm/valid/json_builtin.mochi

```
parse roundtrip error: parse error: 2:3: unexpected token ">" (expected "}")
```

## tests/vm/valid/left_join.mochi

```
parse roundtrip error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
```

## tests/vm/valid/left_join_multi.mochi

```
parse roundtrip error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
```

## tests/vm/valid/len_map.mochi

```
parse roundtrip error: parse error: 2:19: unexpected token "," (expected ":" Expr)
```

## tests/vm/valid/len_string.mochi

```
type roundtrip error: error[T037]: count() expects list or group, got string
  --> :2:9

help:
  Pass a list or group to count().
```

## tests/vm/valid/list_assign.mochi

```
parse roundtrip error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
```

## tests/vm/valid/list_index.mochi

```
parse roundtrip error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
```

## tests/vm/valid/list_nested_assign.mochi

```
parse roundtrip error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
```

## tests/vm/valid/list_set_ops.mochi

```
type roundtrip error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/load_yaml.mochi

```
parse roundtrip error: parse error: 5:351: lexer: invalid input text "#, clojure_data_..."
```

## tests/vm/valid/map_assign.mochi

```
parse roundtrip error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/map_in_operator.mochi

```
parse roundtrip error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/map_index.mochi

```
parse roundtrip error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/map_int_key.mochi

```
parse roundtrip error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/map_literal_dynamic.mochi

```
parse roundtrip error: parse error: 8:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/map_membership.mochi

```
parse roundtrip error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/map_nested_assign.mochi

```
parse roundtrip error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/match_expr.mochi

```
parse roundtrip error: parse error: 3:78: unexpected token "else" (expected PostfixExpr)
```

## tests/vm/valid/match_full.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/membership.mochi

```
parse roundtrip error: parse error: 3:14: lexer: invalid input text "#, 2 == %, nums)..."
```

## tests/vm/valid/min_max_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: apply
  --> :3:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/nested_function.mochi

```
parse roundtrip error: parse error: 2:28: unexpected token "," (expected ")")
```

## tests/vm/valid/order_by_map.mochi

```
parse roundtrip error: parse error: 2:15: unexpected token ":" (expected "}")
```

## tests/vm/valid/outer_join.mochi

```
parse roundtrip error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
```

## tests/vm/valid/partial_application.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/pure_fold.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/pure_global_fold.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/query_sum_select.mochi

```
parse roundtrip error: parse error: 2:23: unexpected token "+" (expected ")")
```

## tests/vm/valid/record_assign.mochi

```
parse roundtrip error: parse error: 2:4: unexpected token ":" (expected "}")
```

## tests/vm/valid/right_join.mochi

```
parse roundtrip error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
parse roundtrip error: parse error: 2:187: lexer: invalid input text "#, str(get(r, %,..."
```

## tests/vm/valid/short_circuit.mochi

```
parse roundtrip error: parse error: 2:22: unexpected token "," (expected PostfixExpr)
```

## tests/vm/valid/slice.mochi

```
type roundtrip error: error[T003]: unknown function: subvec
  --> :2:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/sort_stable.mochi

```
parse roundtrip error: parse error: 2:16: unexpected token ":" (expected "}")
```

## tests/vm/valid/string_compare.mochi

```
type roundtrip error: error[T003]: unknown function: compare
  --> :2:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/string_concat.mochi

```
type roundtrip error: error[T039]: function str expects 1 arguments, got 2
  --> :2:9

help:
  Pass exactly 1 arguments to `str`.
```

## tests/vm/valid/string_contains.mochi

```
type roundtrip error: error[T003]: unknown function: clojure_string_includes_p
  --> :3:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/string_in_operator.mochi

```
type roundtrip error: error[T003]: unknown function: clojure_string_includes_p
  --> :3:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/string_index.mochi

```
parse roundtrip error: parse error: 2:43: unexpected token "}" (expected PostfixExpr)
```

## tests/vm/valid/string_prefix_slice.mochi

```
type roundtrip error: error[T003]: unknown function: subs
  --> :4:9

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/sum_builtin.mochi

```
parse roundtrip error: parse error: 2:16: unexpected token "+" (expected ")")
```

## tests/vm/valid/tail_recursion.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "," (expected ")")
```

## tests/vm/valid/test_block.mochi

```
type roundtrip error: error[T003]: unknown function: assert
  --> :3:3

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/tree_sum.mochi

```
parse roundtrip error: parse error: 2:4: unexpected token ":" (expected "}")
```

## tests/vm/valid/two-sum.mochi

```
parse roundtrip error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
```

## tests/vm/valid/typed_var.mochi

```
type roundtrip error: error[T002]: undefined variable: nil
  --> :2:11

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/update_stmt.mochi

```
parse roundtrip error: parse error: 2:35: lexer: invalid input text "#, get(m, %), fi..."
```

## tests/vm/valid/user_type_literal.mochi

```
parse roundtrip error: parse error: 2:4: unexpected token ":" (expected "}")
```

## tests/vm/valid/values_builtin.mochi

```
parse roundtrip error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
```

## tests/vm/valid/while_loop.mochi

```
parse roundtrip error: parse error: 3:8: unexpected token "," (expected ")")
```

