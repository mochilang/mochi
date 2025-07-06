# Lua roundtrip VM test failures

## tests/vm/valid/bool_chain.mochi

```
error[T008]: type mismatch: expected void, got bool
  --> :3:10

help:
  Change the value to match the expected type.
```

## tests/vm/valid/break_continue.mochi

```
parse error: 2:6: unexpected token "," (expected "in" Expr (".." Expr)? "{" Statement* "}")
```

## tests/vm/valid/cast_struct.mochi

```
parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/closure.mochi

```
parse error: 2:17: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/cross_join.mochi

```
parse error: 3:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/cross_join_filter.mochi

```
parse error: 3:19: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/cross_join_triple.mochi

```
parse error: 4:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
parse error: 18:16: lexer: invalid input text "; i <= skip; i +..."
```

## tests/vm/valid/dataset_where_filter.mochi

```
parse error: 2:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/exists_builtin.mochi

```
parse error: 2:27: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/for_list_collection.mochi

```
parse error: 1:6: unexpected token "," (expected "in" Expr (".." Expr)? "{" Statement* "}")
```

## tests/vm/valid/for_loop.mochi

```
parse error: 1:10: lexer: invalid input text "; i <= 4 - 1; i ..."
```

## tests/vm/valid/for_map_collection.mochi

```
error[T003]: unknown function: pairs
  --> :2:10

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/fun_call.mochi

```
error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/fun_expr_in_let.mochi

```
parse error: 1:21: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/fun_three_args.mochi

```
error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/group_by.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_having.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_join.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_left_join.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_multi_join.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_by_sort.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/group_items_iteration.mochi

```
parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/if_then_else.mochi

```
parse error: 2:17: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/if_then_else_nested.mochi

```
parse error: 2:17: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/in_operator_extended.mochi

```
parse error: 14:14: lexer: invalid input text "~= null)\nprint(m..."
```

## tests/vm/valid/inner_join.mochi

```
parse error: 3:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/join_multi.mochi

```
parse error: 4:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/json_builtin.mochi

```
error[T003]: unknown function: __json
  --> :2:1

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/left_join.mochi

```
parse error: 3:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/left_join_multi.mochi

```
parse error: 4:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/list_assign.mochi

```
error[T024]: cannot assign to `nums` (immutable)
  --> :2:1

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_nested_assign.mochi

```
error[T024]: cannot assign to `matrix` (immutable)
  --> :2:1

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/load_yaml.mochi

```
parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/map_assign.mochi

```
error[T024]: cannot assign to `scores` (immutable)
  --> :2:1

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/map_in_operator.mochi

```
parse error: 2:12: lexer: invalid input text "~= null)\nprint(m..."
```

## tests/vm/valid/map_membership.mochi

```
parse error: 2:14: lexer: invalid input text "~= null)\nprint(m..."
```

## tests/vm/valid/map_nested_assign.mochi

```
error[T024]: cannot assign to `data` (immutable)
  --> :2:1

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/match_expr.mochi

```
parse error: 2:19: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/match_full.mochi

```
parse error: 2:16: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/min_max_builtin.mochi

```
error[T003]: unknown function: __min
  --> :2:7

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/nested_function.mochi

```
error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/order_by_map.mochi

```
parse error: 2:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/outer_join.mochi

```
parse error: 3:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/partial_application.mochi

```
error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/pure_fold.mochi

```
error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/pure_global_fold.mochi

```
error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/query_sum_select.mochi

```
parse error: 2:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/record_assign.mochi

```
parse error: 3:12: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/right_join.mochi

```
parse error: 3:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
parse error: 2:19: unexpected token "," (expected PostfixExpr)
```

## tests/vm/valid/short_circuit.mochi

```
error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/sort_stable.mochi

```
parse error: 2:20: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/str_builtin.mochi

```
error[T003]: unknown function: tostring
  --> :1:7

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/sum_builtin.mochi

```
error[T003]: unknown function: __sum
  --> :1:7

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/tail_recursion.mochi

```
error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
```

## tests/vm/valid/test_block.mochi

```
error[T003]: unknown function: __run_tests
  --> :9:1

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/tree_sum.mochi

```
parse error: 2:16: unexpected token "{" (expected "=>" Expr)
```

## tests/vm/valid/two-sum.mochi

```
parse error: 3:12: lexer: invalid input text "; i <= n - 1; i ..."
```

## tests/vm/valid/unary_neg.mochi

```
parse error: 2:12: unexpected token "-" (expected PostfixExpr)
```

## tests/vm/valid/update_stmt.mochi

```
parse error: 14:12: lexer: invalid input text "; _i0 <= len(peo..."
```

## tests/vm/valid/user_type_literal.mochi

```
parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
```

## tests/vm/valid/values_builtin.mochi

```
error[T003]: unknown function: __values
  --> :2:7

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/var_assignment.mochi

```
error[T024]: cannot assign to `x` (immutable)
  --> :2:1

help:
  Use `var` to declare mutable variables.
```

