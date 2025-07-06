# Dart roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
output mismatch
-- got --

-- want --
1 2 3
```

## tests/vm/valid/avg_builtin.mochi

```
parse roundtrip error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- got --

-- want --
7
true
true
```

## tests/vm/valid/binary_precedence.mochi

```
output mismatch
-- got --

-- want --
7
9
7
8
```

## tests/vm/valid/bool_chain.mochi

```
output mismatch
-- got --

-- want --
true
false
false
```

## tests/vm/valid/break_continue.mochi

```
parse roundtrip error: parse error: 10:57: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/cast_string_to_int.mochi

```
type roundtrip error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
```

## tests/vm/valid/cast_struct.mochi

```
parse roundtrip error: parse error: 7:18: lexer: invalid input text "'Todo'] = (m) =>..."
```

## tests/vm/valid/closure.mochi

```
parse roundtrip error: parse error: 3:14: unexpected token "=>" (expected "}")
```

## tests/vm/valid/count_builtin.mochi

```
parse roundtrip error: parse error: 8:41: lexer: invalid input text "; if (items is L..."
```

## tests/vm/valid/cross_join.mochi

```
parse roundtrip error: parse error: 9:245: lexer: invalid input text "' '))\n  }\n}\n"
```

## tests/vm/valid/cross_join_filter.mochi

```
parse roundtrip error: parse error: 10:49: lexer: invalid input text "' '))\n  }\n}\n"
```

## tests/vm/valid/cross_join_triple.mochi

```
parse roundtrip error: parse error: 10:65: lexer: invalid input text "' '))\n  }\n}\n"
```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
parse roundtrip error: parse error: 14:86: lexer: invalid input text "' '))\n  }\n}\n"
```

## tests/vm/valid/dataset_where_filter.mochi

```
parse roundtrip error: parse error: 9:94: lexer: invalid input text "? \" (senior)\" : ..."
```

## tests/vm/valid/exists_builtin.mochi

```
parse roundtrip error: parse error: 13:11: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/for_list_collection.mochi

```
parse roundtrip error: parse error: 2:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
```

## tests/vm/valid/for_loop.mochi

```
output mismatch
-- got --

-- want --
1
2
3
```

## tests/vm/valid/for_map_collection.mochi

```
parse roundtrip error: parse error: 4:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
```

## tests/vm/valid/fun_call.mochi

```
output mismatch
-- got --

-- want --
5
```

## tests/vm/valid/fun_expr_in_let.mochi

```
parse roundtrip error: parse error: 1:18: unexpected token "=>"
```

## tests/vm/valid/fun_three_args.mochi

```
output mismatch
-- got --

-- want --
6
```

## tests/vm/valid/group_by.mochi

```
parse roundtrip error: parse error: 16:128: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
parse roundtrip error: parse error: 27:26: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/group_by_having.mochi

```
parse roundtrip error: parse error: 18:41: lexer: invalid input text "; if (items is L..."
```

## tests/vm/valid/group_by_join.mochi

```
parse roundtrip error: parse error: 18:78: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/group_by_left_join.mochi

```
parse roundtrip error: parse error: 25:78: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/group_by_multi_join.mochi

```
parse roundtrip error: parse error: 24:19: lexer: invalid input text "; i < a.length; ..."
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
parse roundtrip error: parse error: 34:19: lexer: invalid input text "; i < a.length; ..."
```

## tests/vm/valid/group_by_sort.mochi

```
parse roundtrip error: parse error: 29:26: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/group_items_iteration.mochi

```
parse roundtrip error: parse error: 2:48: unexpected token "=>" (expected ")")
```

## tests/vm/valid/if_else.mochi

```
output mismatch
-- got --

-- want --
big
```

## tests/vm/valid/if_then_else.mochi

```
parse roundtrip error: parse error: 2:29: lexer: invalid input text "? \"yes\" : \"no\")\n..."
```

## tests/vm/valid/if_then_else_nested.mochi

```
parse roundtrip error: parse error: 2:29: lexer: invalid input text "? \"big\" : ((x > ..."
```

## tests/vm/valid/in_operator.mochi

```
type roundtrip error: error[T027]: [int] is not a struct
  --> :3:10

help:
  Field access is only valid on struct types.
```

## tests/vm/valid/in_operator_extended.mochi

```
parse roundtrip error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/inner_join.mochi

```
parse roundtrip error: parse error: 9:155: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/join_multi.mochi

```
parse roundtrip error: parse error: 10:80: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/json_builtin.mochi

```
type roundtrip error: error[T003]: unknown function: _json
  --> :3:3

help:
  Ensure the function is defined before it's called.
```

## tests/vm/valid/left_join.mochi

```
parse roundtrip error: parse error: 10:157: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/left_join_multi.mochi

```
parse roundtrip error: parse error: 11:77: lexer: invalid input text "' '))\n  }\n}\nfun ..."
```

## tests/vm/valid/len_builtin.mochi

```
output mismatch
-- got --

-- want --
3
```

## tests/vm/valid/len_map.mochi

```
output mismatch
-- got --

-- want --
2
```

## tests/vm/valid/len_string.mochi

```
output mismatch
-- got --

-- want --
5
```

## tests/vm/valid/let_and_print.mochi

```
output mismatch
-- got --

-- want --
30
```

## tests/vm/valid/list_assign.mochi

```
type roundtrip error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_index.mochi

```
output mismatch
-- got --

-- want --
20
```

## tests/vm/valid/list_nested_assign.mochi

```
type roundtrip error: error[T024]: cannot assign to `matrix` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/list_set_ops.mochi

```
parse roundtrip error: parse error: 8:13: unexpected token "<" (expected PostfixExpr)
```

## tests/vm/valid/load_yaml.mochi

```
parse roundtrip error: parse error: 13:18: lexer: invalid input text "'Person'] = (m) ..."
```

## tests/vm/valid/map_assign.mochi

```
type roundtrip error: error[T024]: cannot assign to `scores` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/map_in_operator.mochi

```
type roundtrip error: error[T027]: {int: string} is not a struct
  --> :3:10

help:
  Field access is only valid on struct types.
```

## tests/vm/valid/map_index.mochi

```
output mismatch
-- got --

-- want --
2
```

## tests/vm/valid/map_int_key.mochi

```
output mismatch
-- got --

-- want --
a
```

## tests/vm/valid/map_literal_dynamic.mochi

```
parse roundtrip error: parse error: 5:53: lexer: invalid input text "' '))\n}\n"
```

## tests/vm/valid/map_membership.mochi

```
type roundtrip error: error[T004]: `` is not callable
  --> :3:23

help:
  Use a function or closure in this position.
```

## tests/vm/valid/map_nested_assign.mochi

```
type roundtrip error: error[T024]: cannot assign to `data` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/match_expr.mochi

```
parse roundtrip error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/match_full.mochi

```
parse roundtrip error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/math_ops.mochi

```
parse roundtrip error: parse error: 3:12: lexer: invalid input text "~/ 2))\n  print((..."
```

## tests/vm/valid/membership.mochi

```
type roundtrip error: error[T027]: [int] is not a struct
  --> :3:10

help:
  Field access is only valid on struct types.
```

## tests/vm/valid/min_max_builtin.mochi

```
parse roundtrip error: parse error: 9:26: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/nested_function.mochi

```
type roundtrip error: error[T002]: undefined variable: x
  --> :4:11

help:
  Check if the variable was declared in this scope.
```

## tests/vm/valid/order_by_map.mochi

```
parse roundtrip error: parse error: 2:40: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/outer_join.mochi

```
parse roundtrip error: parse error: 12:161: lexer: invalid input text "' '))\n        } ..."
```

## tests/vm/valid/partial_application.mochi

```
output mismatch
-- got --

-- want --
8
```

## tests/vm/valid/print_hello.mochi

```
output mismatch
-- got --

-- want --
hello
```

## tests/vm/valid/pure_fold.mochi

```
output mismatch
-- got --

-- want --
9
```

## tests/vm/valid/pure_global_fold.mochi

```
output mismatch
-- got --

-- want --
5
```

## tests/vm/valid/query_sum_select.mochi

```
parse roundtrip error: parse error: 12:26: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/record_assign.mochi

```
parse roundtrip error: parse error: 10:18: lexer: invalid input text "'Counter'] = (m)..."
```

## tests/vm/valid/right_join.mochi

```
parse roundtrip error: parse error: 11:174: lexer: invalid input text "' '))\n      } el..."
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
parse roundtrip error: parse error: 6:21: lexer: invalid input text "?['format'] ?? '..."
```

## tests/vm/valid/short_circuit.mochi

```
output mismatch
-- got --

-- want --
false
true
```

## tests/vm/valid/slice.mochi

```
type roundtrip error: error[T004]: `` is not callable
  --> :2:26

help:
  Use a function or closure in this position.
```

## tests/vm/valid/sort_stable.mochi

```
parse roundtrip error: parse error: 2:22: unexpected token ")" (expected PostfixExpr)
```

## tests/vm/valid/str_builtin.mochi

```
type roundtrip error: error[T004]: `` is not callable
  --> :2:21

help:
  Use a function or closure in this position.
```

## tests/vm/valid/string_compare.mochi

```
type roundtrip error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
```

## tests/vm/valid/string_concat.mochi

```
output mismatch
-- got --

-- want --
hello world
```

## tests/vm/valid/string_contains.mochi

```
output mismatch
-- got --

-- want --
true
false
```

## tests/vm/valid/string_in_operator.mochi

```
output mismatch
-- got --

-- want --
true
false
```

## tests/vm/valid/string_index.mochi

```
parse roundtrip error: parse error: 11:22: lexer: invalid input text "'index out of ra..."
```

## tests/vm/valid/string_prefix_slice.mochi

```
type roundtrip error: error[T027]: string is not a struct
  --> :5:10

help:
  Field access is only valid on struct types.
```

## tests/vm/valid/substring_builtin.mochi

```
output mismatch
-- got --

-- want --
och
```

## tests/vm/valid/sum_builtin.mochi

```
parse roundtrip error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
```

## tests/vm/valid/tail_recursion.mochi

```
output mismatch
-- got --

-- want --
55
```

## tests/vm/valid/test_block.mochi

```
parse roundtrip error: parse error: 8:38: lexer: invalid input text "'expect failed')..."
```

## tests/vm/valid/tree_sum.mochi

```
parse roundtrip error: parse error: 9:29: lexer: invalid input text "; }\n  if (_t is ..."
```

## tests/vm/valid/two-sum.mochi

```
output mismatch
-- got --

-- want --
0
1
```

## tests/vm/valid/typed_let.mochi

```
output mismatch
-- got --

-- want --
<nil>
```

## tests/vm/valid/typed_var.mochi

```
output mismatch
-- got --

-- want --
<nil>
```

## tests/vm/valid/unary_neg.mochi

```
output mismatch
-- got --

-- want --
-3
3
```

## tests/vm/valid/update_stmt.mochi

```
parse roundtrip error: parse error: 14:242: lexer: invalid input text "'expect failed')..."
```

## tests/vm/valid/user_type_literal.mochi

```
parse roundtrip error: parse error: 12:18: lexer: invalid input text "'Person'] = (m) ..."
```

## tests/vm/valid/values_builtin.mochi

```
output mismatch
-- got --

-- want --
1 2 3
```

## tests/vm/valid/var_assignment.mochi

```
type roundtrip error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
```

## tests/vm/valid/while_loop.mochi

```
compile roundtrip error: assignment to undeclared variable: i
```

