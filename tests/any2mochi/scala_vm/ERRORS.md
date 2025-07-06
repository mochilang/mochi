# Errors

- append_builtin.mochi: ok
- avg_builtin.mochi: roundtrip type error: error[T002]: undefined variable: scala
  --> :1:8

help:
  Check if the variable was declared in this scope.
- basic_compare.mochi: ok
- binary_precedence.mochi: ok
- bool_chain.mochi: roundtrip type error: error[T008]: type mismatch: expected void, got bool
  --> :3:10

help:
  Change the value to match the expected type.
- break_continue.mochi: roundtrip parse error: parse error: 13:1: unexpected token "}"
- cast_string_to_int.mochi: roundtrip type error: error[T002]: undefined variable: _cast
  --> :1:7

help:
  Check if the variable was declared in this scope.
- cast_struct.mochi: roundtrip parse error: parse error: 1:68: unexpected token ">" (expected PostfixExpr)
- closure.mochi: roundtrip parse error: parse error: 2:3: unexpected token ">" (expected "}")
- count_builtin.mochi: roundtrip type error: error[T002]: undefined variable: scala
  --> :1:11

help:
  Check if the variable was declared in this scope.
- cross_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- cross_join_filter.mochi: roundtrip parse error: parse error: 3:27: unexpected token "]" (expected ">")
- cross_join_triple.mochi: roundtrip parse error: parse error: 4:28: unexpected token "]" (expected ">")
- dataset_sort_take_limit.mochi: roundtrip parse error: parse error: 1:30: unexpected token "]" (expected ">")
- dataset_where_filter.mochi: roundtrip parse error: parse error: 1:28: unexpected token "]" (expected ">")
- exists_builtin.mochi: roundtrip parse error: parse error: 2:28: unexpected token ")" (expected PostfixExpr)
- for_list_collection.mochi: roundtrip type error: error[T002]: undefined variable: scala
  --> :1:11

help:
  Check if the variable was declared in this scope.
- for_loop.mochi: ok
- for_map_collection.mochi: roundtrip parse error: parse error: 1:43: unexpected token ">" (expected PostfixExpr)
- fun_call.mochi: roundtrip type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- fun_expr_in_let.mochi: roundtrip parse error: parse error: 1:13: unexpected token "(" (expected TypeRef)
- fun_three_args.mochi: roundtrip type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- group_by.mochi: roundtrip parse error: parse error: 1:28: unexpected token "]" (expected ">")
- group_by_conditional_sum.mochi: roundtrip parse error: parse error: 1:27: unexpected token "]" (expected ">")
- group_by_having.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- group_by_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- group_by_left_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- group_by_multi_join.mochi: roundtrip parse error: parse error: 1:29: unexpected token "]" (expected ">")
- group_by_multi_join_sort.mochi: roundtrip parse error: parse error: 1:28: unexpected token "]" (expected ">")
- group_by_sort.mochi: roundtrip parse error: parse error: 1:27: unexpected token "]" (expected ">")
- group_items_iteration.mochi: roundtrip parse error: parse error: 1:26: unexpected token "]" (expected ">")
- if_else.mochi: roundtrip parse error: parse error: 4:1: unexpected token "else" (expected "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- if_then_else.mochi: roundtrip parse error: parse error: 2:34: unexpected token "yes" (expected (("{" Expr "}") | ("then" Expr)) (("else" IfExpr) | ("else" (("{" Expr "}") | Expr)))?)
- if_then_else_nested.mochi: roundtrip parse error: parse error: 2:34: unexpected token "big" (expected (("{" Expr "}") | ("then" Expr)) (("else" IfExpr) | ("else" (("{" Expr "}") | Expr)))?)
- in_operator.mochi: roundtrip type error: error[T027]: [int] is not a struct
  --> :2:7

help:
  Field access is only valid on struct types.
- in_operator_extended.mochi: roundtrip parse error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
- inner_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- join_multi.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- json_builtin.mochi: roundtrip parse error: parse error: 1:61: unexpected token ">" (expected PostfixExpr)
- left_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- left_join_multi.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- len_builtin.mochi: roundtrip type error: error[T002]: undefined variable: scala
  --> :1:11

help:
  Check if the variable was declared in this scope.
- len_map.mochi: roundtrip parse error: parse error: 1:45: unexpected token ">" (expected PostfixExpr)
- len_string.mochi: ok
- let_and_print.mochi: ok
- list_assign.mochi: ok
- list_index.mochi: ok
- list_nested_assign.mochi: roundtrip type error: error[T002]: undefined variable: scala
  --> :1:15

help:
  Check if the variable was declared in this scope.
- list_set_ops.mochi: roundtrip parse error: parse error: 1:52: unexpected token "+" (expected PostfixExpr)
- load_yaml.mochi: roundtrip parse error: parse error: 1:109: unexpected token ">" (expected PostfixExpr)
- map_assign.mochi: roundtrip parse error: parse error: 1:52: unexpected token ">" (expected PostfixExpr)
- map_in_operator.mochi: roundtrip parse error: parse error: 1:59: unexpected token ">" (expected PostfixExpr)
- map_index.mochi: roundtrip parse error: parse error: 1:61: unexpected token ">" (expected PostfixExpr)
- map_int_key.mochi: roundtrip parse error: parse error: 1:59: unexpected token ">" (expected PostfixExpr)
- map_literal_dynamic.mochi: roundtrip parse error: parse error: 3:43: unexpected token ">" (expected PostfixExpr)
- map_membership.mochi: roundtrip parse error: parse error: 1:61: unexpected token ">" (expected PostfixExpr)
- map_nested_assign.mochi: roundtrip parse error: parse error: 1:50: unexpected token ">" (expected PostfixExpr)
- match_expr.mochi: roundtrip parse error: parse error: 2:11: unexpected token "label" (expected "{" MatchCase* "}")
- match_full.mochi: roundtrip parse error: parse error: 9:11: unexpected token "label" (expected "{" MatchCase* "}")
- math_ops.mochi: ok
- membership.mochi: roundtrip type error: error[T027]: [int] is not a struct
  --> :2:7

help:
  Field access is only valid on struct types.
- min_max_builtin.mochi: ok
- nested_function.mochi: roundtrip parse error: parse error: 2:14: unexpected token ":" (expected ")")
- order_by_map.mochi: roundtrip parse error: parse error: 1:26: unexpected token "]" (expected ">")
- outer_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- partial_application.mochi: roundtrip type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- print_hello.mochi: ok
- pure_fold.mochi: roundtrip type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- pure_global_fold.mochi: roundtrip type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- query_sum_select.mochi: roundtrip parse error: parse error: 2:29: unexpected token ")" (expected PostfixExpr)
- record_assign.mochi: roundtrip parse error: parse error: 5:28: unexpected token "=" (expected ")")
- right_join.mochi: roundtrip parse error: parse error: 1:31: unexpected token "]" (expected ">")
- save_jsonl_stdout.mochi: roundtrip parse error: parse error: 1:28: unexpected token "]" (expected ">")
- short_circuit.mochi: roundtrip type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- slice.mochi: roundtrip parse error: parse error: 10:18: unexpected token "start" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- sort_stable.mochi: roundtrip parse error: parse error: 1:27: unexpected token "]" (expected ">")
- str_builtin.mochi: ok
- string_compare.mochi: ok
- string_concat.mochi: ok
- string_contains.mochi: ok
- string_in_operator.mochi: ok
- string_index.mochi: roundtrip parse error: parse error: 7:16: unexpected token "idx" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- string_prefix_slice.mochi: roundtrip parse error: parse error: 12:18: unexpected token "start" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- substring_builtin.mochi: roundtrip parse error: parse error: 8:18: unexpected token "start" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- sum_builtin.mochi: roundtrip type error: error[T002]: undefined variable: scala
  --> :1:11

help:
  Check if the variable was declared in this scope.
- tail_recursion.mochi: roundtrip type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- test_block.mochi: roundtrip parse error: parse error: 3:13: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- tree_sum.mochi: roundtrip parse error: parse error: 6:10: unexpected token "=>" (expected ":" Expr)
- two-sum.mochi: roundtrip type error: error[T005]: parameter `nums` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- typed_let.mochi: roundtrip type error: error[T002]: undefined variable: y
  --> :1:7

help:
  Check if the variable was declared in this scope.
- typed_var.mochi: ok
- unary_neg.mochi: ok
- update_stmt.mochi: roundtrip parse error: parse error: 2:18: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- user_type_literal.mochi: roundtrip parse error: parse error: 1:29: unexpected token "=" (expected ")")
- values_builtin.mochi: roundtrip parse error: parse error: 1:61: unexpected token ">" (expected PostfixExpr)
- var_assignment.mochi: ok
- while_loop.mochi: ok
