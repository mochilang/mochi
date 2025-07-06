# Errors

- append_builtin: type error: error[T003]: unknown function: listOf
  --> :2:11

help:
  Ensure the function is defined before it's called.
- avg_builtin: type error: error[T003]: unknown function: listOf
  --> :2:9

help:
  Ensure the function is defined before it's called.
- basic_compare: ok
- binary_precedence: ok
- bool_chain: ok
- break_continue: parse error: parse error: 4:13: unexpected token ")" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- cast_string_to_int: type error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- cast_struct: parse error: parse error: 5:36: unexpected token "to" (expected ")")
- closure: parse error: parse error: 2:28: unexpected token "{" (expected "<" TypeRef ("," TypeRef)* ">")
- count_builtin: type error: error[T003]: unknown function: listOf
  --> :2:9

help:
  Ensure the function is defined before it's called.
- cross_join: parse error: parse error: 2:44: unexpected token "to" (expected ")")
- cross_join_filter: parse error: parse error: 6:45: unexpected token "," (expected PostfixExpr)
- cross_join_triple: parse error: parse error: 7:45: unexpected token "," (expected PostfixExpr)
- dataset_sort_take_limit: parse error: parse error: 2:45: unexpected token "to" (expected ")")
- dataset_where_filter: parse error: parse error: 2:43: unexpected token "to" (expected ")")
- exists_builtin: parse error: parse error: 4:3: unexpected token "var" (expected "}")
- for_list_collection: type error: error[T003]: unknown function: listOf
  --> :2:12

help:
  Ensure the function is defined before it's called.
- for_loop: ok
- for_map_collection: parse error: parse error: 2:28: unexpected token "to" (expected ")")
- fun_call: ok
- fun_expr_in_let: parse error: parse error: 2:34: unexpected token "{" (expected "<" TypeRef ("," TypeRef)* ">")
- fun_three_args: ok
- group_by: parse error: parse error: 7:32: lexer: invalid input text "?>\n  res = res.m..."
- group_by_conditional_sum: parse error: parse error: 6:20: lexer: invalid input text "?>) -> Boolean) ..."
- group_by_having: parse error: parse error: 30:20: lexer: invalid input text "?): String = whe..."
- group_by_join: parse error: parse error: 6:20: lexer: invalid input text "?>) -> Boolean) ..."
- group_by_left_join: parse error: parse error: 6:20: lexer: invalid input text "?>) -> Boolean) ..."
- group_by_multi_join: parse error: parse error: 6:20: lexer: invalid input text "?>) -> Boolean) ..."
- group_by_multi_join_sort: parse error: parse error: 6:20: lexer: invalid input text "?>) -> Boolean) ..."
- group_by_sort: parse error: parse error: 6:20: lexer: invalid input text "?>) -> Boolean) ..."
- group_items_iteration: parse error: parse error: 7:23: lexer: invalid input text "?>()\n  for g in ..."
- if_else: ok
- if_then_else: parse error: parse error: 3:27: unexpected token "yes" (expected (("{" Expr "}") | ("then" Expr)) (("else" IfExpr) | ("else" (("{" Expr "}") | Expr)))?)
- if_then_else_nested: parse error: parse error: 3:27: unexpected token "big" (expected (("{" Expr "}") | ("then" Expr)) (("else" IfExpr) | ("else" (("{" Expr "}") | Expr)))?)
- in_operator: type error: error[T003]: unknown function: listOf
  --> :2:12

help:
  Ensure the function is defined before it's called.
- in_operator_extended: parse error: parse error: 5:25: unexpected token ">" (expected PostfixExpr)
- inner_join: parse error: parse error: 3:20: lexer: invalid input text "?>) -> Boolean) ..."
- join_multi: parse error: parse error: 3:20: lexer: invalid input text "?>) -> Boolean) ..."
- json_builtin: parse error: parse error: 6:20: lexer: invalid input text "?): String = whe..."
- left_join: parse error: parse error: 3:20: lexer: invalid input text "?>) -> Boolean) ..."
- left_join_multi: parse error: parse error: 3:20: lexer: invalid input text "?>) -> Boolean) ..."
- len_builtin: type error: error[T003]: unknown function: listOf
  --> :2:9

help:
  Ensure the function is defined before it's called.
- len_map: parse error: parse error: 2:26: unexpected token "to" (expected ")")
- len_string: ok
- let_and_print: ok
- list_assign: type error: error[T003]: unknown function: listOf
  --> :2:14

help:
  Ensure the function is defined before it's called.
- list_index: type error: error[T003]: unknown function: listOf
  --> :2:12

help:
  Ensure the function is defined before it's called.
- list_nested_assign: type error: error[T003]: unknown function: listOf
  --> :2:16

help:
  Ensure the function is defined before it's called.
- list_set_ops: parse error: parse error: 8:30: unexpected token ")" (expected PostfixExpr)
- load_yaml: parse error: parse error: 28:47: lexer: invalid input text "?): List<Map<Str..."
- map_assign: parse error: parse error: 2:37: unexpected token "to" (expected ")")
- map_in_operator: parse error: parse error: 2:26: unexpected token "to" (expected ")")
- map_index: parse error: parse error: 2:28: unexpected token "to" (expected ")")
- map_int_key: parse error: parse error: 2:26: unexpected token "to" (expected ")")
- map_literal_dynamic: parse error: parse error: 4:28: unexpected token "to" (expected ")")
- map_membership: parse error: parse error: 2:28: unexpected token "to" (expected ")")
- map_nested_assign: parse error: parse error: 2:35: unexpected token "to" (expected ")")
- match_expr: parse error: parse error: 6:12: unexpected token ">" (expected PostfixExpr)
- match_full: parse error: parse error: 5:12: unexpected token ">" (expected PostfixExpr)
- math_ops: ok
- membership: type error: error[T003]: unknown function: listOf
  --> :2:14

help:
  Ensure the function is defined before it's called.
- min_max_builtin: parse error: parse error: 7:21: lexer: invalid input text "?>? = null\n  whe..."
- nested_function: ok
- order_by_map: parse error: parse error: 2:38: unexpected token "to" (expected ")")
- outer_join: parse error: parse error: 3:20: lexer: invalid input text "?>) -> Boolean) ..."
- partial_application: ok
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type error: error[T002]: undefined variable: k
  --> :2:15

help:
  Check if the variable was declared in this scope.
- query_sum_select: parse error: parse error: 12:21: lexer: invalid input text "?>? = null\n  whe..."
- record_assign: parse error: parse error: 5:5: unexpected token "=" (expected ":" TypeRef)
- right_join: parse error: parse error: 3:20: lexer: invalid input text "?>) -> Boolean) ..."
- save_jsonl_stdout: parse error: parse error: 5:57: lexer: invalid input text "?):  {\n  let row..."
- short_circuit: ok
- slice: parse error: parse error: 12:12: unexpected token "=" (expected PostfixExpr)
- sort_stable: parse error: parse error: 2:39: unexpected token "to" (expected ")")
- str_builtin: type error: error[T004]: `` is not callable
  --> :2:21

help:
  Use a function or closure in this position.
- string_compare: ok
- string_concat: ok
- string_contains: type error: error[T015]: index must be an integer
  --> :3:10

help:
  Use an `int` value for indexing (e.g., `list[0]`).
- string_in_operator: ok
- string_index: parse error: parse error: 9:10: unexpected token "=" (expected PostfixExpr)
- string_prefix_slice: parse error: parse error: 14:12: unexpected token "=" (expected PostfixExpr)
- substring_builtin: ok
- sum_builtin: parse error: parse error: 5:21: lexer: invalid input text "?>? = null\n  whe..."
- tail_recursion: ok
- test_block: type error: error[T003]: unknown function: check
  --> :3:3

help:
  Ensure the function is defined before it's called.
- tree_sum: parse error: parse error: 8:15: unexpected token ">" (expected PostfixExpr)
- two-sum: parse error: parse error: 5:27: unexpected token ")" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- typed_let: type error: error[T002]: undefined variable: y
  --> :3:9

help:
  Check if the variable was declared in this scope.
- typed_var: ok
- unary_neg: ok
- update_stmt: parse error: parse error: 7:33: unexpected token "=" (expected ")")
- user_type_literal: parse error: parse error: 10:25: unexpected token "=" (expected ")")
- values_builtin: parse error: parse error: 2:28: unexpected token "to" (expected ")")
- var_assignment: ok
- while_loop: ok
