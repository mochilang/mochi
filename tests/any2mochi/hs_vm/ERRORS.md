# Errors

- append_builtin: ok
- avg_builtin: ok
- basic_compare: ok
- binary_precedence: parse error: parse error: 1:17: unexpected token ")"
- bool_chain: parse error: parse error: 1:37: lexer: invalid input text "$)\nprint(1 < 2))..."
- break_continue: parse error: parse error: 2:23: lexer: invalid input text "`mod` 2) == 0) t..."
- cast_string_to_int: ok
- cast_struct: parse error: parse error: 1:34: unexpected token "," (expected ")")
- closure: parse error: parse error: 1:32: lexer: invalid input text "\\x(->, (x, +, n)..."
- count_builtin: type error: error[T003]: unknown function: length
  --> :1:7

help:
  Ensure the function is defined before it's called.
- cross_join: parse error: parse error: 1:37: unexpected token "," (expected ")")
- cross_join_filter: parse error: parse error: 3:96: lexer: invalid input text "`mod` 2) == 0)]\n..."
- cross_join_triple: parse error: parse error: 4:33: unexpected token "," (expected ")")
- dataset_sort_take_limit: parse error: parse error: 1:38: unexpected token "," (expected ")")
- dataset_where_filter: parse error: parse error: 2:285: lexer: invalid input text "\\person -> (from..."
- exists_builtin: parse error: parse error: 1:42: lexer: invalid input text "\\x, ->, (x, ==, ..."
- for_list_collection: ok
- for_loop: ok
- for_map_collection: parse error: parse error: 1:27: unexpected token "," (expected ")")
- fun_call: ok
- fun_expr_in_let: parse error: parse error: 1:15: lexer: invalid input text "\\x(->, (x, *, x)..."
- fun_three_args: ok
- group_by: parse error: parse error: 2:208: lexer: invalid input text "\\person -> fromM..."
- group_by_conditional_sum: parse error: parse error: 2:274: lexer: invalid input text "\\(i), ->, fromMa..."
- group_by_having: parse error: parse error: 2:114: lexer: invalid input text "\\p -> fromMaybe ..."
- group_by_join: parse error: parse error: 3:264: lexer: invalid input text "\\(o, c) -> fromM..."
- group_by_left_join: parse error: parse error: 3:98: lexer: invalid input text "\\r -> fromMaybe ..."
- group_by_multi_join: parse error: parse error: 5:176: lexer: invalid input text "\\x -> fromMaybe ..."
- group_by_multi_join_sort: parse error: parse error: 7:2041: lexer: invalid input text "\\(c, o, l, n), -..."
- group_by_sort: parse error: parse error: 2:317: lexer: invalid input text "\\(i), ->, fromMa..."
- group_items_iteration: parse error: parse error: 1:41: lexer: invalid input text "\\d -> fromMaybe ..."
- if_else: compile error: unsupported statement in main
- if_then_else: ok
- if_then_else_nested: ok
- in_operator: parse error: parse error: 3:16: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- in_operator_extended: parse error: parse error: 2:28: lexer: invalid input text "\\x -> ((x `mod` ..."
- inner_join: parse error: parse error: 1:37: unexpected token "," (expected ")")
- join_multi: parse error: parse error: 1:37: unexpected token "," (expected ")")
- json_builtin: parse error: parse error: 1:27: unexpected token "," (expected ")")
- left_join: parse error: parse error: 1:37: unexpected token "," (expected ")")
- left_join_multi: parse error: parse error: 1:37: unexpected token "," (expected ")")
- len_builtin: type error: error[T003]: unknown function: length
  --> :1:7

help:
  Ensure the function is defined before it's called.
- len_map: parse error: parse error: 1:33: unexpected token "," (expected ")")
- len_string: type error: error[T003]: unknown function: length
  --> :1:7

help:
  Ensure the function is defined before it's called.
- let_and_print: ok
- list_assign: compile error: unsupported statement in main
- list_index: parse error: parse error: 2:12: unexpected token "," (expected PostfixExpr)
- list_nested_assign: compile error: unsupported statement in main
- list_set_ops: parse error: parse error: 2:22: lexer: invalid input text "\\ [2])\nprint(Lis..."
- load_yaml: parse error: parse error: 2:86: lexer: invalid input text "\\p -> (age (p) >..."
- map_assign: compile error: unsupported statement in main
- map_in_operator: parse error: parse error: 1:25: unexpected token "," (expected ")")
- map_index: parse error: parse error: 1:27: unexpected token "," (expected ")")
- map_int_key: parse error: parse error: 1:25: unexpected token "," (expected ")")
- map_literal_dynamic: parse error: parse error: 3:27: unexpected token "," (expected ")")
- map_membership: parse error: parse error: 1:27: unexpected token "," (expected ")")
- map_nested_assign: compile error: unsupported statement in main
- match_expr: ok
- match_full: ok
- math_ops: ok
- membership: type error: error[T003]: unknown function: elem
  --> :2:7

help:
  Ensure the function is defined before it's called.
- min_max_builtin: ok
- nested_function: ok
- order_by_map: parse error: parse error: 1:35: unexpected token "," (expected ")")
- outer_join: parse error: parse error: 1:37: unexpected token "," (expected ")")
- partial_application: ok
- print_hello: ok
- pure_fold: parse error: parse error: 2:16: unexpected token "," (expected ")")
- pure_global_fold: type error: error[T002]: undefined variable: k
  --> :1:29

help:
  Check if the variable was declared in this scope.
- query_sum_select: parse error: parse error: 2:36: lexer: invalid input text "\\n -> (n > 1)) n..."
- record_assign: parse error: parse error: 2:19: unexpected token "," (expected ":" Expr)
- right_join: parse error: parse error: 1:37: unexpected token "," (expected ")")
- save_jsonl_stdout: parse error: parse error: 1:36: unexpected token "," (expected ")")
- short_circuit: parse error: parse error: 1:53: lexer: invalid input text "$) }\nprint(false..."
- slice: ok
- sort_stable: parse error: parse error: 1:32: unexpected token "," (expected ")")
- str_builtin: ok
- string_compare: ok
- string_concat: parse error: parse error: 1:17: unexpected token "+" (expected PostfixExpr)
- string_contains: parse error: parse error: 2:23: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- string_in_operator: type error: error[T003]: unknown function: elem
  --> :2:7

help:
  Ensure the function is defined before it's called.
- string_index: type error: error[T003]: unknown function: _indexString
  --> :2:7

help:
  Ensure the function is defined before it's called.
- string_prefix_slice: parse error: parse error: 6:43: unexpected token "==" (expected PostfixExpr)
- substring_builtin: ok
- sum_builtin: ok
- tail_recursion: parse error: parse error: 1:53: lexer: invalid input text "$) }\nprint(sum_r..."
- test_block: parse error: parse error: 1:5: unexpected token "expect" (expected <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- tree_sum: parse error: parse error: 2:19: unexpected token "," (expected ":" Expr)
- two-sum: parse error: parse error: 3:16: unexpected token "," (expected PostfixExpr)
- typed_let: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- typed_var: parse error: parse error: 1:10: unexpected token ")" (expected PostfixExpr)
- unary_neg: parse error: parse error: 3:1: unexpected token "<EOF>" (expected ")")
- update_stmt: compile error: unsupported statement in main
- user_type_literal: parse error: parse error: 1:23: unexpected token "," (expected ":" Expr)
- values_builtin: parse error: parse error: 1:27: unexpected token "," (expected ")")
- var_assignment: compile error: unsupported statement in main
- while_loop: ok
