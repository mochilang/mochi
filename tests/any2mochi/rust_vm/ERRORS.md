# Errors

- append_builtin: parse error: parse error: 3:33: unexpected token "|" (expected ")")
- avg_builtin: parse error: parse error: 2:14: lexer: invalid input text "&vec![1, 2, 3]))..."
- basic_compare: ok
- binary_precedence: ok
- bool_chain: ok
- break_continue: type error: error[T002]: undefined variable: vec
  --> :2:17

help:
  Check if the variable was declared in this scope.
- cast_string_to_int: ok
- cast_struct: parse error: parse error: 5:18: unexpected token ":" (expected "(" (Expr ("," Expr)*)? ")")
- closure: parse error: parse error: 1:28: unexpected token "Fn" (expected "{" Statement* "}")
- count_builtin: parse error: parse error: 2:16: lexer: invalid input text "&vec![1, 2, 3]))..."
- cross_join: parse error: parse error: 5:30: lexer: invalid input text ";\n    for o in o..."
- cross_join_filter: parse error: parse error: 5:30: lexer: invalid input text ";\n    for n in n..."
- cross_join_triple: parse error: parse error: 6:30: lexer: invalid input text ";\n    for n in n..."
- dataset_sort_take_limit: parse error: parse error: 4:32: lexer: invalid input text ";\n    for p in p..."
- dataset_where_filter: parse error: parse error: 4:30: lexer: invalid input text ";\n    for person..."
- exists_builtin: parse error: parse error: 4:30: lexer: invalid input text ";\n    for x in d..."
- for_list_collection: parse error: parse error: 2:15: unexpected token "!" (expected "{" Statement* "}")
- for_loop: ok
- for_map_collection: parse error: parse error: 2:14: unexpected token ":" (expected "}")
- fun_call: ok
- fun_expr_in_let: parse error: parse error: 2:19: unexpected token ":" (expected "}")
- fun_three_args: ok
- group_by: parse error: parse error: 4:5: lexer: invalid input text "#[derive(Clone, ..."
- group_by_conditional_sum: parse error: parse error: 4:32: lexer: invalid input text ";\n    for i in i..."
- group_by_having: parse error: parse error: 4:5: lexer: invalid input text "#[derive(Clone, ..."
- group_by_join: parse error: parse error: 5:5: lexer: invalid input text "#[derive(Clone, ..."
- group_by_left_join: parse error: parse error: 5:5: lexer: invalid input text "#[derive(Clone, ..."
- group_by_multi_join: parse error: parse error: 6:30: lexer: invalid input text ";\n    for ps in ..."
- group_by_multi_join_sort: parse error: parse error: 9:32: lexer: invalid input text ";\n    for c in c..."
- group_by_sort: parse error: parse error: 4:32: lexer: invalid input text ";\n    for i in i..."
- group_items_iteration: parse error: parse error: 4:5: lexer: invalid input text "#[derive(Clone, ..."
- if_else: parse error: parse error: 5:3: unexpected token "else" (expected "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- if_then_else: ok
- if_then_else_nested: ok
- in_operator: parse error: parse error: 3:21: lexer: invalid input text "&2))\n  print(!(x..."
- in_operator_extended: parse error: parse error: 4:30: lexer: invalid input text ";\n    for x in x..."
- inner_join: parse error: parse error: 5:30: lexer: invalid input text ";\n    for o in o..."
- join_multi: parse error: parse error: 6:30: lexer: invalid input text ";\n    for o in o..."
- json_builtin: parse error: parse error: 2:14: unexpected token ":" (expected "}")
- left_join: parse error: parse error: 5:30: lexer: invalid input text ";\n    for o in o..."
- left_join_multi: parse error: parse error: 6:30: lexer: invalid input text ";\n    for o in o..."
- len_builtin: parse error: parse error: 2:12: unexpected token "!" (expected ")")
- len_map: parse error: parse error: 2:12: unexpected token ":" (expected ")")
- len_string: type error: error[T004]: `` is not callable
  --> :2:20

help:
  Use a function or closure in this position.
- let_and_print: ok
- list_assign: type error: error[T002]: undefined variable: vec
  --> :2:14

help:
  Check if the variable was declared in this scope.
- list_index: type error: error[T002]: undefined variable: vec
  --> :2:12

help:
  Check if the variable was declared in this scope.
- list_nested_assign: parse error: parse error: 2:24: unexpected token "!" (expected "]")
- list_set_ops: parse error: parse error: 2:16: lexer: invalid input text "&vec![1, 2], &ve..."
- load_yaml: parse error: parse error: 9:30: lexer: invalid input text ";\n    for p in p..."
- map_assign: parse error: parse error: 2:19: unexpected token ":" (expected "}")
- map_in_operator: parse error: parse error: 3:24: lexer: invalid input text "&1))\n  print(m.c..."
- map_index: parse error: parse error: 2:14: unexpected token ":" (expected "}")
- map_int_key: parse error: parse error: 2:14: unexpected token ":" (expected "}")
- map_literal_dynamic: parse error: parse error: 4:14: unexpected token ":" (expected "}")
- map_membership: parse error: parse error: 3:24: lexer: invalid input text "&\"a\"))\n  print(m..."
- map_nested_assign: parse error: parse error: 2:17: unexpected token ":" (expected "}")
- match_expr: parse error: parse error: 3:16: unexpected token "||" (expected PostfixExpr)
- match_full: parse error: parse error: 2:11: unexpected token "||" (expected PostfixExpr)
- math_ops: ok
- membership: parse error: parse error: 3:23: lexer: invalid input text "&2))\n  print(num..."
- min_max_builtin: type error: error[T002]: undefined variable: vec
  --> :2:14

help:
  Check if the variable was declared in this scope.
- nested_function: type error: error[T003]: unknown function: inner
  --> :2:10

help:
  Ensure the function is defined before it's called.
- order_by_map: parse error: parse error: 4:32: lexer: invalid input text ";\n    for x in d..."
- outer_join: parse error: parse error: 5:30: lexer: invalid input text ";\n    for o in o..."
- partial_application: ok
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type error: error[T002]: undefined variable: k
  --> :2:14

help:
  Check if the variable was declared in this scope.
- query_sum_select: parse error: parse error: 4:30: lexer: invalid input text ";\n    for n in n..."
- record_assign: type error: error[T009]: cannot assign int to `c` (expected Counter)
  --> :5:3

help:
  Make sure the assigned value is compatible with `c`.
- right_join: parse error: parse error: 5:30: lexer: invalid input text ";\n    for c in c..."
- save_jsonl_stdout: parse error: parse error: 2:24: unexpected token ":" (expected "]")
- short_circuit: ok
- slice: parse error: parse error: 2:12: unexpected token "!" (expected ")")
- sort_stable: parse error: parse error: 4:32: lexer: invalid input text ";\n    for i in i..."
- str_builtin: parse error: parse error: 2:15: unexpected token "!" (expected ")")
- string_compare: ok
- string_concat: parse error: parse error: 2:15: unexpected token "!" (expected ")")
- string_contains: type error: error[T004]: `` is not callable
  --> :2:28

help:
  Use a function or closure in this position.
- string_in_operator: type error: error[T004]: `` is not callable
  --> :2:28

help:
  Use a function or closure in this position.
- string_index: parse error: parse error: 3:19: lexer: invalid input text "&s; let mut idx ..."
- string_prefix_slice: parse error: parse error: 14:11: unexpected token "=" (expected PostfixExpr)
- substring_builtin: ok
- sum_builtin: parse error: parse error: 2:14: lexer: invalid input text "&vec![1, 2, 3]))..."
- tail_recursion: ok
- test_block: parse error: parse error: 9:5: unexpected token "expect" (expected <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- tree_sum: parse error: parse error: 5:11: unexpected token "||" (expected PostfixExpr)
- two-sum: parse error: parse error: 1:18: unexpected token "[" (expected TypeRef)
- typed_let: parse error: parse error: 2:23: unexpected token ":" (expected "}")
- typed_var: parse error: parse error: 2:23: unexpected token ":" (expected "}")
- unary_neg: ok
- update_stmt: compile error: unsupported statement at 14:1
- user_type_literal: type error: error[T004]: `` is not callable
  --> :10:42

help:
  Use a function or closure in this position.
- values_builtin: parse error: parse error: 2:14: unexpected token ":" (expected "}")
- var_assignment: ok
- while_loop: ok
