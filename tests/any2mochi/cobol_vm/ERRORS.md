# Errors

- append_builtin: append_builtin: parse2 error: parse error: 2:8: unexpected token "=" (expected "}")
- avg_builtin: avg_builtin: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- basic_compare: basic_compare: parse2 error: parse error: 5:16: unexpected token "=" (expected "}")
- binary_precedence: ok
- bool_chain: bool_chain: type2 error: error[T020]: operator `*` cannot be used on types bool and bool
  --> :2:22

help:
  Choose an operator that supports these operand types.
- break_continue: break_continue: parse2 error: parse error: 2:14: unexpected token "=" (expected "}")
- cast_string_to_int: ok
- cast_struct: cast_struct: type2 error: error[T002]: undefined variable: TODO_TITLE
  --> :3:9

help:
  Check if the variable was declared in this scope.
- closure: closure: type2 error: error[T003]: unknown function: fmakeadder
  --> :3:3

help:
  Ensure the function is defined before it's called.
- count_builtin: ok
- cross_join: cross_join: convert error: unsupported feature at line 42:  unsupported for-in loop
  41 |     DISPLAY "--- Cross Join: All order-customer pairs ---"
  42 |     *> unsupported for-in loop
  43 |     STOP RUN.
- cross_join_filter: cross_join_filter: convert error: unsupported feature at line 43:  unsupported for-in loop
  42 |     DISPLAY "--- Even pairs ---"
  43 |     *> unsupported for-in loop
  44 |     STOP RUN.
- cross_join_triple: cross_join_triple: convert error: unsupported feature at line 49:  unsupported for-in loop
  48 |     DISPLAY "--- Cross Join of three lists ---"
  49 |     *> unsupported for-in loop
  50 |     STOP RUN.
- dataset_sort_take_limit: dataset_sort_take_limit: convert error: unsupported feature at line 36:  unsupported for-in loop
  35 |     DISPLAY "--- Top products (excluding most expensive) ---"
  36 |     *> unsupported for-in loop
  37 |     STOP RUN.
- dataset_where_filter: dataset_where_filter: convert error: unsupported feature at line 35:  unsupported for-in loop
  34 |     DISPLAY "--- Adults ---"
  35 |     *> unsupported for-in loop
  36 |     STOP RUN.
- exists_builtin: exists_builtin: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- for_list_collection: for_list_collection: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- for_loop: ok
- for_map_collection: for_map_collection: convert error: unsupported feature at line 11:  unsupported for-in loop
  10 |     COMPUTE M = 0
  11 |     *> unsupported for-in loop
  12 |     STOP RUN.
- fun_call: ok
- fun_expr_in_let: fun_expr_in_let: type2 error: error[T003]: unknown function: flambda0
  --> :3:3

help:
  Ensure the function is defined before it's called.
- fun_three_args: fun_three_args: type2 error: error[T003]: unknown function: fsum3
  --> :5:3

help:
  Ensure the function is defined before it's called.
- group_by: group_by: convert error: unsupported feature at line 35:  unsupported for-in loop
  34 |     DISPLAY "--- People grouped by city ---"
  35 |     *> unsupported for-in loop
  36 |     STOP RUN.
- group_by_conditional_sum: group_by_conditional_sum: parse2 error: parse error: 2:12: unexpected token "=" (expected "}")
- group_by_having: group_by_having: parse2 error: parse error: 2:13: unexpected token "=" (expected "}")
- group_by_join: group_by_join: convert error: unsupported feature at line 43:  unsupported for-in loop
  42 |     DISPLAY "--- Orders per customer ---"
  43 |     *> unsupported for-in loop
  44 |     STOP RUN.
- group_by_left_join: group_by_left_join: convert error: unsupported feature at line 44:  unsupported for-in loop
  43 |     DISPLAY "--- Group Left Join ---"
  44 |     *> unsupported for-in loop
  45 |     STOP RUN.
- group_by_multi_join: group_by_multi_join: parse2 error: parse error: 2:14: unexpected token "=" (expected "}")
- group_by_multi_join_sort: group_by_multi_join_sort: parse2 error: parse error: 2:13: unexpected token "=" (expected "}")
- group_by_sort: group_by_sort: parse2 error: parse error: 2:12: unexpected token "=" (expected "}")
- group_items_iteration: group_items_iteration: convert error: unsupported feature at line 36:  unsupported for-in loop
  35 |     END-PERFORM
  36 |     *> unsupported for-in loop
  37 |     MOVE 0 TO TMP2
- if_else: ok
- if_then_else: if_then_else: type2 error: error[T002]: undefined variable: tmp0
  --> :8:13

help:
  Check if the variable was declared in this scope.
- if_then_else_nested: if_then_else_nested: type2 error: error[T002]: undefined variable: tmp0
  --> :13:13

help:
  Check if the variable was declared in this scope.
- in_operator: in_operator: parse2 error: parse error: 2:9: unexpected token "=" (expected "}")
- in_operator_extended: in_operator_extended: parse2 error: parse error: 2:9: unexpected token "=" (expected "}")
- inner_join: inner_join: convert error: unsupported feature at line 45:  unsupported for-in loop
  44 |     DISPLAY "--- Orders with customer info ---"
  45 |     *> unsupported for-in loop
  46 |     STOP RUN.
- join_multi: join_multi: convert error: unsupported feature at line 51:  unsupported for-in loop
  50 |     DISPLAY "--- Multi Join ---"
  51 |     *> unsupported for-in loop
  52 |     STOP RUN.
- json_builtin: ok
- left_join: left_join: convert error: unsupported feature at line 42:  unsupported for-in loop
  41 |     DISPLAY "--- Left Join ---"
  42 |     *> unsupported for-in loop
  43 |     STOP RUN.
- left_join_multi: left_join_multi: convert error: unsupported feature at line 50:  unsupported for-in loop
  49 |     DISPLAY "--- Left Join Multi ---"
  50 |     *> unsupported for-in loop
  51 |     STOP RUN.
- len_builtin: ok
- len_map: len_map: type2 error: error[T002]: undefined variable: FUNCTION
  --> :2:14

help:
  Check if the variable was declared in this scope.
- len_string: len_string: type2 error: error[T002]: undefined variable: FUNCTION
  --> :2:14

help:
  Check if the variable was declared in this scope.
- let_and_print: let_and_print: type2 error: error[T002]: undefined variable: b
  --> :3:18

help:
  Check if the variable was declared in this scope.
- list_assign: list_assign: compile panic: interface conversion: interface {} is nil, not string
- list_index: list_index: parse2 error: parse error: 2:9: unexpected token "=" (expected "}")
- list_nested_assign: list_nested_assign: compile panic: interface conversion: interface {} is nil, not string
- list_set_ops: list_set_ops: parse2 error: parse error: 8:32: unexpected token "union_all" (expected ")")
- load_yaml: load_yaml: convert error: unsupported feature at line 31:  unsupported for-in loop
  30 |     END-PERFORM
  31 |     *> unsupported for-in loop
  32 |     STOP RUN.
- map_assign: map_assign: compile panic: interface conversion: interface {} is nil, not string
- map_in_operator: map_in_operator: type2 error: error[T020]: operator `in` cannot be used on types int and int
  --> :3:16

help:
  Choose an operator that supports these operand types.
- map_index: map_index: type2 error: error[T004]: `m` is not callable
  --> :3:14

help:
  Use a function or closure in this position.
- map_int_key: map_int_key: type2 error: error[T004]: `m` is not callable
  --> :3:14

help:
  Use a function or closure in this position.
- map_literal_dynamic: ok
- map_membership: map_membership: type2 error: error[T020]: operator `in` cannot be used on types string and int
  --> :3:18

help:
  Choose an operator that supports these operand types.
- map_nested_assign: map_nested_assign: compile panic: interface conversion: interface {} is nil, not string
- match_expr: match_expr: parse2 error: parse error: 4:11: unexpected token "=" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- match_full: match_full: parse2 error: parse error: 4:11: unexpected token "=" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- math_ops: math_ops: type2 error: error[T002]: undefined variable: FUNCTION
  --> :6:14

help:
  Check if the variable was declared in this scope.
- membership: membership: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- min_max_builtin: min_max_builtin: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- nested_function: nested_function: type2 error: error[T003]: unknown function: fouter
  --> :3:3

help:
  Ensure the function is defined before it's called.
- order_by_map: order_by_map: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- outer_join: outer_join: convert error: unsupported feature at line 46:  unsupported for-in loop
  45 |     DISPLAY "--- Outer Join using syntax ---"
  46 |     *> unsupported for-in loop
  47 |     STOP RUN.
- partial_application: partial_application: type2 error: error[T001]: assignment to undeclared variable: add_p0
  --> :2:3

help:
  Declare `add_p0` first using `let`.
- print_hello: ok
- pure_fold: pure_fold: type2 error: error[T003]: unknown function: ftriple
  --> :4:3

help:
  Ensure the function is defined before it's called.
- pure_global_fold: pure_global_fold: type2 error: error[T003]: unknown function: finc
  --> :4:3

help:
  Ensure the function is defined before it's called.
- query_sum_select: query_sum_select: convert error: unsupported feature at line 25:  unsupported reduce
  24 |     IF N > 1
  25 |             *> unsupported reduce
  26 |             COMPUTE TMP2 = 0
- record_assign: record_assign: type2 error: error[T002]: undefined variable: C
  --> :3:16

help:
  Check if the variable was declared in this scope.
- right_join: right_join: convert error: unsupported feature at line 45:  unsupported for-in loop
  44 |     DISPLAY "--- Right Join using syntax ---"
  45 |     *> unsupported for-in loop
  46 |     STOP RUN.
- save_jsonl_stdout: save_jsonl_stdout: parse2 error: parse error: 2:13: unexpected token "=" (expected "}")
- short_circuit: short_circuit: type2 error: error[T003]: unknown function: fboom
  --> :4:3

help:
  Ensure the function is defined before it's called.
- slice: slice: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- sort_stable: sort_stable: parse2 error: parse error: 2:12: unexpected token "=" (expected "}")
- str_builtin: ok
- string_compare: ok
- string_concat: string_concat: parse2 error: parse error: 2:23: lexer: invalid input text "& \"world\"\n  prin..."
- string_contains: ok
- string_in_operator: ok
- string_index: string_index: parse2 error: parse error: 9:20: unexpected token ":" (expected ")")
- string_prefix_slice: string_prefix_slice: parse2 error: parse error: 7:21: unexpected token ":" (expected ")")
- substring_builtin: ok
- sum_builtin: sum_builtin: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- tail_recursion: tail_recursion: type2 error: error[T003]: unknown function: fsum_rec
  --> :4:3

help:
  Ensure the function is defined before it's called.
- test_block: test_block: parse2 error: parse error: 4:13: unexpected token "=" (expected ")")
- tree_sum: tree_sum: type2 error: error[T002]: undefined variable: LEAF
  --> :2:16

help:
  Check if the variable was declared in this scope.
- two-sum: two-sum: parse2 error: parse error: 2:11: unexpected token "=" (expected "}")
- typed_let: ok
- typed_var: ok
- unary_neg: unary_neg: parse2 error: parse error: 3:18: unexpected token "-" (expected PostfixExpr)
- update_stmt: update_stmt: convert error: unsupported feature at line 10:  unsupported update statement
   9 | PROCEDURE DIVISION.
  10 |     *> unsupported update statement
  11 | DISPLAY "-- TEST update adult status --"
- user_type_literal: ok
- values_builtin: ok
- var_assignment: ok
- while_loop: ok
