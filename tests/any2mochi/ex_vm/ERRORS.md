# Errors

- append_builtin: parse error: parse error: 3:16: unexpected token "(" (expected <ident>)
- avg_builtin: ok
- basic_compare: ok
- binary_precedence: ok
- bool_chain: parse error: parse error: 3:10: unexpected token ":" (expected "}")
- break_continue: parse error: parse error: 5:13: unexpected token ":" (expected ")")
- cast_string_to_int: ok
- cast_struct: parse error: parse error: 3:18: unexpected token ":" (expected "}")
- closure: parse error: parse error: 2:10: unexpected token ":" (expected "}")
- count_builtin: ok
- cross_join: parse error: parse error: 32:5: lexer: invalid input text "&to_string(&1)\n ..."
- cross_join_filter: parse error: parse error: 4:15: unexpected token "for" (expected PostfixExpr)
- cross_join_triple: parse error: parse error: 5:16: unexpected token "for" (expected PostfixExpr)
- dataset_sort_take_limit: parse error: parse error: 3:3: unexpected token "%" (expected "]")
- dataset_where_filter: parse error: parse error: 3:3: unexpected token "%" (expected "]")
- exists_builtin: parse error: parse error: 3:22: unexpected token "for" (expected ")")
- for_list_collection: ok
- for_loop: ok
- for_map_collection: parse error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- fun_call: parse error: parse error: 2:10: unexpected token ":" (expected "}")
- fun_expr_in_let: parse error: parse error: 2:22: unexpected token ">" (expected PostfixExpr)
- fun_three_args: parse error: parse error: 2:10: unexpected token ":" (expected "}")
- group_by: parse error: parse error: 18:77: lexer: invalid input text "&to_string(&1)),..."
- group_by_conditional_sum: parse error: parse error: 3:3: unexpected token "%" (expected "]")
- group_by_having: parse error: parse error: 3:3: unexpected token "%" (expected "]")
- group_by_join: parse error: parse error: 2:20: unexpected token "%" (expected "]")
- group_by_left_join: parse error: parse error: 2:20: unexpected token "%" (expected "]")
- group_by_multi_join: parse error: parse error: 2:18: unexpected token "%" (expected "]")
- group_by_multi_join_sort: parse error: parse error: 2:17: unexpected token "%" (expected "]")
- group_by_sort: parse error: parse error: 2:16: unexpected token "%" (expected "]")
- group_items_iteration: parse error: parse error: 2:15: unexpected token "%" (expected "]")
- if_else: ok
- if_then_else: parse error: parse error: 4:8: unexpected token ">" (expected PostfixExpr)
- if_then_else_nested: parse error: parse error: 4:8: unexpected token ">" (expected PostfixExpr)
- in_operator: parse error: parse error: 3:39: lexer: invalid input text "?(xs, 2), else: ..."
- in_operator_extended: parse error: parse error: 4:39: lexer: invalid input text "?(ys, 1), else: ..."
- inner_join: parse error: parse error: 20:5: lexer: invalid input text "&to_string(&1)\n ..."
- join_multi: parse error: parse error: 2:20: unexpected token "%" (expected "]")
- json_builtin: parse error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- left_join: parse error: parse error: 21:5: lexer: invalid input text "&to_string(&1)\n ..."
- left_join_multi: parse error: parse error: 2:20: unexpected token "%" (expected "]")
- len_builtin: ok
- len_map: parse error: parse error: 2:13: unexpected token "%" (expected ")")
- len_string: ok
- let_and_print: ok
- list_assign: type error: error[T002]: undefined variable: Map
  --> :4:14

help:
  Check if the variable was declared in this scope.
- list_index: type error: error[T002]: undefined variable: Enum
  --> :3:9

help:
  Check if the variable was declared in this scope.
- list_nested_assign: type error: error[T002]: undefined variable: Map
  --> :4:16

help:
  Check if the variable was declared in this scope.
- list_set_ops: type error: error[T003]: unknown function: _union
  --> :5:13

help:
  Ensure the function is defined before it's called.
- load_yaml: parse error: parse error: 2:58: unexpected token "%" (expected PostfixExpr)
- map_assign: parse error: parse error: 2:16: unexpected token "%" (expected PostfixExpr)
- map_in_operator: parse error: parse error: 3:38: lexer: invalid input text "?(m, 1), else: E..."
- map_index: parse error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- map_int_key: parse error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- map_literal_dynamic: parse error: parse error: 6:11: unexpected token "%" (expected PostfixExpr)
- map_membership: parse error: parse error: 3:24: lexer: invalid input text "?(m, \"a\"))\n  pri..."
- map_nested_assign: parse error: parse error: 2:14: unexpected token "%" (expected PostfixExpr)
- match_expr: parse error: parse error: 4:8: unexpected token ">" (expected PostfixExpr)
- match_full: parse error: parse error: 3:4: unexpected token ":" (expected "}")
- math_ops: ok
- membership: parse error: parse error: 3:41: lexer: invalid input text "?(nums, 2), else..."
- min_max_builtin: type error: error[T003]: unknown function: _min
  --> :3:9

help:
  Ensure the function is defined before it's called.
- nested_function: parse error: parse error: 2:21: unexpected token ">" (expected PostfixExpr)
- order_by_map: parse error: parse error: 2:15: unexpected token "%" (expected "]")
- outer_join: parse error: parse error: 33:9: lexer: invalid input text "&to_string(&1)\n ..."
- partial_application: parse error: parse error: 2:10: unexpected token ":" (expected "}")
- print_hello: ok
- pure_fold: parse error: parse error: 2:10: unexpected token ":" (expected "}")
- pure_global_fold: parse error: parse error: 2:10: unexpected token ":" (expected "}")
- query_sum_select: parse error: parse error: 3:16: unexpected token "for" (expected PostfixExpr)
- record_assign: parse error: parse error: 3:14: unexpected token ":" (expected "}")
- right_join: parse error: parse error: 38:7: lexer: invalid input text "&to_string(&1)\n ..."
- save_jsonl_stdout: parse error: parse error: 2:17: unexpected token "%" (expected "]")
- short_circuit: parse error: parse error: 3:10: unexpected token ":" (expected "}")
- slice: type error: error[T002]: undefined variable: Enum
  --> :2:9

help:
  Check if the variable was declared in this scope.
- sort_stable: parse error: parse error: 2:16: unexpected token "%" (expected "]")
- str_builtin: type error: error[T003]: unknown function: to_string
  --> :2:9

help:
  Ensure the function is defined before it's called.
- string_compare: ok
- string_concat: parse error: parse error: 2:19: unexpected token ">" (expected PostfixExpr)
- string_contains: parse error: parse error: 3:24: lexer: invalid input text "?(s, \"cat\"))\n  p..."
- string_in_operator: parse error: parse error: 3:24: lexer: invalid input text "?(s, \"cat\"))\n  p..."
- string_index: ok
- string_prefix_slice: type error: error[T003]: unknown function: _slice_string
  --> :4:9

help:
  Ensure the function is defined before it's called.
- substring_builtin: parse error: parse error: 2:19: unexpected token "(" (expected <ident>)
- sum_builtin: ok
- tail_recursion: parse error: parse error: 3:12: unexpected token ":" (expected "}")
- test_block: ok
- tree_sum: parse error: parse error: 3:4: unexpected token ":" (expected "}")
- two-sum: parse error: parse error: 6:16: unexpected token ":" (expected "}")
- typed_let: compile error: nil expr
- typed_var: type error: error[T002]: undefined variable: nil
  --> :2:11

help:
  Check if the variable was declared in this scope.
- unary_neg: parse error: parse error: 3:13: unexpected token "-" (expected PostfixExpr)
- update_stmt: parse error: parse error: 3:3: unexpected token "%" (expected "]")
- user_type_literal: parse error: parse error: 2:14: unexpected token "%" (expected PostfixExpr)
- values_builtin: parse error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- var_assignment: ok
- while_loop: parse error: parse error: 4:17: unexpected token "," (expected "}")
