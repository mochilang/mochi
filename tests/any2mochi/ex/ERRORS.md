# Errors

- append_builtin.mochi: parse2 error: parse error: 3:16: unexpected token "(" (expected <ident>)
- bool_chain.mochi: parse2 error: parse error: 4:10: unexpected token ":" (expected "}")
- break_continue.mochi: parse2 error: parse error: 3:9: unexpected token "<" (expected "in" Expr (".." Expr)? "{" Statement* "}")
- cast_struct.mochi: parse2 error: parse error: 3:18: unexpected token ":" (expected "}")
- closure.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- cross_join.mochi: parse2 error: parse error: 7:168: lexer: invalid input text "&to_string(&1)),..."
- cross_join_filter.mochi: parse2 error: parse error: 7:40: lexer: invalid input text "&to_string(&1)),..."
- cross_join_triple.mochi: parse2 error: parse error: 8:45: lexer: invalid input text "&to_string(&1)),..."
- dataset_sort_take_limit.mochi: parse2 error: parse error: 6:64: lexer: invalid input text "&to_string(&1)),..."
- dataset_where_filter.mochi: parse2 error: parse error: 2:17: unexpected token "%" (expected "]")
- exists_builtin.mochi: parse2 error: parse error: 3:22: unexpected token "for" (expected ")")
- for_list_collection.mochi: parse2 error: parse error: 2:9: unexpected token "<" (expected "in" Expr (".." Expr)? "{" Statement* "}")
- for_loop.mochi: parse2 error: parse error: 2:9: unexpected token "<" (expected "in" Expr (".." Expr)? "{" Statement* "}")
- for_map_collection.mochi: parse2 error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- fun_call.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- fun_expr_in_let.mochi: parse2 error: parse error: 2:22: unexpected token ">" (expected PostfixExpr)
- fun_three_args.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- group_by.mochi: parse2 error: parse error: 6:86: lexer: invalid input text "&to_string(&1)),..."
- group_by_conditional_sum.mochi: parse2 error: parse error: 2:16: unexpected token "%" (expected "]")
- group_by_having.mochi: parse2 error: parse error: 2:17: unexpected token "%" (expected "]")
- group_by_join.mochi: parse2 error: parse error: 16:58: lexer: invalid input text "&to_string(&1)),..."
- group_by_left_join.mochi: parse2 error: parse error: 16:58: lexer: invalid input text "&to_string(&1)),..."
- group_by_multi_join.mochi: parse2 error: parse error: 2:18: unexpected token "%" (expected "]")
- group_by_multi_join_sort.mochi: parse2 error: parse error: 2:17: unexpected token "%" (expected "]")
- group_by_sort.mochi: parse2 error: parse error: 2:16: unexpected token "%" (expected "]")
- group_items_iteration.mochi: parse2 error: parse error: 2:15: unexpected token "%" (expected "]")
- if_else.mochi: parse2 error: parse error: 3:14: unexpected token "do" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- if_then_else.mochi: parse2 error: parse error: 3:18: unexpected token ">" (expected PostfixExpr)
- if_then_else_nested.mochi: parse2 error: parse error: 3:18: unexpected token ">" (expected PostfixExpr)
- in_operator.mochi: parse2 error: parse error: 3:40: lexer: invalid input text "?(xs, 2), else: ..."
- in_operator_extended.mochi: parse2 error: parse error: 4:40: lexer: invalid input text "?(ys, 1), else: ..."
- inner_join.mochi: parse2 error: parse error: 7:100: lexer: invalid input text "&to_string(&1)),..."
- join_multi.mochi: parse2 error: parse error: 8:60: lexer: invalid input text "&to_string(&1)),..."
- json_builtin.mochi: parse2 error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- left_join.mochi: parse2 error: parse error: 12:104: lexer: invalid input text "&to_string(&1)),..."
- left_join_multi.mochi: parse2 error: parse error: 14:57: lexer: invalid input text "&to_string(&1)),..."
- len_builtin.mochi: type2 error: error[T003]: unknown function: length
  --> :2:9

help:
  Ensure the function is defined before it's called.
- len_map.mochi: parse2 error: parse error: 2:16: unexpected token "%" (expected ")")
- len_string.mochi: type2 error: error[T003]: unknown function: length
  --> :2:9

help:
  Ensure the function is defined before it's called.
- list_assign.mochi: type2 error: error[T002]: undefined variable: Map
  --> :4:14

help:
  Check if the variable was declared in this scope.
- list_index.mochi: type2 error: error[T002]: undefined variable: Enum
  --> :3:9

help:
  Check if the variable was declared in this scope.
- list_nested_assign.mochi: type2 error: error[T002]: undefined variable: Map
  --> :4:16

help:
  Check if the variable was declared in this scope.
- list_set_ops.mochi: type2 error: error[T003]: unknown function: length
  --> :5:9

help:
  Ensure the function is defined before it's called.
- load_yaml.mochi: parse2 error: parse error: 5:47: lexer: invalid input text "&to_string(&1)),..."
- map_assign.mochi: parse2 error: parse error: 2:16: unexpected token "%" (expected PostfixExpr)
- map_in_operator.mochi: parse2 error: parse error: 3:39: lexer: invalid input text "?(m, 1), else: E..."
- map_index.mochi: parse2 error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- map_int_key.mochi: parse2 error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- map_literal_dynamic.mochi: parse2 error: parse error: 8:64: lexer: invalid input text "&to_string(&1)),..."
- map_membership.mochi: parse2 error: parse error: 3:24: lexer: invalid input text "?(m, \"a\"))\n  pri..."
- map_nested_assign.mochi: parse2 error: parse error: 2:14: unexpected token "%" (expected PostfixExpr)
- match_expr.mochi: parse2 error: parse error: 3:20: unexpected token ">" (expected PostfixExpr)
- match_full.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- math_ops.mochi: type2 error: error[T003]: unknown function: rem
  --> :4:9

help:
  Ensure the function is defined before it's called.
- membership.mochi: parse2 error: parse error: 3:42: lexer: invalid input text "?(nums, 2), else..."
- min_max_builtin.mochi: type2 error: error[T003]: unknown function: _min
  --> :3:9

help:
  Ensure the function is defined before it's called.
- nested_function.mochi: parse2 error: parse error: 3:21: unexpected token ">" (expected PostfixExpr)
- order_by_map.mochi: parse2 error: parse error: 2:15: unexpected token "%" (expected "]")
- outer_join.mochi: parse2 error: parse error: 14:102: lexer: invalid input text "&to_string(&1)),..."
- partial_application.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- pure_fold.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- pure_global_fold.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- query_sum_select.mochi: parse2 error: parse error: 3:16: unexpected token "for" (expected PostfixExpr)
- record_assign.mochi: parse2 error: parse error: 4:14: unexpected token ":" (expected "}")
- right_join.mochi: parse2 error: parse error: 13:117: lexer: invalid input text "&to_string(&1)),..."
- save_jsonl_stdout.mochi: parse2 error: parse error: 2:17: unexpected token "%" (expected "]")
- short_circuit.mochi: parse2 error: parse error: 4:10: unexpected token ":" (expected "}")
- slice.mochi: type2 error: error[T002]: undefined variable: Enum
  --> :2:9

help:
  Check if the variable was declared in this scope.
- sort_stable.mochi: parse2 error: parse error: 2:16: unexpected token "%" (expected "]")
- str_builtin.mochi: type2 error: error[T003]: unknown function: to_string
  --> :2:9

help:
  Ensure the function is defined before it's called.
- string_concat.mochi: parse2 error: parse error: 2:20: unexpected token ">" (expected PostfixExpr)
- string_contains.mochi: parse2 error: parse error: 3:24: lexer: invalid input text "?(s, \"cat\"))\n  p..."
- string_in_operator.mochi: parse2 error: parse error: 3:24: lexer: invalid input text "?(s, \"cat\"))\n  p..."
- string_prefix_slice.mochi: type2 error: error[T003]: unknown function: _slice_string
  --> :4:10

help:
  Ensure the function is defined before it's called.
- substring_builtin.mochi: parse2 error: parse error: 2:19: unexpected token "(" (expected <ident>)
- tail_recursion.mochi: parse2 error: parse error: 3:7: unexpected token "if" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- tree_sum.mochi: parse2 error: parse error: 3:10: unexpected token ":" (expected "}")
- two-sum.mochi: parse2 error: parse error: 4:9: unexpected token "<" (expected "in" Expr (".." Expr)? "{" Statement* "}")
- typed_let.mochi: compile error: nil expr
- typed_var.mochi: type2 error: error[T002]: undefined variable: nil
  --> :2:11

help:
  Check if the variable was declared in this scope.
- unary_neg.mochi: parse2 error: parse error: 3:14: unexpected token "-" (expected PostfixExpr)
- update_stmt.mochi: parse2 error: parse error: 2:17: unexpected token "%" (expected "]")
- user_type_literal.mochi: parse2 error: parse error: 2:14: unexpected token "%" (expected PostfixExpr)
- values_builtin.mochi: parse2 error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- while_loop.mochi: parse2 error: parse error: 4:17: unexpected token "," (expected "}")
