# Errors

- append_builtin: type back error: error[T002]: undefined variable: a
  --> :2:16

help:
  Check if the variable was declared in this scope.
- avg_builtin: parse back error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
- basic_compare: type back error: error[T002]: undefined variable: a
  --> :2:9

help:
  Check if the variable was declared in this scope.
- binary_precedence: ok
- bool_chain: ok
- break_continue: parse back error: parse error: 9:57: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- cast_string_to_int: type back error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- cast_struct: parse back error: parse error: 5:18: lexer: invalid input text "'Todo'] = (m) =>..."
- closure: parse back error: parse error: 2:14: unexpected token "=>" (expected "}")
- count_builtin: parse back error: parse error: 8:41: lexer: invalid input text "; if (items is L..."
- cross_join: parse back error: parse error: 4:245: lexer: invalid input text "' '))\n  }\n}\n"
- cross_join_filter: parse back error: parse error: 4:49: lexer: invalid input text "' '))\n  }\n}\n"
- cross_join_triple: parse back error: parse error: 4:65: lexer: invalid input text "' '))\n  }\n}\n"
- dataset_sort_take_limit: parse back error: parse error: 4:86: lexer: invalid input text "' '))\n  }\n}\n"
- dataset_where_filter: parse back error: parse error: 4:94: lexer: invalid input text "? \" (senior)\" : ..."
- exists_builtin: parse back error: parse error: 8:11: lexer: invalid input text "'items'] is List..."
- for_list_collection: parse back error: parse error: 2:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- for_loop: ok
- for_map_collection: parse back error: parse error: 3:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- fun_call: ok
- fun_expr_in_let: type back error: error[T003]: unknown function: square
  --> :2:9

help:
  Ensure the function is defined before it's called.
- fun_three_args: ok
- group_by: parse back error: parse error: 7:128: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- group_by_conditional_sum: parse back error: parse error: 10:26: lexer: invalid input text "'items'] is List..."
- group_by_having: parse back error: parse error: 11:41: lexer: invalid input text "; if (items is L..."
- group_by_join: parse back error: parse error: 7:78: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- group_by_left_join: parse back error: parse error: 7:78: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- group_by_multi_join: parse back error: parse error: 10:19: lexer: invalid input text "; i < a.length; ..."
- group_by_multi_join_sort: parse back error: parse error: 10:19: lexer: invalid input text "; i < a.length; ..."
- group_by_sort: parse back error: parse error: 10:26: lexer: invalid input text "'items'] is List..."
- group_items_iteration: parse back error: parse error: 5:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- if_else: ok
- if_then_else: type back error: error[T002]: undefined variable: msg
  --> :2:9

help:
  Check if the variable was declared in this scope.
- if_then_else_nested: type back error: error[T002]: undefined variable: msg
  --> :2:9

help:
  Check if the variable was declared in this scope.
- in_operator: type back error: error[T002]: undefined variable: xs
  --> :2:10

help:
  Check if the variable was declared in this scope.
- in_operator_extended: type back error: error[T002]: undefined variable: ys
  --> :2:10

help:
  Check if the variable was declared in this scope.
- inner_join: parse back error: parse error: 4:155: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- join_multi: parse back error: parse error: 4:80: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- json_builtin: type back error: error[T003]: unknown function: _json
  --> :2:3

help:
  Ensure the function is defined before it's called.
- left_join: parse back error: parse error: 4:157: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- left_join_multi: parse back error: parse error: 4:77: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- len_builtin: ok
- len_map: ok
- len_string: ok
- let_and_print: type back error: error[T002]: undefined variable: a
  --> :2:10

help:
  Check if the variable was declared in this scope.
- list_assign: type back error: error[T001]: assignment to undeclared variable: nums
  --> :2:3

help:
  Declare `nums` first using `let`.
- list_index: type back error: error[T002]: undefined variable: xs
  --> :2:9

help:
  Check if the variable was declared in this scope.
- list_nested_assign: type back error: error[T001]: assignment to undeclared variable: matrix
  --> :2:3

help:
  Declare `matrix` first using `let`.
- list_set_ops: parse back error: parse error: 8:13: unexpected token "<" (expected PostfixExpr)
- load_yaml: parse back error: parse error: 7:18: lexer: invalid input text "'Person'] = (m) ..."
- map_assign: type back error: error[T001]: assignment to undeclared variable: scores
  --> :2:3

help:
  Declare `scores` first using `let`.
- map_in_operator: type back error: error[T002]: undefined variable: m
  --> :2:10

help:
  Check if the variable was declared in this scope.
- map_index: type back error: error[T002]: undefined variable: m
  --> :2:9

help:
  Check if the variable was declared in this scope.
- map_int_key: type back error: error[T002]: undefined variable: m
  --> :2:9

help:
  Check if the variable was declared in this scope.
- map_literal_dynamic: parse back error: parse error: 2:53: lexer: invalid input text "' '))\n}\n"
- map_membership: type back error: error[T002]: undefined variable: m
  --> :2:10

help:
  Check if the variable was declared in this scope.
- map_nested_assign: type back error: error[T001]: assignment to undeclared variable: data
  --> :2:3

help:
  Declare `data` first using `let`.
- match_expr: type back error: error[T002]: undefined variable: label
  --> :2:9

help:
  Check if the variable was declared in this scope.
- match_full: parse back error: parse error: 2:12: unexpected token ")" (expected PostfixExpr)
- math_ops: parse back error: parse error: 3:12: lexer: invalid input text "~/ 2))\n  print((..."
- membership: type back error: error[T002]: undefined variable: nums
  --> :2:10

help:
  Check if the variable was declared in this scope.
- min_max_builtin: parse back error: parse error: 8:26: lexer: invalid input text "'items'] is List..."
- nested_function: type back error: error[T002]: undefined variable: x
  --> :4:11

help:
  Check if the variable was declared in this scope.
- order_by_map: type back error: error[T002]: undefined variable: sorted
  --> :2:9

help:
  Check if the variable was declared in this scope.
- outer_join: parse back error: parse error: 6:161: lexer: invalid input text "' '))\n        } ..."
- partial_application: type back error: error[T003]: unknown function: add5
  --> :5:9

help:
  Ensure the function is defined before it's called.
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type back error: error[T002]: undefined variable: k
  --> :2:15

help:
  Check if the variable was declared in this scope.
- query_sum_select: parse back error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
- record_assign: parse back error: parse error: 8:18: lexer: invalid input text "'Counter'] = (m)..."
- right_join: parse back error: parse error: 5:174: lexer: invalid input text "' '))\n      } el..."
- save_jsonl_stdout: parse back error: parse error: 5:21: lexer: invalid input text "?['format'] ?? '..."
- short_circuit: ok
- slice: type back error: error[T004]: `` is not callable
  --> :2:26

help:
  Use a function or closure in this position.
- sort_stable: type back error: error[T002]: undefined variable: result
  --> :2:9

help:
  Check if the variable was declared in this scope.
- str_builtin: type back error: error[T004]: `` is not callable
  --> :2:21

help:
  Use a function or closure in this position.
- string_compare: type back error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- string_concat: ok
- string_contains: type back error: error[T002]: undefined variable: s
  --> :2:9

help:
  Check if the variable was declared in this scope.
- string_in_operator: type back error: error[T002]: undefined variable: s
  --> :2:10

help:
  Check if the variable was declared in this scope.
- string_index: parse back error: parse error: 10:22: lexer: invalid input text "'index out of ra..."
- string_prefix_slice: type back error: error[T002]: undefined variable: s1
  --> :2:10

help:
  Check if the variable was declared in this scope.
- substring_builtin: ok
- sum_builtin: parse back error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
- tail_recursion: ok
- test_block: parse back error: parse error: 3:38: lexer: invalid input text "'expect failed')..."
- tree_sum: parse back error: parse error: 4:29: lexer: invalid input text "; }\n  if (_t is ..."
- two-sum: type back error: error[T002]: undefined variable: result
  --> :13:9

help:
  Check if the variable was declared in this scope.
- typed_let: type back error: error[T002]: undefined variable: y
  --> :2:9

help:
  Check if the variable was declared in this scope.
- typed_var: type back error: error[T002]: undefined variable: x
  --> :2:9

help:
  Check if the variable was declared in this scope.
- unary_neg: ok
- update_stmt: parse back error: parse error: 7:242: lexer: invalid input text "'expect failed')..."
- user_type_literal: parse back error: parse error: 10:18: lexer: invalid input text "'Person'] = (m) ..."
- values_builtin: type back error: error[T002]: undefined variable: m
  --> :2:16

help:
  Check if the variable was declared in this scope.
- var_assignment: type back error: error[T001]: assignment to undeclared variable: x
  --> :2:3

help:
  Declare `x` first using `let`.
- while_loop: vm compile error: assignment to undeclared variable: i
