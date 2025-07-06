# Errors

- append_builtin: ok
- avg_builtin: parse2 error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
- basic_compare: ok
- binary_precedence: ok
- bool_chain: ok
- break_continue: parse2 error: parse error: 10:57: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- cast_string_to_int: type2 error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- cast_struct: parse2 error: parse error: 7:18: lexer: invalid input text "'Todo'] = (m) =>..."
- closure: parse2 error: parse error: 3:14: unexpected token "=>" (expected "}")
- count_builtin: parse2 error: parse error: 8:41: lexer: invalid input text "; if (items is L..."
- cross_join: parse2 error: parse error: 9:245: lexer: invalid input text "' '))\n  }\n}\n"
- cross_join_filter: parse2 error: parse error: 10:49: lexer: invalid input text "' '))\n  }\n}\n"
- cross_join_triple: parse2 error: parse error: 10:65: lexer: invalid input text "' '))\n  }\n}\n"
- dataset_sort_take_limit: parse2 error: parse error: 6:15: lexer: invalid input text "'price']\nlet key..."
- dataset_where_filter: parse2 error: parse error: 4:47: lexer: invalid input text "'age'] >= 18)).t..."
- exists_builtin: parse2 error: parse error: 13:11: lexer: invalid input text "'items'] is List..."
- for_list_collection: parse2 error: parse error: 2:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- for_loop: ok
- for_map_collection: parse2 error: parse error: 4:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- fun_call: ok
- fun_expr_in_let: parse2 error: parse error: 1:18: unexpected token "=>"
- fun_three_args: ok
- group_by: parse2 error: parse error: 2:74: lexer: invalid input text "'city']).map((g)..."
- group_by_conditional_sum: parse2 error: parse error: 5:13: lexer: invalid input text "'cat']\nlet ks = ..."
- group_by_having: parse2 error: parse error: 2:62: lexer: invalid input text "'city']).map((g)..."
- group_by_join: parse2 error: parse error: 6:13: lexer: invalid input text "'name']\nlet ks =..."
- group_by_left_join: parse2 error: parse error: 10:13: lexer: invalid input text "'name']\nlet ks =..."
- group_by_multi_join: parse2 error: parse error: 7:68: lexer: invalid input text "'part']).map((g)..."
- group_by_multi_join_sort: parse2 error: parse error: 10:27: lexer: invalid input text "'c_custkey'], \"c..."
- group_by_sort: parse2 error: parse error: 5:13: lexer: invalid input text "'cat']\nlet ks = ..."
- group_items_iteration: parse2 error: parse error: 2:53: lexer: invalid input text "'tag']).map((g) ..."
- if_else: ok
- if_then_else: parse2 error: parse error: 2:29: lexer: invalid input text "? \"yes\" : \"no\")\n..."
- if_then_else_nested: parse2 error: parse error: 2:29: lexer: invalid input text "? \"big\" : ((x > ..."
- in_operator: type2 error: error[T027]: [int] is not a struct
  --> :3:10

help:
  Field access is only valid on struct types.
- in_operator_extended: parse2 error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
- inner_join: parse2 error: parse error: 9:155: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- join_multi: parse2 error: parse error: 10:80: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- json_builtin: type2 error: error[T003]: unknown function: _json
  --> :3:3

help:
  Ensure the function is defined before it's called.
- left_join: parse2 error: parse error: 10:157: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- left_join_multi: parse2 error: parse error: 11:77: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- len_builtin: ok
- len_map: ok
- len_string: ok
- let_and_print: ok
- list_assign: type2 error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- list_index: ok
- list_nested_assign: type2 error: error[T024]: cannot assign to `matrix` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- list_set_ops: parse2 error: parse error: 8:13: unexpected token "<" (expected PostfixExpr)
- load_yaml: parse2 error: parse error: 13:18: lexer: invalid input text "'Person'] = (m) ..."
- map_assign: type2 error: error[T024]: cannot assign to `scores` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_in_operator: type2 error: error[T027]: {int: string} is not a struct
  --> :3:10

help:
  Field access is only valid on struct types.
- map_index: ok
- map_int_key: ok
- map_literal_dynamic: parse2 error: parse error: 5:53: lexer: invalid input text "' '))\n}\n"
- map_membership: type2 error: error[T004]: `` is not callable
  --> :3:23

help:
  Use a function or closure in this position.
- map_nested_assign: type2 error: error[T024]: cannot assign to `data` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- match_expr: parse2 error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
- match_full: parse2 error: parse error: 2:23: unexpected token ")" (expected PostfixExpr)
- math_ops: parse2 error: parse error: 3:12: lexer: invalid input text "~/ 2))\n  print((..."
- membership: type2 error: error[T027]: [int] is not a struct
  --> :3:10

help:
  Field access is only valid on struct types.
- min_max_builtin: parse2 error: parse error: 9:26: lexer: invalid input text "'items'] is List..."
- nested_function: type2 error: error[T002]: undefined variable: x
  --> :4:11

help:
  Check if the variable was declared in this scope.
- order_by_map: parse2 error: parse error: 6:20: lexer: invalid input text "'a'], \"b\": x['b'..."
- outer_join: parse2 error: parse error: 12:161: lexer: invalid input text "' '))\n        } ..."
- partial_application: type2 error: error[T003]: unknown function: add
  --> :1:12

help:
  Ensure the function is defined before it's called.
- print_hello: ok
- pure_fold: ok
- pure_global_fold: ok
- query_sum_select: parse2 error: parse error: 12:26: lexer: invalid input text "'items'] is List..."
- record_assign: parse2 error: parse error: 10:18: lexer: invalid input text "'Counter'] = (m)..."
- right_join: parse2 error: parse error: 11:174: lexer: invalid input text "' '))\n      } el..."
- save_jsonl_stdout: parse2 error: parse error: 6:21: lexer: invalid input text "?['format'] ?? '..."
- short_circuit: ok
- slice: type2 error: error[T004]: `` is not callable
  --> :2:26

help:
  Use a function or closure in this position.
- sort_stable: parse2 error: parse error: 6:14: lexer: invalid input text "'n']\nlet keyB = ..."
- str_builtin: type2 error: error[T004]: `` is not callable
  --> :2:21

help:
  Use a function or closure in this position.
- string_compare: type2 error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- string_concat: ok
- string_contains: ok
- string_in_operator: ok
- string_index: parse2 error: parse error: 11:22: lexer: invalid input text "'index out of ra..."
- string_prefix_slice: type2 error: error[T027]: string is not a struct
  --> :5:10

help:
  Field access is only valid on struct types.
- substring_builtin: ok
- sum_builtin: parse2 error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
- tail_recursion: ok
- test_block: parse2 error: parse error: 8:38: lexer: invalid input text "'expect failed')..."
- tree_sum: parse2 error: parse error: 9:29: lexer: invalid input text "; }\n  if (_t is ..."
- two-sum: type2 error: error[T003]: unknown function: twoSum
  --> :1:25

help:
  Ensure the function is defined before it's called.
- typed_let: ok
- typed_var: ok
- unary_neg: ok
- update_stmt: parse2 error: parse error: 14:242: lexer: invalid input text "'expect failed')..."
- user_type_literal: parse2 error: parse error: 12:18: lexer: invalid input text "'Person'] = (m) ..."
- values_builtin: ok
- var_assignment: type2 error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- while_loop: vm compile error: assignment to undeclared variable: i
