# Errors

- append_builtin: type2 error: error[T003]: unknown function: _1
  --> :2:11

help:
  Ensure the function is defined before it's called.
- avg_builtin: parse2 error: parse error: 2:9: unexpected token "let" (expected ")")
- basic_compare: ok
- binary_precedence: ok
- bool_chain: parse2 error: parse error: 2:22: unexpected token "," (expected PostfixExpr)
- break_continue: parse2 error: parse error: 3:40: unexpected token "let" (expected PostfixExpr)
- cast_string_to_int: type2 error: error[T003]: unknown function: int
  --> :2:9

help:
  Ensure the function is defined before it's called.
- cast_struct: parse2 error: parse error: 2:96: lexer: invalid input text "#, get(m, %), fi..."
- closure: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- count_builtin: type2 error: error[T003]: unknown function: _1
  --> :2:15

help:
  Ensure the function is defined before it's called.
- cross_join: parse2 error: parse error: 2:20: unexpected token ":" (expected "}")
- cross_join_filter: parse2 error: parse error: 4:19: unexpected token ">" (expected ")")
- cross_join_triple: parse2 error: parse error: 5:20: unexpected token ">" (expected ")")
- dataset_sort_take_limit: parse2 error: parse error: 2:19: unexpected token ":" (expected "}")
- dataset_where_filter: parse2 error: parse error: 2:17: unexpected token ":" (expected "}")
- exists_builtin: parse2 error: parse error: 3:25: unexpected token ">" (expected ")")
- for_list_collection: parse2 error: parse error: 2:41: unexpected token "let" (expected PostfixExpr)
- for_loop: parse2 error: parse error: 2:25: unexpected token "let" (expected PostfixExpr)
- for_map_collection: parse2 error: parse error: 3:34: unexpected token "let" (expected PostfixExpr)
- fun_call: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- fun_expr_in_let: parse2 error: parse error: 2:28: unexpected token "," (expected ")")
- fun_three_args: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- group_by: parse2 error: parse error: 9:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_conditional_sum: parse2 error: parse error: 6:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_having: parse2 error: parse error: 6:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_join: parse2 error: parse error: 6:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_left_join: parse2 error: parse error: 3:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_multi_join: parse2 error: parse error: 6:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_multi_join_sort: parse2 error: parse error: 6:105: lexer: invalid input text "@groups, ks), sw..."
- group_by_sort: parse2 error: parse error: 6:105: lexer: invalid input text "@groups, ks), sw..."
- group_items_iteration: parse2 error: parse error: 3:105: lexer: invalid input text "@groups, ks), sw..."
- if_else: ok
- if_then_else: ok
- if_then_else_nested: ok
- in_operator: parse2 error: parse error: 3:14: lexer: invalid input text "#, 2 == %, xs))\n..."
- in_operator_extended: parse2 error: parse error: 4:14: lexer: invalid input text "#, 1 == %, ys))\n..."
- inner_join: parse2 error: parse error: 2:20: unexpected token ":" (expected "}")
- join_multi: parse2 error: parse error: 2:20: unexpected token ":" (expected "}")
- json_builtin: parse2 error: parse error: 2:3: unexpected token ">" (expected "}")
- left_join: parse2 error: parse error: 2:160: lexer: invalid input text "@items), let(m(a..."
- left_join_multi: parse2 error: parse error: 2:160: lexer: invalid input text "@items), let(m(a..."
- len_builtin: type2 error: error[T003]: unknown function: _1
  --> :2:15

help:
  Ensure the function is defined before it's called.
- len_map: parse2 error: parse error: 2:19: unexpected token "," (expected ":" Expr)
- len_string: type2 error: error[T037]: count() expects list or group, got string
  --> :2:9

help:
  Pass a list or group to count().
- let_and_print: ok
- list_assign: parse2 error: parse error: 2:6: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- list_index: parse2 error: parse error: 2:6: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- list_nested_assign: parse2 error: parse error: 2:6: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- list_set_ops: type2 error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- load_yaml: parse2 error: parse error: 2:176: lexer: invalid input text "#, str(\"c\", %), ..."
- map_assign: parse2 error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- map_in_operator: parse2 error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- map_index: parse2 error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
- map_int_key: parse2 error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
- map_literal_dynamic: parse2 error: parse error: 8:1: unexpected token "<EOF>" (expected "}")
- map_membership: parse2 error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- map_nested_assign: parse2 error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- match_expr: parse2 error: parse error: 3:15: unexpected token "let" (expected PostfixExpr)
- match_full: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- math_ops: ok
- membership: parse2 error: parse error: 3:14: lexer: invalid input text "#, 2 == %, nums)..."
- min_max_builtin: type2 error: error[T003]: unknown function: _3
  --> :2:14

help:
  Ensure the function is defined before it's called.
- nested_function: parse2 error: parse error: 2:28: unexpected token "," (expected ")")
- order_by_map: parse2 error: parse error: 2:15: unexpected token ":" (expected "}")
- outer_join: parse2 error: parse error: 2:160: lexer: invalid input text "@items), let(m(a..."
- partial_application: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- print_hello: ok
- pure_fold: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- pure_global_fold: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- query_sum_select: parse2 error: parse error: 2:6: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- record_assign: parse2 error: parse error: 2:4: unexpected token ":" (expected "}")
- right_join: parse2 error: parse error: 2:160: lexer: invalid input text "@items), let(m(a..."
- save_jsonl_stdout: parse2 error: parse error: 2:252: lexer: invalid input text "#, str(get(r, %,..."
- short_circuit: parse2 error: parse error: 2:22: unexpected token "," (expected PostfixExpr)
- slice: type2 error: error[T003]: unknown function: subvec
  --> :2:9

help:
  Ensure the function is defined before it's called.
- sort_stable: parse2 error: parse error: 2:16: unexpected token ":" (expected "}")
- str_builtin: ok
- string_compare: type2 error: error[T003]: unknown function: compare
  --> :2:9

help:
  Ensure the function is defined before it's called.
- string_concat: type2 error: error[T039]: function str expects 1 arguments, got 2
  --> :2:9

help:
  Pass exactly 1 arguments to `str`.
- string_contains: type2 error: error[T003]: unknown function: clojure_string_includes_p
  --> :3:9

help:
  Ensure the function is defined before it's called.
- string_in_operator: type2 error: error[T003]: unknown function: clojure_string_includes_p
  --> :3:9

help:
  Ensure the function is defined before it's called.
- string_index: parse2 error: parse error: 2:6: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- string_prefix_slice: type2 error: error[T003]: unknown function: subs
  --> :4:9

help:
  Ensure the function is defined before it's called.
- substring_builtin: ok
- sum_builtin: parse2 error: parse error: 2:16: unexpected token "+" (expected ")")
- tail_recursion: parse2 error: parse error: 2:7: unexpected token "," (expected ")")
- test_block: type2 error: error[T003]: unknown function: assert
  --> :3:3

help:
  Ensure the function is defined before it's called.
- tree_sum: parse2 error: parse error: 2:4: unexpected token ":" (expected "}")
- two-sum: parse2 error: parse error: 2:6: unexpected token "(" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- typed_let: type2 error: error[T002]: undefined variable: y
  --> :2:9

help:
  Check if the variable was declared in this scope.
- typed_var: type2 error: error[T002]: undefined variable: nil
  --> :2:11

help:
  Check if the variable was declared in this scope.
- unary_neg: ok
- update_stmt: parse2 error: parse error: 2:96: lexer: invalid input text "#, get(m, %), fi..."
- user_type_literal: parse2 error: parse error: 2:4: unexpected token ":" (expected "}")
- values_builtin: parse2 error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
- var_assignment: ok
- while_loop: parse2 error: parse error: 3:8: unexpected token "," (expected ")")
