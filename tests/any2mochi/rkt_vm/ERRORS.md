# Errors

- append_builtin: ok
- avg_builtin: parse error: parse error: 1:12: unexpected token "," (expected PostfixExpr)
- basic_compare: parse error: parse error: 4:12: lexer: invalid input text "?(a, 7))\nprint(l..."
- binary_precedence: parse error: parse error: 1:16: unexpected token "," (expected PostfixExpr)
- bool_chain: parse error: parse error: 2:52: lexer: invalid input text "?, 1, ), (, stri..."
- break_continue: ok
- cast_string_to_int: ok
- cast_struct: type error: error[T002]: undefined variable: title
  --> :2:3

help:
  Check if the variable was declared in this scope.
- closure: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- count_builtin: parse error: parse error: 1:14: unexpected token "," (expected PostfixExpr)
- cross_join: parse error: parse error: 5:22: lexer: invalid input text "?, result), (has..."
- cross_join_filter: parse error: parse error: 5:18: lexer: invalid input text "?, pairs), (hash..."
- cross_join_triple: parse error: parse error: 6:18: lexer: invalid input text "?, combos), (has..."
- dataset_sort_take_limit: parse error: parse error: 4:21: lexer: invalid input text "?, expensive), (..."
- dataset_where_filter: parse error: parse error: 4:23: lexer: invalid input text "?, adults), (has..."
- exists_builtin: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- for_list_collection: parse error: parse error: 1:18: lexer: invalid input text "?, (list, 1, 2, ..."
- for_loop: parse error: parse error: 1:10: unexpected token "in" (expected PostfixExpr)
- for_map_collection: parse error: parse error: 2:18: lexer: invalid input text "?, m), (hash-key..."
- fun_call: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- fun_expr_in_let: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- fun_three_args: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- group_by: parse error: parse error: 8:18: lexer: invalid input text "?, stats), (hash..."
- group_by_conditional_sum: parse error: parse error: 5:15: unexpected token "," (expected PostfixExpr)
- group_by_having: parse error: parse error: 5:16: unexpected token "," (expected PostfixExpr)
- group_by_join: parse error: parse error: 9:18: lexer: invalid input text "?, stats), (hash..."
- group_by_left_join: parse error: parse error: 9:18: lexer: invalid input text "?, stats), (hash..."
- group_by_multi_join: parse error: parse error: 5:17: unexpected token "," (expected PostfixExpr)
- group_by_multi_join_sort: parse error: parse error: 5:16: unexpected token "," (expected PostfixExpr)
- group_by_sort: parse error: parse error: 5:15: unexpected token "," (expected PostfixExpr)
- group_items_iteration: parse error: parse error: 5:14: unexpected token "," (expected PostfixExpr)
- if_else: ok
- if_then_else: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- if_then_else_nested: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- in_operator: parse error: parse error: 2:22: lexer: invalid input text "?, xs, ), (, has..."
- in_operator_extended: parse error: parse error: 3:22: lexer: invalid input text "?, ys, ), (, has..."
- inner_join: parse error: parse error: 5:22: lexer: invalid input text "?, result), (has..."
- join_multi: parse error: parse error: 6:18: lexer: invalid input text "?, result), (has..."
- json_builtin: parse error: parse error: 2:14: unexpected token ">" (expected PostfixExpr)
- left_join: compile error: join sides not supported
- left_join_multi: compile error: join sides not supported
- len_builtin: parse error: parse error: 1:14: unexpected token "," (expected PostfixExpr)
- len_map: parse error: parse error: 1:14: unexpected token "," (expected PostfixExpr)
- len_string: type error: error[T037]: count() expects list or group, got string
  --> :1:7

help:
  Pass a list or group to count().
- let_and_print: type error: error[T003]: unknown function: _add
  --> :3:7

help:
  Ensure the function is defined before it's called.
- list_assign: type error: error[T003]: unknown function: idx
  --> :2:7

help:
  Ensure the function is defined before it's called.
- list_index: type error: error[T003]: unknown function: idx
  --> :2:7

help:
  Ensure the function is defined before it's called.
- list_nested_assign: parse error: parse error: 1:16: unexpected token "," (expected PostfixExpr)
- list_set_ops: parse error: parse error: 1:14: unexpected token "," (expected PostfixExpr)
- load_yaml: parse error: parse error: 12:18: lexer: invalid input text "?, adults), (has..."
- map_assign: type error: error[T003]: unknown function: idx
  --> :2:7

help:
  Ensure the function is defined before it's called.
- map_in_operator: parse error: parse error: 2:22: lexer: invalid input text "?, m, ), (, hash..."
- map_index: type error: error[T003]: unknown function: idx
  --> :2:7

help:
  Ensure the function is defined before it's called.
- map_int_key: type error: error[T003]: unknown function: idx
  --> :2:7

help:
  Ensure the function is defined before it's called.
- map_literal_dynamic: parse error: parse error: 4:25: unexpected token "," (expected PostfixExpr)
- map_membership: parse error: parse error: 2:22: lexer: invalid input text "?, m, ), (, hash..."
- map_nested_assign: parse error: parse error: 1:21: unexpected token "," (expected PostfixExpr)
- match_expr: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- match_full: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- math_ops: type error: error[T003]: unknown function: _div
  --> :2:7

help:
  Ensure the function is defined before it's called.
- membership: parse error: parse error: 2:22: lexer: invalid input text "?, nums, ), (, h..."
- min_max_builtin: type error: error[T003]: unknown function: list
  --> :2:11

help:
  Ensure the function is defined before it's called.
- nested_function: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- order_by_map: parse error: parse error: 1:14: unexpected token "," (expected PostfixExpr)
- outer_join: compile error: join sides not supported
- partial_application: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- print_hello: ok
- pure_fold: parse error: parse error: 2:15: unexpected token "," (expected PostfixExpr)
- pure_global_fold: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- query_sum_select: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- record_assign: type error: error[T002]: undefined variable: n
  --> :2:3

help:
  Check if the variable was declared in this scope.
- right_join: compile error: join sides not supported
- save_jsonl_stdout: parse error: parse error: 5:16: unexpected token "," (expected PostfixExpr)
- short_circuit: parse error: parse error: 2:19: unexpected token "," (expected PostfixExpr)
- slice: parse error: parse error: 1:14: unexpected token "," (expected PostfixExpr)
- sort_stable: parse error: parse error: 1:15: unexpected token "," (expected PostfixExpr)
- str_builtin: type error: error[T003]: unknown function: format
  --> :1:7

help:
  Ensure the function is defined before it's called.
- string_compare: parse error: parse error: 1:36: lexer: invalid input text "?, \"a\", ), (, st..."
- string_concat: type error: error[T003]: unknown function: _add
  --> :1:7

help:
  Ensure the function is defined before it's called.
- string_contains: parse error: parse error: 2:22: lexer: invalid input text "?, s, ), (, hash..."
- string_in_operator: parse error: parse error: 2:22: lexer: invalid input text "?, s, ), (, hash..."
- string_index: type error: error[T003]: unknown function: idx
  --> :2:7

help:
  Ensure the function is defined before it's called.
- string_prefix_slice: parse error: parse error: 3:12: lexer: invalid input text "?((, slice, s1, ..."
- substring_builtin: ok
- sum_builtin: parse error: parse error: 1:12: unexpected token "," (expected PostfixExpr)
- tail_recursion: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- test_block: ok
- tree_sum: parse error: parse error: 3:40: unexpected token "," (expected PostfixExpr)
- two-sum: type error: error[T005]: parameter `nums` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- typed_let: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- typed_var: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- unary_neg: parse error: parse error: 2:16: unexpected token "," (expected PostfixExpr)
- update_stmt: compile error: unsupported statement
- user_type_literal: parse error: parse error: 9:35: unexpected token "," (expected PostfixExpr)
- values_builtin: ok
- var_assignment: ok
- while_loop: ok
