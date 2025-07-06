# Errors

- append_builtin: type error: error[T001]: assignment to undeclared variable: a
  --> :1:1

help:
  Declare `a` first using `let`.
- avg_builtin: ok
- basic_compare: type error: error[T001]: assignment to undeclared variable: a
  --> :1:1

help:
  Declare `a` first using `let`.
- binary_precedence: ok
- bool_chain: type error: error[T008]: type mismatch: expected void, got bool
  --> :3:10

help:
  Change the value to match the expected type.
- break_continue: parse error: parse error: 2:6: unexpected token "," (expected "in" Expr (".." Expr)? "{" Statement* "}")
- cast_string_to_int: ok
- cast_struct: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- closure: parse error: parse error: 2:17: unexpected token "{" (expected "=>" Expr)
- count_builtin: ok
- cross_join: parse error: parse error: 3:16: unexpected token "{" (expected "=>" Expr)
- cross_join_filter: parse error: parse error: 3:15: unexpected token "{" (expected "=>" Expr)
- cross_join_triple: parse error: parse error: 4:16: unexpected token "{" (expected "=>" Expr)
- dataset_sort_take_limit: parse error: parse error: 18:16: lexer: invalid input text "; i <= skip; i +..."
- dataset_where_filter: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- exists_builtin: parse error: parse error: 2:23: unexpected token "{" (expected "=>" Expr)
- for_list_collection: parse error: parse error: 1:6: unexpected token "," (expected "in" Expr (".." Expr)? "{" Statement* "}")
- for_loop: parse error: parse error: 1:10: lexer: invalid input text "; i <= 4 - 1; i ..."
- for_map_collection: type error: error[T001]: assignment to undeclared variable: m
  --> :1:1

help:
  Declare `m` first using `let`.
- fun_call: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- fun_expr_in_let: parse error: parse error: 1:17: unexpected token "{" (expected "=>" Expr)
- fun_three_args: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- group_by: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_conditional_sum: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_having: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_join: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_left_join: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_multi_join: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_multi_join_sort: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by_sort: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_items_iteration: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- if_else: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- if_then_else: parse error: parse error: 2:13: unexpected token "{" (expected "=>" Expr)
- if_then_else_nested: parse error: parse error: 2:13: unexpected token "{" (expected "=>" Expr)
- in_operator: type error: error[T001]: assignment to undeclared variable: xs
  --> :1:1

help:
  Declare `xs` first using `let`.
- in_operator_extended: parse error: parse error: 14:14: lexer: invalid input text "~= null)\nprint(m..."
- inner_join: parse error: parse error: 3:16: unexpected token "{" (expected "=>" Expr)
- join_multi: parse error: parse error: 4:16: unexpected token "{" (expected "=>" Expr)
- json_builtin: type error: error[T001]: assignment to undeclared variable: m
  --> :1:1

help:
  Declare `m` first using `let`.
- left_join: parse error: parse error: 3:16: unexpected token "{" (expected "=>" Expr)
- left_join_multi: parse error: parse error: 4:16: unexpected token "{" (expected "=>" Expr)
- len_builtin: ok
- len_map: ok
- len_string: ok
- let_and_print: type error: error[T001]: assignment to undeclared variable: a
  --> :1:1

help:
  Declare `a` first using `let`.
- list_assign: type error: error[T001]: assignment to undeclared variable: nums
  --> :1:1

help:
  Declare `nums` first using `let`.
- list_index: type error: error[T001]: assignment to undeclared variable: xs
  --> :1:1

help:
  Declare `xs` first using `let`.
- list_nested_assign: type error: error[T001]: assignment to undeclared variable: matrix
  --> :1:1

help:
  Declare `matrix` first using `let`.
- list_set_ops: ok
- load_yaml: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- map_assign: type error: error[T001]: assignment to undeclared variable: scores
  --> :1:1

help:
  Declare `scores` first using `let`.
- map_in_operator: parse error: parse error: 2:12: lexer: invalid input text "~= null)\nprint(m..."
- map_index: type error: error[T001]: assignment to undeclared variable: m
  --> :1:1

help:
  Declare `m` first using `let`.
- map_int_key: type error: error[T001]: assignment to undeclared variable: m
  --> :1:1

help:
  Declare `m` first using `let`.
- map_literal_dynamic: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- map_membership: parse error: parse error: 2:14: lexer: invalid input text "~= null)\nprint(m..."
- map_nested_assign: type error: error[T001]: assignment to undeclared variable: data
  --> :1:1

help:
  Declare `data` first using `let`.
- match_expr: parse error: parse error: 2:15: unexpected token "{" (expected "=>" Expr)
- match_full: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- math_ops: ok
- membership: type error: error[T001]: assignment to undeclared variable: nums
  --> :1:1

help:
  Declare `nums` first using `let`.
- min_max_builtin: type error: error[T001]: assignment to undeclared variable: nums
  --> :1:1

help:
  Declare `nums` first using `let`.
- nested_function: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- order_by_map: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- outer_join: parse error: parse error: 3:16: unexpected token "{" (expected "=>" Expr)
- partial_application: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- print_hello: ok
- pure_fold: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- pure_global_fold: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- query_sum_select: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- record_assign: parse error: parse error: 3:12: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- right_join: parse error: parse error: 3:16: unexpected token "{" (expected "=>" Expr)
- save_jsonl_stdout: parse error: parse error: 2:19: unexpected token "," (expected PostfixExpr)
- short_circuit: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- slice: ok
- sort_stable: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- str_builtin: type error: error[T003]: unknown function: tostring
  --> :1:7

help:
  Ensure the function is defined before it's called.
- string_compare: ok
- string_concat: ok
- string_contains: type error: error[T001]: assignment to undeclared variable: s
  --> :1:1

help:
  Declare `s` first using `let`.
- string_in_operator: type error: error[T001]: assignment to undeclared variable: s
  --> :1:1

help:
  Declare `s` first using `let`.
- string_index: type error: error[T001]: assignment to undeclared variable: s
  --> :1:1

help:
  Declare `s` first using `let`.
- string_prefix_slice: type error: error[T001]: assignment to undeclared variable: prefix
  --> :1:1

help:
  Declare `prefix` first using `let`.
- substring_builtin: ok
- sum_builtin: type error: error[T003]: unknown function: __sum
  --> :1:7

help:
  Ensure the function is defined before it's called.
- tail_recursion: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- test_block: type error: error[T003]: unknown function: __run_tests
  --> :9:1

help:
  Ensure the function is defined before it's called.
- tree_sum: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- two-sum: parse error: parse error: 3:12: lexer: invalid input text "; i <= n - 1; i ..."
- typed_let: type error: error[T001]: assignment to undeclared variable: y
  --> :1:1

help:
  Declare `y` first using `let`.
- typed_var: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- unary_neg: parse error: parse error: 2:12: unexpected token "-" (expected PostfixExpr)
- update_stmt: parse error: parse error: 14:12: lexer: invalid input text "; _i0 <= len(peo..."
- user_type_literal: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- values_builtin: type error: error[T001]: assignment to undeclared variable: m
  --> :1:1

help:
  Declare `m` first using `let`.
- var_assignment: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- while_loop: type error: error[T001]: assignment to undeclared variable: i
  --> :1:1

help:
  Declare `i` first using `let`.
