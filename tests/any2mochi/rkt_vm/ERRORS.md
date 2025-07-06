# Errors

- append_builtin: parse error: parse error: 2:16: unexpected token "a" (expected "(" (Expr ("," Expr)*)? ")")
- avg_builtin: parse error: parse error: 1:20: unexpected token "1" (expected ")")
- basic_compare: parse error: parse error: 4:14: lexer: invalid input text "? a 7 ))\nprint((..."
- binary_precedence: parse error: parse error: 1:14: unexpected token "1" (expected "(" (Expr ("," Expr)*)? ")")
- bool_chain: parse error: parse error: 2:45: lexer: invalid input text "? 1 ) ( string->..."
- break_continue: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- cast_string_to_int: ok
- cast_struct: parse error: parse error: 5:18: unexpected token "todo" (expected "(" (Expr ("," Expr)*)? ")")
- closure: parse error: parse error: 3:15: unexpected token "7" (expected "(" (Expr ("," Expr)*)? ")")
- count_builtin: parse error: parse error: 1:22: unexpected token "1" (expected ")")
- cross_join: parse error: parse error: 5:23: lexer: invalid input text "? result) (hash-..."
- cross_join_filter: parse error: parse error: 5:19: lexer: invalid input text "? pairs) (hash-k..."
- cross_join_triple: parse error: parse error: 6:19: lexer: invalid input text "? combos) (hash-..."
- dataset_sort_take_limit: parse error: parse error: 4:22: lexer: invalid input text "? expensive) (ha..."
- dataset_where_filter: parse error: parse error: 4:24: lexer: invalid input text "? adults) (hash-..."
- exists_builtin: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- for_list_collection: parse error: parse error: 1:19: lexer: invalid input text "? (list 1 2 3)) ..."
- for_loop: parse error: parse error: 1:11: unexpected token "in" (expected PostfixExpr)
- for_map_collection: parse error: parse error: 2:19: lexer: invalid input text "? m) (hash-keys ..."
- fun_call: parse error: parse error: 2:13: unexpected token "2" (expected "(" (Expr ("," Expr)*)? ")")
- fun_expr_in_let: parse error: parse error: 2:16: unexpected token "6" (expected "(" (Expr ("," Expr)*)? ")")
- fun_three_args: parse error: parse error: 2:14: unexpected token "1" (expected "(" (Expr ("," Expr)*)? ")")
- group_by: parse error: parse error: 8:19: lexer: invalid input text "? stats) (hash-k..."
- group_by_conditional_sum: type error: error[T002]: undefined variable: key
  --> :2:3

help:
  Check if the variable was declared in this scope.
- group_by_having: parse error: parse error: 7:16: unexpected token ">" (expected PostfixExpr)
- group_by_join: parse error: parse error: 9:19: lexer: invalid input text "? stats) (hash-k..."
- group_by_left_join: parse error: parse error: 9:19: lexer: invalid input text "? stats) (hash-k..."
- group_by_multi_join: type error: error[T002]: undefined variable: key
  --> :2:3

help:
  Check if the variable was declared in this scope.
- group_by_multi_join_sort: type error: error[T002]: undefined variable: key
  --> :2:3

help:
  Check if the variable was declared in this scope.
- group_by_sort: type error: error[T002]: undefined variable: key
  --> :2:3

help:
  Check if the variable was declared in this scope.
- group_items_iteration: type error: error[T002]: undefined variable: key
  --> :2:3

help:
  Check if the variable was declared in this scope.
- if_else: ok
- if_then_else: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- if_then_else_nested: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- in_operator: parse error: parse error: 2:22: lexer: invalid input text "? xs ) ( hash-ha..."
- in_operator_extended: parse error: parse error: 3:22: lexer: invalid input text "? ys ) ( hash-ha..."
- inner_join: parse error: parse error: 5:23: lexer: invalid input text "? result) (hash-..."
- join_multi: parse error: parse error: 6:19: lexer: invalid input text "? result) (hash-..."
- json_builtin: parse error: parse error: 2:16: unexpected token ">" (expected PostfixExpr)
- left_join: compile error: join sides not supported
- left_join_multi: compile error: join sides not supported
- len_builtin: parse error: parse error: 1:22: unexpected token "1" (expected ")")
- len_map: parse error: parse error: 1:22: unexpected token "a" (expected ")")
- len_string: parse error: parse error: 1:15: unexpected token "mochi" (expected "(" (Expr ("," Expr)*)? ")")
- let_and_print: parse error: parse error: 3:14: unexpected token "a" (expected "(" (Expr ("," Expr)*)? ")")
- list_assign: parse error: parse error: 2:13: unexpected token "nums" (expected "(" (Expr ("," Expr)*)? ")")
- list_index: parse error: parse error: 2:13: unexpected token "xs" (expected "(" (Expr ("," Expr)*)? ")")
- list_nested_assign: parse error: parse error: 2:19: unexpected token "matrix" (expected ")")
- list_set_ops: parse error: parse error: 1:22: unexpected token "1" (expected ")")
- load_yaml: parse error: parse error: 12:19: lexer: invalid input text "? adults) (hash-..."
- map_assign: parse error: parse error: 2:13: unexpected token "scores" (expected "(" (Expr ("," Expr)*)? ")")
- map_in_operator: parse error: parse error: 2:22: lexer: invalid input text "? m ) ( hash-has..."
- map_index: parse error: parse error: 2:13: unexpected token "m" (expected "(" (Expr ("," Expr)*)? ")")
- map_int_key: parse error: parse error: 2:13: unexpected token "m" (expected "(" (Expr ("," Expr)*)? ")")
- map_literal_dynamic: parse error: parse error: 4:16: unexpected token "~a ~a" (expected "(" (Expr ("," Expr)*)? ")")
- map_membership: parse error: parse error: 2:22: lexer: invalid input text "? m ) ( hash-has..."
- map_nested_assign: parse error: parse error: 2:19: unexpected token "data" (expected ")")
- match_expr: type error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- match_full: parse error: parse error: 11:18: unexpected token "0" (expected "(" (Expr ("," Expr)*)? ")")
- math_ops: parse error: parse error: 2:14: unexpected token "7" (expected "(" (Expr ("," Expr)*)? ")")
- membership: parse error: parse error: 2:22: lexer: invalid input text "? nums ) ( hash-..."
- min_max_builtin: parse error: parse error: 2:18: unexpected token "nums" (expected "(" (Expr ("," Expr)*)? ")")
- nested_function: parse error: parse error: 2:15: unexpected token "3" (expected "(" (Expr ("," Expr)*)? ")")
- order_by_map: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- outer_join: compile error: join sides not supported
- partial_application: parse error: parse error: 3:14: unexpected token "3" (expected "(" (Expr ("," Expr)*)? ")")
- print_hello: ok
- pure_fold: parse error: parse error: 2:23: unexpected token "1" (expected ")")
- pure_global_fold: parse error: parse error: 3:13: unexpected token "3" (expected "(" (Expr ("," Expr)*)? ")")
- query_sum_select: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- record_assign: parse error: parse error: 6:18: unexpected token "c" (expected "(" (Expr ("," Expr)*)? ")")
- right_join: compile error: join sides not supported
- save_jsonl_stdout: type error: error[T002]: undefined variable: key
  --> :2:3

help:
  Check if the variable was declared in this scope.
- short_circuit: parse error: parse error: 2:13: unexpected token "false" (expected "(" (Expr ("," Expr)*)? ")")
- slice: parse error: parse error: 1:22: unexpected token "1" (expected ")")
- sort_stable: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- str_builtin: parse error: parse error: 1:16: unexpected token "~a" (expected "(" (Expr ("," Expr)*)? ")")
- string_compare: parse error: parse error: 1:33: lexer: invalid input text "? \"a\" ) ( string..."
- string_concat: parse error: parse error: 1:14: unexpected token "hello " (expected "(" (Expr ("," Expr)*)? ")")
- string_contains: parse error: parse error: 2:22: lexer: invalid input text "? s ) ( hash-has..."
- string_in_operator: parse error: parse error: 2:22: lexer: invalid input text "? s ) ( hash-has..."
- string_index: parse error: parse error: 2:13: unexpected token "s" (expected "(" (Expr ("," Expr)*)? ")")
- string_prefix_slice: parse error: parse error: 3:14: lexer: invalid input text "? ( slice s1 0 (..."
- substring_builtin: parse error: parse error: 1:19: unexpected token "mochi" (expected "(" (Expr ("," Expr)*)? ")")
- sum_builtin: parse error: parse error: 1:20: unexpected token "1" (expected ")")
- tail_recursion: parse error: parse error: 2:17: unexpected token "10" (expected "(" (Expr ("," Expr)*)? ")")
- test_block: ok
- tree_sum: parse error: parse error: 4:18: unexpected token "t" (expected "(" (Expr ("," Expr)*)? ")")
- two-sum: parse error: parse error: 3:13: unexpected token "result" (expected "(" (Expr ("," Expr)*)? ")")
- typed_let: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- typed_var: type error: error[T000]: `let` requires a type or a value
  --> :1:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- unary_neg: parse error: parse error: 2:14: unexpected token "5" (expected "(" (Expr ("," Expr)*)? ")")
- update_stmt: compile error: unsupported statement
- user_type_literal: parse error: parse error: 10:29: unexpected token "book" (expected ")")
- values_builtin: parse error: parse error: 2:16: unexpected token "m" (expected "(" (Expr ("," Expr)*)? ")")
- var_assignment: ok
- while_loop: ok
