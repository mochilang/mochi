# Errors

- append_builtin: parse error: parse error: 5:1: unexpected token "}" (expected ")")
- avg_builtin: panic: runtime error: index out of range [1] with length 1
- basic_compare: type error: error[T024]: cannot assign to `a` (immutable)
  --> :4:3

help:
  Use `var` to declare mutable variables.
- binary_precedence: parse error: parse error: 3:3: unexpected token "print" (expected ")")
- bool_chain: parse error: parse error: 7:3: unexpected token "print" (expected ")")
- break_continue: type error: error[T024]: cannot assign to `numbers` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- cast_string_to_int: ok
- cast_struct: type error: error[T024]: cannot assign to `todo` (immutable)
  --> :6:3

help:
  Use `var` to declare mutable variables.
- closure: parse error: parse error: 1:12: unexpected token "(" (expected TypeRef)
- count_builtin: parse error: parse error: 7:1: unexpected token "}" (expected ")")
- cross_join: parse error: parse error: 22:7: lexer: invalid input text "\")\n  }\n}\n"
- cross_join_filter: type error: error[T024]: cannot assign to `nums` (immutable)
  --> :5:3

help:
  Use `var` to declare mutable variables.
- cross_join_triple: parse error: parse error: 15:5: unexpected token "}" (expected "{" Statement* "}")
- dataset_sort_take_limit: panic: runtime error: index out of range [1] with length 1
- dataset_where_filter: parse error: parse error: 13:24: lexer: invalid input text "? \" (senior)\n  }..."
- exists_builtin: parse error: parse error: 6:16: unexpected token "where" (expected ")")
- for_list_collection: ok
- for_loop: ok
- for_map_collection: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- fun_call: ok
- fun_expr_in_let: parse error: parse error: 1:13: unexpected token "(" (expected TypeRef)
- fun_three_args: ok
- group_by: panic: runtime error: index out of range [1] with length 1
- group_by_conditional_sum: panic: runtime error: index out of range [1] with length 1
- group_by_having: panic: runtime error: index out of range [1] with length 1
- group_by_join: panic: runtime error: index out of range [1] with length 1
- group_by_left_join: panic: runtime error: index out of range [1] with length 1
- group_by_multi_join: panic: runtime error: index out of range [1] with length 1
- group_by_multi_join_sort: panic: runtime error: index out of range [1] with length 1
- group_by_sort: panic: runtime error: index out of range [1] with length 1
- group_items_iteration: panic: runtime error: index out of range [1] with length 1
- if_else: type error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- if_then_else: parse error: parse error: 5:18: lexer: invalid input text "? \"yes\" : \"no\"\n ..."
- if_then_else_nested: parse error: parse error: 5:18: lexer: invalid input text "? \"big\" : ((x > ..."
- in_operator: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- in_operator_extended: parse error: parse error: 8:27: unexpected token ")" (expected "}")
- inner_join: panic: runtime error: index out of range [1] with length 1
- join_multi: panic: runtime error: index out of range [1] with length 1
- json_builtin: panic: runtime error: index out of range [1] with length 1
- left_join: panic: runtime error: index out of range [1] with length 1
- left_join_multi: panic: runtime error: index out of range [1] with length 1
- len_builtin: parse error: parse error: 6:14: unexpected token ")" (expected PostfixExpr)
- len_map: parse error: parse error: 6:1: unexpected token "}" (expected ")")
- len_string: ok
- let_and_print: type error: error[T024]: cannot assign to `a` (immutable)
  --> :4:3

help:
  Use `var` to declare mutable variables.
- list_assign: type error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- list_index: type error: error[T024]: cannot assign to `xs` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- list_nested_assign: type error: error[T024]: cannot assign to `matrix` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- list_set_ops: parse error: parse error: 9:3: unexpected token "print" (expected ")")
- load_yaml: panic: runtime error: index out of range [1] with length 1
- map_assign: type error: error[T024]: cannot assign to `scores` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_in_operator: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- map_index: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_int_key: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_literal_dynamic: type error: error[T024]: cannot assign to `x` (immutable)
  --> :5:3

help:
  Use `var` to declare mutable variables.
- map_membership: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- map_nested_assign: type error: error[T024]: cannot assign to `data` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- match_expr: panic: runtime error: index out of range [1] with length 1
- match_full: panic: runtime error: index out of range [1] with length 1
- math_ops: parse error: parse error: 4:3: unexpected token "print" (expected ")")
- membership: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- min_max_builtin: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- nested_function: parse error: parse error: 7:1: unexpected token "}" (expected ")")
- order_by_map: panic: runtime error: index out of range [1] with length 1
- outer_join: panic: runtime error: index out of range [1] with length 1
- partial_application: parse error: parse error: 8:1: unexpected token "}" (expected ")")
- print_hello: ok
- pure_fold: parse error: parse error: 6:1: unexpected token "}" (expected ")")
- pure_global_fold: parse error: parse error: 8:1: unexpected token "}" (expected ")")
- query_sum_select: panic: runtime error: index out of range [1] with length 1
- record_assign: type error: error[T009]: cannot assign int to `c` (expected Counter)
  --> :6:3

help:
  Make sure the assigned value is compatible with `c`.
- right_join: panic: runtime error: index out of range [1] with length 1
- save_jsonl_stdout: panic: runtime error: index out of range [1] with length 1
- short_circuit: parse error: parse error: 7:3: unexpected token "print" (expected ")")
- slice: parse error: parse error: 7:3: unexpected token "print" (expected ")")
- sort_stable: panic: runtime error: index out of range [1] with length 1
- str_builtin: ok
- string_compare: ok
- string_concat: ok
- string_contains: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- string_in_operator: parse error: parse error: 5:3: unexpected token "print" (expected ")")
- string_index: parse error: parse error: 5:1: unexpected token "}" (expected ")")
- string_prefix_slice: parse error: parse error: 8:3: unexpected token "s2" (expected ")")
- substring_builtin: ok
- sum_builtin: panic: runtime error: index out of range [1] with length 1
- tail_recursion: parse error: parse error: 9:1: unexpected token "}" (expected ")")
- test_block: ok
- tree_sum: parse error: parse error: 6:3: unexpected token "left" (expected "}")
- two-sum: panic: runtime error: index out of range [1] with length 1
- typed_let: type error: error[T002]: undefined variable: undefined
  --> :3:7

help:
  Check if the variable was declared in this scope.
- typed_var: type error: error[T002]: undefined variable: undefined
  --> :3:7

help:
  Check if the variable was declared in this scope.
- unary_neg: parse error: parse error: 4:1: unexpected token "}" (expected ")")
- update_stmt: panic: runtime error: index out of range [1] with length 1
- user_type_literal: type error: error[T024]: cannot assign to `book` (immutable)
  --> :11:3

help:
  Use `var` to declare mutable variables.
- values_builtin: parse error: parse error: 5:1: unexpected token "}" (expected ")")
- var_assignment: type error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- while_loop: type error: error[T024]: cannot assign to `i` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
