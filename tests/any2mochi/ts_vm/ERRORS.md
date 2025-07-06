# Errors

- append_builtin: type error: error[T024]: cannot assign to `a` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- avg_builtin: panic: runtime error: index out of range [1] with length 1
- basic_compare: type error: error[T024]: cannot assign to `a` (immutable)
  --> :4:3

help:
  Use `var` to declare mutable variables.
- binary_precedence: ok
- bool_chain: ok
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
- count_builtin: type error: error[T003]: unknown function: _count
  --> :2:9

help:
  Ensure the function is defined before it's called.
- cross_join: parse error: parse error: 12:3: unexpected token "}" (expected "{" Statement* "}")
- cross_join_filter: type error: error[T024]: cannot assign to `nums` (immutable)
  --> :5:3

help:
  Use `var` to declare mutable variables.
- cross_join_triple: parse error: parse error: 15:5: unexpected token "}" (expected "{" Statement* "}")
- dataset_sort_take_limit: panic: runtime error: index out of range [1] with length 1
- dataset_where_filter: parse error: parse error: 13:24: lexer: invalid input text "? \" (senior)\" : ..."
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
- in_operator: type error: error[T024]: cannot assign to `xs` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- in_operator_extended: parse error: parse error: 8:27: unexpected token ")" (expected "}")
- inner_join: panic: runtime error: index out of range [1] with length 1
- join_multi: panic: runtime error: index out of range [1] with length 1
- json_builtin: panic: runtime error: index out of range [1] with length 1
- left_join: panic: runtime error: index out of range [1] with length 1
- left_join_multi: panic: runtime error: index out of range [1] with length 1
- len_builtin: ok
- len_map: type error: error[T002]: undefined variable: Object
  --> :2:9

help:
  Check if the variable was declared in this scope.
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
- list_set_ops: type error: error[T003]: unknown function: _union
  --> :2:9

help:
  Ensure the function is defined before it's called.
- load_yaml: panic: runtime error: index out of range [1] with length 1
- map_assign: type error: error[T024]: cannot assign to `scores` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_in_operator: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
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
- map_membership: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_nested_assign: type error: error[T024]: cannot assign to `data` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- match_expr: panic: runtime error: index out of range [1] with length 1
- match_full: panic: runtime error: index out of range [1] with length 1
- math_ops: type error: error[T002]: undefined variable: Math
  --> :3:9

help:
  Check if the variable was declared in this scope.
- membership: type error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- min_max_builtin: parse error: parse error: 8:19: unexpected token "|" (expected "}")
- nested_function: type error: error[T002]: undefined variable: x
  --> :3:11

help:
  Check if the variable was declared in this scope.
- order_by_map: panic: runtime error: index out of range [1] with length 1
- outer_join: panic: runtime error: index out of range [1] with length 1
- partial_application: type error: error[T024]: cannot assign to `add5` (immutable)
  --> :6:3

help:
  Use `var` to declare mutable variables.
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type error: error[T024]: cannot assign to `k` (immutable)
  --> :6:3

help:
  Use `var` to declare mutable variables.
- query_sum_select: panic: runtime error: index out of range [1] with length 1
- record_assign: type error: error[T009]: cannot assign int to `c` (expected Counter)
  --> :6:3

help:
  Make sure the assigned value is compatible with `c`.
- right_join: panic: runtime error: index out of range [1] with length 1
- save_jsonl_stdout: panic: runtime error: index out of range [1] with length 1
- short_circuit: ok
- slice: parse error: parse error: 19:3: unexpected token ")" (expected "}")
- sort_stable: panic: runtime error: index out of range [1] with length 1
- str_builtin: ok
- string_compare: ok
- string_concat: ok
- string_contains: type error: error[T024]: cannot assign to `s` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- string_in_operator: type error: error[T024]: cannot assign to `s` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- string_index: parse error: parse error: 8:3: unexpected token ")" (expected "}")
- string_prefix_slice: parse error: parse error: 16:3: unexpected token ")" (expected "}")
- substring_builtin: ok
- sum_builtin: panic: runtime error: index out of range [1] with length 1
- tail_recursion: ok
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
- unary_neg: ok
- update_stmt: panic: runtime error: index out of range [1] with length 1
- user_type_literal: type error: error[T024]: cannot assign to `book` (immutable)
  --> :11:3

help:
  Use `var` to declare mutable variables.
- values_builtin: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- var_assignment: type error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- while_loop: type error: error[T024]: cannot assign to `i` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
