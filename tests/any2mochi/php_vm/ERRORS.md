# Errors

- append_builtin: ok
- avg_builtin: ok
- basic_compare: ok
- binary_precedence: ok
- bool_chain: ok
- break_continue: ok
- cast_string_to_int: ok
- cast_struct: type error: error[T002]: undefined variable: todo
  --> :6:7

help:
  Check if the variable was declared in this scope.
- closure: ok
- count_builtin: ok
- cross_join: parse error: parse error: 2:24: unexpected token "Alice" (expected "]")
- cross_join_filter: parse error: parse error: 3:18: unexpected token "A" (expected "]")
- cross_join_triple: parse error: parse error: 3:18: unexpected token "A" (expected "]")
- dataset_sort_take_limit: parse error: parse error: 3:20: unexpected token "Laptop" (expected "]")
- dataset_where_filter: parse error: parse error: 2:18: unexpected token "Alice" (expected "]")
- exists_builtin: type error: error[T002]: undefined variable: flag
  --> :3:7

help:
  Check if the variable was declared in this scope.
- for_list_collection: ok
- for_loop: ok
- for_map_collection: ok
- fun_call: ok
- fun_expr_in_let: ok
- fun_three_args: ok
- group_by: parse error: parse error: 8:18: unexpected token "Alice" (expected "]")
- group_by_conditional_sum: type error: error[T002]: undefined variable: result
  --> :3:7

help:
  Check if the variable was declared in this scope.
- group_by_having: parse error: parse error: 7:18: unexpected token "Alice" (expected "]")
- group_by_join: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- group_by_left_join: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- group_by_multi_join: parse error: parse error: 9:22: unexpected token "A" (expected "]")
- group_by_multi_join_sort: parse error: parse error: 3:21: unexpected token "BRAZIL" (expected "]")
- group_by_sort: parse error: parse error: 3:17: unexpected token "a" (expected "]")
- group_items_iteration: parse error: parse error: 9:16: unexpected token "a" (expected "]")
- if_else: ok
- if_then_else: type error: error[T002]: undefined variable: msg
  --> :3:7

help:
  Check if the variable was declared in this scope.
- if_then_else_nested: type error: error[T002]: undefined variable: msg
  --> :3:7

help:
  Check if the variable was declared in this scope.
- in_operator: ok
- in_operator_extended: type error: error[T002]: undefined variable: hello
  --> :4:11

help:
  Check if the variable was declared in this scope.
- inner_join: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- join_multi: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- json_builtin: ok
- left_join: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- left_join_multi: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- len_builtin: ok
- len_map: ok
- len_string: ok
- let_and_print: ok
- list_assign: ok
- list_index: ok
- list_nested_assign: ok
- list_set_ops: ok
- load_yaml: ok
- map_assign: parse error: parse error: 3:16: unexpected token "bob" (expected "]")
- map_in_operator: parse error: parse error: 2:12: unexpected token "a" (expected "]")
- map_index: parse error: parse error: 3:11: unexpected token "b" (expected "]")
- map_int_key: parse error: parse error: 2:12: unexpected token "a" (expected "]")
- map_literal_dynamic: ok
- map_membership: ok
- map_nested_assign: parse error: parse error: 3:14: unexpected token "outer" (expected "]")
- match_expr: compile error: unsupported expression
- match_full: compile error: unsupported expression
- math_ops: ok
- membership: ok
- min_max_builtin: ok
- nested_function: ok
- order_by_map: type error: error[T002]: undefined variable: sorted
  --> :4:7

help:
  Check if the variable was declared in this scope.
- outer_join: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- partial_application: ok
- print_hello: parse error: parse error: 2:9: unexpected token "hello" (expected ")")
- pure_fold: ok
- pure_global_fold: ok
- query_sum_select: type error: error[T002]: undefined variable: result
  --> :3:7

help:
  Check if the variable was declared in this scope.
- record_assign: type error: error[T002]: undefined variable: c
  --> :7:7

help:
  Check if the variable was declared in this scope.
- right_join: parse error: parse error: 3:24: unexpected token "Alice" (expected "]")
- save_jsonl_stdout: parse error: parse error: 2:18: unexpected token "Alice" (expected "]")
- short_circuit: ok
- slice: ok
- sort_stable: parse error: parse error: 3:20: unexpected token "a" (expected "]")
- str_builtin: ok
- string_compare: ok
- string_concat: ok
- string_contains: type error: error[T002]: undefined variable: catch
  --> :2:11

help:
  Check if the variable was declared in this scope.
- string_in_operator: type error: error[T002]: undefined variable: catch
  --> :2:11

help:
  Check if the variable was declared in this scope.
- string_index: type error: error[T002]: undefined variable: mochi
  --> :2:11

help:
  Check if the variable was declared in this scope.
- string_prefix_slice: type error: error[T002]: undefined variable: fore
  --> :2:16

help:
  Check if the variable was declared in this scope.
- substring_builtin: ok
- sum_builtin: ok
- tail_recursion: ok
- test_block: parse error: parse error: 3:9: unexpected token "ok" (expected ")")
- tree_sum: compile error: unsupported expression
- two-sum: type error: error[T002]: undefined variable: result
  --> :3:7

help:
  Check if the variable was declared in this scope.
- typed_let: type error: error[T002]: undefined variable: y
  --> :2:7

help:
  Check if the variable was declared in this scope.
- typed_var: type error: error[T002]: undefined variable: x
  --> :2:7

help:
  Check if the variable was declared in this scope.
- unary_neg: ok
- update_stmt: parse error: parse error: 9:9: unexpected token "ok" (expected ")")
- user_type_literal: type error: error[T002]: undefined variable: book
  --> :12:7

help:
  Check if the variable was declared in this scope.
- values_builtin: ok
- var_assignment: ok
- while_loop: ok
