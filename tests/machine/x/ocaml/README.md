# OCaml Machine Output

This directory contains OCaml source code generated from Mochi programs and the corresponding outputs/errors.

## Summary

- 61/97 programs compiled and executed successfully.
- 36 programs failed to compile or run.

### Successful
- append_builtin
- avg_builtin
- basic_compare
- binary_precedence
- bool_chain
- break_continue
- cast_string_to_int
- cast_struct
- closure
- count_builtin
- for_list_collection
- for_loop
- fun_call
- fun_expr_in_let
- fun_three_args
- if_else
- if_then_else
- if_then_else_nested
- in_operator
- len_builtin
- len_map
- len_string
- let_and_print
- list_assign
- list_index
- list_nested_assign
- list_set_ops
- map_assign
- map_in_operator
- map_index
- map_literal_dynamic
- map_membership
- math_ops
- membership
- min_max_builtin
- partial_application
- print_hello
- pure_fold
- pure_global_fold
- record_assign
- short_circuit
- slice
- str_builtin
- string_compare
- string_concat
- string_contains
- substring_builtin
- sum_builtin
- typed_let
- typed_var
- unary_neg
- user_type_literal
- match_expr
- match_full
- map_nested_assign
- var_assignment
- while_loop

### Failed
- cross_join
- cross_join_filter
- cross_join_triple
- dataset_sort_take_limit
- dataset_where_filter
- exists_builtin
- for_map_collection
- group_by
- group_by_conditional_sum
- group_by_having
- group_by_join
- group_by_left_join
- group_by_multi_join
- group_by_multi_join_sort
- group_by_sort
- group_items_iteration
- in_operator_extended
- inner_join
- join_multi
- json_builtin
- left_join
- left_join_multi
- load_yaml
- map_int_key
- nested_function
- order_by_map
- outer_join
- query_sum_select
- right_join
- save_jsonl_stdout
- sort_stable
- string_in_operator
- string_index
- string_prefix_slice
- tail_recursion
- test_block
- tree_sum
- two-sum
- update_stmt
- values_builtin

## Remaining Tasks

- [ ] Implement support for advanced query joins and aggregation.
 - [x] Handle nested map updates and struct patterns.
- [ ] Add runtime to run compiled programs during tests.
