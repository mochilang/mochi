# a2mochi Clojure Converter

This directory holds golden outputs for the Clojure to Mochi converter. Each `.clj` source in `tests/transpiler/x/clj` has a matching `.mochi` and `.ast` file generated by the tests.

Completed programs: 12/102
Date: 2025-07-30 00:53 +07

## Checklist
- [ ] append_builtin
- [ ] avg_builtin
- [ ] basic_compare
- [x] bench_block
- [ ] binary_precedence
- [ ] bool_chain
- [ ] break_continue
- [ ] cast_string_to_int
- [ ] cast_struct
- [ ] closure
- [x] count_builtin
- [ ] cross_join
- [ ] cross_join_filter
- [ ] cross_join_triple
- [ ] dataset_sort_take_limit
- [ ] dataset_where_filter
- [ ] exists_builtin
- [x] for_list_collection
- [x] for_loop
- [ ] for_map_collection
- [ ] fun_call
- [ ] fun_expr_in_let
- [ ] fun_three_args
- [ ] go_auto
- [ ] group_by
- [ ] group_by_conditional_sum
- [ ] group_by_having
- [ ] group_by_join
- [ ] group_by_left_join
- [ ] group_by_multi_join
- [ ] group_by_multi_join_sort
- [ ] group_by_multi_sort
- [ ] group_by_sort
- [ ] group_items_iteration
- [ ] if_else
- [ ] if_then_else
- [ ] if_then_else_nested
- [ ] in_operator
- [ ] in_operator_extended
- [ ] inner_join
- [ ] join_multi
- [ ] json_builtin
- [ ] left_join
- [ ] left_join_multi
- [x] len_builtin
- [x] len_map
- [x] len_string
- [x] let_and_print
- [ ] list_assign
- [ ] list_index
- [ ] list_nested_assign
- [ ] list_set_ops
- [ ] load_yaml
- [ ] map_assign
- [ ] map_in_operator
- [ ] map_index
- [ ] map_int_key
- [ ] map_literal_dynamic
- [ ] map_membership
- [ ] map_nested_assign
- [ ] match_expr
- [ ] match_full
- [x] math_ops
- [ ] membership
- [ ] min_max_builtin
- [ ] nested_function
- [ ] order_by_map
- [ ] outer_join
- [ ] partial_application
- [x] print_hello
- [ ] pure_fold
- [ ] pure_global_fold
- [ ] python_auto
- [ ] python_math
- [ ] query_sum_select
- [ ] record_assign
- [ ] right_join
- [ ] save_jsonl_stdout
- [ ] short_circuit
- [ ] slice
- [ ] sort_stable
- [ ] str_builtin
- [ ] string_compare
- [ ] string_concat
- [ ] string_contains
- [ ] string_in_operator
- [ ] string_index
- [ ] string_prefix_slice
- [ ] substring_builtin
- [ ] sum_builtin
- [ ] tail_recursion
- [ ] test_block
- [ ] tree_sum
- [ ] two-sum
- [ ] typed_let
- [x] typed_var
- [ ] unary_neg
- [ ] update_stmt
- [ ] user_type_literal
- [ ] values_builtin
- [ ] var_assignment
- [x] while_loop
