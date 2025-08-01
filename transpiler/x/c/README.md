# C Transpiler Golden Tests

This directory stores C translations generated from programs in `tests/vm/valid`. Each file is compiled and executed during tests. Successful runs keep the generated `.c` source along with a matching `.out` file. Failures are recorded in `.error` files when tests run with `-update`.

Checklist of programs that currently transpile and run (82/105) - Last updated 2025-08-02 12:08 +0700:
- [x] append_builtin
- [x] avg_builtin
- [x] basic_compare
- [x] bench_block
- [x] binary_precedence
- [x] bool_chain
- [x] break_continue
- [x] cast_string_to_int
- [x] cast_struct
- [x] closure
- [x] count_builtin
- [x] cross_join
- [x] cross_join_filter
- [x] cross_join_triple
- [x] dataset_sort_take_limit
- [x] dataset_where_filter
- [x] exists_builtin
- [x] for_list_collection
- [x] for_loop
- [x] for_map_collection
- [x] fun_call
- [x] fun_expr_in_let
- [x] fun_three_args
- [x] go_auto
- [x] group_by
- [x] group_by_conditional_sum
- [x] group_by_having
- [x] group_by_join
- [x] group_by_left_join
- [x] group_by_multi_join
- [x] group_by_multi_join_sort
- [ ] group_by_multi_sort
- [x] group_by_sort
- [ ] group_items_iteration
- [x] if_else
- [x] if_then_else
- [x] if_then_else_nested
- [x] in_operator
- [x] in_operator_extended
- [x] inner_join
- [x] join_multi
- [x] json_builtin
- [x] left_join
- [x] left_join_multi
- [x] len_builtin
- [x] len_map
- [x] len_string
- [x] let_and_print
- [x] list_assign
- [x] list_index
- [x] list_nested_assign
- [x] list_set_ops
- [ ] load_jsonl
- [ ] load_yaml
- [ ] map_assign
- [ ] map_in_operator
- [x] map_index
- [x] map_int_key
- [ ] map_literal_dynamic
- [ ] map_membership
- [ ] map_nested_assign
- [x] match_expr
- [x] match_full
- [x] math_ops
- [x] membership
- [x] min_max_builtin
- [ ] mix_go_python
- [ ] nested_function
- [ ] order_by_map
- [ ] outer_join
- [ ] pairs_loop
- [ ] partial_application
- [x] print_hello
- [x] pure_fold
- [x] pure_global_fold
- [ ] python_auto
- [ ] python_math
- [ ] query_sum_select
- [x] record_assign
- [ ] right_join
- [ ] save_jsonl_stdout
- [x] short_circuit
- [x] slice
- [ ] sort_stable
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [x] string_contains
- [x] string_in_operator
- [x] string_index
- [x] string_prefix_slice
- [x] substring_builtin
- [x] sum_builtin
- [x] tail_recursion
- [x] test_block
- [ ] tree_sum
- [x] two-sum
- [x] typed_let
- [x] typed_var
- [x] unary_neg
- [ ] update_stmt
- [x] user_type_literal
- [x] values_builtin
- [x] var_assignment
- [x] while_loop
