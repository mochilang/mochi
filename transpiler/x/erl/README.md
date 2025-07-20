# Erlang Transpiler Output (34/100 generated and run)

This directory contains a minimal transpiler that converts a very small
subset of Mochi into Erlang. Generated programs are executed with
`escript` to verify runtime behaviour.

## VM Valid Checklist

The following programs under `tests/vm/valid` have golden outputs. A
checked item means the Erlang transpiler can successfully generate code
that produces the same output as the Mochi VM.

- [x] append_builtin
- [x] avg_builtin
- [x] basic_compare
- [x] binary_precedence
- [x] bool_chain
- [ ] break_continue
- [x] cast_string_to_int
- [ ] cast_struct
- [ ] closure
- [x] count_builtin
- [ ] cross_join
- [ ] cross_join_filter
- [ ] cross_join_triple
- [ ] dataset_sort_take_limit
- [ ] dataset_where_filter
- [ ] exists_builtin
- [ ] for_list_collection
- [ ] for_loop
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
- [ ] group_by_sort
- [ ] group_items_iteration
- [x] if_else
- [x] if_then_else
- [x] if_then_else_nested
- [x] in_operator
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
- [x] list_index
- [ ] list_nested_assign
- [ ] list_set_ops
- [ ] load_yaml
- [ ] map_assign
- [x] map_in_operator
- [x] map_index
- [x] map_int_key
- [ ] map_literal_dynamic
- [x] map_membership
- [ ] map_nested_assign
- [ ] match_expr
- [ ] match_full
- [x] math_ops
- [x] membership
- [x] min_max_builtin
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
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [ ] string_contains
- [ ] string_in_operator
- [x] string_index
- [ ] string_prefix_slice
- [ ] substring_builtin
- [x] sum_builtin
- [ ] tail_recursion
- [ ] test_block
- [ ] tree_sum
- [ ] two-sum
- [x] typed_let
- [x] typed_var
- [x] unary_neg
- [ ] update_stmt
- [ ] user_type_literal
- [x] values_builtin
- [x] var_assignment
- [ ] while_loop
