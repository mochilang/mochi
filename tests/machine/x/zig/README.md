# Zig Machine Output

This directory should contain Zig source code generated from the Mochi compiler. Each test file in `tests/vm/valid` would normally be compiled and executed. In this environment the `zig` compiler is unavailable so the generation tests were skipped. The files present were produced by prior runs and may be outdated.

Compiled programs: 36/97

## Checklist
- [x] append_builtin
- [x] avg_builtin
- [x] basic_compare
- [x] binary_precedence
- [x] bool_chain
- [x] break_continue
- [x] cast_string_to_int
- [x] cast_struct
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
- [x] fun_call
- [ ] fun_expr_in_let
- [x] fun_three_args
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
- [x] in_operator_extended
- [ ] inner_join
- [ ] join_multi
- [ ] json_builtin
- [ ] left_join
- [ ] left_join_multi
- [x] len_builtin
- [ ] len_map
- [x] len_string
- [x] let_and_print
- [ ] list_assign
- [x] list_index
- [ ] list_nested_assign
- [x] list_set_ops
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
- [ ] math_ops
- [x] membership
- [x] min_max_builtin
- [ ] nested_function
- [ ] order_by_map
- [ ] outer_join
- [ ] partial_application
- [x] print_hello
- [x] pure_fold
- [x] pure_global_fold
- [ ] query_sum_select
- [ ] record_assign
- [ ] right_join
- [ ] save_jsonl_stdout
- [ ] short_circuit
- [x] slice
- [ ] sort_stable
- [x] str_builtin
- [ ] string_compare
- [x] string_concat
- [ ] string_contains
- [ ] string_in_operator
- [ ] string_index
- [x] string_prefix_slice
- [ ] substring_builtin
- [x] sum_builtin
- [x] tail_recursion
- [x] test_block
- [ ] tree_sum
- [ ] two-sum
- [ ] typed_let
- [ ] typed_var
- [x] unary_neg
- [ ] update_stmt
- [x] user_type_literal
- [ ] values_builtin
- [ ] var_assignment
- [ ] while_loop

## Remaining Tasks

The programs unchecked above either failed to compile or were not executed because the `zig` compiler was missing. Installing `zig` and re-running the tests will regenerate fresh outputs.
