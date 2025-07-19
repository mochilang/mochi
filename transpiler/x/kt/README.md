# Kotlin Transpiler

Generated Kotlin sources for golden tests are stored in `tests/transpiler/x/kt`.

The transpiler currently supports expression programs with `print`, integer and list literals, mutable variables and built-ins `count`, `sum`, `avg`, `len`, `str`, `append`, `min`, `max`, `substring` and `values`.

Completed golden tests: **31/100**

### Golden test checklist



- [x] append_builtin.mochi
- [x] avg_builtin.mochi
- [x] basic_compare.mochi
- [x] binary_precedence.mochi
- [x] bool_chain.mochi
- [ ] break_continue.mochi
- [ ] cast_string_to_int.mochi
- [ ] cast_struct.mochi
- [ ] closure.mochi
- [x] count_builtin.mochi
- [ ] cross_join.mochi
- [ ] cross_join_filter.mochi
- [ ] cross_join_triple.mochi
- [ ] dataset_sort_take_limit.mochi
- [ ] dataset_where_filter.mochi
- [ ] exists_builtin.mochi
- [ ] for_list_collection.mochi
- [ ] for_loop.mochi
- [ ] for_map_collection.mochi
- [ ] fun_call.mochi
- [ ] fun_expr_in_let.mochi
- [ ] fun_three_args.mochi
- [ ] go_auto.mochi
- [ ] group_by.mochi
- [ ] group_by_conditional_sum.mochi
- [ ] group_by_having.mochi
- [ ] group_by_join.mochi
- [ ] group_by_left_join.mochi
- [ ] group_by_multi_join.mochi
- [ ] group_by_multi_join_sort.mochi
- [ ] group_by_sort.mochi
- [ ] group_items_iteration.mochi
 - [x] if_else.mochi
 - [x] if_then_else.mochi
 - [x] if_then_else_nested.mochi
- [ ] in_operator.mochi
- [ ] in_operator_extended.mochi
- [ ] inner_join.mochi
- [ ] join_multi.mochi
- [ ] json_builtin.mochi
- [ ] left_join.mochi
- [ ] left_join_multi.mochi
- [x] len_builtin.mochi
- [x] len_map.mochi
- [x] len_string.mochi
- [x] let_and_print.mochi
- [ ] list_assign.mochi
- [x] list_index.mochi
- [ ] list_nested_assign.mochi
- [ ] list_set_ops.mochi
- [ ] load_yaml.mochi
- [ ] map_assign.mochi
- [ ] map_in_operator.mochi
- [ ] map_index.mochi
- [ ] map_int_key.mochi
- [ ] map_literal_dynamic.mochi
- [ ] map_membership.mochi
- [ ] map_nested_assign.mochi
- [ ] match_expr.mochi
- [ ] match_full.mochi
- [x] math_ops.mochi
- [ ] membership.mochi
- [x] min_max_builtin.mochi
- [ ] nested_function.mochi
- [ ] order_by_map.mochi
- [ ] outer_join.mochi
- [ ] partial_application.mochi
- [x] print_hello.mochi
- [ ] pure_fold.mochi
- [ ] pure_global_fold.mochi
- [ ] python_auto.mochi
- [ ] python_math.mochi
- [ ] query_sum_select.mochi
- [ ] record_assign.mochi
- [ ] right_join.mochi
- [ ] save_jsonl_stdout.mochi
- [ ] short_circuit.mochi
- [ ] slice.mochi
- [ ] sort_stable.mochi
- [x] str_builtin.mochi
- [x] string_compare.mochi
- [x] string_concat.mochi
- [x] string_contains.mochi
- [x] string_in_operator.mochi
- [x] string_index.mochi
- [ ] string_prefix_slice.mochi
- [x] substring_builtin.mochi
- [x] sum_builtin.mochi
- [ ] tail_recursion.mochi
- [ ] test_block.mochi
- [ ] tree_sum.mochi
- [ ] two-sum.mochi
- [x] typed_let.mochi
- [x] typed_var.mochi
- [x] unary_neg.mochi
- [ ] update_stmt.mochi
- [ ] user_type_literal.mochi
- [x] values_builtin.mochi
- [x] var_assignment.mochi
 - [x] while_loop.mochi

