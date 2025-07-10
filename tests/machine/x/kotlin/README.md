# Machine-generated Kotlin Programs

This directory contains Kotlin code compiled from Mochi programs in `tests/vm/valid` using the experimental compiler.

Each generated file now includes only the runtime helper functions that are actually used by that program.

## Progress

Compiled: 100/100 programs

Successfully ran: 83/100 programs

## Checklist

- [x] append_builtin.mochi
- [x] avg_builtin.mochi
- [x] basic_compare.mochi
- [x] binary_precedence.mochi
- [x] bool_chain.mochi
- [x] break_continue.mochi
- [x] cast_string_to_int.mochi
- [x] cast_struct.mochi
- [x] closure.mochi
- [x] count_builtin.mochi
- [x] cross_join.mochi
- [x] cross_join_filter.mochi
- [x] cross_join_triple.mochi
- [x] dataset_sort_take_limit.mochi
- [x] dataset_where_filter.mochi
- [x] exists_builtin.mochi
- [x] for_list_collection.mochi
- [x] for_loop.mochi
- [x] for_map_collection.mochi
- [x] fun_call.mochi
- [x] fun_expr_in_let.mochi
- [x] fun_three_args.mochi
- [x] group_by.mochi
- [x] group_by_conditional_sum.mochi
 - [x] group_by_having.mochi
- [x] group_by_join.mochi
- [x] group_by_left_join.mochi
- [x] group_by_multi_join.mochi
 - [ ] group_by_multi_join_sort.mochi
 - [ ] group_by_sort.mochi
 - [ ] group_items_iteration.mochi
- [x] if_else.mochi
- [x] if_then_else.mochi
- [x] if_then_else_nested.mochi
- [x] in_operator.mochi
- [x] in_operator_extended.mochi
- [x] inner_join.mochi
- [x] join_multi.mochi
- [x] json_builtin.mochi
- [x] left_join.mochi
- [x] left_join_multi.mochi
- [x] len_builtin.mochi
- [x] len_map.mochi
- [x] len_string.mochi
- [x] let_and_print.mochi
- [x] list_assign.mochi
- [x] list_index.mochi
- [x] list_nested_assign.mochi
- [x] list_set_ops.mochi
- [x] load_yaml.mochi
- [x] map_assign.mochi
- [x] map_in_operator.mochi
- [x] map_index.mochi
- [x] map_int_key.mochi
- [x] map_literal_dynamic.mochi
- [x] map_membership.mochi
- [x] map_nested_assign.mochi
- [x] match_expr.mochi
- [x] match_full.mochi
- [x] math_ops.mochi
- [x] membership.mochi
- [x] min_max_builtin.mochi
 - [x] nested_function.mochi
- [ ] order_by_map.mochi
- [x] outer_join.mochi
- [x] partial_application.mochi
- [x] print_hello.mochi
- [x] pure_fold.mochi
- [x] pure_global_fold.mochi
- [ ] query_sum_select.mochi
- [x] record_assign.mochi
- [x] right_join.mochi
- [x] save_jsonl_stdout.mochi
- [x] short_circuit.mochi
- [x] slice.mochi
 - [ ] sort_stable.mochi
- [x] str_builtin.mochi
 - [ ] string_compare.mochi
- [x] string_concat.mochi
- [x] string_contains.mochi
- [x] string_in_operator.mochi
- [x] string_index.mochi
- [x] string_prefix_slice.mochi
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
- [x] user_type_literal.mochi
- [x] values_builtin.mochi
- [x] var_assignment.mochi
- [x] while_loop.mochi
- [ ] go_auto.mochi
- [ ] python_auto.mochi
- [ ] python_math.mochi

## Remaining Work

- [ ] Compare output with reference implementations
- [x] Emit data classes for uniform map lists
