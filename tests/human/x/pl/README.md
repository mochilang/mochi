# Manual Prolog Translations

This directory contains Prolog versions of the Mochi programs found in
`tests/vm/valid`. Every `.mochi` file has a corresponding `.pl` program
written by hand.  These translations were based on the original source
programs and reproduce their behaviour using standard Prolog.

## Files

```
 - README.md
 - append_builtin.pl
 - avg_builtin.pl
 - basic_compare.pl
 - binary_precedence.pl
 - bool_chain.pl
 - break_continue.pl
 - cast_string_to_int.pl
 - cast_struct.pl
 - closure.pl
 - count_builtin.pl
 - cross_join.pl
 - cross_join_filter.pl
 - cross_join_triple.pl
 - dataset_sort_take_limit.pl
 - dataset_where_filter.pl
 - exists_builtin.pl
 - for_list_collection.pl
 - for_loop.pl
 - for_map_collection.pl
 - fun_call.pl
 - fun_expr_in_let.pl
 - fun_three_args.pl
 - group_by.pl
 - group_by_conditional_sum.pl
 - group_by_having.pl
 - group_by_join.pl
 - group_by_left_join.pl
 - group_by_multi_join.pl
 - group_by_multi_join_sort.pl
 - group_by_sort.pl
 - group_items_iteration.pl
 - if_else.pl
 - if_then_else.pl
 - if_then_else_nested.pl
 - in_operator.pl
 - in_operator_extended.pl
 - inner_join.pl
 - join_multi.pl
 - json_builtin.pl
 - left_join.pl
 - left_join_multi.pl
 - len_builtin.pl
 - len_map.pl
 - len_string.pl
 - let_and_print.pl
 - list_assign.pl
 - list_index.pl
 - list_nested_assign.pl
 - list_set_ops.pl
 - load_yaml.pl
 - map_assign.pl
 - map_in_operator.pl
 - map_index.pl
 - map_int_key.pl
 - map_literal_dynamic.pl
 - map_membership.pl
 - map_nested_assign.pl
 - match_expr.pl
 - match_full.pl
 - math_ops.pl
 - membership.pl
 - min_max_builtin.pl
 - nested_function.pl
 - order_by_map.pl
 - outer_join.pl
 - partial_application.pl
 - print_hello.pl
 - pure_fold.pl
 - pure_global_fold.pl
 - query_sum_select.pl
 - record_assign.pl
 - right_join.pl
 - save_jsonl_stdout.pl
 - short_circuit.pl
 - slice.pl
 - sort_stable.pl
 - str_builtin.pl
 - string_compare.pl
 - string_concat.pl
 - string_contains.pl
 - string_in_operator.pl
 - string_index.pl
 - string_prefix_slice.pl
 - substring_builtin.pl
 - sum_builtin.pl
 - tail_recursion.pl
 - test_block.pl
 - tree_sum.pl
 - two-sum.pl
 - typed_let.pl
 - typed_var.pl
 - unary_neg.pl
 - update_stmt.pl
 - user_type_literal.pl
 - values_builtin.pl
 - var_assignment.pl
 - while_loop.pl
```
\nAll programs translated.
