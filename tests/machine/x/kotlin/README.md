# Kotlin Machine Output

This directory contains Kotlin source files generated from Mochi programs along with their runtime output or error logs. The compiler now supports `group by` queries, but running the programs requires the Kotlin toolchain.

## Summary

- 38/97 programs compiled and executed successfully.
- 14 programs failed to compile or run.
- 45 programs have not been processed yet.

### Successful
append_builtin
avg_builtin
basic_compare
binary_precedence
bool_chain
break_continue
cast_string_to_int
cast_struct
closure
count_builtin
cross_join
cross_join_filter
cross_join_triple
dataset_sort_take_limit
for_list_collection
for_loop
for_map_collection
fun_call
fun_expr_in_let
fun_three_args
if_else
if_then_else
if_then_else_nested
in_operator
in_operator_extended
len_builtin
len_map
len_string
list_assign
list_index
list_nested_assign
map_assign
map_index
dataset_where_filter
exists_builtin
values_builtin
let_and_print
list_set_ops

### Failed
group_by
group_by_conditional_sum
group_by_having
group_by_join
group_by_left_join
group_by_multi_join
group_by_multi_join_sort
group_by_sort
group_items_iteration
inner_join
join_multi
json_builtin
left_join
left_join_multi
