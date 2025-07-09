# F# Machine Output

This directory contains F# source code generated from Mochi programs in `tests/vm/valid`.
All files were produced using the minimal F# compiler in `compiler/x/fs`.

## Generated Programs (37)

The following programs were successfully compiled to F#. Runtime execution is not
performed in this environment, so only the source files are provided.

- [x] append_builtin
- [x] avg_builtin
- [x] basic_compare
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
- [x] group_by
- [x] group_by_conditional_sum
- [x] group_by_having
- [x] group_by_join
- [x] group_by_left_join
- [x] group_by_multi_join
- [x] group_by_multi_join_sort
- [x] group_by_sort
- [x] group_items_iteration
- [x] if_else
- [x] if_then_else
- [x] if_then_else_nested
- [x] in_operator
- [x] in_operator_extended
- [x] inner_join

## Remaining Work

- Integrate an F# toolchain to compile and execute the generated programs.
