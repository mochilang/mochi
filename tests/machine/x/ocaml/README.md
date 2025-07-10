# OCaml Machine Output

Generated `.ml` files are produced by running the Go compiler for OCaml.
When `ocamlc` is available each program is built and executed. Programs
that run successfully have a `.out` file while failures generate a `.error` report.

## Compiled successfully
- [x] append_builtin
- [x] avg_builtin
- [x] basic_compare
- [x] binary_precedence
- [x] bool_chain
- [x] break_continue
- [x] cast_string_to_int
- [x] cast_struct
- [x] count_builtin
- [x] cross_join
- [x] cross_join_filter
- [x] cross_join_triple
- [x] dataset_sort_take_limit
- [x] dataset_where_filter
- [x] exists_builtin
- [x] for_list_collection
- [x] for_loop
- [x] fun_call
- [x] fun_expr_in_let
- [x] fun_three_args
- [x] if_else
- [x] if_then_else
- [x] if_then_else_nested
- [x] in_operator
- [x] inner_join
- [x] join_multi
- [x] len_builtin
- [x] len_map
- [x] len_string
- [x] let_and_print
- [x] list_assign
- [x] list_index
- [x] list_set_ops
- [x] map_in_operator
- [x] map_index
- [x] map_int_key
- [x] map_membership
- [x] match_expr
- [x] match_full
- [x] math_ops
- [x] membership
- [x] min_max_builtin
- [x] nested_function
- [x] partial_application
- [x] print_hello
- [x] pure_fold
- [x] pure_global_fold
- [x] record_assign
- [x] short_circuit
- [x] slice
- [x] sort_stable
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [x] substring_builtin
- [x] sum_builtin
- [x] tree_sum
- [x] typed_let
- [x] typed_var
- [x] unary_neg
- [x] user_type_literal
- [x] var_assignment
- [x] while_loop

## Failed to build or run
- [ ] closure
- [ ] for_map_collection
- [ ] group_by
- [ ] group_by_conditional_sum
- [ ] group_by_having
- [ ] group_by_join
- [ ] group_by_left_join
- [ ] group_by_multi_join
- [ ] group_by_multi_join_sort
- [ ] group_by_sort
- [ ] group_items_iteration
- [ ] in_operator_extended
- [ ] json_builtin
- [ ] left_join
- [ ] left_join_multi
- [ ] list_nested_assign
 - [x] load_yaml
- [ ] map_assign
- [ ] map_literal_dynamic
- [ ] map_nested_assign
- [ ] order_by_map
- [ ] outer_join
- [ ] query_sum_select
- [ ] right_join
 - [x] save_jsonl_stdout
- [ ] string_contains
- [ ] string_in_operator
- [ ] string_index
- [ ] string_prefix_slice
- [ ] tail_recursion
 - [x] test_block
- [ ] two-sum
 - [x] update_stmt
- [ ] values_builtin

## Remaining Tasks
- [ ] Improve support for complex query groups and joins
- [ ] Integrate an OCaml runtime to execute compiled programs in CI
