# OCaml Machine Output

Generated `.ml` files in this directory are produced by the Mochi to OCaml compiler.
If an OCaml toolchain is available each program is built and executed.  Successful
runs produce a `.out` file while failures generate a `.error` report.

## Compiled and ran successfully

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
- [x] len_builtin
- [x] len_map
- [x] len_string
- [x] let_and_print
- [x] list_assign
- [x] list_index
- [x] map_assign
- [x] map_in_operator
- [x] map_index
- [x] map_literal_dynamic
- [x] map_membership
- [x] math_ops
- [x] membership
- [x] min_max_builtin
- [x] partial_application
- [x] print_hello
- [x] pure_fold
- [x] pure_global_fold
- [x] record_assign
- [x] short_circuit
- [x] slice
- [x] str_builtin
- [x] string_compare
- [x] string_concat
- [x] string_contains
- [x] substring_builtin
- [x] cross_join
- [x] cross_join_filter
- [x] cross_join_triple
- [x] group_by
- [x] group_by_conditional_sum
- [x] group_by_having
- [x] group_by_join
- [x] group_by_left_join
- [x] group_by_multi_join
- [x] group_by_multi_join_sort
- [x] group_by_sort
- [x] inner_join
- [x] join_multi
- [x] left_join
- [x] left_join_multi
- [x] outer_join
- [x] right_join
- [x] typed_let
- [x] typed_var
- [x] unary_neg
- [x] user_type_literal
- [x] var_assignment
- [x] while_loop
- [x] match_expr
- [x] match_full
- [x] tree_sum
- [x] group_items_iteration
- [x] two-sum

## Failed to build or run

- [ ] load_yaml
 - [x] nested_function
- [ ] save_jsonl_stdout
- [ ] test_block
- [ ] update_stmt

## Remaining Tasks
- [ ] Implement support for advanced query joins and aggregation.
- [ ] Add runtime to run compiled programs during tests.
