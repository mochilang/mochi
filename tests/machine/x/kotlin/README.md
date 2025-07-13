# Kotlin Machine Outputs

This directory stores Kotlin source files automatically generated from the Mochi programs in `tests/vm/valid`.  Each `.kt` file was produced by the Kotlin backend in `compiler/x/kotlin`.

## Program status

The table below lists every Mochi example and whether the generated Kotlin code compiled and ran successfully.  A checked box means the program produced the expected output and has a corresponding `.out` file.  Unchecked items have a `.error` file with compiler or runtime diagnostics.

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
- [x] go_auto.mochi
- [x] group_by.mochi
- [x] group_by_conditional_sum.mochi
- [x] group_by_having.mochi
- [ ] group_by_join.mochi
- [ ] group_by_left_join.mochi
- [x] group_by_multi_join.mochi
- [ ] group_by_multi_join_sort.mochi
- [ ] group_by_sort.mochi
- [ ] group_items_iteration.mochi
- [x] if_else.mochi
- [x] if_then_else.mochi
- [x] if_then_else_nested.mochi
- [x] in_operator.mochi
- [ ] in_operator_extended.mochi
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
- [ ] outer_join.mochi
- [x] partial_application.mochi
- [x] print_hello.mochi
- [x] pure_fold.mochi
- [x] pure_global_fold.mochi
- [x] python_auto.mochi
- [x] python_math.mochi
- [ ] query_sum_select.mochi
- [x] record_assign.mochi
- [ ] right_join.mochi
- [x] save_jsonl_stdout.mochi
- [x] short_circuit.mochi
- [x] slice.mochi
- [ ] sort_stable.mochi
- [x] str_builtin.mochi
- [x] string_compare.mochi
- [x] string_concat.mochi
- [x] string_contains.mochi
- [x] string_in_operator.mochi
- [x] string_index.mochi
- [x] string_prefix_slice.mochi
- [x] substring_builtin.mochi
- [x] sum_builtin.mochi
- [x] tail_recursion.mochi
- [x] test_block.mochi
- [x] tree_sum.mochi
- [x] two-sum.mochi
- [x] typed_let.mochi
- [x] typed_var.mochi
- [x] unary_neg.mochi
- [x] update_stmt.mochi
- [x] user_type_literal.mochi
- [x] values_builtin.mochi
- [x] var_assignment.mochi
- [x] while_loop.mochi
 - [x] q1.mochi (TPC-H)
 - [ ] q2.mochi (TPC-H)

## Running TPC-H q1

Follow the steps below to compile and run the first TPC‑H query:

1. **Install tools** &mdash; ensure `kotlinc` and `java` are available on your `PATH`.
2. **Generate the program** by running:

   ```bash
   go test ./compiler/x/kotlin -tags slow -run TestKotlinCompiler_TPCH -count=1
   ```

   This writes `q1.kt` and its output `q1.out` to this directory.
3. **Review** the generated Kotlin source and compare `q1.out` to the expected
   results in `tests/dataset/tpc-h/compiler/kt/q1.out`.

## TODO

- [ ] Implement dataset join and group-by operations in the Kotlin backend.
- [ ] Provide better support for the Python `math` helpers and other foreign imports.
- [ ] Finish compiling the remaining unchecked programs above.
- [ ] Get TPC-H `q2.mochi` compiling and running successfully.
- [ ] Improve numeric division so queries like `q1.mochi` compute floating point results correctly.

## Recent improvements

- Added support for pattern matching on union variants.
- Grouped query results can now be sorted by group keys.
- Fixed row type inference so grouped join results use proper map types.
- Corrected group key type inference so joins use `String` keys when possible.
