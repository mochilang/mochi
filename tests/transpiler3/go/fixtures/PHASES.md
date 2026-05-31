# MEP-54 Go Transpiler — Fixture Phase Map

Every fixture directory under `tests/transpiler3/go/fixtures/` belongs to
exactly one phase. The gate test for each phase runs only the fixtures that
match its prefix list.

`TestPhase1Hello` (phase01_test.go) continues to run ALL fixtures as a
catch-all regression gate. Per-phase tests are additive: a failure in
`TestPhase1Hello` also appears in the specific phase test, making it easy
to bisect which phase a regression belongs to.

## Phase 2 — Scalars, arithmetic, control flow, strings (`phase02_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `int_` | int_arith, int_cmp, int_mod, int_neg |
| `float_` | float_arith, float_cmp |
| `bool_` | bool_ops |
| `string_` | string_concat |
| `str_bool`, `str_concat`, `str_float`, `str_int` | str_bool, str_concat, str_float, str_int |
| `str_in_condition`, `str_in_function`, `str_string_identity` | str_in_condition, str_in_function, str_string_identity |
| `if_` | if_else |
| `while_` | while_loop |
| `for_range` | for_range |
| `divzero` | (no fixture; gate validates absence) |
| `nan_` | (no fixture; gate validates absence) |
| `user_fn_` | (no fixture; user_fn_ prefix reserved) |
| `fun_` | fun_basic, fun_bool_arg, fun_call_in_arith, fun_call_in_if, fun_call_in_loop, fun_call_twice, fun_list_arg, fun_no_args, fun_pass_var, fun_record_arg, fun_record_return, fun_recursive, fun_return_bool, fun_return_float, fun_return_int, fun_return_string, fun_string_arg, fun_two_funs, fun_unit_return |
| `var_let` | var_let |

## Phase 3.1 — Lists (`phase03_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `list_` | list_append, list_bools, list_contains_bool, list_contains_int, list_contains_str, list_empty, list_floats, list_for_each, list_index, list_len, list_lit, list_max_int, list_max_string, list_min_float, list_min_int, list_nested_loop, list_set, list_strings, list_sum_loop |

Note: `list_record_*` fixtures belong to Phase 3.4.

## Phase 3.2 — Maps (`phase03_2_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `map_` | map_bool_values, map_float_values, map_get, map_has, map_keys_sorted, map_len, map_lit, map_put, map_string_keys, map_values_sorted |

## Phase 3.3 — Sets (`phase03_3_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `set_` | set_add, set_count_unique, set_dedup, set_empty, set_floats, set_for_each, set_has, set_len, set_lit, set_strings |

## Phase 3.4 — List-of-records (`phase03_4_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `list_record_` | list_record_append, list_record_basic, list_record_bool_field, list_record_count_matching, list_record_empty, list_record_field_sum, list_record_filter_loop, list_record_float_field, list_record_index, list_record_iter, list_record_len, list_record_lookup, list_record_max_field, list_record_predicate_print, list_record_print_loop, list_record_reverse_loop, list_record_set, list_record_string_field, list_record_string_lookup, list_record_three_fields, list_record_two_records |

## Phase 4 — Records + equality (`phase04_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `record_` | record_alias_check, record_basic, record_bool_field, record_eq, record_eq_bool, record_eq_string, record_field_arith, record_field_concat, record_field_in_if, record_field_in_while, record_field_negate, record_field_used_twice, record_float_field, record_int_field, record_mixed_compare, record_ne, record_pair_print, record_string_field, record_swap, record_three_fields, record_two_fields, record_two_types, record_var_assign |
| `join_inner_`, `join_cross_`, `join_left_` | join_cross_basic, join_cross_strings, join_cross_where, join_inner_int, join_inner_sum, join_inner_where, join_left_basic, join_left_filtered |

## Phase 5 — Sum types (`phase05_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `sum_` | sum_arith_arm, sum_basic, sum_bool_field, sum_default_arm, sum_eq_self, sum_in_if, sum_int, sum_int_field, sum_let_var, sum_loop_match, sum_match_print_sum, sum_mixed_field_types, sum_nested_call, sum_no_fields, sum_string_field, sum_three_variants, sum_two_arms, sum_two_fields, sum_two_unions, sum_use_binding, sum_var_assign |
| `option_` | (none currently; prefix reserved) |
| `result_` | (none currently; prefix reserved) |

## Phase 6 — Closures + HOF (`phase06_test.go`)

| Prefix | Example fixtures |
|--------|-----------------|
| `closure_` | closure_apply_user_fn, closure_capture_bool, closure_capture_float, closure_capture_int, closure_capture_string, closure_capture_two, closure_make_adder, closure_pass_fn_ref |
| `lambda_` | lambda_as_arg, lambda_basic, lambda_bool, lambda_call_twice, lambda_concat, lambda_float, lambda_in_arith, lambda_int_id, lambda_string, lambda_two_args |
| `hof_` | hof_combined, hof_filter, hof_filter_str, hof_map, hof_map_str, hof_map_to_bool, hof_reduce, hof_reduce_float |

## Phase 7.1–7.5 — Query DSL (`phase07_test.go` → TestPhase7Query)

| Prefix | Example fixtures |
|--------|-----------------|
| `query_` | query_bool_filter, query_filter, query_filter_map, query_in_function, query_map, query_nested, query_order_by, query_order_by_strings, query_order_filter, query_order_skip_take, query_order_take, query_skip, query_skip_take, query_string_contains_filter, query_string_filter, query_string_len_filter, query_string_map, query_take |
| `arena_` | arena_bool_filter, arena_float_select, arena_int_filter, arena_join_inner, arena_large_result, arena_nested_query, arena_order_take, arena_str_select |

## Phase 7.6 — String builtins (`phase07_test.go` → TestPhase7Strings)

| Prefix | Example fixtures |
|--------|-----------------|
| `str_upper` | str_upper, str_upper_lower |
| `str_lower` | str_lower |
| `str_reverse` | str_reverse |
| `str_split` | str_split_basic, str_split_join, str_split_spaces |
| `str_join` | str_join_basic, str_join_single |
| `str_substring` | str_substring |
| `str_contains` | str_contains_false, str_contains_true |
| `str_index` | str_index_concat, str_index_in_function, str_index_simple |
| `str_methods` | str_methods_combined, str_list_values |

## Phase 7.7 — File I/O (`phase07_test.go` → TestPhase7FileIO)

| Prefix | Example fixtures |
|--------|-----------------|
| `file_` | file_append_basic, file_read_long, file_write_overwrite, file_write_read_basic, file_write_read_newlines |
| `lines_` | lines_basic, lines_empty_file, lines_no_trailing_newline |

## Phase 7.8 — CSV (`phase07_test.go` → TestPhase7CSV)

| Prefix | Example fixtures |
|--------|-----------------|
| `csv_` | csv_load_basic, csv_load_colcount, csv_load_empty_file, csv_load_multirow, csv_load_single_row, csv_quoted_fields, csv_roundtrip, csv_save_basic |

## Phase 7.9 — Math (`phase07_test.go` → TestPhase7Math)

| Prefix | Example fixtures |
|--------|-----------------|
| `abs_` | abs_float, abs_int |
| `ceil_` | ceil_basic |
| `floor_` | floor_basic |
| `math_` | math_combined |
| `min_max` | min_max_combined |

## Phase 7.10 — Ordered maps (`phase07_test.go` → TestPhase7OMap)

| Prefix | Example fixtures |
|--------|-----------------|
| `omap_` | omap_get, omap_has, omap_int_keys, omap_len, omap_literal, omap_overwrite, omap_put_stmt, omap_set |

## Phase 7.11 — JSON decode (`phase07_test.go` → TestPhase7JSON)

| Prefix | Example fixtures |
|--------|-----------------|
| `json_` | json_decode_basic, json_decode_concat, json_decode_empty, json_decode_two |

## Phase 7.12 — Error handling (`phase07_test.go` → TestPhase7Errors)

| Prefix | Example fixtures |
|--------|-----------------|
| `try_catch_` | try_catch_div_zero, try_catch_in_fun, try_catch_index_oob, try_catch_nested, try_catch_no_raise, try_catch_reraise |
| `user_panic_` | user_panic_basic |

## Phases 8+ — Not covered here

Phases 8 onward (Datalog, agents, async, channels, streams, fetch, LLM, FFI,
Windows, repro, Wasm) have their own dedicated test files:
`phase08_test.go`, `phase09_2_test.go`, `phase09_3_test.go`, `phase10_test.go`,
`phase11_test.go`, `phase13_test.go`, `phase14_test.go`, `phase14_2_test.go`,
`phase15_test.go`, `phase16_test.go`, `phase17_test.go`, `phase18_test.go`.

## Fixtures with no phase test (misc)

The following fixtures are covered only by `TestPhase1Hello` (catch-all):

| Fixture | Notes |
|---------|-------|
| `hello`, `hello_bool`, `hello_float`, `hello_int`, `hello_newline` | Phase 1 hello |
| `go_ffi_add_ints`, `go_ffi_str_upper` | Phase 12 Go FFI |
| `fetch_basic`, `fetch_concat`, `fetch_reuse`, `fetch_string` | Phase 13 HTTP fetch |
| `generate_*` | Phase 14 LLM generate |
| `async_*` | Phase 11 async/await |
| `chan_*` | Phase 10 channels |
| `stream_*` | Phase 10 streams |
| `agent_*` | Phase 9 agents |
| `dl_*` | Phase 8 Datalog |
