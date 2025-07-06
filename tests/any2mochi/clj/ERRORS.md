# Errors

- /workspace/mochi/tests/vm/valid/avg_builtin.mochi: reparse error: parse error: 2:56: unexpected token "+" (expected ")")
- /workspace/mochi/tests/vm/valid/binary_precedence.mochi: output mismatch
-- got --
7
7
7
7
-- want --
7
9
7
8
- /workspace/mochi/tests/vm/valid/bool_chain.mochi: reparse error: parse error: 2:22: unexpected token "," (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/break_continue.mochi: reparse error: parse error: 3:72: unexpected token "break" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/cast_string_to_int.mochi: retype error: error[T003]: unknown function: int
  --> :2:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/cast_struct.mochi: reparse error: parse error: 2:35: lexer: invalid input text "#, get(m, %), fi..."
- /workspace/mochi/tests/vm/valid/closure.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/cross_join.mochi: reparse error: parse error: 2:20: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/cross_join_filter.mochi: reparse error: parse error: 4:19: unexpected token ">" (expected ")")
- /workspace/mochi/tests/vm/valid/cross_join_triple.mochi: reparse error: parse error: 5:20: unexpected token ">" (expected ")")
- /workspace/mochi/tests/vm/valid/dataset_sort_take_limit.mochi: reparse error: parse error: 2:19: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/dataset_where_filter.mochi: reparse error: parse error: 2:17: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/exists_builtin.mochi: reparse error: parse error: 3:25: unexpected token ">" (expected ")")
- /workspace/mochi/tests/vm/valid/for_list_collection.mochi: reparse error: parse error: 2:74: unexpected token "break" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/for_loop.mochi: reparse error: parse error: 2:46: unexpected token "break" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/for_map_collection.mochi: reparse error: parse error: 3:66: unexpected token "break" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/fun_call.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/fun_expr_in_let.mochi: reparse error: parse error: 2:28: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/fun_three_args.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/group_by.mochi: reparse error: parse error: 9:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_conditional_sum.mochi: reparse error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_having.mochi: reparse error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_join.mochi: reparse error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_left_join.mochi: reparse error: parse error: 3:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_multi_join.mochi: reparse error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_multi_join_sort.mochi: reparse error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_by_sort.mochi: reparse error: parse error: 6:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/group_items_iteration.mochi: reparse error: parse error: 3:65: lexer: invalid input text "@groups, ks), sw..."
- /workspace/mochi/tests/vm/valid/if_else.mochi: output mismatch
-- got --

-- want --
big
- /workspace/mochi/tests/vm/valid/in_operator.mochi: reparse error: parse error: 3:14: lexer: invalid input text "#, 2 == %, xs))\n..."
- /workspace/mochi/tests/vm/valid/in_operator_extended.mochi: reparse error: parse error: 4:14: lexer: invalid input text "#, 1 == %, ys))\n..."
- /workspace/mochi/tests/vm/valid/inner_join.mochi: reparse error: parse error: 2:20: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/join_multi.mochi: reparse error: parse error: 2:20: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/json_builtin.mochi: reparse error: parse error: 2:3: unexpected token ">" (expected "}")
- /workspace/mochi/tests/vm/valid/left_join.mochi: reparse error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
- /workspace/mochi/tests/vm/valid/left_join_multi.mochi: reparse error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
- /workspace/mochi/tests/vm/valid/len_map.mochi: reparse error: parse error: 2:19: unexpected token "," (expected ":" Expr)
- /workspace/mochi/tests/vm/valid/len_string.mochi: retype error: error[T037]: count() expects list or group, got string
  --> :2:9

help:
  Pass a list or group to count().
- /workspace/mochi/tests/vm/valid/list_assign.mochi: reparse error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/list_index.mochi: reparse error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/list_nested_assign.mochi: reparse error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/list_set_ops.mochi: retype error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- /workspace/mochi/tests/vm/valid/load_yaml.mochi: reparse error: parse error: 5:351: lexer: invalid input text "#, clojure_data_..."
- /workspace/mochi/tests/vm/valid/map_assign.mochi: reparse error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/map_in_operator.mochi: reparse error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/map_index.mochi: reparse error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/map_int_key.mochi: reparse error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/map_literal_dynamic.mochi: reparse error: parse error: 8:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/map_membership.mochi: reparse error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/map_nested_assign.mochi: reparse error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/match_expr.mochi: reparse error: parse error: 3:78: unexpected token "else" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/match_full.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/membership.mochi: reparse error: parse error: 3:14: lexer: invalid input text "#, 2 == %, nums)..."
- /workspace/mochi/tests/vm/valid/min_max_builtin.mochi: retype error: error[T003]: unknown function: apply
  --> :3:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/nested_function.mochi: reparse error: parse error: 2:28: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/order_by_map.mochi: reparse error: parse error: 2:15: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/outer_join.mochi: reparse error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
- /workspace/mochi/tests/vm/valid/partial_application.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/pure_fold.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/pure_global_fold.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/query_sum_select.mochi: reparse error: parse error: 2:23: unexpected token "+" (expected ")")
- /workspace/mochi/tests/vm/valid/record_assign.mochi: reparse error: parse error: 2:4: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/right_join.mochi: reparse error: parse error: 2:108: lexer: invalid input text "@items), (fun(m)..."
- /workspace/mochi/tests/vm/valid/save_jsonl_stdout.mochi: reparse error: parse error: 2:187: lexer: invalid input text "#, str(get(r, %,..."
- /workspace/mochi/tests/vm/valid/short_circuit.mochi: reparse error: parse error: 2:22: unexpected token "," (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/slice.mochi: retype error: error[T003]: unknown function: subvec
  --> :2:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/sort_stable.mochi: reparse error: parse error: 2:16: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/string_compare.mochi: retype error: error[T003]: unknown function: compare
  --> :2:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/string_concat.mochi: retype error: error[T039]: function str expects 1 arguments, got 2
  --> :2:9

help:
  Pass exactly 1 arguments to `str`.
- /workspace/mochi/tests/vm/valid/string_contains.mochi: retype error: error[T003]: unknown function: clojure_string_includes_p
  --> :3:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/string_in_operator.mochi: retype error: error[T003]: unknown function: clojure_string_includes_p
  --> :3:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/string_index.mochi: reparse error: parse error: 2:43: unexpected token "}" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/string_prefix_slice.mochi: retype error: error[T003]: unknown function: subs
  --> :4:9

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/sum_builtin.mochi: reparse error: parse error: 2:16: unexpected token "+" (expected ")")
- /workspace/mochi/tests/vm/valid/tail_recursion.mochi: reparse error: parse error: 2:7: unexpected token "," (expected ")")
- /workspace/mochi/tests/vm/valid/test_block.mochi: retype error: error[T003]: unknown function: assert
  --> :3:3

help:
  Ensure the function is defined before it's called.
- /workspace/mochi/tests/vm/valid/tree_sum.mochi: reparse error: parse error: 2:4: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/two-sum.mochi: reparse error: parse error: 2:50: unexpected token "}" (expected PostfixExpr)
- /workspace/mochi/tests/vm/valid/typed_let.mochi: retype error: error[T002]: undefined variable: y
  --> :2:9

help:
  Check if the variable was declared in this scope.
- /workspace/mochi/tests/vm/valid/typed_var.mochi: retype error: error[T002]: undefined variable: nil
  --> :2:11

help:
  Check if the variable was declared in this scope.
- /workspace/mochi/tests/vm/valid/update_stmt.mochi: reparse error: parse error: 2:35: lexer: invalid input text "#, get(m, %), fi..."
- /workspace/mochi/tests/vm/valid/user_type_literal.mochi: reparse error: parse error: 2:4: unexpected token ":" (expected "}")
- /workspace/mochi/tests/vm/valid/values_builtin.mochi: reparse error: parse error: 6:1: unexpected token "<EOF>" (expected "}")
- /workspace/mochi/tests/vm/valid/while_loop.mochi: reparse error: parse error: 3:8: unexpected token "," (expected ")")
