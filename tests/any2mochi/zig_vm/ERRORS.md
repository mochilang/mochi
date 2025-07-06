# Errors

- append_builtin: parse error: parse error: 5:7: lexer: invalid input text "&[_]i32{1, 2}\n  ..."
- avg_builtin: parse error: parse error: 5:25: lexer: invalid input text "@floatFromInt(it..."
- basic_compare: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- binary_precedence: ok
- bool_chain: parse error: parse error: 8:21: unexpected token "and" (expected ")")
- break_continue: parse error: parse error: 5:13: lexer: invalid input text "&[_]i32{1, 2, 3,..."
- cast_string_to_int: ok
- cast_struct: parse error: parse error: 9:95: lexer: invalid input text "; m.put(\"title\",..."
- closure: parse error: parse error: 5:15: unexpected token ":" (expected ")")
- count_builtin: parse error: parse error: 3:22: lexer: invalid input text "&[_]i32{1, 2, 3]..."
- cross_join: parse error: parse error: 9:15: lexer: invalid input text "&[_]std.AutoHash..."
- cross_join_filter: parse error: parse error: 9:10: lexer: invalid input text "&[_]i32{1, 2, 3}..."
- cross_join_triple: parse error: parse error: 11:10: lexer: invalid input text "&[_]i32{1, 2}\n  ..."
- dataset_sort_take_limit: parse error: parse error: 28:14: lexer: invalid input text "&[_]std.AutoHash..."
- dataset_where_filter: parse error: parse error: 7:12: lexer: invalid input text "&[_]std.AutoHash..."
- exists_builtin: parse error: parse error: 7:10: lexer: invalid input text "&[_]i32{1, 2}\n  ..."
- for_list_collection: parse error: parse error: 3:13: lexer: invalid input text "&[_]i32{@as(i32,..."
- for_loop: parse error: parse error: 3:13: lexer: invalid input text "@as(i32, {\n    p..."
- for_map_collection: parse error: parse error: 5:85: lexer: invalid input text "; m.put(\"a\", 1);..."
- fun_call: ok
- fun_expr_in_let: parse error: parse error: 5:17: unexpected token ":" (expected ")")
- fun_three_args: ok
- group_by: parse error: parse error: 9:25: lexer: invalid input text "@floatFromInt(it..."
- group_by_conditional_sum: parse error: parse error: 8:27: lexer: invalid input text "; }\n  return sum..."
- group_by_having: parse error: parse error: 13:7: lexer: invalid input text "@TypeOf(a) != @T..."
- group_by_join: parse error: parse error: 9:7: lexer: invalid input text "@TypeOf(a) != @T..."
- group_by_left_join: parse error: parse error: 9:7: lexer: invalid input text "@TypeOf(a) != @T..."
- group_by_multi_join: parse error: parse error: 14:27: lexer: invalid input text "; }\n  return sum..."
- group_by_multi_join_sort: parse error: parse error: 18:27: lexer: invalid input text "; }\n  return sum..."
- group_by_sort: parse error: parse error: 8:27: lexer: invalid input text "; }\n  return sum..."
- group_items_iteration: parse error: parse error: 11:7: lexer: invalid input text "@TypeOf(a) != @T..."
- if_else: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- if_then_else: parse error: parse error: 8:31: unexpected token "else" (expected (("{" Expr "}") | ("then" Expr)) (("else" IfExpr) | ("else" (("{" Expr "}") | Expr)))?)
- if_then_else_nested: parse error: parse error: 8:31: unexpected token "else" (expected (("{" Expr "}") | ("then" Expr)) (("else" IfExpr) | ("else" (("{" Expr "}") | Expr)))?)
- in_operator: parse error: parse error: 5:45: lexer: invalid input text "; }\n  return fal..."
- in_operator_extended: parse error: parse error: 11:45: lexer: invalid input text "; }\n  return fal..."
- inner_join: parse error: parse error: 9:15: lexer: invalid input text "&[_]std.AutoHash..."
- join_multi: parse error: parse error: 11:15: lexer: invalid input text "&[_]std.AutoHash..."
- json_builtin: parse error: parse error: 11:78: lexer: invalid input text "; m.put(\"a\", 1);..."
- left_join: parse error: parse error: 9:15: lexer: invalid input text "&[_]std.AutoHash..."
- left_join_multi: parse error: parse error: 11:15: lexer: invalid input text "&[_]std.AutoHash..."
- len_builtin: parse error: parse error: 3:22: lexer: invalid input text "&[_]i32{1, 2, 3]..."
- len_map: parse error: parse error: 3:99: lexer: invalid input text "; m.put(\"a\", 1);..."
- len_string: ok
- let_and_print: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- list_assign: parse error: parse error: 5:10: lexer: invalid input text "&[_]i32{1, 2}\n  ..."
- list_index: parse error: parse error: 5:8: lexer: invalid input text "&[_]i32{10, 20, ..."
- list_nested_assign: parse error: parse error: 5:12: lexer: invalid input text "&[_][]const i32{..."
- list_set_ops: parse error: parse error: 3:57: lexer: invalid input text "; }\n  return fal..."
- load_yaml: parse error: parse error: 14:25: lexer: invalid input text "?[]const u8, dat..."
- map_assign: parse error: parse error: 5:90: lexer: invalid input text "; m.put(\"alice\",..."
- map_in_operator: parse error: parse error: 5:85: lexer: invalid input text "; m.put(1, \"a\");..."
- map_index: parse error: parse error: 5:85: lexer: invalid input text "; m.put(\"a\", 1);..."
- map_int_key: parse error: parse error: 5:85: lexer: invalid input text "; m.put(1, \"a\");..."
- map_literal_dynamic: parse error: parse error: 11:85: lexer: invalid input text "; m.put(\"a\", x);..."
- map_membership: parse error: parse error: 5:85: lexer: invalid input text "; m.put(\"a\", 1);..."
- map_nested_assign: parse error: parse error: 5:88: lexer: invalid input text "; m.put(\"outer\",..."
- match_expr: parse error: parse error: 7:7: lexer: invalid input text "@TypeOf(a) != @T..."
- match_full: parse error: parse error: 15:7: lexer: invalid input text "@TypeOf(a) != @T..."
- math_ops: parse error: parse error: 5:9: lexer: invalid input text "@mod(7, 2))\n}\nma..."
- membership: parse error: parse error: 5:45: lexer: invalid input text "; }\n  return fal..."
- min_max_builtin: parse error: parse error: 7:41: lexer: invalid input text "; }\n  return m\n}..."
- nested_function: parse error: parse error: 3:22: unexpected token ":" (expected ")")
- order_by_map: parse error: parse error: 7:10: lexer: invalid input text "&[_]std.AutoHash..."
- outer_join: parse error: parse error: 9:15: lexer: invalid input text "&[_]std.AutoHash..."
- partial_application: type error: error[T002]: undefined variable: undefined
  --> :2:12

help:
  Check if the variable was declared in this scope.
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- query_sum_select: parse error: parse error: 8:27: lexer: invalid input text "; }\n  return sum..."
- record_assign: parse error: parse error: 13:16: unexpected token "." (expected "}")
- right_join: parse error: parse error: 9:15: lexer: invalid input text "&[_]std.AutoHash..."
- save_jsonl_stdout: parse error: parse error: 6:25: lexer: invalid input text "?[]const u8, dat..."
- short_circuit: parse error: parse error: 8:16: unexpected token "and" (expected ")")
- slice: parse error: parse error: 36:38: lexer: invalid input text "&[_]i32{1, 2, 3]..."
- sort_stable: parse error: parse error: 7:11: lexer: invalid input text "&[_]std.AutoHash..."
- str_builtin: parse error: parse error: 3:78: unexpected token "}" (expected "]")
- string_compare: ok
- string_concat: type error: error[T002]: undefined variable: std
  --> :2:11

help:
  Check if the variable was declared in this scope.
- string_contains: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- string_in_operator: parse error: parse error: 5:45: lexer: invalid input text "; }\n  return fal..."
- string_index: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- string_prefix_slice: type error: error[T002]: undefined variable: start
  --> :2:12

help:
  Check if the variable was declared in this scope.
- substring_builtin: ok
- sum_builtin: parse error: parse error: 4:27: lexer: invalid input text "; }\n  return sum..."
- tail_recursion: ok
- test_block: parse error: parse error: 3:14: lexer: invalid input text "@panic(\"expect f..."
- tree_sum: compile error: union types not supported
- two-sum: parse error: parse error: 7:19: lexer: invalid input text "&[_]i32{2, 7, 11..."
- typed_let: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- typed_var: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- unary_neg: ok
- update_stmt: parse error: parse error: 11:14: lexer: invalid input text "@panic(\"expect f..."
- user_type_literal: parse error: parse error: 15:16: unexpected token "." (expected "}")
- values_builtin: parse error: parse error: 9:85: lexer: invalid input text "; m.put(\"a\", 1);..."
- var_assignment: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
- while_loop: type error: error[T002]: undefined variable: undefined
  --> :2:9

help:
  Check if the variable was declared in this scope.
