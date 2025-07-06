# Errors

- arithmetic.dart: parse error: parse error: 5:12: lexer: invalid input text "~/ 2))\n  print((..."
- avg_builtin.dart: parse error: parse error: 7:26: lexer: invalid input text "'items'] is List..."
- bool_ops.dart: ok
- break_continue.dart: parse error: parse error: 10:57: lexer: invalid input text "' '))\n  }\n}\n"
- cast_struct.dart: type error: error[T002]: undefined variable: title
  --> :2:3

help:
  Check if the variable was declared in this scope.
- closure.dart: parse error: parse error: 2:14: unexpected token "=>" (expected "}")
- count_builtin.dart: parse error: parse error: 8:41: lexer: invalid input text "; if (items is L..."
- cross_join.dart: parse error: parse error: 30:119: invalid quoted string "\", total: \\$\"": invalid syntax
- cross_join_triple.dart: parse error: parse error: 18:65: lexer: invalid input text "' '))\n  }\n}\n"
- dataset.dart: parse error: parse error: 6:42: unexpected token ":" (expected ")")
- dataset_distinct.dart: parse error: parse error: 16:57: lexer: invalid input text "; }\n  res.add(it..."
- dataset_sort_take_limit.dart: parse error: parse error: 35:34: invalid quoted string "\"costs \\$\"": invalid syntax
- factorial.dart: ok
- fetch_http.dart: parse error: parse error: 13:20: lexer: invalid input text "?['method']?.toS..."
- fetch_options.dart: parse error: parse error: 10:20: lexer: invalid input text "?['method']?.toS..."
- fibonacci.dart: ok
- float_literal_precision.dart: ok
- float_ops.dart: ok
- for_loop.dart: ok
- fun_def_infer.dart: ok
- fun_expr_in_let.dart: parse error: parse error: 2:20: unexpected token "=>" (expected "}")
- generate_struct.dart: parse error: parse error: 5:59: unexpected token "," (expected ")")
- hello_world.dart: ok
- if_else.dart: ok
- input_builtin.dart: parse error: parse error: 3:37: lexer: invalid input text "?? ''\n  print(\"E..."
- join.dart: parse error: parse error: 32:106: invalid quoted string "\"- \\$\"": invalid syntax
- list_concat.dart: ok
- list_index.dart: ok
- list_prepend.dart: ok
- list_set.dart: type error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- list_set_ops.dart: parse error: parse error: 10:13: unexpected token "<" (expected PostfixExpr)
- list_slice.dart: type error: error[T004]: `` is not callable
  --> :2:29

help:
  Use a function or closure in this position.
- load_csv_to_json.dart: parse error: parse error: 10:21: lexer: invalid input text "?['format'] ?? '..."
- load_json.dart: parse error: parse error: 17:56: lexer: invalid input text "' '))\n  }\n}\nfun ..."
- load_jsonl_to_csv.dart: parse error: parse error: 11:21: lexer: invalid input text "?['format'] ?? '..."
- local_recursion.dart: parse error: parse error: 7:26: lexer: invalid input text "~/ 2)\n  return N..."
- map_any_hint.dart: type error: error[T018]: type any does not support indexing
  --> :9:10

help:
  Only `list<T>` and `map<K,V>` can be indexed.
- map_index.dart: ok
- map_len.dart: ok
- map_ops.dart: type error: error[T024]: cannot assign to `m` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- map_set.dart: type error: error[T024]: cannot assign to `scores` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- match_capture.dart: parse error: parse error: 4:29: lexer: invalid input text "; }\n  if (_t is ..."
- match_expr.dart: parse error: parse error: 5:30: lexer: invalid input text "; }\n  if (_t == ..."
- match_underscore.dart: parse error: parse error: 4:43: lexer: invalid input text "; })((_t as Node..."
- matrix_search.dart: parse error: parse error: 10:41: lexer: invalid input text "~/ 2))\n    let r..."
- model_decl.dart: parse error: parse error: 6:48: unexpected token "<" (expected ")" (":" TypeRef)? "{" Statement* "}")
- nested_type.dart: parse error: parse error: 11:21: lexer: invalid input text "'x'] as int, y: ..."
- package_decl.dart: ok
- print_hello.dart: ok
- reserved_keyword_var.dart: ok
- save_json_stdout.dart: parse error: parse error: 6:21: lexer: invalid input text "?['format'] ?? '..."
- simple_fn.dart: ok
- slice_remove.dart: ok
- str_builtin.dart: type error: error[T004]: `` is not callable
  --> :2:21

help:
  Use a function or closure in this position.
- stream_on_emit.dart: parse error: parse error: 11:37: lexer: invalid input text "'Sensor')\n}\nfun ..."
- string_compare.dart: type error: error[T004]: `` is not callable
  --> :2:23

help:
  Use a function or closure in this position.
- string_concat.dart: ok
- string_escape.dart: parse error: parse error: 2:9: invalid quoted string "\"price is \\$5\\\\nnext line\"": invalid syntax
- string_for_loop.dart: parse error: parse error: 3:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- string_index.dart: parse error: parse error: 11:22: lexer: invalid input text "'index out of ra..."
- string_negative_index.dart: parse error: parse error: 11:22: lexer: invalid input text "'index out of ra..."
- string_slice.dart: type error: error[T004]: `` is not callable
  --> :2:26

help:
  Use a function or closure in this position.
- tpch_q1.dart: parse error: parse error: 5:294: lexer: invalid input text "'expect failed')..."
- two_sum.dart: type error: error[T018]: type any does not support indexing
  --> :14:9

help:
  Only `list<T>` and `map<K,V>` can be indexed.
- typed_list_negative.dart: ok
- underscore_for_loop.dart: parse error: parse error: 6:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
- union_inorder.dart: parse error: parse error: 4:30: lexer: invalid input text "; }\n  if (_t is ..."
- union_match.dart: parse error: parse error: 4:32: lexer: invalid input text "; }\n  return fal..."
- union_slice.dart: type error: error[T003]: unknown function: Empty
  --> :2:11

help:
  Ensure the function is defined before it's called.
- update_statement.dart: parse error: parse error: 7:242: lexer: invalid input text "'expect failed')..."
- var_assignment.dart: type error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
- while_loop.dart: ok
- while_membership.dart: parse error: parse error: 3:7: unexpected token "(" (expected <ident> "in" Expr (".." Expr)? "{" Statement* "}")
