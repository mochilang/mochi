# Errors

- avg_builtin.kt: type error: error[T003]: unknown function: listOf
  --> :2:9

help:
  Ensure the function is defined before it's called.
- break_continue.kt: parse error: parse error: 4:13: unexpected token ")" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- count_builtin.kt: type error: error[T003]: unknown function: listOf
  --> :2:9

help:
  Ensure the function is defined before it's called.
- cross_join.kt: parse error: parse error: 17:38: unexpected token "=" (expected ")")
- cross_join_triple.kt: parse error: parse error: 7:45: unexpected token "," (expected PostfixExpr)
- dataset.kt: parse error: parse error: 6:35: unexpected token "=" (expected ")")
- dataset_sort_take_limit.kt: parse error: parse error: 6:38: unexpected token "=" (expected ")")
- fetch_builtin.kt: parse error: parse error: 11:47: lexer: invalid input text "?): Any {\n  fun ..."
- fetch_http.kt: parse error: parse error: 15:47: lexer: invalid input text "?): Any {\n  fun ..."
- for_list_collection.kt: type error: error[T003]: unknown function: listOf
  --> :2:12

help:
  Ensure the function is defined before it's called.
- group_by.kt: parse error: parse error: 6:35: unexpected token ">" (expected PostfixExpr)
- input_builtin.kt: type error: error[T003]: unknown function: readln
  --> :3:16

help:
  Ensure the function is defined before it's called.
- join_filter_pag.kt: parse error: parse error: 12:20: lexer: invalid input text "?>) -> Boolean) ..."
- json_builtin.kt: parse error: parse error: 5:20: lexer: invalid input text "?): String = whe..."
- list_concat.kt: type error: error[T003]: unknown function: _concat
  --> :2:9

help:
  Ensure the function is defined before it's called.
- list_index.kt: type error: error[T003]: unknown function: listOf
  --> :2:12

help:
  Ensure the function is defined before it's called.
- load_jsonl_stdin.kt: parse error: parse error: 19:47: lexer: invalid input text "?): List<Map<Str..."
- load_save_json.kt: parse error: parse error: 26:47: lexer: invalid input text "?): List<Map<Str..."
- load_yaml.kt: parse error: parse error: 28:47: lexer: invalid input text "?): List<Map<Str..."
- local_recursion.kt: parse error: parse error: 10:20: unexpected token "=" (expected ")")
- map_index.kt: parse error: parse error: 2:37: unexpected token "to" (expected ")")
- map_iterate.kt: parse error: parse error: 2:53: unexpected token "," (expected "}")
- map_literal.kt: parse error: parse error: 2:30: unexpected token "to" (expected ")")
- match_capture.kt: parse error: parse error: 8:15: unexpected token ">" (expected PostfixExpr)
- match_underscore.kt: parse error: parse error: 8:15: unexpected token ">" (expected PostfixExpr)
- matrix_search.kt: type error: error[T027]: [[int]] is not a struct
  --> :2:11

help:
  Field access is only valid on struct types.
- now_builtin.kt: type error: error[T002]: undefined variable: System
  --> :2:11

help:
  Check if the variable was declared in this scope.
- str_builtin.kt: type error: error[T004]: `` is not callable
  --> :2:21

help:
  Use a function or closure in this position.
- string_index.kt: parse error: parse error: 9:10: unexpected token "=" (expected PostfixExpr)
- string_negative_index.kt: parse error: parse error: 9:10: unexpected token "=" (expected PostfixExpr)
- string_slice.kt: parse error: parse error: 10:12: unexpected token "=" (expected PostfixExpr)
- test_block.kt: type error: error[T003]: unknown function: check
  --> :3:3

help:
  Ensure the function is defined before it's called.
- two_sum.kt: parse error: parse error: 5:27: unexpected token ")" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- type_method.kt: parse error: parse error: 4:11: unexpected token "." (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- underscore_for_loop.kt: parse error: parse error: 12:28: unexpected token "to" (expected ")")
- union_inorder.kt: parse error: parse error: 8:15: unexpected token ">" (expected PostfixExpr)
- union_match.kt: parse error: parse error: 8:15: unexpected token ">" (expected PostfixExpr)
- union_slice.kt: type error: error[T003]: unknown function: listOf
  --> :5:10

help:
  Ensure the function is defined before it's called.
- update_stmt.kt: parse error: parse error: 7:39: unexpected token "=" (expected ")")
- while_membership.kt: parse error: parse error: 2:55: unexpected token "," (expected "}")
