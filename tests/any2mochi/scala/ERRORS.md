# Errors

- avg_builtin.scala: type error: error[T002]: undefined variable: scala
  --> :1:8

help:
  Check if the variable was declared in this scope.
- closure.scala: parse error: parse error: 2:3: unexpected token ">" (expected "}")
- count_builtin.scala: type error: error[T002]: undefined variable: scala
  --> :1:11

help:
  Check if the variable was declared in this scope.
- dataset_sort_take_limit.scala: parse error: parse error: 1:54: unexpected token ">" (expected PostfixExpr)
- factorial.scala: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- fetch_builtin.scala: parse error: parse error: 1:90: unexpected token "," (expected "]")
- fetch_options.scala: parse error: parse error: 1:81: unexpected token ">" (expected PostfixExpr)
- if_else.scala: parse error: parse error: 4:3: unexpected token "else" (expected "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- input_builtin.scala: type error: error[T002]: undefined variable: scala
  --> :2:14

help:
  Check if the variable was declared in this scope.
- join.scala: parse error: parse error: 1:30: unexpected token "=" (expected ")")
- json_builtin.scala: parse error: parse error: 6:2: unexpected token "<EOF>" (expected "{" MatchCase* "}")
- list_concat.scala: parse error: parse error: 1:52: unexpected token "+" (expected PostfixExpr)
- list_prepend.scala: parse error: parse error: 2:19: unexpected token "=" (expected TypeRef)
- load_save_json.scala: parse error: parse error: 1:63: unexpected token ">" (expected PostfixExpr)
- map_any_hint.scala: parse error: parse error: 2:51: unexpected token "," (expected "]")
- map_index.scala: type error: error[T004]: `scores` is not callable
  --> :2:7

help:
  Use a function or closure in this position.
- map_len.scala: parse error: parse error: 1:45: unexpected token ">" (expected PostfixExpr)
- matrix_search.scala: parse error: parse error: 7:13: unexpected token "=" (expected TypeRef)
- now_builtin.scala: parse error: parse error: 1:44: unexpected token "L" (expected ")")
- reduce_builtin.scala: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- reserved_keyword_var.scala: type error: error[T004]: `map` is not callable
  --> :2:7

help:
  Use a function or closure in this position.
- set_ops.scala: parse error: parse error: 3:11: unexpected token "+" (expected PostfixExpr)
- string_in.scala: type error: error[T004]: `` is not callable
  --> :1:23

help:
  Use a function or closure in this position.
- string_index.scala: parse error: parse error: 7:16: unexpected token "idx" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- string_negative_index.scala: parse error: parse error: 7:16: unexpected token "idx" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- string_slice.scala: parse error: parse error: 8:18: unexpected token "start" (expected "{" Statement* "}" (("else" IfStmt) | ("else" "{" Statement* "}"))?)
- to_json_builtin.scala: parse error: parse error: 1:50: unexpected token ">" (expected PostfixExpr)
- tpch_q1.scala: parse error: parse error: 2:18: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- two_sum.scala: type error: error[T005]: parameter `nums` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- underscore_for_loop.scala: parse error: parse error: 1:8: unexpected token "=" (expected TypeRef)
- union_inorder.scala: parse error: parse error: 6:10: unexpected token "=>" (expected ":" Expr)
- union_match.scala: parse error: parse error: 11:24: unexpected token "=" (expected ")")
- union_slice.scala: type error: error[T008]: type mismatch: expected void, got [Foo]
  --> :5:10

help:
  Change the value to match the expected type.
