# Errors

- avg_builtin.hs: parse error: parse error: 1:14: unexpected token "," (expected "]")
- bool_ops.hs: type error: error[T003]: unknown function: not
  --> :3:7

help:
  Ensure the function is defined before it's called.
- closure.hs: parse error: parse error: 1:32: lexer: invalid input text "\\x(->, (x, +, n)..."
- count_builtin.hs: parse error: parse error: 1:17: unexpected token "," (expected "]")
- dataset.hs: parse error: parse error: 3:43: lexer: invalid input text "\\p, ->, (age, (p..."
- fun_expr_in_let.hs: parse error: parse error: 1:15: lexer: invalid input text "\\x(->, (x, *, x)..."
- hello_world.hs: parse error: parse error: 1:22: unexpected token ")"
- if_else.hs: parse error: parse error: 3:1: unexpected token "print" (expected ")")
- input_builtin.hs: parse error: parse error: 3:29: unexpected token ")"
- len_builtin.hs: parse error: parse error: 1:17: unexpected token "," (expected "]")
- list_for_loop.hs: parse error: parse error: 1:28: unexpected token "]" (expected ")")
- list_push.hs: parse error: parse error: 1:12: unexpected token "]" (expected ")")
- load_json_stdin.hs: parse error: parse error: 1:32: unexpected token "|" (expected PostfixExpr)
- map_for_loop.hs: parse error: parse error: 1:36: unexpected token "," (expected ")")
- map_index.hs: parse error: parse error: 1:36: unexpected token "," (expected ")")
- map_keys_builtin.hs: parse error: parse error: 1:27: unexpected token "," (expected ")")
- map_lookup.hs: parse error: parse error: 1:27: unexpected token "," (expected ")")
- reserved_keyword_var.hs: parse error: parse error: 1:29: unexpected token "," (expected ")")
- save_json_stdout.hs: parse error: parse error: 1:32: unexpected token "|" (expected PostfixExpr)
- string_concat.hs: parse error: parse error: 1:17: unexpected token "+" (expected PostfixExpr)
- string_index.hs: type error: error[T003]: unknown function: _indexString
  --> :2:7

help:
  Ensure the function is defined before it's called.
- string_negative_index.hs: parse error: parse error: 3:1: unexpected token "<EOF>" (expected ")")
- struct_literal.hs: parse error: parse error: 3:23: unexpected token "," (expected ":" Expr)
- test_block.hs: parse error: parse error: 1:5: unexpected token "expect" (expected <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- tpc_h_q1.hs: parse error: parse error: 4:1456: lexer: invalid input text "\\(row), ->, Map...."
- two_sum.hs: parse error: parse error: 2:24: unexpected token "," (expected "]")
