# Errors

- arithmetic.ex: type error: error[T003]: unknown function: rem
  --> :6:9

help:
  Ensure the function is defined before it's called.
- break_continue.ex: parse error: parse error: 9:1: unexpected token "<EOF>" (expected "}")
- cast_struct.ex: parse error: parse error: 3:18: unexpected token ":" (expected "}")
- closure.ex: parse error: parse error: 2:16: unexpected token ">" (expected PostfixExpr)
- cross_join.ex: parse error: parse error: 2:20: unexpected token "%" (expected "]")
- cross_join_sorted.ex: parse error: parse error: 4:59: unexpected token "for" (expected PostfixExpr)
- cross_join_triple.ex: parse error: parse error: 5:16: unexpected token "for" (expected PostfixExpr)
- dataset.ex: parse error: parse error: 2:17: unexpected token "%" (expected "]")
- dataset_sort_take_limit.ex: parse error: parse error: 2:19: unexpected token "%" (expected "]")
- fetch_builtin.ex: parse error: parse error: 2:68: unexpected token "%" (expected PostfixExpr)
- fetch_http.ex: type error: error[T003]: unknown function: _fetch
  --> :2:14

help:
  Ensure the function is defined before it's called.
- fun_expr_in_let.ex: parse error: parse error: 2:22: unexpected token ">" (expected PostfixExpr)
- group_by.ex: parse error: parse error: 2:17: unexpected token "%" (expected "]")
- group_by_left_join.ex: parse error: parse error: 2:20: unexpected token "%" (expected "]")
- if_else.ex: parse error: parse error: 17:1: unexpected token "<EOF>" (expected "}")
- input_builtin.ex: type error: error[T003]: unknown function: _input
  --> :3:16

help:
  Ensure the function is defined before it's called.
- join.ex: parse error: parse error: 2:15: unexpected token "%" (expected "]")
- list_index.ex: type error: error[T002]: undefined variable: Enum
  --> :3:9

help:
  Check if the variable was declared in this scope.
- list_prepend.ex: parse error: parse error: 2:25: unexpected token "+" (expected PostfixExpr)
- list_set.ex: type error: error[T002]: undefined variable: Map
  --> :4:14

help:
  Check if the variable was declared in this scope.
- list_slice.ex: type error: error[T002]: undefined variable: Enum
  --> :2:9

help:
  Check if the variable was declared in this scope.
- load_save_json.ex: parse error: parse error: 2:27: unexpected token "%" (expected PostfixExpr)
- map_keys_values.ex: parse error: parse error: 2:11: unexpected token "%" (expected PostfixExpr)
- map_ops.ex: parse error: parse error: 6:36: lexer: invalid input text "?(m, 1), else: E..."
- match_expr.ex: parse error: parse error: 5:12: unexpected token "=" (expected PostfixExpr)
- reserved_keyword_var.ex: parse error: parse error: 2:13: unexpected token "%" (expected PostfixExpr)
- simple_fn.ex: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- str_builtin.ex: type error: error[T003]: unknown function: to_string
  --> :2:9

help:
  Ensure the function is defined before it's called.
- string_concat.ex: parse error: parse error: 2:20: unexpected token ">" (expected PostfixExpr)
- string_for_loop.ex: type error: error[T002]: undefined variable: String
  --> :2:13

help:
  Check if the variable was declared in this scope.
- string_slice_negative.ex: parse error: parse error: 2:19: unexpected token ":-" (expected "]")
- tpch_q1.ex: parse error: parse error: 2:19: unexpected token "%" (expected "]")
- two_sum.ex: parse error: parse error: 15:1: unexpected token "<EOF>" (expected "}")
- underscore_for_loop.ex: parse error: parse error: 4:7: unexpected token "{" (expected <ident> (":" TypeRef)? ("=" Expr)?)
- update_statement.ex: parse error: parse error: 3:3: unexpected token "%" (expected "]")
- while_loop.ex: parse error: parse error: 4:17: unexpected token "," (expected "}")
- while_membership.ex: parse error: parse error: 14:38: lexer: invalid input text "?(set, i), else:..."
