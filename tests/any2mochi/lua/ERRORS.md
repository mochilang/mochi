# Errors

- abs_builtin.lua: type error: error[T002]: undefined variable: math
  --> :1:7

help:
  Check if the variable was declared in this scope.
- break_continue.lua: parse error: parse error: 2:6: unexpected token "," (expected "in" Expr (".." Expr)? "{" Statement* "}")
- builtin_append.lua: type error: error[T001]: assignment to undeclared variable: xs
  --> :1:1

help:
  Declare `xs` first using `let`.
- cast_struct.lua: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- closure.lua: parse error: parse error: 2:17: unexpected token "{" (expected "=>" Expr)
- cross_join.lua: parse error: parse error: 3:13: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- cross_join_triple.lua: parse error: parse error: 4:16: unexpected token "{" (expected "=>" Expr)
- dataset.lua: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- dataset_sort_take_limit.lua: parse error: parse error: 25:16: lexer: invalid input text "; i <= skip; i +..."
- factorial.lua: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- fetch_builtin.lua: parse error: parse error: 3:8: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- fetch_http.lua: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- fetch_options.lua: parse error: parse error: 3:8: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- fetch_stmt.lua: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- fibonacci.lua: type error: error[T005]: parameter `n` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- for_loop.lua: parse error: parse error: 1:10: lexer: invalid input text "; i <= 4 - 1; i ..."
- fun_expr_in_let.lua: parse error: parse error: 1:17: unexpected token "{" (expected "=>" Expr)
- generate_struct.lua: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- group_by.lua: parse error: parse error: 2:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- helper.lua: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- higher_order_apply.lua: parse error: parse error: 8:20: unexpected token "{" (expected "=>" Expr)
- if_else.lua: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- import_basic.lua: type error: error[T003]: unknown function: require
  --> :1:12

help:
  Ensure the function is defined before it's called.
- input_builtin.lua: type error: error[T001]: assignment to undeclared variable: input1
  --> :2:1

help:
  Declare `input1` first using `let`.
- join_where.lua: parse error: parse error: 3:13: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- let_and_print.lua: type error: error[T001]: assignment to undeclared variable: a
  --> :1:1

help:
  Declare `a` first using `let`.
- list_index.lua: type error: error[T001]: assignment to undeclared variable: xs
  --> :1:1

help:
  Declare `xs` first using `let`.
- list_prepend.lua: type error: error[T005]: parameter `level` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- list_set.lua: type error: error[T001]: assignment to undeclared variable: nums
  --> :1:1

help:
  Declare `nums` first using `let`.
- load_builtin.lua: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- load_jsonl_stdin.lua: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- load_save_json.lua: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- load_yaml.lua: parse error: parse error: 3:9: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- local_recursion.lua: parse error: parse error: 12:16: unexpected token "{" (expected "=>" Expr)
- map_any_hint.lua: type error: error[T008]: type mismatch: expected void, got {string: string}
  --> :2:10

help:
  Change the value to match the expected type.
- map_ops.lua: parse error: parse error: 4:9: lexer: invalid input text "~= null {\n  prin..."
- match_capture.lua: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- match_underscore.lua: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- matrix_search.lua: type error: error[T005]: parameter `matrix` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- nested_inner_fn.lua: type error: error[T005]: parameter `a` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- nested_type_decl.lua: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- package_import.lua: type error: error[T005]: parameter `x` is missing a type
  --> :3:3

help:
  Add a type like `x: int` to this parameter.
- reduce_builtin.lua: parse error: parse error: 2:34: unexpected token "{" (expected "=>" Expr)
- reserved_keyword_var.lua: type error: error[T001]: assignment to undeclared variable: map
  --> :1:1

help:
  Declare `map` first using `let`.
- set_ops.lua: type error: error[T001]: assignment to undeclared variable: a
  --> :1:1

help:
  Declare `a` first using `let`.
- simple_fn.lua: type error: error[T005]: parameter `x` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- slice_remove.lua: type error: error[T005]: parameter `nums` is missing a type
  --> :1:1

help:
  Add a type like `x: int` to this parameter.
- str_builtin.lua: type error: error[T003]: unknown function: tostring
  --> :1:7

help:
  Ensure the function is defined before it's called.
- string_for_loop.lua: parse error: parse error: 2:12: lexer: invalid input text "; _i0 <= len(_s0..."
- string_index.lua: type error: error[T001]: assignment to undeclared variable: text
  --> :1:1

help:
  Declare `text` first using `let`.
- string_negative_index.lua: type error: error[T001]: assignment to undeclared variable: text
  --> :1:1

help:
  Declare `text` first using `let`.
- two_sum.lua: parse error: parse error: 3:12: lexer: invalid input text "; i <= n - 1; i ..."
- type_method.lua: parse error: parse error: 3:11: unexpected token "[" (expected "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- typed_list_negative.lua: parse error: parse error: 2:17: unexpected token "-" (expected PostfixExpr)
- underscore_for_loop.lua: parse error: parse error: 2:10: lexer: invalid input text "; _ <= 2 - 1; _ ..."
- union_inorder.lua: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- union_match.lua: parse error: parse error: 2:16: unexpected token "{" (expected "=>" Expr)
- union_slice.lua: type error: error[T008]: type mismatch: expected void, got [{string: string}]
  --> :2:10

help:
  Change the value to match the expected type.
- update_statement.lua: parse error: parse error: 15:12: lexer: invalid input text "; _i0 <= len(peo..."
- var_assignment.lua: type error: error[T001]: assignment to undeclared variable: x
  --> :1:1

help:
  Declare `x` first using `let`.
- while_loop.lua: type error: error[T001]: assignment to undeclared variable: i
  --> :1:1

help:
  Declare `i` first using `let`.
- while_membership.lua: parse error: parse error: 7:13: lexer: invalid input text "~= null {\n  i = ..."
