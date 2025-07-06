# Errors

- append_builtin: parse2 error: parse error: 2:14: unexpected token "a" (expected ")")
- avg_builtin: parse2 error: parse error: 1:21: unexpected token "," (expected "]")
- basic_compare: parse2 error: parse error: 4:10: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- binary_precedence: ok
- bool_chain: type2 error: error[T020]: operator `&&` cannot be used on types bool and fun(): bool
  --> :6:33

help:
  Choose an operator that supports these operand types.
- break_continue: unsupported syntax at line 6: try
  5: let numbers = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]
  6: try
  7:     for n in numbers do
  8:         try
- cast_string_to_int: parse2 error: parse error: 1:12: unexpected token "1995" (expected "(" (Expr ("," Expr)*)? ")")
- cast_struct: parse2 error: parse error: 6:44: unexpected token "," (expected ")")
- closure: parse2 error: parse error: 3:22: unexpected token ")" (expected "<" TypeRef ("," TypeRef)* ">")
- count_builtin: parse2 error: parse error: 1:20: unexpected token "," (expected "]")
- cross_join: parse2 error: parse error: 9:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- cross_join_filter: type2 error: error[T000]: `let` requires a type or a value
  --> :5:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- cross_join_triple: type2 error: error[T000]: `let` requires a type or a value
  --> :7:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- dataset_sort_take_limit: parse2 error: parse error: 3:34: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- dataset_where_filter: parse2 error: parse error: 4:32: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- exists_builtin: parse2 error: parse error: 4:1: unexpected token "<EOF>" (expected ")")
- for_list_collection: parse2 error: parse error: 1:13: lexer: invalid input text "; 2; 3|] {\n  pri..."
- for_loop: ok
- for_map_collection: parse2 error: parse error: 1:25: unexpected token "," (expected ")")
- fun_call: type2 error: error[T002]: undefined variable: a
  --> :2:11

help:
  Check if the variable was declared in this scope.
- fun_expr_in_let: parse2 error: parse error: 1:26: unexpected token ")" (expected "<" TypeRef ("," TypeRef)* ">")
- fun_three_args: type2 error: error[T002]: undefined variable: a
  --> :2:11

help:
  Check if the variable was declared in this scope.
- group_by: unsupported syntax at line 8: type _Group<'T>(key: obj) =
  7: let avg_age = "avg_age"
  8: type _Group<'T>(key: obj) =
  9:   member val key = key with get, set
 10:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_conditional_sum: unsupported syntax at line 7: type _Group<'T>(key: obj) =
  6: let share = "share"
  7: type _Group<'T>(key: obj) =
  8:   member val key = key with get, set
  9:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_having: unsupported syntax at line 6: type _Group<'T>(key: obj) =
  5: let num = "num"
  6: type _Group<'T>(key: obj) =
  7:   member val key = key with get, set
  8:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_join: unsupported syntax at line 7: type _Group<'T>(key: obj) =
  6: let count = "count"
  7: type _Group<'T>(key: obj) =
  8:   member val key = key with get, set
  9:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_left_join: unsupported syntax at line 7: type _Group<'T>(key: obj) =
  6: let count = "count"
  7: type _Group<'T>(key: obj) =
  8:   member val key = key with get, set
  9:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_multi_join: unsupported syntax at line 12: type _Group<'T>(key: obj) =
 11: let total = "total"
 12: type _Group<'T>(key: obj) =
 13:   member val key = key with get, set
 14:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_multi_join_sort: unsupported syntax at line 20: type _Group<'T>(key: obj) =
 19: let revenue = "revenue"
 20: type _Group<'T>(key: obj) =
 21:   member val key = key with get, set
 22:   member val Items = System.Collections.Generic.List<'T>() with get
- group_by_sort: unsupported syntax at line 6: type _Group<'T>(key: obj) =
  5: let total = "total"
  6: type _Group<'T>(key: obj) =
  7:   member val key = key with get, set
  8:   member val Items = System.Collections.Generic.List<'T>() with get
- group_items_iteration: unsupported syntax at line 6: type _Group<'T>(key: obj) =
  5: let total = "total"
  6: type _Group<'T>(key: obj) =
  7:   member val key = key with get, set
  8:   member val Items = System.Collections.Generic.List<'T>() with get
- if_else: ok
- if_then_else: ok
- if_then_else_nested: ok
- in_operator: parse2 error: parse error: 2:22: unexpected token "2" (expected ")")
- in_operator_extended: parse2 error: parse error: 4:22: unexpected token "1" (expected ")")
- inner_join: parse2 error: parse error: 7:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- join_multi: parse2 error: parse error: 6:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- json_builtin: unsupported syntax at line 30: ignore (_json m)
 29: let m = Map.ofList [(a, 1); (b, 2)]
 30: ignore (_json m)
- left_join: parse2 error: parse error: 7:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- left_join_multi: parse2 error: parse error: 7:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- len_builtin: ok
- len_map: parse2 error: parse error: 1:34: unexpected token "," (expected ")")
- len_string: ok
- let_and_print: ok
- list_assign: ok
- list_index: ok
- list_nested_assign: parse2 error: parse error: 2:11: unexpected token "[" (expected <ident>)
- list_set_ops: parse2 error: parse error: 5:22: unexpected token "," (expected "]")
- load_yaml: parse2 error: parse error: 13:85: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- map_assign: parse2 error: parse error: 1:34: unexpected token "," (expected ")")
- map_in_operator: parse2 error: parse error: 1:23: unexpected token "," (expected ")")
- map_index: parse2 error: parse error: 1:25: unexpected token "," (expected ")")
- map_int_key: parse2 error: parse error: 1:23: unexpected token "," (expected ")")
- map_literal_dynamic: parse2 error: parse error: 3:25: unexpected token "," (expected ")")
- map_membership: parse2 error: parse error: 1:25: unexpected token "," (expected ")")
- map_nested_assign: parse2 error: parse error: 1:32: unexpected token "," (expected ")")
- match_expr: parse2 error: parse error: 2:22: unexpected token "with" (expected "{" MatchCase* "}")
- match_full: parse2 error: parse error: 3:19: unexpected token "with" (expected "{" MatchCase* "}")
- math_ops: ok
- membership: parse2 error: parse error: 2:22: unexpected token "2" (expected ")")
- min_max_builtin: parse2 error: parse error: 2:15: unexpected token "nums" (expected ")")
- nested_function: type2 error: error[T002]: undefined variable: x
  --> :2:11

help:
  Check if the variable was declared in this scope.
- order_by_map: parse2 error: parse error: 3:27: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- outer_join: parse2 error: parse error: 7:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- partial_application: type2 error: error[T002]: undefined variable: a
  --> :2:11

help:
  Check if the variable was declared in this scope.
- print_hello: ok
- pure_fold: type2 error: error[T002]: undefined variable: x
  --> :2:11

help:
  Check if the variable was declared in this scope.
- pure_global_fold: type2 error: error[T002]: undefined variable: x
  --> :2:11

help:
  Check if the variable was declared in this scope.
- query_sum_select: type2 error: error[T000]: `let` requires a type or a value
  --> :2:1

help:
  Use `let x = ...` or `let x: int` to declare a variable.
- record_assign: unsupported syntax at line 17: ignore (inc c)
 16: let mutable c = { n = 0 }
 17: ignore (inc c)
 18: ignore (printfn "%A" (c.n))
- right_join: parse2 error: parse error: 7:33: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- save_jsonl_stdout: unsupported syntax at line 47: ignore (_save people Some "-" Some (Map.ofList [(format, "jsonl")]))
 46: let people = [|Map.ofList [(name, "Alice"); (age, 30)]; Map.ofList [(name, "Bob"); (age, 25)]|]
 47: ignore (_save people Some "-" Some (Map.ofList [(format, "jsonl")]))
- short_circuit: type2 error: error[T002]: undefined variable: a
  --> :2:11

help:
  Check if the variable was declared in this scope.
- slice: parse2 error: parse error: 1:17: unexpected token "[" (expected <ident>)
- sort_stable: parse2 error: parse error: 3:28: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- str_builtin: parse2 error: parse error: 1:15: unexpected token "123" (expected "(" (Expr ("," Expr)*)? ")")
- string_compare: ok
- string_concat: ok
- string_contains: parse2 error: parse error: 2:18: unexpected token "cat" (expected ")")
- string_in_operator: type2 error: error[T027]: string is not a struct
  --> :2:7

help:
  Field access is only valid on struct types.
- string_index: parse2 error: parse error: 2:15: unexpected token "s" (expected "(" (Expr ("," Expr)*)? ")")
- string_prefix_slice: parse2 error: parse error: 3:13: unexpected token ".." (expected "]")
- substring_builtin: type2 error: error[T039]: function substring expects 3 arguments, got 1
  --> :1:7

help:
  Pass exactly 3 arguments to `substring`.
- sum_builtin: parse2 error: parse error: 1:17: unexpected token "," (expected "]")
- tail_recursion: parse2 error: parse error: 4:9: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- test_block: unsupported syntax at line 13: let test_addition_works() =
 12: 
 13: let test_addition_works() =
 14:     let x = (1 + 2)
 15:     if not ((x = 3)) then failwith "expect failed"
- tree_sum: parse2 error: parse error: 6:19: unexpected token "with" (expected "{" MatchCase* "}")
- two-sum: parse2 error: parse error: 7:27: unexpected token "[" (expected <ident>)
- typed_let: panic: runtime error: invalid memory address or nil pointer dereference
- typed_var: panic: runtime error: invalid memory address or nil pointer dereference
- unary_neg: ok
- update_stmt: unsupported syntax at line 29: let test_update_adult_status() =
 28:     people.[i] <- item
 29: let test_update_adult_status() =
 30:     if not ((people = [|{ name = "Alice"; age = 17; status = "minor" }; { name = "Bob"; age = 26; status = "adult" }; { name = "Charlie"; age = 19; status = "adult" }; { name = "Diana"; age = 16; status = "minor" }|])) then failwith "expect failed"
 31: 
- user_type_literal: parse2 error: parse error: 9:26: unexpected token "," (expected PostfixExpr)
- values_builtin: parse2 error: parse error: 1:25: unexpected token "," (expected ")")
- var_assignment: ok
- while_loop: ok
