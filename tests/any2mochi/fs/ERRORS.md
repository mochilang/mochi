# Errors

- avg_builtin.fs: parse error: parse error: 7:13: unexpected token "," (expected "]")
- bool_ops.fs: parse error: parse error: 3:12: unexpected token "false" (expected "(" (Expr ("," Expr)*)? ")")
- break_continue.fs: unsupported syntax at line 2: exception BreakException of int
  1: open System
  2: exception BreakException of int
  3: exception ContinueException of int
  4: 
- ceil_builtin.fs: parse error: parse error: 1:26: unexpected token "2.3" (expected ")")
- closure.fs: parse error: parse error: 3:22: unexpected token ")" (expected "<" TypeRef ("," TypeRef)* ">")
- count_builtin.fs: parse error: parse error: 7:15: unexpected token "," (expected "]")
- dataset.fs: parse error: parse error: 1:32: unexpected token "," (expected "(" (Expr ("," Expr)*)? ")")
- dataset_sort_take_limit.fs: parse error: parse error: 9:34: unexpected token "," (expected PostfixExpr)
- expect_simple.fs: parse error: parse error: 2:11: unexpected token "=" (expected "(" (Expr ("," Expr)*)? ")")
- fetch_builtin.fs: type error: error[T002]: undefined variable: None
  --> :9:65

help:
  Check if the variable was declared in this scope.
- fetch_cast.fs: parse error: parse error: 17:32: unexpected token "https://jsonplaceholder.typicode.com/todos/1" (expected "(" (Expr ("," Expr)*)? ")")
- fetch_http.fs: type error: error[T002]: undefined variable: None
  --> :15:66

help:
  Check if the variable was declared in this scope.
- floor_builtin.fs: parse error: parse error: 1:24: unexpected token "2.7" (expected ")")
- fold_pure_let.fs: parse error: parse error: 6:12: unexpected token "n" (expected ")")
- for_list_collection.fs: parse error: parse error: 1:13: lexer: invalid input text "; 2; 3|] {\n  pri..."
- for_string_collection.fs: type error: error[T002]: undefined variable: string
  --> :2:12

help:
  Check if the variable was declared in this scope.
- fun_call.fs: type error: error[T002]: undefined variable: a
  --> :2:11

help:
  Check if the variable was declared in this scope.
- group_by.fs: unsupported syntax at line 3: type _Group<'T>(key: obj) =
  2: 
  3: type _Group<'T>(key: obj) =
  4:   member val key = key with get, set
  5:   member val Items = System.Collections.Generic.List<'T>() with get
- if_else.fs: unsupported syntax at line 11: else
 10:             raise (Return_foo (0))
 11:         else
 12:             raise (Return_foo (1))
 13:         failwith "unreachable"
- list_concat.fs: parse error: parse error: 1:22: unexpected token "," (expected "]")
- list_prepend.fs: type error: error[T002]: undefined variable: level
  --> :2:15

help:
  Check if the variable was declared in this scope.
- list_slice.fs: parse error: parse error: 1:20: unexpected token "[" (expected <ident>)
- load_save_json.fs: unsupported syntax at line 110: ignore (_save people None Some (Map.ofList [(format, "json")]))
109: let people = _load None Some (Map.ofList [(format, "json")]) |> List.map (fun row -> _cast<Person>(row))
110: ignore (_save people None Some (Map.ofList [(format, "json")]))
- map_index.fs: parse error: parse error: 1:34: unexpected token "," (expected ")")
- map_iterate.fs: parse error: parse error: 5:19: unexpected token "m" (expected "{" Statement* "}")
- map_len.fs: parse error: parse error: 1:34: unexpected token "," (expected ")")
- map_set.fs: parse error: parse error: 1:30: unexpected token "," (expected ")")
- map_typed_var.fs: type error: error[T002]: undefined variable: Map
  --> :1:9

help:
  Check if the variable was declared in this scope.
- match_capture.fs: parse error: parse error: 6:19: unexpected token "with" (expected "{" MatchCase* "}")
- nested_type.fs: type error: error[T001]: assignment to undeclared variable: name
  --> :5:13

help:
  Declare `name` first using `let`.
- pow_builtin.fs: parse error: parse error: 1:22: unexpected token "2" (expected ")")
- set_ops.fs: parse error: parse error: 9:14: unexpected token "a" (expected ")")
- simple_struct.fs: parse error: parse error: 9:25: unexpected token "," (expected PostfixExpr)
- str_builtin.fs: parse error: parse error: 1:15: unexpected token "123" (expected "(" (Expr ("," Expr)*)? ")")
- string_in.fs: type error: error[T004]: `` is not callable
  --> :1:23

help:
  Use a function or closure in this position.
- string_index.fs: parse error: parse error: 2:15: unexpected token "text" (expected "(" (Expr ("," Expr)*)? ")")
- string_negative_index.fs: parse error: parse error: 2:15: unexpected token "text" (expected "(" (Expr ("," Expr)*)? ")")
- string_slice.fs: parse error: parse error: 1:15: unexpected token "[" (expected <ident>)
- string_slice_negative.fs: parse error: parse error: 1:15: unexpected token "[" (expected <ident>)
- tpch_q1.fs: unsupported syntax at line 20: type _Group<'T>(key: obj) =
 19: let count_order = "count_order"
 20: type _Group<'T>(key: obj) =
 21:   member val key = key with get, set
 22:   member val Items = System.Collections.Generic.List<'T>() with get
- tpch_q2.fs: unsupported syntax at line 116: ignore (_json result)
115:     |> Array.map snd
116: ignore (_json result)
117: let test_Q2_returns_only_supplier_with_min_cost_in_Europe_for_brass_part() =
118:     if not ((result = [|Map.ofList [(s_acctbal, 1000.0); (s_name, "BestSupplier"); (n_name, "FRANCE"); (p_partkey, 1000); (p_mfgr, "M1"); (s_address, "123 Rue"); (s_phone, "123"); (s_comment, "Fast and reliable"); (ps_supplycost, 10.0)]|])) then failwith "expect failed"
- two_sum.fs: parse error: parse error: 7:27: unexpected token "[" (expected <ident>)
- union_match.fs: parse error: parse error: 6:19: unexpected token "with" (expected "{" MatchCase* "}")
- update_statement.fs: unsupported syntax at line 29: let test_update_adult_status() =
 28:     people.[i] <- item
 29: let test_update_adult_status() =
 30:     if not ((people = [|{ name = "Alice"; age = 17; status = "minor" }; { name = "Bob"; age = 26; status = "adult" }; { name = "Charlie"; age = 19; status = "adult" }; { name = "Diana"; age = 16; status = "minor" }|])) then failwith "expect failed"
 31: 
