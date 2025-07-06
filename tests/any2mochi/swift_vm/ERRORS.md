# Errors

- append_builtin: ok
- avg_builtin: parse error: parse error: 2:39: lexer: invalid input text "$0) }))\n}\nmain()..."
- basic_compare: ok
- binary_precedence: ok
- bool_chain: ok
- break_continue: ok
- cast_string_to_int: type error: error[T003]: unknown function: _cast
  --> :2:11

help:
  Ensure the function is defined before it's called.
- cast_struct: line 16:12: global function '_cast' requires that 'Todo' conform to 'Decodable'
  15| 
  16| let todo = _cast(Todo.self, ["title": "hi"])
    |            ^
  17| func main() {
  18|   print(todo.title)
- closure: parse error: parse error: 3:16: unexpected token ":" (expected "(" (Expr ("," Expr)*)? ")")
- count_builtin: ok
- cross_join: line 22:12: cannot convert value of type '[[String : Any]]' to closure result type '[[String : Int]]'
  21|     var _items = _res
  22|     return _items
    |            ^
  23|   }())
  24| func main() {
- cross_join_filter: parse error: parse error: 9:21: unexpected token ":" (expected "]")
- cross_join_triple: parse error: parse error: 10:23: unexpected token ":" (expected "]")
- dataset_sort_take_limit: line 13:36: unary operator '-' cannot be applied to an operand of type 'Any'
  12|     for p in products {
  13|       _pairs.append((item: p, key: -p["price"]!))
    |                                    ^
  14|     }
  15|     _pairs.sort { a, b in
- dataset_where_filter: line 11:25: cannot convert value of type 'Any' to expected argument type 'Int'
  10|     for person in people {
  11|       if !(person["age"]! >= 18) { continue }
    |                         ^
  12|       _res.append([
  13|         "name": person["name"]!, "age": person["age"]!, "is_senior": person["age"]! >= 60,
- exists_builtin: type error: error[T027]: [any] is not a struct
  --> :7:3

help:
  Field access is only valid on struct types.
- for_list_collection: ok
- for_loop: parse error: parse error: 2:17: unexpected token "<" (expected PostfixExpr)
- for_map_collection: parse error: parse error: 1:13: unexpected token ":" (expected "]")
- fun_call: ok
- fun_expr_in_let: parse error: parse error: 1:18: unexpected token ":" (expected "(" (Expr ("," Expr)*)? ")")
- fun_three_args: ok
- group_by: line 53:16: generic parameter 'T' could not be inferred
  52|     "city": g.key, "count": g.count,
  53|     "avg_age": _avg(
    |                ^
  54|       ({
  55|         var _res: [Any] = []
- group_by_conditional_sum: line 25:20: cannot find 'g' in scope
  24|           item: [
  25|             "cat": g.key,
    |                    ^
  26|             "share": _sum(
  27|               ({
- group_by_having: line 46:55: value of type 'Any' has no subscripts
  45| ]
  46| let big = _group_by(people.map { $0 as Any }, { p in p["city"]! }).map { g in
    |                                                       ^
  47|   ["city": g.key, "num": g.count]
  48| }
- group_by_join: line 13:30: cannot find 'g' in scope
  12|         if !(o["customerId"]! == c["id"]!) { continue }
  13|         _res.append(["name": g.key, "count": g.count])
    |                              ^
  14|       }
  15|     }
- group_by_left_join: line 16:19: cannot find 'g' in scope
  15|         _res.append([
  16|           "name": g.key,
    |                   ^
  17|           "count":
  18|             ({
- group_by_multi_join: line 57:23: cannot convert value of type 'Any' to expected argument type 'Int'
  56|         for n in nations {
  57|           if !(n["id"]! == s["nation"]!) { continue }
    |                       ^
  58|           if !(n["name"]! == "A") { continue }
  59|           _res.append(["part": ps["part"]!, "value": ps["cost"]! * ps["qty"]!])
- group_by_multi_join_sort: line 47:34: cannot find 'g' in scope
  46|                   item: [
  47|                     "c_custkey": g.key.c_custkey, "c_name": g.key.c_name,
    |                                  ^
  48|                     "revenue": _sum(
  49|                       ({
- group_by_sort: line 24:20: cannot find 'g' in scope
  23|           item: [
  24|             "cat": g.key,
    |                    ^
  25|             "total": _sum(
  26|               ({
- group_items_iteration: line 38:63: value of type 'Any' has no subscripts
  37| let data = [["tag": "a", "val": 1], ["tag": "a", "val": 2], ["tag": "b", "val": 3]]
  38| let groups: [Any] = _group_by(data.map { $0 as Any }, { d in d["tag"]! }).map { g in g }
    |                                                               ^
  39| var tmp: [Any] = []
  40| let result =
- if_else: ok
- if_then_else: parse error: parse error: 2:19: lexer: invalid input text "? \"yes\" : \"no\")\n..."
- if_then_else_nested: parse error: parse error: 2:19: lexer: invalid input text "? \"big\" : (x > 5..."
- in_operator: type error: error[T027]: [int] is not a struct
  --> :3:11

help:
  Field access is only valid on struct types.
- in_operator_extended: parse error: parse error: 20:26: lexer: invalid input text "\"))\n}\nmain()\n"
- inner_join: line 15:41: cannot convert value of type 'Any' to expected argument type 'Int'
  14|       for c in customers {
  15|         if !(o["customerId"]! == c["id"]!) { continue }
    |                                         ^
  16|         _res.append(["orderId": o["id"]!, "customerName": c["name"]!, "total": o["total"]!])
  17|       }
- join_multi: line 11:41: cannot convert value of type 'Any' to expected argument type 'Int'
  10|       for c in customers {
  11|         if !(o["customerId"]! == c["id"]!) { continue }
    |                                         ^
  12|         for i in items {
  13|           if !(o["id"]! == i["orderId"]!) { continue }
- json_builtin: parse error: parse error: 1:13: unexpected token ":" (expected "]")
- left_join: line 12:41: cannot convert value of type 'Any' to expected argument type 'Int'
  11|       for c in customers {
  12|         if !(o["customerId"]! == c["id"]!) { continue }
    |                                         ^
  13|         _res.append(["orderId": o["id"]!, "customer": c, "total": o["total"]!])
  14|       }
- left_join_multi: line 11:41: cannot convert value of type 'Any' to expected argument type 'Int'
  10|       for c in customers {
  11|         if !(o["customerId"]! == c["id"]!) { continue }
    |                                         ^
  12|         for i in items {
  13|           if !(o["id"]! == i["orderId"]!) { continue }
- len_builtin: ok
- len_map: parse error: parse error: 2:15: unexpected token ":" (expected "]")
- len_string: ok
- let_and_print: ok
- list_assign: type error: error[T024]: cannot assign to `nums` (immutable)
  --> :3:5

help:
  Use `var` to declare mutable variables.
- list_index: type error: error[T003]: unknown function: _index
  --> :3:11

help:
  Ensure the function is defined before it's called.
- list_nested_assign: type error: error[T024]: cannot assign to `matrix` (immutable)
  --> :3:5

help:
  Use `var` to declare mutable variables.
- list_set_ops: line 22:16: binary operator '+' cannot be applied to operands of type '[Int]' and 'Int'
  21|   print(_intersect([1, 2, 3], [2, 4]))
  22|   print([1, 2] + [2, 3].count)
    |                ^
  23| }
  24| main()
- load_yaml: line 36:28: single-quoted string literal found, use '"'
  35|     let header = (opts?["header"] as? Bool) ?? true
  36|     var delim: Character = ','
    |                            ^
  37|     if let d = opts?["delimiter"] as? String, !d.isEmpty { delim = d.first! }
  38|     let text = _readInput(path)
- map_assign: parse error: parse error: 1:22: unexpected token ":" (expected "]")
- map_in_operator: line 3:25: cannot convert value of type 'String' to expected dictionary key type 'Int'
   2| 
   3| let m: [Int: String] = [String(describing: 1): "a", String(describing: 2): "b"]
    |                         ^
   4| func main() {
   5|   print(m[1] != nil)
- map_index: parse error: parse error: 1:13: unexpected token ":" (expected "]")
- map_int_key: line 3:25: cannot convert value of type 'String' to expected dictionary key type 'Int'
   2| 
   3| let m: [Int: String] = [String(describing: 1): "a", String(describing: 2): "b"]
    |                         ^
   4| func main() {
   5|   print(m[1]!)
- map_literal_dynamic: parse error: parse error: 3:13: unexpected token ":" (expected "]")
- map_membership: parse error: parse error: 1:13: unexpected token ":" (expected "]")
- map_nested_assign: line 5:15: value of optional type '[String : Int]?' must be unwrapped to refer to member 'subscript' of wrapped base type '[String : Int]'
   4| func main() {
   5|   data["outer"]["inner"] = 2
    |               ^
   6|   print(data["outer"]!["inner"]!)
   7| }
- match_expr: type error: error[T008]: type mismatch: expected R, got string
  --> :14:8

help:
  Change the value to match the expected type.
- match_full: line 59:12: 'nil' is not compatible with closure result type 'String'
  58|     }
  59|     return nil
    |            ^
  60|   }())
  61| func main() {
- math_ops: ok
- membership: type error: error[T027]: [int] is not a struct
  --> :3:11

help:
  Field access is only valid on struct types.
- min_max_builtin: type error: error[T003]: unknown function: _min
  --> :3:11

help:
  Ensure the function is defined before it's called.
- nested_function: parse error: parse error: 3:18: unexpected token "y" (expected ")")
- order_by_map: parse error: parse error: 9:21: lexer: invalid input text "? Int, let bi = ..."
- outer_join: line 16:41: cannot convert value of type 'Any' to expected argument type 'Int'
  15|       for c in customers {
  16|         if !(o["customerId"]! == c["id"]!) { continue }
    |                                         ^
  17|         _res.append(["order": o, "customer": c])
  18|       }
- partial_application: line 10:17: missing argument for parameter #2 in call
   9| 
  10| let add5 = add(5)
    |                 ^
  11| func main() {
  12|   print(add5(3))
- print_hello: ok
- pure_fold: ok
- pure_global_fold: type error: error[T002]: undefined variable: k
  --> :3:16

help:
  Check if the variable was declared in this scope.
- query_sum_select: line 20:19: generic parameter 'T' could not be inferred
  19|       if !(n > 1) { continue }
  20|       _res.append(_sum(n.map { Double($0) }))
    |                   ^
  21|     }
  22|     var _items = _res
- record_assign: line 10:11: cannot assign value of type 'Int' to type 'Counter'
   9| 
  10|   c = c.n + 1
    |           ^
  11| }
  12| 
- right_join: line 16:41: cannot convert value of type 'Any' to expected argument type 'Int'
  15|       for o in orders {
  16|         if !(o["customerId"]! == c["id"]!) { continue }
    |                                         ^
  17|         _res.append(["customerName": c["name"]!, "order": o])
  18|       }
- save_jsonl_stdout: line 13:28: single-quoted string literal found, use '"'
  12|     let header = (opts?["header"] as? Bool) ?? false
  13|     var delim: Character = ','
    |                            ^
  14|     if let d = opts?["delimiter"] as? String, !d.isEmpty { delim = d.first! }
  15|     var text = ""
- short_circuit: invalid character '4' looking for beginning of object key string
- slice: type error: error[T003]: unknown function: _slice
  --> :2:11

help:
  Ensure the function is defined before it's called.
- sort_stable: parse error: parse error: 9:21: lexer: invalid input text "? Int, let bi = ..."
- str_builtin: parse error: parse error: 2:28: unexpected token ":" (expected ")")
- string_compare: ok
- string_concat: ok
- string_contains: parse error: parse error: 4:26: lexer: invalid input text "\"))\n}\nmain()\n"
- string_in_operator: parse error: parse error: 4:26: lexer: invalid input text "\"))\n}\nmain()\n"
- string_index: parse error: parse error: 1:9: lexer: invalid input text "\"\nfun main() {\n ..."
- string_prefix_slice: parse error: parse error: 3:10: lexer: invalid input text "\"\nfun main() {\n ..."
- substring_builtin: ok
- sum_builtin: parse error: parse error: 2:39: lexer: invalid input text "$0) }))\n}\nmain()..."
- tail_recursion: ok
- test_block: parse error: parse error: 1:5: unexpected token "expect" (expected <ident> "(" (Param ("," Param)*)? ")" (":" TypeRef)? "{" Statement* "}")
- tree_sum: line 31:20: argument type 'Leaf.Type' does not conform to expected type 'Tree'
  30| 
  31| let t = Node(left: Leaf, value: 1, right: Node(left: Leaf, value: 2, right: Leaf))
    |                    ^
  32| func main() {
  33|   print(sum_tree(t))
- two-sum: parse error: parse error: 5:17: unexpected token "<" (expected PostfixExpr)
- typed_let: type error: error[T002]: undefined variable: y
  --> :2:11

help:
  Check if the variable was declared in this scope.
- typed_var: type error: error[T002]: undefined variable: x
  --> :2:11

help:
  Check if the variable was declared in this scope.
- unary_neg: ok
- update_stmt: line 15:12: operator function '==' requires that 'Person' conform to 'Equatable'
  14|   expect(
  15|     people == [
    |            ^
  16|       Person(name: "Alice", age: 17, status: "minor"),
  17|       Person(name: "Bob", age: 26, status: "adult"),
- user_type_literal: parse error: parse error: 8:1: unexpected token "}" (expected ":" TypeRef)
- values_builtin: parse error: parse error: 1:13: unexpected token ":" (expected "]")
- var_assignment: type error: error[T024]: cannot assign to `x` (immutable)
  --> :3:5

help:
  Use `var` to declare mutable variables.
- while_loop: vm compile error: assignment to undeclared variable: i
