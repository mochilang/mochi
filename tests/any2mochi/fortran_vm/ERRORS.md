# Errors

- append_builtin: ok
- avg_builtin: generated code type error: error[T003]: unknown function: merge
  --> :2:9

help:
  Ensure the function is defined before it's called.
  1: fun main() {
  2:   print(merge(0.0, sum(real([1, 2, 3])) / size([1, 2, 3]), size([1, 2, 3]) == 0))
  3: }
  4: 
- basic_compare: ok
- binary_precedence: ok
- bool_chain: generated code parse error: parse error: 7:9: lexer: invalid input text "'boom')\n  let re..."
  1: fun main() {
  2:   print((((1 < 2))  &&  ((2 < 3)))  &&  ((3 < 4)))
  3:   print((((1 < 2))  &&  ((2 > 3)))  &&  boom())
  4:   print(((((1 < 2))  &&  ((2 < 3)))  &&  ((3 > 4)))  &&  boom())
  5: }
  6: fun boom()() {
  7:   print('boom')
  8:   let res = true
  9:   return
 10:   // end function boom
- break_continue: generated code parse error: parse error: 11:11: lexer: invalid input text "'odd number:', n..."
  1: fun main() {
  2:   let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  3:   for i_n in 0..size(numbers) - 1 {
  4:     let n = numbers((i_n % size(numbers)) + 1)
  5:     if ((n % 2) == 0) {
  6:       continue
  7:     }
  8:     if (n > 7) {
  9:       break
 10:     }
- cast_string_to_int: generated code parse error: parse error: 2:13: lexer: invalid input text "'1995'))\n}\n"
  1: fun main() {
  2:   print(int('1995'))
  3: }
  4: 
- cast_struct: compile error: unsupported expression
- closure: generated code parse error: parse error: 5:17: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   let add10 = makeAdder(10)
  3:   print(add10(7))
  4: }
  5: fun makeAdder(n)() {
  6:   let res = lambda_0
  7:   return
  8:   // end function makeAdder
  9:   // function lambda_0(x) result(res)
 10:   res = (x + n)
- count_builtin: generated code type error: error[T003]: unknown function: size
  --> :2:9

help:
  Ensure the function is defined before it's called.
  1: fun main() {
  2:   print(size([1, 2, 3]))
  3: }
  4: 
- cross_join: compile error: unsupported expression
- cross_join_filter: compile error: unsupported query expression
- cross_join_triple: compile error: unsupported query expression
- dataset_sort_take_limit: compile error: unsupported expression
- dataset_where_filter: compile error: unsupported expression
- exists_builtin: generated code parse error: parse error: 6:19: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   let data = [1, 2]
  3:   let flag = exists(lambda_0(data))
  4:   print(flag)
  5: }
  6: fun lambda_0(vsrc)() {
  7:   let n = 0
  8:   for i in 1..size(vsrc) {
  9:     let x = vsrc(i)
 10:     if (x == 1) {
- for_list_collection: generated code parse error: parse error: 2:15: unexpected token "," (expected "{" Statement* "}")
  1: fun main() {
  2:   for i_n in 0, size((/1, 2..3/)) - 1 {
  3:     let n = [1, 2, 3]((i_n % size([1, 2, 3])) + 1)
  4:     print(n)
  5: }
  6: 
- for_loop: generated code parse error: parse error: 5:1: unexpected token "<EOF>" (expected "}")
  1: fun main() {
  2:   for i in 1..4 - 1 {
  3:     print(i)
  4: }
  5: 
- for_map_collection: compile error: unsupported expression
- fun_call: generated code parse error: parse error: 4:11: unexpected token "(" (expected Param)
  1: fun main() {
  2:   print(add(2, 3))
  3: }
  4: fun add(a,() {
  5:   let res = (a + b)
  6:   return
  7:   // end function add
  8: }
  9: 
- fun_expr_in_let: generated code parse error: parse error: 5:16: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   let square = lambda_0
  3:   print(square(6))
  4: }
  5: fun lambda_0(x)() {
  6:   let res = (x * x)
  7:   return
  8:   // end function lambda_0
  9: }
 10: 
- fun_three_args: generated code parse error: parse error: 4:12: unexpected token "(" (expected Param)
  1: fun main() {
  2:   print(sum3(1, 2, 3))
  3: }
  4: fun sum3(a,() {
  5:   let res = ((a + b) + c)
  6:   return
  7:   // end function sum3
  8: }
  9: 
- group_by: compile error: unsupported expression
- group_by_conditional_sum: compile error: unsupported expression
- group_by_having: compile error: unsupported expression
- group_by_join: compile error: unsupported expression
- group_by_left_join: compile error: unsupported expression
- group_by_multi_join: compile error: unsupported expression
- group_by_multi_join_sort: compile error: unsupported expression
- group_by_sort: compile error: unsupported expression
- group_items_iteration: compile error: unsupported expression
- if_else: generated code parse error: parse error: 4:11: lexer: invalid input text "'big')\n  else {\n..."
  1: fun main() {
  2:   let x = 5
  3:   if (x > 3) {
  4:     print('big')
  5:   else {
  6:     print('small')
  7: }
  8: 
- if_then_else: generated code parse error: parse error: 3:19: lexer: invalid input text "'yes', 'no', (x ..."
  1: fun main() {
  2:   let x = 12
  3:   let msg = merge('yes', 'no', (x > 10))
  4:   print(msg)
  5: }
  6: 
- if_then_else_nested: generated code parse error: parse error: 3:19: lexer: invalid input text "'big', merge('me..."
  1: fun main() {
  2:   let x = 8
  3:   let msg = merge('big', merge('medium', 'small', (x > 5)), (x > 10))
  4:   print(msg)
  5: }
  6: 
- in_operator: generated code type error: error[T003]: unknown function: any
  --> :3:9

help:
  Ensure the function is defined before it's called.
  1: fun main() {
  2:   let xs = [1, 2, 3]
  3:   print(any(xs == 2))
  4:   print(! (any(xs == 5)))
  5: }
  6: 
- in_operator_extended: compile error: unsupported op in
- inner_join: compile error: unsupported expression
- join_multi: compile error: unsupported expression
- json_builtin: compile error: unsupported expression
- left_join: compile error: unsupported expression
- left_join_multi: compile error: unsupported expression
- len_builtin: generated code type error: error[T003]: unknown function: size
  --> :2:9

help:
  Ensure the function is defined before it's called.
  1: fun main() {
  2:   print(size([1, 2, 3]))
  3: }
  4: 
- len_map: compile error: unsupported expression
- len_string: generated code parse error: parse error: 2:13: lexer: invalid input text "'mochi'))\n}\n"
  1: fun main() {
  2:   print(len('mochi'))
  3: }
  4: 
- let_and_print: ok
- list_assign: generated code parse error: parse error: 3:9: unexpected token "_8" (expected ")")
  1: fun main() {
  2:   let nums = [1, 2]
  3:   nums(1_8 + 1) = 3
  4:   print(nums((1 % size(nums)) + 1))
  5: }
  6: 
- list_index: generated code type error: error[T004]: `xs` is not callable
  --> :3:9

help:
  Use a function or closure in this position.
  1: fun main() {
  2:   let xs = [10, 20, 30]
  3:   print(xs((1 % size(xs)) + 1))
  4: }
  5: 
- list_nested_assign: generated code parse error: parse error: 2:18: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   let matrix = [(/1, 2], [3, 4]/)
  3:   matrix(1_8 + 1) = 5
  4:   print(matrix((1 % size(matrix)) + 1)((0 % size(matrix)) + 1))
  5: }
  6: 
- list_set_ops: generated code parse error: parse error: 5:16: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   print(union_int([1, 2], [2, 3]))
  3:   print(except_int([1, 2, 3], [2]))
  4:   print(intersect_int([1, 2, 3], [2, 4]))
  5:   print(size([(/1, 2], [2, 3] /)))
  6: }
  7: fun union_int(a,() {
  8:   let n = 0
  9:   for i in 1..size(a) {
 10:     n = n + 1
- load_yaml: compile error: unsupported expression
- map_assign: compile error: unsupported expression
- map_in_operator: compile error: unsupported expression
- map_index: compile error: unsupported expression
- map_int_key: compile error: unsupported expression
- map_literal_dynamic: compile error: unsupported expression
- map_membership: compile error: unsupported expression
- map_nested_assign: compile error: unsupported expression
- match_expr: compile error: unsupported expression
- match_full: compile error: unsupported expression
- math_ops: ok
- membership: generated code type error: error[T003]: unknown function: any
  --> :3:9

help:
  Ensure the function is defined before it's called.
  1: fun main() {
  2:   let nums = [1, 2, 3]
  3:   print(any(nums == 2))
  4:   print(any(nums == 4))
  5: }
  6: 
- min_max_builtin: generated code type error: error[T003]: unknown function: merge
  --> :3:9

help:
  Ensure the function is defined before it's called.
  1: fun main() {
  2:   let nums = [3, 1, 4]
  3:   print(merge(0, minval(nums), size(nums) == 0))
  4:   print(max(nums))
  5: }
  6: 
- nested_function: compile error: unsupported statement
- order_by_map: compile error: unsupported expression
- outer_join: compile error: unsupported expression
- partial_application: generated code parse error: parse error: 5:11: unexpected token "(" (expected Param)
  1: fun main() {
  2:   let add5 = add(5)
  3:   print(add5(3))
  4: }
  5: fun add(a,() {
  6:   let res = (a + b)
  7:   return
  8:   // end function add
  9: }
 10: 
- print_hello: generated code parse error: parse error: 2:9: lexer: invalid input text "'hello')\n}\n"
  1: fun main() {
  2:   print('hello')
  3: }
  4: 
- pure_fold: generated code parse error: parse error: 4:14: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   print(triple((1 + 2)))
  3: }
  4: fun triple(x)() {
  5:   let res = (x * 3)
  6:   return
  7:   // end function triple
  8: }
  9: 
- pure_global_fold: generated code parse error: parse error: 5:11: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   let k = 2
  3:   print(inc(3))
  4: }
  5: fun inc(x)() {
  6:   let res = (x + k)
  7:   return
  8:   // end function inc
  9: }
 10: 
- query_sum_select: generated code parse error: parse error: 6:19: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   let nums = [1, 2, 3]
  3:   let result = lambda_0(nums)
  4:   print(result)
  5: }
  6: fun lambda_0(vsrc)() {
  7:   let n = 0
  8:   for i in 1..size(vsrc) {
  9:     n = vsrc(i)
 10:     if (n > 1) {
- record_assign: generated code parse error: parse error: 6:20: unexpected token "=" (expected ")")
  1: type Counter {
  2:   n: int
  3: }
  4: fun main() {
  5:   // end type Counter
  6:   let c = Counter(n=0)
  7:   // inc(c)
  8:   print(c.n)
  9: }
 10: fun inc(c)() {
- right_join: compile error: unsupported expression
- save_jsonl_stdout: compile error: unsupported expression
- short_circuit: generated code parse error: parse error: 6:9: lexer: invalid input text "'boom')\n  let re..."
  1: fun main() {
  2:   print(false  &&  boom(1, 2))
  3:   print(true  ||  boom(1, 2))
  4: }
  5: fun boom(a,() {
  6:   print('boom')
  7:   let res = true
  8:   return
  9:   // end function boom
 10: }
- slice: generated code parse error: parse error: 4:9: lexer: invalid input text "'hello'(1 + 1:4)..."
  1: fun main() {
  2:   print(/1, 2, 3/)(1 + 1:3)
  3:   print(/1, 2, 3/)(0 + 1:2)
  4:   print('hello'(1 + 1:4))
  5: }
  6: 
- sort_stable: compile error: unsupported expression
- str_builtin: generated code parse error: parse error: 4:15: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   print(str_int(123))
  3: }
  4: fun str_int(v)() {
  5:   // write(buf,'(I0)') v
  6:   let r = trim(buf)
  7:   // end function str_int
  8: }
  9: 
- string_compare: generated code parse error: parse error: 2:9: lexer: invalid input text "'a' < 'b')\n  pri..."
  1: fun main() {
  2:   print('a' < 'b')
  3:   print('a' <= 'a')
  4:   print('b' > 'a')
  5:   print('b' >= 'b')
  6: }
  7: 
- string_concat: generated code parse error: parse error: 2:14: lexer: invalid input text "'hello ') // tri..."
  1: fun main() {
  2:   print(trim('hello ') // trim('world'))
  3: }
  4: 
- string_contains: generated code parse error: parse error: 2:11: lexer: invalid input text "'catch'\n  print(..."
  1: fun main() {
  2:   let s = 'catch'
  3:   print(s.contains('cat'))
  4:   print(s.contains('dog'))
  5: }
  6: 
- string_in_operator: generated code parse error: parse error: 2:11: lexer: invalid input text "'catch'\n  print(..."
  1: fun main() {
  2:   let s = 'catch'
  3:   print(index(s, 'cat') > 0)
  4:   print(index(s, 'dog') > 0)
  5: }
  6: 
- string_index: generated code parse error: parse error: 2:11: lexer: invalid input text "'mochi'\n  print(..."
  1: fun main() {
  2:   let s = 'mochi'
  3:   print(s((1 % len(s)) + 1:(1 % len(s)) + 1))
  4: }
  5: 
- string_prefix_slice: generated code parse error: parse error: 2:16: lexer: invalid input text "'fore'\n  let s1 ..."
  1: fun main() {
  2:   let prefix = 'fore'
  3:   let s1 = 'forest'
  4:   print(s1((0 % len(s1)) + 1:(len(prefix) % len(s1))) == prefix)
  5:   let s2 = 'desert'
  6:   print(s2((0 % len(s2)) + 1:(len(prefix) % len(s2))) == prefix)
  7: }
  8: 
- substring_builtin: generated code parse error: parse error: 2:19: lexer: invalid input text "'mochi', 1, 4))\n..."
  1: fun main() {
  2:   print(substring('mochi', 1, 4))
  3: }
  4: 
- sum_builtin: ok
- tail_recursion: generated code parse error: parse error: 4:15: unexpected token "(" (expected Param)
  1: fun main() {
  2:   print(sum_rec(10, 0))
  3: }
  4: fun sum_rec(n,() {
  5:   if (n == 0) {
  6:     let res = acc
  7:     return
  8:   }
  9:   res = sum_rec((n - 1), (acc + n))
 10:   return
- test_block: generated code parse error: parse error: 2:9: lexer: invalid input text "'ok')\n  // call ..."
  1: fun main() {
  2:   print('ok')
  3:   // call test_addition_works()
  4: }
  5: fun test_addition_works()() {
  6:   let x = (1 + 2)
  7:   if ! (x == 3) {
  8:     print('expect failed')
  9:     // stop 1
 10:   }
- tree_sum: compile error: union types not supported
- two-sum: generated code parse error: parse error: 6:17: unexpected token "(" (expected Param)
  1: fun main() {
  2:   let result = twoSum([2, 7, 11, 15], 9)
  3:   print(result(0 + 1))
  4:   print(result(1 + 1))
  5: }
  6: fun twoSum(nums,() {
  7:   let n = size(nums)
  8:   for i in 0..n - 1 {
  9:     for j in (i + 1)..n - 1 {
 10:       if ((nums(i + 1) + nums(j + 1)) == target) {
- typed_let: generated code type error: error[T002]: undefined variable: y
  --> :2:9

help:
  Check if the variable was declared in this scope.
  1: fun main() {
  2:   print(y)
  3: }
  4: 
- typed_var: generated code type error: error[T002]: undefined variable: x
  --> :2:9

help:
  Check if the variable was declared in this scope.
  1: fun main() {
  2:   print(x)
  3: }
  4: 
- unary_neg: ok
- update_stmt: compile error: unsupported statement
- user_type_literal: ok
- values_builtin: compile error: unsupported expression
- var_assignment: generated code type error: error[T024]: cannot assign to `x` (immutable)
  --> :3:3

help:
  Use `var` to declare mutable variables.
  1: fun main() {
  2:   let x = 1
  3:   x = 2
  4:   print(x)
  5: }
  6: 
- while_loop: generated code parse error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
  1: fun main() {
  2:   let i = 0
  3:   while (i < 3) {
  4:     print(i)
  5:     i = (i + 1)
  6: }
  7: 
