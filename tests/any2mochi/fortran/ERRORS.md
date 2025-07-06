# Errors

- break_continue: generated code parse error: parse error: 11:1: unexpected token "<EOF>" (expected "}")
  1: fun main() {
  2:   for i in 0..10 - 1 {
  3:     if ((i % 2) == 0) {
  4:       continue
  5:     }
  6:     if (i > 5) {
  7:       break
  8:     }
  9:     print(i)
 10: }
- builtin_append: generated code parse error: parse error: 2:9: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   xs = (/ (/1, 2/), 3 /)
  3:   print(xs((2 % size(xs)) + 1))
  4: }
  5: 
- builtin_lower: generated code parse error: parse error: 2:19: lexer: invalid input text "'HELLO'))\n}\nfun ..."
  1: fun main() {
  2:   print(str_lower('HELLO'))
  3: }
  4: fun str_lower(v)() {
  5:   for i in 1..len(v) {
  6:     if (iachar(v(i:i)) >= iachar('A'))  &&  (iachar(v(i:i)) <= iachar('Z')) {
  7:       r(i:i) = achar(iachar(v(i:i)) + 32)
  8:     else {
  9:       r(i:i) = v(i:i)
 10:     }
- builtin_min_numeric: generated code parse error: parse error: 2:26: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   print(merge(0, minval((/3, 1, 2/)), size((/3, 1, 2/)) == 0))
  3:   print(merge(0.0, minval((/1.5, 2.5/)), size((/1.5, 2.5/)) == 0))
  4: }
  5: 
- builtin_str: generated code parse error: parse error: 4:9: lexer: invalid input text "'hi')\n}\nfun str_..."
  1: fun main() {
  2:   print(str_int(42))
  3:   print(str_float(3.5))
  4:   print('hi')
  5: }
  6: fun str_int(v)() {
  7:   // write(buf,'(I0)') v
  8:   r = trim(buf)
  9:   // end function str_int
 10:   // function str_float(v) result(r)
- builtin_upper: generated code parse error: parse error: 2:19: lexer: invalid input text "'hello'))\n}\nfun ..."
  1: fun main() {
  2:   print(str_upper('hello'))
  3: }
  4: fun str_upper(v)() {
  5:   for i in 1..len(v) {
  6:     if (iachar(v(i:i)) >= iachar('a'))  &&  (iachar(v(i:i)) <= iachar('z')) {
  7:       r(i:i) = achar(iachar(v(i:i)) - 32)
  8:     else {
  9:       r(i:i) = v(i:i)
 10:     }
- dataset_sort_take_limit: generated code parse error: parse error: 7:29: lexer: invalid input text "'Laptop', price=..."
  1: type Product {
  2:   name: string
  3:   price: int
  4: }
  5: fun main() {
  6:   // end type Product
  7:   products = (/Product(name='Laptop', price=1500), Product(name='Smartphone', price=900), Product(name='Tablet', price=600), Product(name='Monitor', price=300), Product(name='Keyboard', price=100), Product(name='Mouse', price=50), Product(name='Headphones', price=200)/)
  8:   expensive = lambda_0(products)(1 + 1:1 + 3)
  9:   print('--- Top products (excluding most expensive) ---')
 10:   for i_item in 0..size(expensive) - 1 {
- dataset_sort_where: generated code parse error: parse error: 7:23: lexer: invalid input text "'A', price=100),..."
  1: type Item {
  2:   name: string
  3:   price: int
  4: }
  5: fun main() {
  6:   // end type Item
  7:   items = (/Item(name='A', price=100), Item(name='B', price=50), Item(name='C', price=200), Item(name='D', price=80)/)
  8:   cheap = lambda_0(items)
  9:   for i_c in 0..size(cheap) - 1 {
 10:     c = cheap((i_c % size(cheap)) + 1)
- fetch_builtin: generated code parse error: parse error: 6:21: lexer: invalid input text "'file://tests/co..."
  1: type Msg {
  2:   message: string
  3: }
  4: fun main() {
  5:   // end type Msg
  6:   v__ = mochi_fetch('file://tests/compiler/fortran/fetch_builtin.json')
  7: }
  8: fun mochi_fetch(url)() {
  9:   cmd = 'curl -s -o mochi_fetch.tmp ' // trim(url)
 10:   // call execute_command_line(cmd)
- fetch_http: generated code parse error: parse error: 2:22: lexer: invalid input text "'https://jsonpla..."
  1: fun main() {
  2:   body = mochi_fetch('https://jsonplaceholder.typicode.com/todos/1')
  3:   print(len(body) > 0)
  4: }
  5: fun mochi_fetch(url)() {
  6:   cmd = 'curl -s -o mochi_fetch.tmp ' // trim(url)
  7:   // call execute_command_line(cmd)
  8:   open(newunit = u, file='mochi_fetch.tmp', access='stream', form='unformatted', action='read')
  9:   inquire(u, size = n)
 10:   // read(u) r
- float_ops: generated code type error: error[T001]: assignment to undeclared variable: x
  --> :2:3

help:
  Declare `x` first using `let`.
  1: fun main() {
  2:   x = 1.5
  3:   y = 2.5
  4:   print(x + y)
  5:   print(y - x)
  6:   print(x * y)
  7:   print(y / x)
  8: }
  9: 
- for_loop: generated code parse error: parse error: 5:1: unexpected token "<EOF>" (expected "}")
  1: fun main() {
  2:   for i in 1..4 - 1 {
  3:     print(i)
  4: }
  5: 
- fun_expr: generated code parse error: parse error: 4:16: unexpected token "(" (expected Param)
  1: fun main() {
  2:   print(lambda_0)(2, 3)
  3: }
  4: fun lambda_0(a,() {
  5:   res = (a + b)
  6:   return
  7:   // end function lambda_0
  8: }
  9: 
- if_else: generated code parse error: parse error: 6:11: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   print(foo((-2)))
  3:   print(foo(0))
  4:   print(foo(3))
  5: }
  6: fun foo(n)() {
  7:   if (n < 0) {
  8:     res = (-1)
  9:     return
 10:   else if (n == 0) {
- list_append: generated code parse error: parse error: 2:9: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   xs = (/1, 2/)
  3:   xs = (/ xs, (/3/) /)
  4:   print(xs((2 % size(xs)) + 1))
  5: }
  6: 
- list_concat: generated code parse error: parse error: 2:9: unexpected token "/" (expected ")")
  1: fun main() {
  2:   print(/ (/1, 2/), (/3, 4/) /)
  3: }
  4: 
- list_index: generated code parse error: parse error: 2:9: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   xs = (/10, 20, 30/)
  3:   print(xs((1 % size(xs)) + 1))
  4: }
  5: 
- list_slice: generated code parse error: parse error: 2:9: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   xs = (/1, 2, 3, 4/)
  3:   print(xs((1 % size(xs)) + 1:(3 % size(xs))))
  4: }
  5: 
- list_union_all: generated code parse error: parse error: 2:9: unexpected token "/" (expected ")")
  1: fun main() {
  2:   print(/ (/1, 2/), (/2, 3/) /)
  3: }
  4: 
- multi_functions: generated code parse error: parse error: 6:11: unexpected token "(" (expected Param)
  1: fun main() {
  2:   print(add(1, 2))
  3:   print(sub(10, 3))
  4:   print(mul(4, 6))
  5: }
  6: fun add(x,() {
  7:   res = (x + y)
  8:   return
  9:   // end function add
 10:   // function sub(x, y) result(res)
- query_basic: generated code parse error: parse error: 2:9: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   xs = (/1, 10, 20, 5/)
  3:   ys = lambda_0(xs)
  4:   for i_v in 0..size(ys) - 1 {
  5:     v = ys((i_v % size(ys)) + 1)
  6:     print(v)
  7:   }
  8: }
  9: fun lambda_0(vsrc)() {
 10:   n = 0
- simple_fn: generated code parse error: parse error: 4:10: unexpected token "(" (expected "{" Statement* "}")
  1: fun main() {
  2:   print(id(123))
  3: }
  4: fun id(x)() {
  5:   res = x
  6:   return
  7:   // end function id
  8: }
  9: 
- string_cmp: generated code parse error: parse error: 2:9: lexer: invalid input text "'a' < 'b')\n  pri..."
  1: fun main() {
  2:   print('a' < 'b')
  3:   print('a' <= 'a')
  4:   print('cat' > 'car')
  5:   print('dog' >= 'dog')
  6:   print('abc' == 'abc')
  7:   print('foo' /= 'bar')
  8: }
  9: 
- string_concat: generated code parse error: parse error: 2:14: lexer: invalid input text "'hello ') // tri..."
  1: fun main() {
  2:   print(trim('hello ') // trim('world'))
  3: }
  4: 
- string_for_loop: generated code parse error: parse error: 2:22: lexer: invalid input text "'cat') - 1 {\n   ..."
  1: fun main() {
  2:   for i_ch in 0..len('cat') - 1 {
  3:     ch = 'cat'((i_ch % len('cat')) + 1:(i_ch % len('cat')) + 1)
  4:     print(ch)
  5: }
  6: 
- string_in: generated code parse error: parse error: 2:15: lexer: invalid input text "'catch', 'cat') ..."
  1: fun main() {
  2:   print(index('catch', 'cat') > 0)
  3:   print(index('catch', 'dog') > 0)
  4: }
  5: 
- string_index: generated code parse error: parse error: 2:10: lexer: invalid input text "'hello'\n  print(..."
  1: fun main() {
  2:   text = 'hello'
  3:   print(text((1 % len(text)) + 1:(1 % len(text)) + 1))
  4: }
  5: 
- string_len: generated code parse error: parse error: 2:13: lexer: invalid input text "'hello'))\n}\n"
  1: fun main() {
  2:   print(len('hello'))
  3: }
  4: 
- string_literal: generated code parse error: parse error: 2:9: lexer: invalid input text "'hello')\n}\n"
  1: fun main() {
  2:   print('hello')
  3: }
  4: 
- string_negative_index: generated code parse error: parse error: 2:10: lexer: invalid input text "'hello'\n  print(..."
  1: fun main() {
  2:   text = 'hello'
  3:   print(text(((-1) % len(text)) + 1:((-1) % len(text)) + 1))
  4: }
  5: 
- string_slice: generated code parse error: parse error: 2:10: lexer: invalid input text "'hello'\n  print(..."
  1: fun main() {
  2:   text = 'hello'
  3:   print(text((1 % len(text)) + 1:(4 % len(text))))
  4: }
  5: 
- two_sum: generated code parse error: parse error: 2:20: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   result = twoSum((/2, 7, 11, 15/), 9)
  3:   print(result(0 + 1))
  4:   print(result(1 + 1))
  5: }
  6: fun twoSum(nums,() {
  7:   n = size(nums)
  8:   for i in 0..n - 1 {
  9:     for j in (i + 1)..n - 1 {
 10:       if ((nums(i + 1) + nums(j + 1)) == target) {
- typed_list_float: generated code parse error: parse error: 2:9: unexpected token "/" (expected PostfixExpr)
  1: fun main() {
  2:   xs = (/1.5, 2.5/)
  3:   print(xs((0 % size(xs)) + 1))
  4: }
  5: 
- typed_list_negative: generated code parse error: parse error: 7:11: lexer: invalid input text "'expect failed')..."
  1: fun main() {
  2:   xs = (/((-1)), 0, 1/)
  3:   // call test_values()
  4: }
  5: fun test_values()() {
  6:   if ! (all(xs == (/((-1)), 0, 1/))) {
  7:     print('expect failed')
  8:     // stop 1
  9:   }
 10:   print('done')
- typed_list_param: generated code parse error: parse error: 2:17: lexer: invalid input text "'hello', 'world'..."
  1: fun main() {
  2:   print(first((/'hello', 'world'/)))
  3: }
  4: fun first(xs)() {
  5:   res = xs((0 % len(xs)) + 1:(0 % len(xs)) + 1)
  6:   return
  7:   // end function first
  8: }
  9: 
- var_assignment: generated code type error: error[T001]: assignment to undeclared variable: x
  --> :2:3

help:
  Declare `x` first using `let`.
  1: fun main() {
  2:   x = 1
  3:   x = 2
  4:   print(x)
  5: }
  6: 
- while_loop: generated code parse error: parse error: 7:1: unexpected token "<EOF>" (expected "}")
  1: fun main() {
  2:   i = 0
  3:   while (i < 3) {
  4:     print(i)
  5:     i = (i + 1)
  6: }
  7: 
