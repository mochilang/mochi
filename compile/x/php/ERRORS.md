# PHP roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
output mismatch
-- php --
[1,2,3]
-- vm --
1 2 3
```

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- php --
7
1
1
-- vm --
7
true
true
```

## tests/vm/valid/bool_chain.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
false
```

## tests/vm/valid/cast_struct.mochi

```
php run error: exit status 255
PHP Fatal error:  Uncaught Error: Cannot use object of type Todo as array in /tmp/php_run2848440466/main.php:10
Stack trace:
#0 {main}
  thrown in /tmp/php_run2848440466/main.php on line 10

```

## tests/vm/valid/cross_join_triple.mochi

```
output mismatch
-- php --
--- Cross Join of three lists ---
1 A 1
1 A 
1 B 1
1 B 
2 A 1
2 A 
2 B 1
2 B
-- vm --
--- Cross Join of three lists ---
1 A true
1 A false
1 B true
1 B false
2 A true
2 A false
2 B true
2 B false
```

## tests/vm/valid/dataset_where_filter.mochi

```
output mismatch
-- php --
--- Adults ---
Alice is 30 
Charlie is 65  (senior)
Diana is 45
-- vm --
--- Adults ---
Alice is 30
Charlie is 65  (senior)
Diana is 45
```

## tests/vm/valid/exists_builtin.mochi

```
output mismatch
-- php --
1
-- vm --
true
```

## tests/vm/valid/for_map_collection.mochi

```
output mismatch
-- php --
1
2
-- vm --
a
b
```

## tests/vm/valid/group_by.mochi

```
output mismatch
-- php --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333
-- vm --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333332
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
php run error: exit status 255
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 42
PHP Warning:  Trying to access array offset on null in /tmp/php_run3233809951/main.php on line 42
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 42
PHP Warning:  Trying to access array offset on null in /tmp/php_run3233809951/main.php on line 42
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 42
PHP Warning:  Trying to access array offset on null in /tmp/php_run3233809951/main.php on line 42
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 6
PHP Warning:  Trying to access array offset on null in /tmp/php_run3233809951/main.php on line 6
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 8
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3233809951/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 14
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3233809951/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 20
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 20
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3233809951/main.php on line 20
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 26
PHP Warning:  Undefined variable $g in /tmp/php_run3233809951/main.php on line 26
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3233809951/main.php on line 26
PHP Fatal error:  Uncaught DivisionByZeroError: Division by zero in /tmp/php_run3233809951/main.php:18
Stack trace:
#0 /tmp/php_run3233809951/main.php(18): intdiv()
#1 /tmp/php_run3233809951/main.php(130): {closure}()
#2 /tmp/php_run3233809951/main.php(5): _query()
#3 /tmp/php_run3233809951/main.php(43): {closure}()
#4 {main}
  thrown in /tmp/php_run3233809951/main.php on line 18

```

## tests/vm/valid/group_by_having.mochi

```
output mismatch
-- php --
[{"city":"Paris","num":4},{"city":"Hanoi","num":3}]
-- vm --
[{"city":"Paris","num":4}]
```

## tests/vm/valid/group_by_join.mochi

```
output mismatch
-- php --
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run2910525598/main.php on line 8
--- Orders per customer ---
<nil> orders: 0
<nil> orders: 0
<nil> orders: 0
-- vm --
--- Orders per customer ---
Alice orders: 2
Bob orders: 1
```

## tests/vm/valid/group_by_left_join.mochi

```
output mismatch
-- php --
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Trying to access array offset on null in /tmp/php_run551964208/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 10
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 10
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  Undefined variable $g in /tmp/php_run551964208/main.php on line 17
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run551964208/main.php on line 17
--- Group Left Join ---
<nil> orders: 0
<nil> orders: 0
<nil> orders: 0
<nil> orders: 0
-- vm --
--- Group Left Join ---
Alice orders: 2
Bob orders: 1
Charlie orders: 0
```

## tests/vm/valid/group_by_multi_join.mochi

```
output mismatch
-- php --
[{"part":100,"total":20},{"part":200,"total":15}]
-- vm --
map[part:100 total:20] map[part:200 total:15]
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
output mismatch
-- php --
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 22
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 22
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3136869074/main.php on line 22
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 14
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 14
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 14
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 14
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 16
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 16
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3136869074/main.php on line 16
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Undefined variable $g in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
PHP Warning:  Trying to access array offset on null in /tmp/php_run3136869074/main.php on line 20
[{"c_custkey":null,"c_name":null,"revenue":0,"c_acctbal":null,"n_name":null,"c_address":null,"c_phone":null,"c_comment":null}]
-- vm --
map[c_acctbal:100 c_address:123 St c_comment:Loyal c_custkey:1 c_name:Alice c_phone:123-456 n_name:BRAZIL revenue:900]
```

## tests/vm/valid/group_by_sort.mochi

```
output mismatch
-- php --
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 14
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Trying to access array offset on null in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Trying to access array offset on null in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Trying to access array offset on null in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Trying to access array offset on null in /tmp/php_run4134217594/main.php on line 6
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  Undefined variable $g in /tmp/php_run4134217594/main.php on line 8
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run4134217594/main.php on line 8
[{"cat":null,"total":0},{"cat":null,"total":0},{"cat":null,"total":0},{"cat":null,"total":0}]
-- vm --
map[cat:b total:7] map[cat:a total:4]
```

## tests/vm/valid/group_items_iteration.mochi

```
output mismatch
-- php --
PHP Warning:  Undefined array key "items" in /tmp/php_run3109445982/main.php on line 15
PHP Warning:  Undefined array key "items" in /tmp/php_run3109445982/main.php on line 15
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3109445982/main.php on line 15
PHP Warning:  Undefined array key "key" in /tmp/php_run3109445982/main.php on line 18
PHP Warning:  Undefined array key "items" in /tmp/php_run3109445982/main.php on line 15
PHP Warning:  Undefined array key "items" in /tmp/php_run3109445982/main.php on line 15
PHP Warning:  foreach() argument must be of type array|object, null given in /tmp/php_run3109445982/main.php on line 15
PHP Warning:  Undefined array key "key" in /tmp/php_run3109445982/main.php on line 18
[{"tag":null,"total":0},{"tag":null,"total":0}]
-- vm --
map[tag:a total:3] map[tag:b total:3]
```

## tests/vm/valid/in_operator.mochi

```
output mismatch
-- php --
1
1
-- vm --
true
true
```

## tests/vm/valid/in_operator_extended.mochi

```
output mismatch
-- php --
1

1

1
-- vm --
true
false
true
false
true
false
```

## tests/vm/valid/left_join.mochi

```
output mismatch
-- php --
--- Left Join ---
Order 100 customer {"id":1,"name":"Alice"} total 250
Order 101 customer <nil> total 80
-- vm --
--- Left Join ---
Order 100 customer map[id:1 name:Alice] total 250
Order 101 customer <nil> total 80
```

## tests/vm/valid/left_join_multi.mochi

```
output mismatch
-- php --
--- Left Join Multi ---
100 Alice {"orderId":100,"sku":"a"}
101 Bob <nil>
-- vm --
--- Left Join Multi ---
100 Alice map[orderId:100 sku:a]
101 Bob <nil>
```

## tests/vm/valid/list_set_ops.mochi

```
output mismatch
-- php --
[1,2,3]
[1,3]
[2]
4
-- vm --
1 2 3
1 3
2
4
```

## tests/vm/valid/load_yaml.mochi

```
php run error: exit status 255
PHP Warning:  fopen(../interpreter/valid/people.yaml): Failed to open stream: No such file or directory in /tmp/php_run3682030502/main.php on line 27
PHP Fatal error:  Uncaught Exception: cannot open ../interpreter/valid/people.yaml in /tmp/php_run3682030502/main.php:28
Stack trace:
#0 /tmp/php_run3682030502/main.php(13): _load_json()
#1 {main}
  thrown in /tmp/php_run3682030502/main.php on line 28

```

## tests/vm/valid/map_in_operator.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
```

## tests/vm/valid/map_membership.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
```

## tests/vm/valid/match_expr.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/match_full.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/math_ops.mochi

```
output mismatch
-- php --
42
3
1
-- vm --
42
3.5
1
```

## tests/vm/valid/membership.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
```

## tests/vm/valid/order_by_map.mochi

```
output mismatch
-- php --
PHP Warning:  Array to string conversion in /tmp/php_run574951552/main.php on line 81
PHP Warning:  Array to string conversion in /tmp/php_run574951552/main.php on line 81
PHP Warning:  Array to string conversion in /tmp/php_run574951552/main.php on line 81
PHP Warning:  Array to string conversion in /tmp/php_run574951552/main.php on line 81
[{"a":1,"b":2},{"a":1,"b":1},{"a":0,"b":5}]
-- vm --
map[a:0 b:5] map[a:1 b:1] map[a:1 b:2]
```

## tests/vm/valid/partial_application.mochi

```
php run error: exit status 255
PHP Fatal error:  Uncaught ArgumentCountError: Too few arguments to function mochi_add(), 1 passed in /tmp/php_run554233850/main.php on line 6 and exactly 2 expected in /tmp/php_run554233850/main.php:2
Stack trace:
#0 /tmp/php_run554233850/main.php(6): mochi_add()
#1 {main}
  thrown in /tmp/php_run554233850/main.php on line 2

```

## tests/vm/valid/pure_global_fold.mochi

```
output mismatch
-- php --
PHP Warning:  Undefined variable $k in /tmp/php_run3678698791/main.php on line 3
PHP Warning:  Undefined variable $k in /tmp/php_run3678698791/main.php on line 3
3
-- vm --
5
```

## tests/vm/valid/query_sum_select.mochi

```
php run error: exit status 255
PHP Fatal error:  Uncaught TypeError: array_sum(): Argument #1 ($array) must be of type array, int given in /tmp/php_run92210235/main.php:7
Stack trace:
#0 /tmp/php_run92210235/main.php(7): array_sum()
#1 /tmp/php_run92210235/main.php(10): {closure}()
#2 {main}
  thrown in /tmp/php_run92210235/main.php on line 7

```

## tests/vm/valid/record_assign.mochi

```
php run error: exit status 255
PHP Fatal error:  Uncaught Error: Cannot use object of type Counter as array in /tmp/php_run319900752/main.php:3
Stack trace:
#0 /tmp/php_run319900752/main.php(14): mochi_inc()
#1 {main}
  thrown in /tmp/php_run319900752/main.php on line 3

```

## tests/vm/valid/save_jsonl_stdout.mochi

```
output mismatch
-- php --
[{"name":"Alice","age":30},{"name":"Bob","age":25}]
-- vm --
{"age":30,"name":"Alice"}
{"age":25,"name":"Bob"}
```

## tests/vm/valid/short_circuit.mochi

```
output mismatch
-- php --
1
-- vm --
false
true
```

## tests/vm/valid/slice.mochi

```
output mismatch
-- php --
[2,3]
[1,2]
ell
-- vm --
2 3
1 2
ell
```

## tests/vm/valid/sort_stable.mochi

```
output mismatch
-- php --
["a","b","c"]
-- vm --
a b c
```

## tests/vm/valid/string_compare.mochi

```
output mismatch
-- php --
1
1
1
1
-- vm --
true
true
true
true
```

## tests/vm/valid/string_contains.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
```

## tests/vm/valid/string_in_operator.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
```

## tests/vm/valid/string_prefix_slice.mochi

```
output mismatch
-- php --
1
-- vm --
true
false
```

## tests/vm/valid/tree_sum.mochi

```
compile error: unsupported expression
```

## tests/vm/valid/update_stmt.mochi

```
vm run error:
expect condition failed at tests/vm/valid/update_stmt.mochi:22: expect people == [
call graph: main
stack trace:
  main at tests/vm/valid/update_stmt.mochi:22
    expect people == [

```

## tests/vm/valid/user_type_literal.mochi

```
php run error: exit status 255
PHP Fatal error:  Uncaught Error: Cannot use object of type Book as array in /tmp/php_run1572847811/main.php:21
Stack trace:
#0 {main}
  thrown in /tmp/php_run1572847811/main.php on line 21

```

## tests/vm/valid/values_builtin.mochi

```
output mismatch
-- php --
[1,2,3]
-- vm --
1 2 3
```

