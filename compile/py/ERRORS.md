# Python roundtrip VM test failures

## tests/vm/valid/avg_builtin.mochi

```
output mismatch
-- python --
2.0
-- vm --
2
```

## tests/vm/valid/cross_join_triple.mochi

```
output mismatch
-- python --
--- Cross Join of three lists ---
1 A True
1 A False
1 B True
1 B False
2 A True
2 A False
2 B True
2 B False
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
-- python --
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

## tests/vm/valid/group_by.mochi

```
output mismatch
-- python --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55.0
Hanoi : count = 3 , avg_age = 27.333333333333332
-- vm --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333332
```

## tests/vm/valid/group_by_conditional_sum.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt1050848786/main.py", line 193, in <module>
    main()
  File "/tmp/pyrt1050848786/main.py", line 188, in main
    result = _q0()
             ^^^^^
  File "/tmp/pyrt1050848786/main.py", line 171, in _q0
    _src = items
           ^^^^^
UnboundLocalError: cannot access local variable 'items' where it is not associated with a value

```

## tests/vm/valid/group_by_having.mochi

```
output mismatch
-- python --
[{"city": "Paris", "num": 4}, {"city": "Hanoi", "num": 3}]
-- vm --
[{"city":"Paris","num":4}]
```

## tests/vm/valid/group_by_left_join.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt1586518385/main.py", line 146, in <module>
    main()
  File "/tmp/pyrt1586518385/main.py", line 124, in main
    stats = _query(
            ^^^^^^^
  File "/tmp/pyrt1586518385/main.py", line 101, in _query
    res.append(opts["select"](*r))
               ^^^^^^^^^^^^^^^^^^
  File "/tmp/pyrt1586518385/main.py", line 135, in <lambda>
    "name": _get(g, "key"),
                 ^
NameError: name 'g' is not defined

```

## tests/vm/valid/group_by_multi_join.mochi

```
output mismatch
-- python --
{'part': 100, 'total': 20.0} {'part': 200, 'total': 15.0}
-- vm --
map[part:100 total:20] map[part:200 total:15]
```

## tests/vm/valid/group_by_multi_join_sort.mochi

```
output mismatch
-- python --
{'c_custkey': 1, 'c_name': 'Alice', 'revenue': 900.0, 'c_acctbal': 100, 'n_name': 'BRAZIL', 'c_address': '123 St', 'c_phone': '123-456', 'c_comment': 'Loyal'}
-- vm --
map[c_acctbal:100 c_address:123 St c_comment:Loyal c_custkey:1 c_name:Alice c_phone:123-456 n_name:BRAZIL revenue:900]
```

## tests/vm/valid/group_by_sort.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt2428824001/main.py", line 187, in <module>
    main()
  File "/tmp/pyrt2428824001/main.py", line 182, in main
    grouped = _q0()
              ^^^^^
  File "/tmp/pyrt2428824001/main.py", line 172, in _q0
    _src = items
           ^^^^^
UnboundLocalError: cannot access local variable 'items' where it is not associated with a value

```

## tests/vm/valid/group_items_iteration.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt653669905/main.py", line 176, in <module>
    main()
  File "/tmp/pyrt653669905/main.py", line 165, in main
    for x in _get(g, "items"):
             ^^^^^^^^^^^^^^^^
  File "/tmp/pyrt653669905/main.py", line 24, in _get
    raise Exception("field not found: " + name)
Exception: field not found: items

```

## tests/vm/valid/json_builtin.mochi

```
output mismatch
-- python --
{"a": 1, "b": 2}
-- vm --
{"a":1,"b":2}
```

## tests/vm/valid/left_join.mochi

```
output mismatch
-- python --
--- Left Join ---
Order 100 customer {'id': 1, 'name': 'Alice'} total 250
Order 101 customer None total 80
-- vm --
--- Left Join ---
Order 100 customer map[id:1 name:Alice] total 250
Order 101 customer <nil> total 80
```

## tests/vm/valid/left_join_multi.mochi

```
output mismatch
-- python --
--- Left Join Multi ---
100 Alice {'orderId': 100, 'sku': 'a'}
101 Bob None
-- vm --
--- Left Join Multi ---
100 Alice map[orderId:100 sku:a]
101 Bob <nil>
```

## tests/vm/valid/load_yaml.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt2039090231/main.py", line 103, in <module>
    main()
  File "/tmp/pyrt2039090231/main.py", line 94, in main
    for _it in _load("../interpreter/valid/people.yaml", dict({"format": "yaml"}))
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/tmp/pyrt2039090231/main.py", line 25, in _load
    f = sys.stdin if path is None else open(path, "r")
                                       ^^^^^^^^^^^^^^^
FileNotFoundError: [Errno 2] No such file or directory: '../interpreter/valid/people.yaml'

```

## tests/vm/valid/math_ops.mochi

```
output mismatch
-- python --
42
3
1
-- vm --
42
3.5
1
```

## tests/vm/valid/order_by_map.mochi

```
output mismatch
-- python --
{'a': 0, 'b': 5} {'a': 1, 'b': 1} {'a': 1, 'b': 2}
-- vm --
map[a:0 b:5] map[a:1 b:1] map[a:1 b:2]
```

## tests/vm/valid/partial_application.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt2356638088/main.py", line 11, in <module>
    add5 = add(5)
           ^^^^^^
TypeError: add() missing 1 required positional argument: 'b'

```

## tests/vm/valid/query_sum_select.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt1174530904/main.py", line 39, in <module>
    main()
  File "/tmp/pyrt1174530904/main.py", line 34, in main
    result = [_sum(n) for n in nums if (n > 1)]
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/tmp/pyrt1174530904/main.py", line 34, in <listcomp>
    result = [_sum(n) for n in nums if (n > 1)]
              ^^^^^^^
  File "/tmp/pyrt1174530904/main.py", line 14, in _sum
    raise Exception("sum() expects list or group")
Exception: sum() expects list or group

```

## tests/vm/valid/record_assign.mochi

```
output mismatch
-- python --
0
-- vm --
1
```

## tests/vm/valid/save_jsonl_stdout.mochi

```
output mismatch
-- python --

-- vm --
{"age":30,"name":"Alice"}
{"age":25,"name":"Bob"}
```

## tests/vm/valid/tree_sum.mochi

```
python run error: exit status 1
Traceback (most recent call last):
  File "/tmp/pyrt3338312139/main.py", line 52, in <module>
    main()
  File "/tmp/pyrt3338312139/main.py", line 48, in main
    print(sum_tree(t))
          ^^^^^^^^^^^
  File "/tmp/pyrt3338312139/main.py", line 9, in sum_tree
    return (
           ^
  File "/tmp/pyrt3338312139/main.py", line 14, in <lambda>
    (
  File "/tmp/pyrt3338312139/main.py", line 16, in <lambda>
    (sum_tree(left) + value) + sum_tree(right)
     ~~~~~~~~~~~~~~~^~~~~~~
TypeError: unsupported operand type(s) for +: 'NoneType' and 'int'

```

## tests/vm/valid/typed_let.mochi

```
output mismatch
-- python --
None
-- vm --
<nil>
```

## tests/vm/valid/typed_var.mochi

```
output mismatch
-- python --
None
-- vm --
<nil>
```

