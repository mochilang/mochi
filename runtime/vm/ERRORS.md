# Interpreter Golden Test Failures using runtime/vm

## tests/interpreter/valid/agent_full.mochi

```
call graph: main -> main
stack trace:
  main:0
  main at tests/interpreter/valid/agent_full.mochi:46
    let s = monitor.status()
          
   ERROR  
          
  Too many args.                                                                                                      

...
```

## tests/interpreter/valid/break_continue.mochi

```
golden mismatch:
-- got --

-- want --
odd number: 1
odd number: 3
odd number: 5
odd number: 7
```

## tests/interpreter/valid/cross_join.mochi

```
golden mismatch:
-- got --
--- Cross Join: All order-customer pairs ---
-- want --
--- Cross Join: All order-customer pairs ---
Order 100 (customerId: 1 , total: $ 250 ) paired with Alice
Order 100 (customerId: 1 , total: $ 250 ) paired with Bob
Order 100 (customerId: 1 , total: $ 250 ) paired with Charlie
Order 101 (customerId: 2 , total: $ 125 ) paired with Alice
Order 101 (customerId: 2 , total: $ 125 ) paired with Bob
Order 101 (customerId: 2 , total: $ 125 ) paired with Charlie
Order 102 (customerId: 1 , total: $ 300 ) paired with Alice
Order 102 (customerId: 1 , total: $ 300 ) paired with Bob
Order 102 (customerId: 1 , total: $ 300 ) paired with Charlie
```

## tests/interpreter/valid/cross_join_triple.mochi

```
golden mismatch:
-- got --
--- Cross Join of three lists ---
-- want --
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

## tests/interpreter/valid/datalog.mochi

```
Grandparents:
call graph: main
stack trace:
  main at tests/interpreter/valid/datalog.mochi:14
    for g in grandparents {
          
   ERROR  
          
  Invalid iterator.                                                                                                   

...
```

## tests/interpreter/valid/dataset_sort_take_limit.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/dataset_sort_take_limit.mochi:12
    sort by -p.price
          
   ERROR  
          
  Sort expects list.                                                                                                  

exit status 1
```

## tests/interpreter/valid/eval_builtin.mochi

```
golden mismatch:
-- got --
1
-- want --
3
```

## tests/interpreter/valid/for_list_collection.mochi

```
golden mismatch:
-- got --

-- want --
1
2
3
```

## tests/interpreter/valid/for_loop.mochi

```
panic: runtime error: index out of range [3] with length 3

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0xc000465c30, 0x38bb520?, {0x0, 0xc000465a20?, 0x6d3b5e?}, {0xc000465ab8, 0x1, 0x1})
	/workspace/mochi/runtime/vm/vm.go:764 +0xdabb
mochi/runtime/vm.(*VM).Run(0xc000465c30)
	/workspace/mochi/runtime/vm/vm.go:536 +0xa5
main.runFile(0xc000380f00)
	/workspace/mochi/cmd/mochi/main.go:256 +0x4d7
main.newRunCmd.func1(0xc0003e0800?, {0xc000393490?, 0x4?, 0x28d50bd?})
...
```

## tests/interpreter/valid/generate_echo.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
echo hello
```

## tests/interpreter/valid/generate_embedding.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/generate_embedding.mochi:4
    print(len(vec))
          
   ERROR  
          
  Invalid len operand.                                                                                                

exit status 1
```

## tests/interpreter/valid/generate_model.mochi

```
golden mismatch:
-- got --
<nil>
<nil>
-- want --
model alias
colon
```

## tests/interpreter/valid/generate_options.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
hello
```

## tests/interpreter/valid/generate_struct.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/generate_struct.mochi:11
    print(p.name)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/go_auto.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/go_auto.mochi:3
    print(testpkg.Add(2,3))
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/go_math.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/go_math.mochi:13
    let area = math.Pi * math.Pow(r, 2.0)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/group_by.mochi

```
golden mismatch:
-- got --

-- want --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333332
```

## tests/interpreter/valid/if_else.mochi

```
golden mismatch:
-- got --
small
-- want --
big
```

## tests/interpreter/valid/inner_join.mochi

```
golden mismatch:
-- got --
--- Orders with customer info ---
-- want --
--- Orders with customer info ---
Order 100 by Alice - $ 250
Order 101 by Bob - $ 125
Order 102 by Alice - $ 300
```

## tests/interpreter/valid/left_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Left Join ---
Order 100 customer map[id:1 name:Alice] total 250
Order 101 customer <nil> total 80
```

## tests/interpreter/valid/load_csv.mochi

```
golden mismatch:
-- got --

-- want --
Alice 30
Charlie 20
```

## tests/interpreter/valid/load_jsonl.mochi

```
golden mismatch:
-- got --

-- want --
Alice alice@example.com
Charlie charlie@example.com
```

## tests/interpreter/valid/load_yaml.mochi

```
golden mismatch:
-- got --

-- want --
Alice alice@example.com
Charlie charlie@example.com
```

## tests/interpreter/valid/local_import.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/local_import.mochi:3
    print("Pi =", constants.pi())
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/main.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/main.mochi:3
    let sum = mathmulti.add(1, 2)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/match_expr.mochi

```
golden mismatch:
-- got --

-- want --
two
```

## tests/interpreter/valid/match_full.mochi

```
golden mismatch:
-- got --

-- want --
two
relaxed
confirmed
zero
many
```

## tests/interpreter/valid/outer_join.mochi

```
golden mismatch:
-- got --

-- want --
--- Outer Join using syntax ---
Order 100 by Alice - $ 250
Order 101 by Bob - $ 125
Order 102 by Alice - $ 300
Order 103 by Unknown - $ 80
Customer Charlie has no orders
Customer Diana has no orders
```

## tests/interpreter/valid/package_auto_alias.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/package_auto_alias.mochi:3
    let sum = mathutils.add(3, 5)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/package_example.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/package_example.mochi:3
    let sum = mathutils.add(3, 5)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/package_import.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/package_import.mochi:3
    let sum = mathutils.add(3, 5)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/python_auto.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/python_auto.mochi:3
    print(math.sqrt(16.0))
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/python_math.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/python_math.mochi:12
    let area = math.pi * math.pow(r, 2.0)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/right_join.mochi

```
golden mismatch:
-- got --
--- Right Join using syntax ---
-- want --
--- Right Join using syntax ---
Customer Alice has order 100 - $ 250
Customer Bob has order 101 - $ 125
Customer Alice has order 102 - $ 300
```

## tests/interpreter/valid/shadow_scope.mochi

```
golden mismatch:
-- got --
10
10
-- want --
20
10
1
2
10
```

## tests/interpreter/valid/short_circuit.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `&&` cannot be used on types bool and void
  --> tests/interpreter/valid/short_circuit.mochi:5:13

[90m  5[0m | print(false && boom())
    | [31;1m            ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
   2. [31;1merror[T020][0;22m: operator `||` cannot be used on types bool and void
...
```

## tests/interpreter/valid/strings_basic.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/strings_basic.mochi:6
    print(strings.ToUpper("hello"))
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/test_expect_pass.mochi

```
panic: runtime error: index out of range [5] with length 1

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0xc000461c30, 0x38bb520?, {0x0, 0xc000461a20?, 0x6d3b5e?}, {0xc000461ab8, 0x1, 0x1})
	/workspace/mochi/runtime/vm/vm.go:1060 +0xb226
mochi/runtime/vm.(*VM).Run(0xc000461c30)
	/workspace/mochi/runtime/vm/vm.go:536 +0xa5
main.runFile(0xc000380f00)
	/workspace/mochi/cmd/mochi/main.go:256 +0x4d7
main.newRunCmd.func1(0xc0003e0800?, {0xc000393490?, 0x4?, 0x28d50bd?})
...
```

## tests/interpreter/valid/tree_sum.mochi

```
ERROR  
          
  Sum expects list.                                                                                                   

exit status 1
```

## tests/interpreter/valid/ts_auto.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/ts_auto.mochi:3
    print(math.pow(2.0,8.0))
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/ts_math.mochi

```
call graph: main
stack trace:
  main at tests/interpreter/valid/ts_math.mochi:10
    let area = math.PI * math.pow(r, 2.0)
          
   ERROR  
          
  Invalid index target.                                                                                               

exit status 1
```

## tests/interpreter/valid/two_sum.mochi

```
golden mismatch:
-- got --
-1
-1
-- want --
0
1
```

## tests/interpreter/valid/while_loop.mochi

```
panic: runtime error: index out of range [3] with length 2

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0xc0004edc30, 0x38bb520?, {0x0, 0xc0004eda20?, 0x6d3b5e?}, {0xc0004edab8, 0x1, 0x1})
	/workspace/mochi/runtime/vm/vm.go:764 +0xdabb
mochi/runtime/vm.(*VM).Run(0xc0004edc30)
	/workspace/mochi/runtime/vm/vm.go:536 +0xa5
main.runFile(0xc000404ee8)
	/workspace/mochi/cmd/mochi/main.go:256 +0x4d7
main.newRunCmd.func1(0xc000464800?, {0xc000417450?, 0x4?, 0x28d50bd?})
...
```

