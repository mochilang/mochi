# Interpreter Golden Test Failures using runtime/vm

## tests/interpreter/valid/agent_full.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc020680598 stack=[0xc020680000, 0xc040680000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f95cdafa288 sp=0x7f95cdafa258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f95cdafa3c0 sp=0x7f95cdafa288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/cast_struct.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
hi
```

## tests/interpreter/valid/closure.mochi

```
panic: runtime error: index out of range [10] with length 3

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0xc000253e40?, 0x2?, {0xc000432a80?, 0xc000253ac8?, 0x73097e?})
	/workspace/mochi/runtime/vm/vm.go:278 +0x3b9f
mochi/runtime/vm.(*VM).call(0xc000253e40, 0x1f?, {0x0, 0xc000402800?, 0xc0004027e0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).Run(...)
	/workspace/mochi/runtime/vm/vm.go:267
main.main()
...
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
golden mismatch:
-- got --
Grandparents:
Siblings:
-- want --
Grandparents:
Alice is grandparent of David
Alice is grandparent of Eva
Siblings:
Bob <-> Carol
Carol <-> Bob
```

## tests/interpreter/valid/dataset_sort_take_limit.mochi

```
golden mismatch:
-- got --
--- Top products (excluding most expensive) ---
-- want --
--- Top products (excluding most expensive) ---
Smartphone costs $ 900
Tablet costs $ 600
Monitor costs $ 300
```

## tests/interpreter/valid/eval_builtin.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
3
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
golden mismatch:
-- got --
0
-- want --
2
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
golden mismatch:
-- got --
<nil>
-- want --
Alice
```

## tests/interpreter/valid/go_auto.mochi

```
panic: runtime error: index out of range [2] with length 1

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0x23bc6a0?, 0xc000418660?, {0xc0000b4f20?, 0x224a5a0?, 0x73?})
	/workspace/mochi/runtime/vm/vm.go:278 +0x3b9f
mochi/runtime/vm.(*VM).call(0xc000235e40, 0xc000235a78?, {0xc0000b4e70, 0xc000235ac8?, 0x73097e?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).call(0xc000235e40, 0x1f?, {0x0, 0xc0003e4840?, 0xc0003e4820?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).Run(...)
...
```

## tests/interpreter/valid/go_math.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc0456a4598 stack=[0xc0456a4000, 0xc0656a4000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7ffd749e1320 sp=0x7ffd749e12f0 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7ffd749e1458 sp=0x7ffd749e1320 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/group_by.mochi

```
golden mismatch:
-- got --
--- People grouped by city ---
-- want --
--- People grouped by city ---
Paris : count = 3 , avg_age = 55
Hanoi : count = 3 , avg_age = 27.333333333333332
```

## tests/interpreter/valid/in_operator.mochi

```
golden mismatch:
-- got --
false
false
-- want --
true
false
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

## tests/interpreter/valid/input_builtin.mochi

```
golden mismatch:
-- got --
Enter first input:
Enter second input:
-- want --
Enter first input:
Enter second input:
You entered: foo , bar
```

## tests/interpreter/valid/left_join.mochi

```
golden mismatch:
-- got --
--- Left Join ---
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
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc0204e0598 stack=[0xc0204e0000, 0xc0404e0000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f0bd4cbb288 sp=0x7f0bd4cbb258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f0bd4cbb3c0 sp=0x7f0bd4cbb288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/main.mochi

```
panic: runtime error: index out of range [1] with length 1

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0x0?, 0x0?, {0xc0000b5340?, 0x4?, 0xc0004053e0?})
	/workspace/mochi/runtime/vm/vm.go:278 +0x3b9f
mochi/runtime/vm.(*VM).call(0xc000235e40, 0xa?, {0xc0000b5290, 0x0?, 0x0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).call(0xc000235e40, 0x1f?, {0x0, 0xc0003e4840?, 0xc0003e4820?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).Run(...)
...
```

## tests/interpreter/valid/map_in_operator.mochi

```
golden mismatch:
-- got --
false
false
-- want --
true
false
```

## tests/interpreter/valid/match_expr.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
two
```

## tests/interpreter/valid/match_full.mochi

```
golden mismatch:
-- got --
<nil>
<nil>
<nil>
<nil>
<nil>
-- want --
two
relaxed
confirmed
zero
many
```

## tests/interpreter/valid/method.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc02050c598 stack=[0xc02050c000, 0xc04050c000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f3bcf7fb288 sp=0x7f3bcf7fb258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f3bcf7fb3c0 sp=0x7f3bcf7fb288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/nested_function.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
8
```

## tests/interpreter/valid/outer_join.mochi

```
golden mismatch:
-- got --
--- Outer Join using syntax ---
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
panic: runtime error: index out of range [3] with length 1

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0x0?, 0x0?, {0xc000145340?, 0x4?, 0xc000423350?})
	/workspace/mochi/runtime/vm/vm.go:278 +0x3b9f
mochi/runtime/vm.(*VM).call(0xc000253e40, 0xa?, {0xc000145290, 0x0?, 0x0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).call(0xc000253e40, 0x1f?, {0x0, 0xc000402800?, 0xc0004027e0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).Run(...)
...
```

## tests/interpreter/valid/package_example.mochi

```
panic: runtime error: index out of range [3] with length 1

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0x0?, 0x0?, {0xc000145340?, 0x4?, 0xc0004233b0?})
	/workspace/mochi/runtime/vm/vm.go:278 +0x3b9f
mochi/runtime/vm.(*VM).call(0xc000253e40, 0xa?, {0xc000145290, 0x0?, 0x0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).call(0xc000253e40, 0x1f?, {0x0, 0xc000402800?, 0xc0004027e0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).Run(...)
...
```

## tests/interpreter/valid/package_import.mochi

```
panic: runtime error: index out of range [3] with length 1

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0x0?, 0x0?, {0xc000145340?, 0x4?, 0xc0004233b0?})
	/workspace/mochi/runtime/vm/vm.go:278 +0x3b9f
mochi/runtime/vm.(*VM).call(0xc000253e40, 0xa?, {0xc000145290, 0x0?, 0x0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).call(0xc000253e40, 0x1f?, {0x0, 0xc000402800?, 0xc0004027e0?})
	/workspace/mochi/runtime/vm/vm.go:471 +0x2d5d
mochi/runtime/vm.(*VM).Run(...)
...
```

## tests/interpreter/valid/python_auto.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc020492598 stack=[0xc020492000, 0xc040492000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f8834cfa288 sp=0x7f8834cfa258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f8834cfa3c0 sp=0x7f8834cfa288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/python_math.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc045780598 stack=[0xc045780000, 0xc065780000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7fa54628a288 sp=0x7fa54628a258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7fa54628a3c0 sp=0x7fa54628a288 pc=0x714d7b
runtime.morestack()
...
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
20
20
1
2
2
-- want --
20
10
1
2
10
```

## tests/interpreter/valid/short_circuit.mochi

```
type error: error[T020]: operator `&&` cannot be used on types bool and void
  --> tests/interpreter/valid/short_circuit.mochi:5:13

  5 | print(false && boom())
    |             ^

help:
  Choose an operator that supports these operand types.
exit status 1
```

## tests/interpreter/valid/string_compare.mochi

```
golden mismatch:
-- got --
false
true
false
true
-- want --
true
true
true
true
```

## tests/interpreter/valid/strings_basic.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc02066a598 stack=[0xc02066a000, 0xc04066a000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f1b42ffa288 sp=0x7f1b42ffa258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f1b42ffa3c0 sp=0x7f1b42ffa288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/test_expect_pass.mochi

```
panic: runtime error: index out of range [0] with length 0

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0xc000235e40, 0x1f?, {0x0, 0xc0003e4840?, 0xc0003e4820?})
	/workspace/mochi/runtime/vm/vm.go:479 +0x383f
mochi/runtime/vm.(*VM).Run(...)
	/workspace/mochi/runtime/vm/vm.go:267
main.main()
	/workspace/mochi/runtime/vm/cmd/runvm/main.go:37 +0x4ab
exit status 2
```

## tests/interpreter/valid/tree_sum.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
3
```

## tests/interpreter/valid/ts_auto.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc02046a598 stack=[0xc02046a000, 0xc04046a000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f93fd5b6288 sp=0x7f93fd5b6258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f93fd5b63c0 sp=0x7f93fd5b6288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/ts_math.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc039580598 stack=[0xc039580000, 0xc059580000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x2305019?, 0x6eb63c?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7f80abffc288 sp=0x7f80abffc258 pc=0x72e7e8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7f80abffc3c0 sp=0x7f80abffc288 pc=0x714d7b
runtime.morestack()
...
```

## tests/interpreter/valid/user_type_literal.mochi

```
golden mismatch:
-- got --
<nil>
-- want --
Bob
```

