# Interpreter Golden Test Failures using runtime/vm

## tests/interpreter/valid/agent_full.mochi

```
runtime: goroutine stack exceeds 1000000000-byte limit
runtime: sp=0xc020590360 stack=[0xc020590000, 0xc040590000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x241d28b?, 0x6eb6dc?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7fa0d35eb288 sp=0x7fa0d35eb258 pc=0x72eca8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7fa0d35eb3c0 sp=0x7fa0d35eb288 pc=0x71517b
runtime.morestack()
...
```

## tests/interpreter/valid/cross_join.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/cross_join_triple.mochi

```
run error: invalid iterator
exit status 1
```

## tests/interpreter/valid/datalog.mochi

```
run error: invalid iterator
exit status 1
```

## tests/interpreter/valid/dataset_sort_take_limit.mochi

```
run error: invalid map key
exit status 1
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
run error: invalid len operand
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
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/go_auto.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/go_math.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/group_by.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/inner_join.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/input_builtin.mochi

```
golden mismatch:
-- got --
Enter first input:
Enter second input:
You entered:  ,
-- want --
Enter first input:
Enter second input:
You entered: foo , bar
```

## tests/interpreter/valid/left_join.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/load_csv.mochi

```
run error: open ../tests/interpreter/valid/people.csv: no such file or directory
exit status 1
```

## tests/interpreter/valid/load_jsonl.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/load_yaml.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/local_import.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/main.mochi

```
run error: invalid index target
exit status 1
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
runtime: sp=0xc021080388 stack=[0xc021080000, 0xc041080000]
fatal error: stack overflow

runtime stack:
runtime.throw({0x241d28b?, 0x6eb6dc?})
	/usr/local/go/src/runtime/panic.go:1101 +0x48 fp=0x7fd464939288 sp=0x7fd464939258 pc=0x72eca8
runtime.newstack()
	/usr/local/go/src/runtime/stack.go:1107 +0x5bb fp=0x7fd4649393c0 sp=0x7fd464939288 pc=0x71517b
runtime.morestack()
...
```

## tests/interpreter/valid/outer_join.mochi

```
run error: invalid map key
exit status 1
```

## tests/interpreter/valid/package_auto_alias.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/package_example.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/package_import.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/python_auto.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/python_math.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/right_join.mochi

```
run error: invalid map key
exit status 1
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
  --> short_circuit.mochi:5:13

  5 | print(false && boom())
    |             ^

help:
  Choose an operator that supports these operand types.
exit status 1
```

## tests/interpreter/valid/strings_basic.mochi

```
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/test_expect_pass.mochi

```
panic: runtime error: index out of range [0] with length 0

goroutine 1 [running]:
mochi/runtime/vm.(*VM).call(0xc000235e38, 0x0?, {0x0, 0xc0000870e0?, 0x23da0c0?}, {0xc000235da8, 0x1, 0x1})
	/workspace/mochi/runtime/vm/vm.go:1160 +0xbf34
mochi/runtime/vm.(*VM).Run(0xc000235e38)
	/workspace/mochi/runtime/vm/vm.go:483 +0xa5
main.main()
	/workspace/mochi/runtime/vm/cmd/runvm/main.go:45 +0x648
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
run error: invalid index target
exit status 1
```

## tests/interpreter/valid/ts_math.mochi

```
run error: invalid index target
exit status 1
```

