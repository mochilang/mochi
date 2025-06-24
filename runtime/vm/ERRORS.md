# Interpreter Golden Test Failures using runtime/vm

## tests/interpreter/valid/agent_full.mochi

```
run error: too many args
exit status 1
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

