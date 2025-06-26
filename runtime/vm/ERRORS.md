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

## tests/interpreter/valid/eval_builtin.mochi

```
golden mismatch:
-- got --
1
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

