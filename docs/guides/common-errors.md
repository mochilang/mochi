# Common Language Errors

This guide highlights a few frequent mistakes seen when writing Mochi code and how to resolve them.

## Assignment to an undeclared variable

```
x = 3
```

This fails with error `T001: assignment to undeclared variable`.
Declare the variable first:

```
let x = 3
```

## Reassigning an immutable binding

```
let x = 1
x = 2
```

Results in `T024: cannot assign to x (immutable)`. Use `var` for mutable values:

```
var x = 1
x = 2
```

## Unterminated string literal

```
let s = "Hello
```

The lexer reports `P040: invalid input`. Ensure strings are closed with `"`:

```
let s = "Hello"
```

## Missing closing bracket or brace

```
let xs = [1,2,3
```

Produces `P999: unexpected token "<EOF>" (expected "]")`. Add the missing bracket:

```
let xs = [1,2,3]
```

## Non-boolean condition in `expect`

```
expect 5
```

This triggers `T008: type mismatch: expected bool, got int`.
Provide a boolean expression instead:

```
expect 2 + 3 == 5
```

## Iterating over a non-collection

```
for i in 3 {
  print(i)
}
```

The compiler reports `T022: cannot iterate over type int`.
Use a list or range:

```
for i in 0..3 {
  print(i)
}
```
