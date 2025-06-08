## Control Flow

Mochi offers standard conditionals and several looping constructs. The `if` statement evaluates a boolean expression and selects a branch to execute.

```mochi
if count > 0 {
  print("positive")
} else {
  print("non-positive")
}
```

Numeric ranges are written with `start..end` and can be used directly in `for` loops:

```mochi
for i in 0..3 {
  print(i)
}
```

`while` repeats the body as long as its condition evaluates to `true`. Use `break` to exit early or `continue` to skip to the next iteration.

```mochi
var j = 0
while j < 3 {
  if j == 1 {
    j = j + 1
    continue
  }
  print(j)
  j = j + 1
}
```

The `match` expression provides pattern matching similar to switch statements in other languages. Each arm is checked in order:

```mochi
match value {
  0 => print("zero"),
  1 => print("one"),
  _ => print("many")
}
```
