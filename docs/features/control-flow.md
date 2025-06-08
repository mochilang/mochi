## Control Flow

Mochi offers standard conditionals and ranges for looping.

```mochi
if count > 0 {
  print("positive")
} else {
  print("non-positive")
}

for i in 0..3 {
  print(i)
}

var j = 0
while j < 3 {
  print(j)
  j = j + 1
}
```

`while` repeats the body as long as its condition evaluates to `true`. Use `break`
to exit early or `continue` to skip to the next iteration.
