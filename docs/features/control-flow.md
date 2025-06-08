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
