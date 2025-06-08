## Variables

Bindings are immutable by default using `let`. Use `var` for mutable values. When the type annotation is omitted the compiler infers it from the initializer.

```mochi
let name = "Mochi"
var count = 1
```

A `let` binding cannot be reassigned. Attempting to do so will produce a compile-time error. Mutable `var` bindings may change value any number of times.

```mochi
var total = 0
for i in 1..5 {
  total = total + i
}
```

Variables are scoped to the block in which they are declared. Nested blocks may shadow outer bindings.

```mochi
let id = "outer"
if true {
  let id = "inner"
  print(id)  // "inner"
}
print(id)    // "outer"
```

Mochi also supports destructuring from lists and maps for convenience:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
print(a + b)
print(n, age)
```
