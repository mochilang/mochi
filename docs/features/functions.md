## Functions

Functions are declared with `fun`. Arrow functions provide a concise alternative.

```mochi
fun double(x: int): int {
  return x * 2
}

let square = fun(x: int): int => x * x
```

Parameters may be annotated with types and functions always specify a return type. Functions are first-class values and can be passed around like variables.

```mochi
fun apply(f: fun(int): int, value: int): int {
  return f(value)
}

print(apply(double, 4))
```

Recursion is supported and there is no implicit tail-call optimization, so beware of deep call stacks.

```mochi
fun fact(n: int): int {
  if n <= 1 { return 1 }
  return n * fact(n - 1)
}
```
