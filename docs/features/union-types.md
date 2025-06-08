## Union Types and Methods

Unions use `|` syntax to represent a value that can be one of several variants. Pattern matching with `match` makes it easy to work with them. Types may also contain inline methods for convenience.

```mochi
type Tree =
  Leaf
  | Node(left: Tree, value: int, right: Tree)
```

Methods defined inside a type block operate on the current instance via an implicit `self` parameter.

```mochi
type Circle {
  radius: float

  fun area(): float {
    return 3.14 * radius * radius
  }
}
```

Use `match` to deconstruct union values:

```mochi
fun sum(t: Tree): int {
  return match t {
    Leaf => 0
    Node(l, v, r) => sum(l) + v + sum(r)
  }
}
```
