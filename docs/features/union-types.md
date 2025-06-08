## Union Types and Methods

Unions use `|` syntax. Types can contain inline methods.

```mochi
type Tree =
  Leaf
  | Node(left: Tree, value: int, right: Tree)

fun sum(t: Tree): int {
  return match t {
    Leaf => 0
    Node(l, v, r) => sum(l) + v + sum(r)
  }
}
```
