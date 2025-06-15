## ./105/construct-binary-tree-from-preorder-and-inorder-traversal.mochi
```
[31;1mtype error:[0;22m
   1. [31;1merror[T013][0;22m: incompatible types in comparison
  --> ./105/construct-binary-tree-from-preorder-and-inorder-traversal.mochi:72:15

[90m 72[0m |   expect tree == Leaf
    | [31;1m              ^[0;22m

[33mhelp:[0m
  Use comparable types like numbers or strings with `<`, `>`.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./106/construct-binary-tree-from-inorder-and-postorder-traversal.mochi
```
[31;1mtype error:[0;22m
   1. [31;1merror[T013][0;22m: incompatible types in comparison
  --> ./106/construct-binary-tree-from-inorder-and-postorder-traversal.mochi:66:28

[90m 66[0m |   expect buildTree([], []) == Leaf
    | [31;1m                           ^[0;22m

[33mhelp:[0m
  Use comparable types like numbers or strings with `<`, `>`.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./110/balanced-binary-tree.mochi
```
[31;1mtest failed:[0;22m parse error:
  [31;1merror[P999][0;22m: ./110/balanced-binary-tree.mochi:12:20: unexpected token "{" (expected TypeVariant ("|" TypeVariant)*)
  --> ./110/balanced-binary-tree.mochi:12:20

[90m 12[0m | type BalanceInfo = { h: int, balanced: bool }
    | [31;1m                   ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
## ./111/minimum-depth-of-binary-tree.mochi
```
   [33mtest[0m example 1                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and int
  --> ./111/minimum-depth-of-binary-tree.mochi:45:25

[90m 45[0m |   expect minDepth(tree) == 2
    | [31;1m                        ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (29.0µs)
   [33mtest[0m example 2                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and int
  --> ./111/minimum-depth-of-binary-tree.mochi:54:25

[90m 54[0m |   expect minDepth(tree) == 2
    | [31;1m                        ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (9.0µs)
   [33mtest[0m single node                    ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and int
  --> ./111/minimum-depth-of-binary-tree.mochi:58:69

[90m 58[0m |   expect minDepth(Node { left: Leaf {}, value: 1, right: Leaf {} }) == 1
    | [31;1m                                                                    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (6.0µs)
   [33mtest[0m empty                          ... [32mok[0m (3.0µs)

[31m[FAIL][0m 3 test(s) failed.
[31;1mtest failed:[0;22m test failed: 3 test(s) failed
```
## ./112/path-sum.mochi
```
   [33mtest[0m example 1                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (40.0µs)
   [33mtest[0m example 2                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (10.0µs)
   [33mtest[0m example 3                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (8.0µs)
   [33mtest[0m single node                    ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (7.0µs)
   [33mtest[0m empty                          ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (7.0µs)

[31m[FAIL][0m 5 test(s) failed.
[31;1mtest failed:[0;22m test failed: 5 test(s) failed
```
## ./117/populating-next-right-pointers-in-each-node-ii.mochi
```
[31;1mtype error:[0;22m
   1. [31;1merror[T008][0;22m: type mismatch: expected R, got Node
  --> ./117/populating-next-right-pointers-in-each-node-ii.mochi:37:16

[90m 37[0m |         return Node { left: left, value: v, right: right, next: nxt }
    | [31;1m               ^[0;22m

[33mhelp:[0m
  Change the value to match the expected type.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./124/binary-tree-maximum-path-sum.mochi
```
[31;1mtest failed:[0;22m parse error:
  [31;1merror[P999][0;22m: ./124/binary-tree-maximum-path-sum.mochi:67:69: unexpected token "-" (expected PostfixExpr)
  --> ./124/binary-tree-maximum-path-sum.mochi:67:69

[90m 67[0m |   expect maxPathSum(Node { left: Leaf, value: -3, right: Leaf }) == -3
    | [31;1m                                                                    ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
## ./133/clone-graph.mochi
```
[31;1mtype error:[0;22m
   1. [31;1merror[T027][0;22m: Node is not a struct
  --> ./133/clone-graph.mochi:45:16

[90m 45[0m |   return build(node.val)
    | [31;1m               ^[0;22m

[33mhelp:[0m
  Field access is only valid on struct types.
   2. [31;1merror[T027][0;22m: Node is not a struct
  --> ./133/clone-graph.mochi:58:10

[90m 58[0m |   expect cloned.val == 1
    | [31;1m         ^[0;22m

[33mhelp:[0m
  Field access is only valid on struct types.
   3. [31;1merror[T027][0;22m: Node is not a struct
  --> ./133/clone-graph.mochi:64:24

[90m 64[0m |   let vals = from x in cloned.neighbors select x.val
    | [31;1m                       ^[0;22m

[33mhelp:[0m
  Field access is only valid on struct types.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./138/copy-list-with-random-pointer.mochi
```
[31;1mtype error:[0;22m
   1. [31;1merror[T008][0;22m: type mismatch: expected R, got Node
  --> ./138/copy-list-with-random-pointer.mochi:28:16

[90m 28[0m |         return res
    | [31;1m               ^[0;22m

[33mhelp:[0m
  Change the value to match the expected type.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./146/lru-cache.mochi
```
[31;1mtest failed:[0;22m parse error:
  [31;1merror[P999][0;22m: ./146/lru-cache.mochi:5:17: unexpected token "{" (expected TypeVariant ("|" TypeVariant)*)
  --> ./146/lru-cache.mochi:5:17

[90m  5[0m | type LRUCache = { cap: int, data: map<int, int>, order: list<int> }
    | [31;1m                ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
