
## ./105/construct-binary-tree-from-preorder-and-inorder-traversal.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `+` cannot be used on types [int] and [int]
  --> ./105/construct-binary-tree-from-preorder-and-inorder-traversal.mochi:40:26

[90m 40[0m |     Node(l, v, r) => [v] + preorderTraversal(l) + preorderTraversal(r)
    | [31;1m                         ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
   2. [31;1merror[T020][0;22m: operator `+` cannot be used on types [int] and [int]
  --> ./105/construct-binary-tree-from-preorder-and-inorder-traversal.mochi:48:42

[90m 48[0m |     Node(l, v, r) => inorderTraversal(l) + [v] + inorderTraversal(r)
    | [31;1m                                         ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
   3. [31;1merror[T013][0;22m: incompatible types in comparison
  --> ./105/construct-binary-tree-from-preorder-and-inorder-traversal.mochi:72:15

[90m 72[0m |   expect tree == Leaf
    | [31;1m              ^[0;22m

[33mhelp:[0m
  Use comparable types like numbers or strings with `<`, `>`.
[31;1merror:[0;22m aborted due to type errors
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
[31;1merror:[0;22m aborted due to type errors
```
## ./109/convert-sorted-list-to-binary-search-tree.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `+` cannot be used on types [int] and [int]
  --> ./109/convert-sorted-list-to-binary-search-tree.mochi:31:33

[90m 31[0m |     Node(l, v, r) => inorder(l) + [v] + inorder(r)
    | [31;1m                                ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
[31;1merror:[0;22m aborted due to type errors
```
## ./110/balanced-binary-tree.mochi

```
[31;1merror:[0;22m parse error:
  [31;1merror[P999][0;22m: ./110/balanced-binary-tree.mochi:12:20: unexpected token "{" (expected TypeVariant ("|" TypeVariant)*)
  --> ./110/balanced-binary-tree.mochi:12:20

[90m 12[0m | type BalanceInfo = { h: int, balanced: bool }
    | [31;1m                   ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
## ./111/minimum-depth-of-binary-tree.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T008][0;22m: type mismatch: expected Tree, got fun(): Tree
  --> ./111/minimum-depth-of-binary-tree.mochi:62:19

[90m 62[0m |   expect minDepth(Leaf) == 0
    | [31;1m                  ^[0;22m

[33mhelp:[0m
  Change the value to match the expected type.
[31;1merror:[0;22m aborted due to type errors
```
## ./113/path-sum-ii.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `+` cannot be used on types [int] and [int]
  --> ./113/path-sum-ii.mochi:15:23

[90m 15[0m |       let newPath = p + [v]
    | [31;1m                      ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
[31;1merror:[0;22m aborted due to type errors
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
[31;1merror:[0;22m aborted due to type errors
```
## ./124/binary-tree-maximum-path-sum.mochi

```
[31;1merror:[0;22m parse error:
  [31;1merror[P999][0;22m: ./124/binary-tree-maximum-path-sum.mochi:26:24: unexpected token "if" (expected PostfixExpr)
  --> ./124/binary-tree-maximum-path-sum.mochi:26:24

[90m 26[0m |         let leftBest = if leftDown > 0 { leftDown } else { 0 }
    | [31;1m                       ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
## ./3580/find-consistently-improving-employees.mochi

```
[31;1merror:[0;22m parse error:
  [31;1merror[P999][0;22m: ./3580/find-consistently-improving-employees.mochi:1:43: unexpected token "{" (expected TypeRef ("," TypeRef)* ">")
  --> ./3580/find-consistently-improving-employees.mochi:1:43

[90m  1[0m | fun consistentlyImproving(employees: list<{ employee_id: int, name: string }>, reviews: list<{ employee_id: int, review_date: string, rating: int }>): list<{ employee_id: int, name: string, improvement_score: int }> {
    | [31;1m                                          ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
## ./94/binary-tree-inorder-traversal.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `+` cannot be used on types [int] and [int]
  --> ./94/binary-tree-inorder-traversal.mochi:12:42

[90m 12[0m |     Node(l, v, r) => inorderTraversal(l) + [v] + inorderTraversal(r)
    | [31;1m                                         ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
[31;1merror:[0;22m aborted due to type errors
```
## ./95/unique-binary-search-trees-ii.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `+` cannot be used on types [Tree] and [Node]
  --> ./95/unique-binary-search-trees-ii.mochi:20:25

[90m 20[0m |         result = result + [Node { left: l, val: i, right: r }]
    | [31;1m                        ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
[31;1merror:[0;22m aborted due to type errors
```
## ./99/recover-binary-search-tree.mochi

```
[31;1mtype error:[0;22m
   1. [31;1merror[T020][0;22m: operator `+` cannot be used on types [int] and [int]
  --> ./99/recover-binary-search-tree.mochi:14:33

[90m 14[0m |     Node(l, v, r) => inorder(l) + [v] + inorder(r)
    | [31;1m                                ^[0;22m

[33mhelp:[0m
  Choose an operator that supports these operand types.
[31;1merror:[0;22m aborted due to type errors
```
