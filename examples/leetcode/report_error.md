## ./103/binary-tree-zigzag-level-order-traversal.mochi
```
   [33mtest[0m example 1                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and list
  --> ./103/binary-tree-zigzag-level-order-traversal.mochi:61:34

[90m 61[0m |   expect (zigzagLevelOrder(tree) == [[3], [20, 9], [15, 7]])
    | [31;1m                                 ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (34.0µs)
   [33mtest[0m single node                    ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and list
  --> ./103/binary-tree-zigzag-level-order-traversal.mochi:65:78

[90m 65[0m |   expect (zigzagLevelOrder(Node { left: Leaf {}, value: 1, right: Leaf {} }) == [[1]])
    | [31;1m                                                                             ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (9.0µs)
   [33mtest[0m empty                          ... [32mok[0m (6.0µs)
   [33mtest[0m unbalanced                     ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and list
  --> ./103/binary-tree-zigzag-level-order-traversal.mochi:86:34

[90m 86[0m |   expect (zigzagLevelOrder(tree) == [[1], [3, 2], [4, 5]])
    | [31;1m                                 ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (14.0µs)

[31m[FAIL][0m 3 test(s) failed.
[31;1mtest failed:[0;22m test failed: 3 test(s) failed
```
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
## ./107/binary-tree-level-order-traversal-ii.mochi
```
   [33mtest[0m example 1                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and list
  --> ./107/binary-tree-level-order-traversal-ii.mochi:55:37

[90m 55[0m |   expect levelOrderBottom(example1) == [[15,7],[9,20],[3]]
    | [31;1m                                    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (22.0µs)
   [33mtest[0m single node                    ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types func and list
  --> ./107/binary-tree-level-order-traversal-ii.mochi:59:77

[90m 59[0m |   expect levelOrderBottom(Node { left: Leaf {}, value: 1, right: Leaf {} }) == [[1]]
    | [31;1m                                                                            ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (15.0µs)
   [33mtest[0m empty                          ... [32mok[0m (4.0µs)

[31m[FAIL][0m 2 test(s) failed.
[31;1mtest failed:[0;22m test failed: 2 test(s) failed
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
[31;1mtype error:[0;22m
   1. [31;1merror[T008][0;22m: type mismatch: expected Tree, got fun(): Tree
  --> ./111/minimum-depth-of-binary-tree.mochi:62:19

[90m 62[0m |   expect minDepth(Leaf) == 0
    | [31;1m                  ^[0;22m

[33mhelp:[0m
  Change the value to match the expected type.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./112/path-sum.mochi
```
   [33mtest[0m example 1                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (46.0µs)
   [33mtest[0m example 2                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (11.0µs)
   [33mtest[0m example 3                      ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (10.0µs)
   [33mtest[0m single node                    ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (5.0µs)
   [33mtest[0m empty                          ... [31mfail[0m [31;1merror[I008][0;22m: cannot apply operator '==' to types map and map
  --> ./112/path-sum.mochi:11:5

[90m 11[0m |     Leaf {} => false
    | [31;1m    ^[0;22m

[33mhelp:[0m
  Use compatible types for the operator. (4.0µs)

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
## ./132/palindrome-partitioning-ii.mochi
```
   [33mtest[0m example 1                      ... [31mfail[0m [31;1merror[I014][0;22m: index 3 out of bounds for length 3
  --> ./132/palindrome-partitioning-ii.mochi:34:46

[90m 34[0m |       if s[j] == s[i] && (i - j <= 1 || isPal[j+1][i-1]) {
    | [31;1m                                             ^[0;22m

[33mhelp:[0m
  Use an index within bounds of the list or string. (134.0µs)
   [33mtest[0m example 2                      ... [32mok[0m (8.0µs)
   [33mtest[0m already palindrome             ... [31mfail[0m [31;1merror[I014][0;22m: index 3 out of bounds for length 3
  --> ./132/palindrome-partitioning-ii.mochi:34:46

[90m 34[0m |       if s[j] == s[i] && (i - j <= 1 || isPal[j+1][i-1]) {
    | [31;1m                                             ^[0;22m

[33mhelp:[0m
  Use an index within bounds of the list or string. (104.0µs)
   [33mtest[0m all same                       ... [31mfail[0m [31;1merror[I014][0;22m: index 4 out of bounds for length 4
  --> ./132/palindrome-partitioning-ii.mochi:34:46

[90m 34[0m |       if s[j] == s[i] && (i - j <= 1 || isPal[j+1][i-1]) {
    | [31;1m                                             ^[0;22m

[33mhelp:[0m
  Use an index within bounds of the list or string. (216.0µs)

[31m[FAIL][0m 3 test(s) failed.
[31;1mtest failed:[0;22m test failed: 3 test(s) failed
```
## ./136/single-number.mochi
```
[31;1mtest failed:[0;22m parse error:
  [31;1merror[P040][0;22m: ./136/single-number.mochi:6:21: lexer: invalid input text "^ n\n  }\n  return..."
  --> ./136/single-number.mochi:6:21

[90m  6[0m |     result = result ^ n
    | [31;1m                    ^[0;22m

[33mhelp:[0m
  String literals must be properly closed with a `"`.
```
## ./138/copy-list-with-random-pointer.mochi
```
[31;1mtype error:[0;22m
   1. [31;1merror[T008][0;22m: type mismatch: expected Node, got Nil
  --> ./138/copy-list-with-random-pointer.mochi:18:7

[90m 18[0m |       Nil => Nil {}
    | [31;1m      ^[0;22m

[33mhelp:[0m
  Change the value to match the expected type.
   2. [31;1merror[T008][0;22m: type mismatch: expected Node, got Nil
  --> ./138/copy-list-with-random-pointer.mochi:61:7

[90m 61[0m |       Nil => {}
    | [31;1m      ^[0;22m

[33mhelp:[0m
  Change the value to match the expected type.
[31;1mtest failed:[0;22m aborted due to type errors
```
## ./3580/find-consistently-improving-employees.mochi
```
[31;1mtest failed:[0;22m parse error:
  [31;1merror[P999][0;22m: ./3580/find-consistently-improving-employees.mochi:1:43: unexpected token "{" (expected TypeRef ("," TypeRef)* ">")
  --> ./3580/find-consistently-improving-employees.mochi:1:43

[90m  1[0m | fun consistentlyImproving(employees: list<{ employee_id: int, name: string }>, reviews: list<{ employee_id: int, review_date: string, rating: int }>): list<{ employee_id: int, name: string, improvement_score: int }> {
    | [31;1m                                          ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
## ./60/permutation-sequence.mochi
```
[31;1mtest failed:[0;22m parse error:
  [31;1merror[P999][0;22m: ./60/permutation-sequence.mochi:3:7: unexpected token "fact" (expected <ident> (":" TypeRef)? ("=" Expr)?)
  --> ./60/permutation-sequence.mochi:3:7

[90m  3[0m |   var fact = 1
    | [31;1m      ^[0;22m

[33mhelp:[0m
  Parse error occurred. Check syntax near this location.
```
