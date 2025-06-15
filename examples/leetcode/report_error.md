# Failed LeetCode Examples

The following Mochi solutions produced errors when running `mochi test` on 2025-06-15.

## 110 - Balanced Binary Tree
- **Issue**: `error[I007]` – cannot access field `balanced` on non-object.
- **Snippet**:
```text
42 |   return result.balanced
    |          ^
```
- **Tests failed**: example 1, example 2, single node.

## 111 - Minimum Depth of Binary Tree
- **Issue**: `error[I008]` – invalid `==` comparison between `null` and `int`.
- **Snippet**:
```text
43 |   expect minDepth(tree) == 2
    |                         ^
```
- **Tests failed**: example 1, example 2, single node, empty.

## 112 - Path Sum
- **Issue**: `error[I008]` – invalid `==` comparison between `null` and `bool`.
- **Snippet**:
```text
52 |   expect hasPathSum(tree, 22) == true
    |                               ^
```
- **Tests failed**: example 1–3, single node, empty.

## 117 - Populating Next Right Pointers in Each Node II
- **Issue**: test run never completes (likely infinite loop). No output produced before timeout.

## 124 - Binary Tree Maximum Path Sum
- **Issue**: interpreter panic during tests (`nil pointer dereference`).
- **Snippet**:
```text
panic: runtime error: invalid memory address or nil pointer dereference
```

## 133 - Clone Graph
- **Issue**: parse error due to unexpected `.` token.
- **Snippet**:
```text
error[P001]: 133/clone-graph.mochi:41:24: unexpected token "." (expected "}")
```

## 138 - Copy List with Random Pointer
- **Issue**: `error[T008]` – type mismatch, returning `Node` instead of result type.
- **Snippet**:
```text
25 |         return res
    |                ^
```

