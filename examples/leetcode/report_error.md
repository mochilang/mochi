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

## 117 - Populating Next Right Pointers in Each Node II
- **Issue**: test run never completes (likely infinite loop). Timed out after 5s.

## 124 - Binary Tree Maximum Path Sum
- **Issue**: interpreter panic during tests (`nil pointer dereference`).
- **Snippet**:
```text
panic: runtime error: invalid memory address or nil pointer dereference
```

## 138 - Copy List with Random Pointer
- **Issue**: `error[T008]` – type mismatch, returning `Node` instead of result type.
- **Snippet**:
```text
25 |         return res
    |                ^
```
