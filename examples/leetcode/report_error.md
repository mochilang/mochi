# Failed LeetCode Examples

The following Mochi solutions produced errors when running `mochi test` on 2025-06-15.

## 124 - Binary Tree Maximum Path Sum
- **Issue**: interpreter panic during tests (`nil pointer dereference`).
- **Log snippet**:
```text
panic: runtime error: invalid memory address or nil pointer dereference
mochi/runtime/ffi.(*Manager).Lookup...
```

## 110 - Balanced Binary Tree
- **Issue**: `error[T014]` – invalid primary expression.
- **Snippet**:
```text
23 |         let diff = if left.height > right.height {
   |                    ^
```

## 117 - Populating Next Right Pointers in Each Node II
- **Issue**: test run never completes (likely infinite loop). Timed out after 5s.

## 138 - Copy List with Random Pointer
- **Issue**: test run never completes (likely infinite loop). Timed out after 5s.
