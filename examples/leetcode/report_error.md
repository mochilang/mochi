# Failed LeetCode Examples

The following Mochi solutions produced errors when running `make test` on 2025-06-15.

## 124 - Binary Tree Maximum Path Sum
- **Issue**: interpreter panic during tests (`nil pointer dereference`).
- **Log snippet**:
```text
   [33mtest[0m example 1                      ...panic: runtime error: invalid memory address or nil pointer dereference
[signal SIGSEGV: segmentation violation code=0x1 addr=0x0 pc=0xc7b395]

goroutine 1 [running]:
```

## 138 - Copy List with Random Pointer
- **Issue**: test run never completes (likely infinite loop). Timed out after 5s.
