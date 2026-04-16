# VM Enhancement Spec

Issues discovered while solving SPOJ problems in Mochi.

## 1. List literal sharing across function calls

**Bug:** A list literal like `[1]` in a function body is reused (same object) across
multiple calls to that function. Mutations inside one call persist into the next.

**Example:**
```mochi
fun f(): list<int> {
  var xs: list<int> = [1]  // same list object every call
  xs = append(xs, 2)
  return xs
}
print(f())  // [1, 2]
print(f())  // [1, 2, 2]  -- BUG: should be [1, 2]
```

**Fix:** Each function call must allocate a fresh copy of any list/map literal
that appears in the function body. The compiler should emit a "clone literal"
instruction rather than a "load constant" instruction for mutable collection
literals inside functions.

**Workaround (current):** Initialize with `var xs: list<int> = []` then
`xs = append(xs, 1)` to guarantee a fresh list.

---

## 2. String slicing fails in callee after deep call chain

**Bug:** When a function `A` calls `B` which calls `C`, and `C` uses a string
slice expression `s[i:n]` where `s` is a parameter and `n = len(s)`, the VM
raises "invalid index target" -- even though `s`, `i`, and `n` are all valid.

Doing the same slice inline in `B` (without calling `C`) works correctly.

**Example:**
```mochi
fun strip(s: string): string {
  let n = len(s)
  return s[0:n]  // fails if called after addStr -> revStr -> strip chain
}
```

**Suspected cause:** The VM's register or stack frame is not fully cleaned up
after returning from a callee that used string operations. A stale register
from the previous call overwrites the incoming parameter register in the new
callee, making the string appear to be a non-string type.

**Fix:** Audit the call/return sequence in the VM bytecode compiler to ensure
each new stack frame is fully initialized and previous frame registers do not
alias into the new frame.

**Workaround (current):** Inline the helper logic into the calling function.

---

## 3. Division operator always returns float

**Current behavior:** `a / b` always returns a float, even when both `a` and `b`
are integers.

**Example:**
```mochi
print(10 / 2)   // 5.0 (float)
print(10 % 3)   // 1   (int)
```

This is inconsistent with `%` which returns int. It forces every integer
division to be wrapped in `int(...)`:
```mochi
carry = int(v / 10)
```

**Fix options:**
- Add `//` integer division operator (Python-style).
- Make `/` return int when both operands are int.
- Document the current behavior clearly in the spec as intentional.

The cleanest approach is to keep `/` as always-float (for simplicity) but add
`//` as integer division. Update the spec and all affected examples.
