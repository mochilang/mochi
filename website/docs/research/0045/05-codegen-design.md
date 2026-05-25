# MEP-45 research note 05, Codegen design

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

This note specifies the *shape* of the C the transpiler emits and the IR
it uses to get there. It is the technical heart of MEP-45 and is
intended to be a working blueprint. Lowering of the eight sub-languages
is described in note 01 §1–10; this note covers the cross-cutting
mechanics.

## 1. Pipeline overview

```
.mochi source
     │
     │  (existing) lexer, parser, type-checker, MIR construction
     ▼
Mochi MIR  ─►  M2C lowering  ─►  C IR (in-memory)  ─►  C printer  ─►  .c / .h files
                                                                     │
                                                                     ▼
                                                                cc + ld  ─►  native binary
```

The first three stages are reused from the existing Mochi compiler (the
parser, the type-checker, the MIR). MEP-45 adds the rightmost three:
**M2C lowering**, **C IR**, and **C printer**. M2C is the only place
where Mochi-specific knowledge meets C-specific knowledge.

A separate "**build driver**" stage handles `cc` invocation, runtime
archive selection, and linking; that is covered in note 09.

## 2. Why a C IR

The naive option is to print C strings directly from MIR. We reject this
because:

- C has lexical constraints (forward declarations, header/source split,
  the one-definition rule) that string-printing handles poorly.
- Optimisations and stylisations (constant folding, dead-store
  elimination, comment placement, name de-duplication) want a
  manipulable tree.
- The test harness needs to compare C output structurally, not
  textually, for golden-test stability across formatting changes.
- A C IR is also our format for the `--emit-c-ir` debug flag, useful
  for users who want to inspect the lowering without reading the printed
  C.

The C IR is a small data structure modelled loosely on TinyCC's AST and
on the Cone/Roc lowering IR. It has 14 node kinds:

| Kind | Note |
|------|------|
| `TUnit` | Translation unit (a `.c` file with its includes, decls, defs) |
| `TInclude` | `#include "..."` or `<...>` |
| `TPragma` | `#pragma` line |
| `TTypedef` | `typedef T name;` |
| `TStructDecl` | Forward / full struct declaration |
| `TUnionDecl` | Same for unions (rare; used by sum-type lowering) |
| `TGlobalDecl` | `extern T name;` in a header |
| `TGlobalDef` | `static T name = init;` in a source file |
| `TFunctionDecl` | Prototype in a header |
| `TFunctionDef` | Body in a source file |
| `TBlock` | `{ stmts... }` |
| `TStmt` | If, While, For, Switch, Return, Break, Continue, Expr, Goto, Label, Asm |
| `TExpr` | Lit, Var, Call, BinOp, UnOp, Index, Member, Cast, Cond, Sizeof, Compound |
| `TComment` | Block / inline; preserved for source-map back-trace |

This is *not* a portable C AST (no clang AST replica, no GCC GENERIC
clone). It is *just enough* to round-trip the subset of C we emit, and
not one node kind more.

## 3. Name mangling

Deterministic, reversible, and ABI-stable. The rules:

1. Package qualifier: `<package>_`, with package path components joined by
   `_`. A package at `./util/format.mochi` becomes prefix `util_format_`.
2. Symbol name: 1-for-1 ASCII identifier characters; non-ASCII gets `_u<hex>`
   (e.g. `π` → `_u03c0`).
3. Type parameter instantiation: appended as `__<inst>`, with `<inst>`
   constructed by joining type names with `_`, and primitive types using
   the abbrevs `i64` / `f64` / `bool` / `str`. So `list<int>` becomes
   `mochi_list__i64`, and a user `Pair<int, string>` becomes
   `pkg_Pair__i64_str`.
4. Method on a type: `<type>__<method>`. `Circle.area` becomes
   `pkg_Circle__area`.
5. Variant constructor: `<typename>__<variantname>`. `Tree.Leaf` becomes
   `pkg_Tree__Leaf`. The discriminant tag is `pkg_Tree_TAG__Leaf`.
6. Internal helpers: prefix `mochi_internal__`. Reserved; never collides
   with user code because Mochi identifiers cannot contain `__`.
7. Mochi keywords that overlap C keywords: `import` is not emitted (the
   transpiler resolves it before codegen); user-defined `static` /
   `extern` / `volatile` / `register` / `restrict` / `inline` are
   rewritten with a `_` suffix (`extern_`, `static_`).

The reverse-map (demangler) is a tool, `mochi-demangle <name>`, used by
debug output and crash reports.

## 4. Type lowering

| Mochi type | C lowering | Notes |
|------------|------------|-------|
| `int` | `int64_t` | Always 64-bit |
| `float` | `double` | IEEE 754 binary64 |
| `bool` | `bool` | C23 keyword |
| `string` | `mochi_str` | { uint8_t *bytes; size_t len; uint32_t hash; } |
| `time` | `mochi_time` (int64_t ns since Unix epoch UTC) | |
| `duration` | `mochi_dur` (int64_t ns) | |
| `list<T>` | `mochi_list__<T>` | { T *data; size_t len, cap; uint32_t hdr; } |
| `map<K,V>` | `mochi_map__<K>_<V>` | Swiss-table or chained-hash |
| `set<T>` | `mochi_set__<T>` | alias for `mochi_map__<T>_unit` |
| Record `T { ... }` | `struct pkg_T` | Plain C struct |
| Sum type `T = A | B(...)` | `struct pkg_T { pkg_T_tag tag; union { ... } u; }` | Tag word + payload union |
| `fun(A,B): C` | `mochi_closure_<sig>` | { code-ptr; env-ptr; } two-word fat pointer |
| `error` | `mochi_error` | { code: int; message: mochi_str; cause: opt<error>; } |

Sizes (on the canonical LP64 target):

- `mochi_str`: 16 bytes for the pointer/len + 4 hash = 24 bytes after
  padding. We accept the 4-byte tax for membership-test fast paths.
- `mochi_list__T`: 24 bytes header + payload buffer.
- `mochi_map__K_V`: 32 bytes header + slots buffer.
- closure: 16 bytes.
- sum type: tag word (4 bytes) + payload (variant-max + padding).

The `<hdr>` 32-bit word in lists/maps/strings packs an interning bit, a
GC-managed flag, and a tag for the Perceus RC backend.

## 5. Value representation: monomorphic, with niche cases

Per design philosophy §5, monomorphic by default. Three special cases:

### 5.1 `any` and unknown-shape FFI values

A `fetch` of an untyped JSON or a `generate text` returns `string`, but
a `load X as map<string, any>` returns a heterogeneous map. We add a
`mochi_value` boxed type:

```c
typedef struct {
    uint8_t  kind;     // tag
    uint32_t reserved;
    union {
        int64_t      i;
        double       f;
        bool         b;
        mochi_str    s;
        void        *p;  // list/map/record handle
    } v;
} mochi_value;
```

Used **only** at the FFI boundary and for `any`-typed program text. The
type-checker proves where `any` actually appears, so the boxed type does
not leak into pure-Mochi loops.

### 5.2 Closures crossing module boundaries

When a closure is exported across packages, monomorphisation of its
captures is unsafe (the receiver does not know the env's shape). The
codegen emits an erased version with a `void *` env and a per-call
dispatch shim. The type-checker guarantees the shim's signature.

### 5.3 Sum-type niche optimisation

A `?T` (option) over a pointer-like type folds the `None` tag into the
nullptr representation, saving one tag word. A `bool | None` lowers to a
single 4-bit code. The codegen has a small table of niche rules; if a
type has no niche, it falls back to the explicit tag+union layout.

## 6. Expression lowering

Mochi expressions translate one-for-one to C expressions with three
exceptions:

1. **Short-circuit `&&` / `||`** are emitted as C `&&` / `||` and
   inherit C's short-circuit semantics. No special lowering.
2. **`if/then/else` expression** lowers to a C ternary `cond ? then :
   else`. Statement-position `if/else` lowers to a C `if` with a
   yielded-value temp:

   ```c
   T tmp; if (cond) { tmp = ...; } else { tmp = ...; }
   ```

3. **`match` expressions** lower to Maranget's decision-tree algorithm
   (see §10).

Note on overflow: `int + int` is `mochi_iadd(a, b)`, a macro that
expands to `__builtin_add_overflow` checked addition in debug builds
(traps via `mochi_panic`), and plain `+` in release builds. Same for
`isub`, `imul`, `ineg`, `idiv` (also checks for zero).

## 7. Statement lowering

| Mochi | C |
|-------|---|
| `let x = e` | `T x = (e);` in scope (also enforces no rebinding via flag) |
| `var x = e` | `T x = (e);` |
| `x = e` | `x = (e);` (only if `x` is `var`) |
| `if c { ... } else { ... }` | `if (c) { ... } else { ... }` |
| `while c { ... }` | `while (c) { ... }` |
| `for x in range_or_list { ... }` | see §8 |
| `break` / `continue` | `break;` / `continue;` |
| `return e` | `return e;` |
| `try { ... } catch e { ... }` | see §9 |

Indentation: 4 spaces, K&R braces, max line length 100 columns.

## 8. `for` lowering

Three syntactic forms cover everything in the language:

```mochi
for i in lo..hi  { body }            // range
for x in list_expr { body }          // list / set
for k in map.keys() { body }         // map (and similarly .values(), .entries())
for ch in string_expr { body }       // utf-8 code-point cursor
```

Lowering for the range form is a plain C `for` loop:
```c
for (mochi_int i = (lo); i < (hi); i++) { body }
```
Lowering for the list form unrolls the cursor into a temp to avoid
re-evaluating the source expression:
```c
{
    mochi_list__T __xs = (list_expr);
    for (size_t __i = 0; __i < __xs.len; __i++) {
        T x = __xs.data[__i];
        body
    }
}
```
The string form uses the `mochi_str_iter` cursor:
```c
{
    mochi_str __s = (string_expr);
    mochi_str_iter __it = mochi_str_iter_init(__s);
    mochi_str ch;
    while (mochi_str_iter_next(&__it, &ch)) {
        body
    }
}
```
Map iteration uses the hash-table cursor, which is unordered for
`.keys()`/`.values()` and insertion-ordered when stability is required
(query layer uses an insertion-ordered specialisation).

Break and continue traverse a single enclosing for/while, Mochi has no
labelled break, simplifying the lowering.

## 9. `try / catch` lowering

The unit of unwinding is the try-block. Lowering:

```c
{
    mochi_jmp_buf __jb;
    mochi_jmp_buf *__prev = mochi_set_jb(&__jb);
    if (setjmp(__jb.buf) == 0) {
        // try body
    } else {
        mochi_error e = mochi_take_pending_error();
        mochi_set_jb(__prev);
        { /* catch body, e in scope */ }
        goto __try_end;
    }
    mochi_set_jb(__prev);
    __try_end: ;
}
```

A `throw e` (only emitted by built-ins; Mochi has no surface `throw`)
calls `mochi_throw(e)`, which writes the error into thread-local storage
and `longjmp`s to the current `mochi_jmp_buf`.

This costs ~50 ns per `try` on M-class hardware and avoids C++'s
table-based unwinding (which we cannot use from plain C portably).
Algebraic effect handlers are reserved for Phase 3; the localised
structure of this lowering makes that swap-in tractable.

## 10. Pattern-matching lowering

We implement Maranget 2008 ("Compiling Pattern Matching to Good Decision
Trees") as the canonical algorithm. The compiler:

1. Builds a decision tree from the arms, sharing common prefixes.
2. Reports non-exhaustive matches as compile-time errors.
3. Reports redundant arms as compile-time warnings.
4. Emits a nested C `if/else if` chain or a `switch (tag)` when the head
   discriminator is a tag word.

For sum types with N variants, the lowering is:

```c
switch (val.tag) {
case PKG_TREE_TAG__Leaf:
    /* arm body for Leaf */
    break;
case PKG_TREE_TAG__Node:
    {
        T1 l = val.u.Node.left;
        T2 v = val.u.Node.value;
        T3 r = val.u.Node.right;
        /* arm body for Node */
    }
    break;
default:
    mochi_unreachable("non-exhaustive match");
}
```

Wildcard `_` arms collapse to the `default` case. Nested patterns
recurse: a `Node(Leaf, _, _)` outer pattern unfolds into two switches.

## 11. Module compilation

One Mochi package → one C translation unit (`.c`) plus one header
(`.h`). The header exports `extern` declarations for `export`-marked
symbols. The source defines everything; non-exported symbols are
`static`.

Inter-package references go through the alias prefix mapping (§3.1).
The transpiler emits an `#include "<package_path>.h"` at the top of any
file that references another package.

For circular package imports (which Mochi disallows but the C output
must not depend on that): the typechecker breaks the cycle by
forward-declaring opaque struct shells in a "shared types" header.

Header organisation:

```c
// util_format.h
#ifndef MOCHI_PKG_util_format_H
#define MOCHI_PKG_util_format_H
#include "mochi/core.h"
extern mochi_str util_format_debug(mochi_value x);
extern mochi_str util_format_json(mochi_value x);
#endif
```

```c
// util_format.c
#include "util_format.h"
mochi_str util_format_debug(mochi_value x) { ... }
mochi_str util_format_json(mochi_value x) { ... }
```

A program's entry point is generated into `main.c`:

```c
#include "mochi/core.h"
#include "main.h"
int main(int argc, char **argv) {
    mochi_init(argc, argv);
    int rc = mochi_internal__main_program();
    return mochi_shutdown(rc);
}
```

## 12. Whole-program amalgamation (optional)

The MEP body permits an `--amalgamate` flag that emits a single `.c`
file containing all packages, runtime included, sqlite-style. Useful
for:

- Reproducible single-file distributions for embedding.
- LTO without ThinLTO machinery; the C compiler sees everything.
- Cosmopolitan / wasi-libc compilations that benefit from
  single-file input.

The amalgamation is just a deterministic concatenation of the per-package
files plus the runtime source. Identifiers do not collide because the
name mangling (§3) is package-prefixed.

## 13. Debug information

The transpiler emits `#line` directives mapping every C statement back
to its Mochi source line:

```c
#line 42 "main.mochi"
mochi_int n = mochi_iadd(a, b);
```

This is sufficient for `gdb` / `lldb` to step Mochi source. For DWARF
expression-level debug, a Phase-2 enhancement emits per-Mochi-expression
synthetic locals that the debugger can pretty-print via a Python
pretty-printer.

## 14. Optimisations the transpiler performs (not the C compiler)

The C compiler handles ordinary optimisations (inlining, CSE, DCE, LICM).
The transpiler performs *Mochi-specific* optimisations the C compiler
cannot see:

1. **List literal cloning** (VM-enhance 0951 §1): inside a function body,
   `[1, 2, 3]` is cloned on each call. Inferred as a stack-local array
   when the type-checker proves no escape.
2. **String interning**: constant strings are deduplicated into a
   per-module symbol table.
3. **Sum-type niche packing** (§5.3).
4. **`for` over `lo..hi`** with constant bounds and a simple body
   unrolls 1–4× when the body has no side effects.
5. **Closure devirtualisation**: when a closure is passed to a higher-order
   function and the call site is a single-target, the indirect call
   resolves to a direct call.
6. **Query fusion**: `from x in xs where p select f` becomes a single
   `for` loop with `if (p) result.push(f(x))`, no intermediate
   materialisation.
7. **Tail-position recognition** (Phase 2, opt-in): a self-tail-call
   uses `[[gnu::musttail]]` when supported, falling back to a `goto` loop
   conversion when not.

Each optimisation is independently switchable via `--O-<name>=on|off`
for testability.

## 15. Output stability

The C output is deterministic given the inputs:

- File order: lexicographic by package path.
- Symbol order within a file: type decls → globals → fns, each
  alphabetised by mangled name.
- Comments include source path (`-ffile-prefix-map` keeps these
  reproducible).
- `#line` directives use the path as the type-checker saw it (after
  `--remap-source`).

This determinism is a *gate*: the BG CI runs the transpiler twice and
diffs the output; any change fails the build.

## 16. Per-target adjustments

A small `target.json` selects per-target codegen tweaks:

| Target | Tweak |
|--------|-------|
| linux-amd64 | Default. `-fcf-protection=full`. |
| linux-arm64 | `-mbranch-protection=standard` (PAC+BTI). |
| macos-arm64 | `MAP_JIT` not needed (no JIT in C target). Same `-mbranch-protection`. |
| windows-x64 | MSVC ABI; struct returns via hidden pointer (small structs by value). |
| wasm32-wasi | No `setjmp/longjmp` on Asyncify; switch error model to `__builtin_wasm_throw`-style trap-and-resume. |
| freebsd-amd64 | Like linux, with `-lpthread` -> `-lthr`. |
| baremetal | Replace `mochi-net` / `mochi-stream` with stubs. Static allocator only. |

These are passive overrides; the user's source is the same on every
target.
