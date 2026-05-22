# MEP-45 research note 06, Type-system lowering details

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

Companion to note 05. Focused on the type-system corners and how they
become C types and C invariants. This note assumes the codegen scaffold
in note 05 and elaborates one piece at a time.

## 1. Generics and monomorphisation

Mochi's surface generics are limited to **type parameters on collections
and on user record/sum types** (no higher-kinded types in the docs, no
trait/typeclass system surfaced). The transpiler performs full
monomorphisation:

1. After type-checking, the elaborator records every concrete
   instantiation observed in the program (`list<int>`, `Pair<string,
   list<float>>`, `Option<Tree>`).
2. For each instantiation, a fresh C type and a fresh set of method/helper
   functions are emitted.
3. De-duplication: identical instantiations across packages collapse to a
   single C type using the mangling rules in note 05 §3.
4. Polymorphic recursion is rejected at type-check time (a generic
   function cannot call itself at a strictly larger type), this keeps
   the monomorphisation set finite. (The MEP body must verify the spec
   rules this out; if not, a Phase-2 "polymorphic stub" using a boxed
   `mochi_value` is the fallback.)

The set of instantiations is a closed analysis over the whole program,
so it requires the transpiler to see all packages at once. For the
`mochi build` path this is fine; for separate compilation (Phase 3) the
transpiler would have to emit specialisation stubs lazily.

## 2. Records (struct types)

```mochi
type Book {
  title: string
  author: Person
  pages: int
  tags: list<string>
  metadata: map<string, string>
  published: bool
}
```

Lowering: a packed C struct. Field order in the C struct is the *source
order* (so `published: bool` lives at the end of `Book`, and the
compiler does not silently re-order for cache reasons). This rule
exists for two reasons:

- Deterministic ABI: a Book emitted by this transpiler must lay out
  identically on every supported target.
- Round-trip with `print(book)`: pretty-printers walk fields in source
  order.

C struct layout still has C padding rules. The transpiler does not
inject `__attribute__((packed))` (that would trigger UB on misaligned
loads on arm64). Instead, the transpiler may *re-order at the source
level* if and only if the user opted in with `@layout(compact)` on the
type (Phase 2). For v1, source order wins.

Constructors:

```c
struct pkg_Book pkg_Book__new(
    mochi_str title, struct pkg_Person author, mochi_int pages,
    mochi_list__str tags, mochi_map__str_str metadata, bool published);
```

Field access `b.title` lowers to `b.title` (the C compiler matches the
field name). Field update `b.pages = 5` is **not allowed** if the
binding holding `b` is `let`; under `var`, the lowering is plain C
assignment.

Structural equality is field-wise:

```c
bool pkg_Book__eq(struct pkg_Book a, struct pkg_Book b);
```

emitted automatically when the type is used in `==` / `!=` /
`set<Book>` / `map<Book, ...>`.

## 3. Sum types

```mochi
type Tree = Leaf | Node(left: Tree, value: int, right: Tree)
```

Lowering:

```c
typedef enum {
    PKG_TREE_TAG__Leaf,
    PKG_TREE_TAG__Node,
} pkg_Tree_tag;

struct pkg_Tree {
    pkg_Tree_tag tag;
    union {
        struct { /* empty */ } Leaf;
        struct {
            struct pkg_Tree *left;     // boxed, to break recursion
            mochi_int       value;
            struct pkg_Tree *right;
        } Node;
    } u;
};
```

Constructors emit factory functions:

```c
struct pkg_Tree pkg_Tree__Leaf(void);
struct pkg_Tree pkg_Tree__Node(struct pkg_Tree *l, mochi_int v, struct pkg_Tree *r);
```

Recursive variant payloads use heap-allocated boxes; non-recursive
payloads inline. The transpiler proves recursion at type-check time and
boxes accordingly.

Equality: tag compare, then per-variant field compare. Auto-derived;
shared with the records path.

### 3.1 Niche optimisation

Two special cases:

- `?T` (`type Option<T> = None | Some(T)`) with `T` a pointer-shaped
  value (string, list, map, record handle): `None` is `nullptr`, `Some(x)`
  is `x`. No tag word.
- A two-variant nullary enum (e.g. `type Side = Left | Right`) lowers to
  a single `uint8_t`.

A third case considered: pointer tagging on platforms where the low 3
bits of a pointer are unused. Deferred to Phase 2, the codegen saving
is real (~6 % on the BG corpus per the Lobster paper) but the
portability cost is high (wasm32 pointers, arm64 with MTE).

### 3.2 GADTs and refined patterns

Not in the language surface. No work to do.

## 4. Function types and closures

Mochi function types are structural: `fun(int, string): bool` matches
any function (free or method or arrow) with that signature.

Lowering:

```c
typedef struct {
    bool (*code)(void *env, mochi_int a, mochi_str b);
    void *env;
} mochi_closure__bool_i64_str;

bool mochi_closure__bool_i64_str__call(
    mochi_closure__bool_i64_str f, mochi_int a, mochi_str b) {
    return f.code(f.env, a, b);
}
```

Free functions get an `env == NULL` adapter. Arrow functions get an
`env`-struct holding their captures. Method values get an `env == self`
shim.

The transpiler tries to devirtualise where it can (note 05 §14, item 5),
but a closure call site never compiles into something *less* than this
two-load + indirect-call sequence. The cost is ~3-4 cycles on modern x86
and arm64, well-predicted by the branch target predictor as long as the
call site is monomorphic.

### 4.1 Closure environment heap discipline

If the closure escapes its lexical scope (returned upwards, stored in a
record, sent on a channel), the env struct is heap-allocated and
GC-managed. If the closure does not escape (single use inside the same
function), the env struct is stack-allocated.

The escape analysis is conservative: any closure passed to a function
parameter is treated as escaping unless the callee is annotated
`@no_escape` (Phase 2 attribute). For v1, all closures heap-allocate
their env when they capture anything. This is the safe-and-correct
default; performance work is Phase 2.

## 5. Strings

`mochi_str` is an immutable UTF-8 byte slice with a precomputed hash:

```c
typedef struct {
    const uint8_t *bytes;
    size_t         len;       // byte length
    uint32_t       hash;      // FxHash or wyhash, computed at construction
    uint32_t       flags;     // interned, gc-managed, …
} mochi_str;
```

Indexing `s[i]` returns the i-th *code point* as a one-character
`mochi_str`. The cursor:

```c
typedef struct { const uint8_t *p; const uint8_t *end; } mochi_str_iter;
mochi_str_iter mochi_str_iter_init(mochi_str s);
bool mochi_str_iter_next(mochi_str_iter *it, mochi_str *out_char);
```

The cursor handles BOM, surrogates, and invalid UTF-8 (replacement
character per Unicode TR-36).

Concatenation `+`:

```c
mochi_str mochi_str_cat(mochi_str a, mochi_str b);
```

Short-string optimisation: strings of ≤ 22 bytes are stored inline in the
`mochi_str` struct (`bytes` reinterpreted as a buffer, length in low 6
bits of `flags`). This is a Phase-2 optimisation, v1 always heap-allocates.

Interning: string literals in the source are deduplicated per-module at
codegen time. Runtime interning is *not* automatic; an opt-in
`mochi_str_intern(s)` returns a canonical handle for `map<string, T>`
hot loops.

## 6. Lists

`mochi_list__T` is a growable, dense vector:

```c
typedef struct {
    T       *data;
    size_t   len;
    size_t   cap;
    uint32_t flags;     // gc, frozen, owned, ...
} mochi_list__T;
```

Capacity policy: amortised doubling with a floor of 4. Append is
amortised O(1). Concatenation `a + b` allocates a fresh list of length
`a.len + b.len`.

The "list literal is cloned per call" rule (VM-enhance 0951 §1):

- Compile-time constant literal in a `let` binding at top level: emitted
  as a `static const` array; the wrapper is built once at startup.
- Constant literal in a function body: emitted as a `mochi_list__T_new3(1,
  2, 3)` call, which always allocates.
- Variable literal in a function body: same. `[]` always returns a
  fresh empty list.

This is per the VM-enhance spec which is normative for the language.

## 7. Maps

`mochi_map__K_V` is a Swiss-table (Abseil-style) for `K` hashable. The
hash function is statically resolved from `K`:

- `int`, `bool`, enum-tag-like sum tags: identity (then mixed with
  multiplicative hash to break clustering)
- `string`: precomputed `hash` field on `mochi_str`
- record types: field-wise hash, combined with a rotating mixer

```c
typedef struct {
    /* opaque slot array */
    void    *slots;
    size_t   len;
    size_t   cap;
    uint32_t flags;
} mochi_map__K_V;
```

API:

```c
V        mochi_map__K_V__get(mochi_map__K_V m, K key);     // panic on miss
bool     mochi_map__K_V__try_get(mochi_map__K_V m, K key, V *out);
void     mochi_map__K_V__set(mochi_map__K_V *m, K key, V value);
bool     mochi_map__K_V__contains(mochi_map__K_V m, K key);
size_t   mochi_map__K_V__len(mochi_map__K_V m);
```

The query layer needs insertion-ordered iteration. A second flavour,
`mochi_omap__K_V`, holds an auxiliary `mochi_list__K` of insertion order
keys and is the type the query DSL emits for `from x in m`. This is a
separate type so the cost is not paid by hot non-query maps.

## 8. Sets

A `mochi_set__T` is structurally `mochi_map__T_unit` where `unit` is a
zero-byte sentinel. The map's value slot is omitted by the codegen,
saving the value-side storage.

## 9. Time and duration

```c
typedef int64_t mochi_time;       // ns since Unix epoch UTC
typedef int64_t mochi_dur;        // ns
```

`std/time.now()` calls `clock_gettime(CLOCK_REALTIME, &ts)` on POSIX,
`GetSystemTimePreciseAsFileTime` on Windows. `parse_iso` and
`format_iso` are runtime functions. Arithmetic is plain integer
arithmetic with overflow checks.

## 10. Error values

```c
typedef struct mochi_error {
    int32_t        code;       // 0 = no-error, negative = built-in, positive = user
    mochi_str      message;
    struct mochi_error *cause; // chained, optional
} mochi_error;
```

Built-in codes are documented in `mochi/errors.h`:

| Code | Name | Source |
|------|------|--------|
| -1 | `MOCHI_ERR_FETCH` | network or HTTP non-2xx |
| -2 | `MOCHI_ERR_PARSE` | JSON / YAML / CSV decode |
| -3 | `MOCHI_ERR_TYPE` | runtime type mismatch (from `mochi_value` unwrap) |
| -4 | `MOCHI_ERR_INDEX` | OOB index / missing key |
| -5 | `MOCHI_ERR_DIVZERO` | integer divide by zero |
| -6 | `MOCHI_ERR_OVERFLOW` | integer overflow (debug builds only) |
| -7 | `MOCHI_ERR_FFI` | FFI subprocess failure |
| -8 | `MOCHI_ERR_LLM` | provider error from `generate` |
| -9 | `MOCHI_ERR_ASSERT` | `expect false` |

User errors get codes ≥ 1; the runtime issues them via a `mochi_panic`
counterpart that constructs a `mochi_error` and `mochi_throw`s.

## 11. Effect labels (future)

The current language surface has no algebraic effect system in the
source. The MEP body should record that *if* a future MEP adds one (the
threat-model document mentions a `meta` effect on `OpUnseal`), the C
target is prepared: a per-effect capability is threaded as a hidden
parameter through any function that uses it, matching the Koka
capability-passing translation.

For v1: no extra parameters.

## 12. Type-checker → codegen interface

The MIR carries every type as a fully-instantiated `MType`. The codegen
visits each `MType` exactly once via a memoised lowering function
`lower_type(MType) -> CType`. Memoisation ensures that two identical
`MType`s share a single C type and a single set of helpers.

The lowering is total: every `MType` the type-checker emits has a C
lowering. The MEP body must list every case and the test gate must
cover all of them with at least one fixture each.
