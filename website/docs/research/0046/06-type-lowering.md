# MEP-46 research note 06, Lowering Mochi's static type system onto BEAM terms

Author: research pass for MEP-46 (Mochi → Erlang/BEAM transpiler).
Date: 2026-05-22 (GMT+7).

Mochi is a statically typed language. BEAM is a dynamically typed virtual
machine: every value is a tagged term, every operator dispatches at runtime,
and the only static guarantee comes from Dialyzer's success-typing pass over
`-spec` declarations. The Mochi → BEAM transpiler therefore lives in a regime
that is the opposite of the C backend in `transpiler3/c/`: there is no
type-erased monomorphisation problem, because every value travels with its
tag at runtime, but there is a fresh problem of choosing one canonical BEAM
representation per Mochi type so that the compiler, the runtime helpers, and
any hand-written Erlang FFI module all agree on what a Mochi value looks
like in memory.

This note fixes those choices. It is the contract that every later phase of
MEP-46 (lower, emit, runtime, FFI) is allowed to assume.

## 1. Design principles

Three principles drive every decision below.

**P1, Prefer the native BEAM representation.** When the obvious BEAM term
already carries Mochi semantics correctly, use it. Lists become cons-cells,
ints become integers, atoms become atoms. The transpiler is not in the
business of inventing new wrapper layers; those waste both heap words and
BIF affinity. A `list<int>` in Mochi is just `[1, 2, 3]` in Erlang, full
stop, with no tag, no version field, no module dispatch.

**P2, One representation per Mochi type, everywhere.** A `list<int>` and a
`list<string>` share a representation. A record `Point { x: int; y: int }`
is always a tagged tuple `{point, X, Y}`, never sometimes a map. Hot paths
and FFI boundaries do not get to pick alternate forms, because that would
force every consumer to branch. If a user wants a map view of a record (for
serialisation, for example), that conversion is an explicit BIF call, not
an implicit lowering.

**P3, Push as much as possible onto BEAM's pattern matcher.** BEAM compiles
`case` expressions into decision trees in the kernel-compile pass
(`v3_kernel`, `beam_match_state`). Mochi `match`, destructuring `let`, and
function-head pattern matching should all lower to vanilla Erlang clauses
so that we inherit that decision-tree compiler for free. The job of the
Mochi front end is restricted to exhaustiveness checking (which the static
type system already does) and to elaborating sugar (option, sum-tag
conventions) into Erlang's pattern surface.

These principles produce a few non-obvious recommendations later, notably:
tagged tuples beat maps for records, sorted-key omap is a distinct type
from map, and time is an integer not a tuple. The rationale for each is in
the corresponding section.

## 2. Scalar types

### 2.1 `int`

**Lowering:** BEAM integer. No wrapper.

Mochi documents `int` as 64-bit signed. BEAM integers are arbitrary
precision, internally split between small ints (60-bit signed on 64-bit
BEAM, fitting in a single tagged word) and bignums (3 or more heap words).
For the values Mochi programs care about, almost every integer fits in a
small int, and arithmetic on small ints is one immediate-tagged machine op.
The transpiler emits Erlang `+`, `-`, `*`, `div`, `rem` directly and does
not insert range guards.

This produces one user-visible difference: Mochi on the BG backend wraps at
2^63, BEAM widens to a bignum. We accept the divergence and document it. A
future `--strict-int` flag could insert
`Result =:= (Result band 16#FFFFFFFFFFFFFFFF)` guards on every arithmetic
op, but that triples the cost of every add and is not the default.

### 2.2 `float`

**Lowering:** BEAM float (IEEE 754 double, heap-boxed in 3 words on 64-bit
BEAM).

The painful corner is that BEAM raises `badarith` on any operation that
would produce `Inf` or `NaN`. `1.0 / 0.0`, `0.0 / 0.0`, `math:log(0.0)`,
`math:sqrt(-1.0)` all throw. Mochi's other backends propagate `NaN` and
`Inf` per IEEE 754. Three options:

1. **Pre-check every float op.** Insert guards that handle the boundary
   cases manually. Heavy and pervasive.
2. **Wrap every float op in `try`.** Convert `badarith` into Mochi's
   NaN/Inf sentinel. Even slower, and loses tail-call optimisation.
3. **Document the divergence, supply a `mochi_float` helper module** that
   exposes `safe_div/2`, `safe_log/1`, etc. for users who want IEEE
   semantics.

We adopt option 3. The compiler emits raw `+`, `-`, `*`, `/`. The
`mochi_float` runtime module provides the IEEE-conformant variants and is
what `math:nan()`, `math:inf()`, and any explicit `float.div_safe` user
code lowers to. This keeps the hot path on raw BIFs while giving an opt-in
escape hatch.

### 2.3 `bool`

**Lowering:** atoms `true` and `false`.

This is the universal BEAM convention. `true` and `false` are not a
distinct primitive type in Erlang; they are atoms that happen to be
honoured by every guard and short-circuit operator. Mochi `&&`, `||`, `!`,
and every comparison naturally lower to `andalso`, `orelse`, `not`, and the
comparison operators, all of which return these two atoms.

### 2.4 `string`

**Lowering:** UTF-8 binary.

This is the choice with the most ecosystem friction, so the rationale is
worth spelling out.

Erlang has two historical string representations: a list of code points
(the original) and a UTF-8 binary (modern, recommended since OTP 17 and
the redesigned `string` module). For Mochi the binary wins for three
reasons:

- **Memory.** A 10-character ASCII string is 10 bytes in a binary; the
  same as a list is 20 words (40 bytes on 64-bit) of cons cells plus the
  heap header.
- **Interop.** The `string` module, `binary` module, all modern web/JSON
  libraries, and Elixir all assume UTF-8 binaries.
- **Hashing/equality.** Binaries support O(1) literal sharing via the
  literal area, and equality is a `memcmp`.

The cost is that a binary indexed by byte does not match Mochi's documented
"code point" semantics. We handle this in §7.

### 2.5 `time`

**Lowering:** BEAM integer, nanoseconds since the Unix epoch.

Two reasonable representations exist on BEAM:

- An integer in some unit (`erlang:system_time(nanosecond)` style).
- A tuple `{MegaSec, Sec, Micro}` (the legacy `erlang:timestamp()` shape).

We choose nanoseconds-since-epoch as a plain integer. Reasons: arithmetic
is one BIF call (`+`, `-`), conversion to and from
`erlang:system_time(nanosecond)` is a no-op, and the integer fits in a
small int well past the year 2200 on 64-bit BEAM (2^59 ns is roughly the
year 2038, 2^62 ns is roughly the year 2116, beyond that a bignum cost
kicks in once, not per op).

Conversion to calendar form is a runtime helper: `mochi_time:to_calendar/1`
calls `calendar:system_time_to_universal_time(Ns, nanosecond)` and packs
the result. Conversion the other way is `mochi_time:from_calendar/1`.
ISO-8601 in/out is one call further.

### 2.6 `duration`

**Lowering:** BEAM integer, nanoseconds.

Same representation as `time`, distinct nominal type. `t1 - t0` returns a
`duration`; `t + d` returns a `time`. The type system enforces the
distinction; the runtime cannot, because both are just integers. This is
fine: we have already paid for the type check at compile time.

## 3. Composite types

### 3.1 `list<T>`

**Lowering:** BEAM proper list (cons cells terminating in `[]`).

The reasons: pattern matching, `[H|T]` head/tail splits, the entire `lists`
module, list comprehensions, and BEAM's intrinsic familiarity with the
cons-cell shape (one word for the cons header, one word for the head, one
word for the tail pointer; tail-shared structures are cheap).

The cost: random access `xs[i]` is `lists:nth(I+1, Xs)`, which is O(n).
`length(xs)` is `length/1`, also O(n). Append is O(len of left operand).
Mochi has historically pretended `list<T>` is a vector. On BEAM it really
is not.

We accept this for the default. The compiler emits `lists:nth/2`,
`length/1`, `lists:reverse/1`, list comprehensions, etc. Code paths that
need O(log n) random access can opt into `array<T>`, a separate Mochi type
that lowers to OTP's `array` module (functional tree, O(log32 n) get and
set). The transpiler does not auto-promote; we want users to make that
decision explicitly.

For sequential iteration, lists win on BEAM by a wide margin and Mochi's
`for x in xs` loop lowers to either `lists:foreach/2` (for side-effect
loops) or a `lists:foldl/3` (for accumulating loops) directly.

### 3.2 `map<K, V>`

**Lowering:** BEAM map.

BEAM maps have two internal representations, switching at 32 entries:

- ≤32: flat map. Two parallel tuples, one of sorted keys, one of values.
  O(n) lookup but with a tiny constant; for small maps this is faster than
  a hash table.
- >32: HAMT (hash array mapped trie, Phil Bagwell's design, the same data
  structure Clojure and Scala use). O(log32 n) get and put, with structural
  sharing on update.

The transition is invisible to the user; both shapes answer the same BIFs.
Atom keys are the most efficient (single-word tagged value, no hashing
required for small maps because BEAM compares keys directly), then small
integers, then everything else. Mochi's `map<atom, V>` (or `map<string, V>`
where the string set is small and known) should be encouraged when
possible.

Iteration order is **not** insertion order. For flat maps it is sorted key
order. For HAMT it is hash order. Mochi's documented `for k, v in m` says
iteration order is unspecified, and this matches BEAM's behaviour. Programs
that need stable iteration must use `omap<K, V>`.

`m[k]` lowers to `maps:get(K, M)`, `k in m` to `maps:is_key(K, M)`,
`m[k] = v` to `maps:put(K, V, M)` (which returns a new map; Mochi `map` is
conceptually immutable from the runtime's point of view).

### 3.3 `omap<K, V>` (insertion-ordered map)

**Lowering:** tuple `{KeyList, MapValues}` where `KeyList :: [K]` is the
list of keys in insertion order and `MapValues :: #{K => V}` is the value
store.

Mochi exposes `omap` separately for query DSLs and JSON-like serialisation
where iteration order matters. Inserting a fresh key appends to `KeyList`;
overwriting an existing key only updates `MapValues`. Iteration walks
`KeyList`. Lookup goes through `MapValues`. This is two words of overhead
per `omap` plus one cons cell per key.

### 3.4 `set<T>`

**Lowering:** OTP `sets` module (version 2, the default on OTP 24+).

OTP's `sets` module historically held an opaque tree representation. Since
OTP 24 it ships a v2 representation that is map-backed (the set is
essentially a `#{Element => []}`), and on OTP 28+ v2 is the default. The
runtime characteristics piggyback on BEAM maps: small-set flat
representation, large-set HAMT, structural sharing on insert.

Mochi's `set<T>` lowers via `sets:new()`, `sets:add_element/2`,
`sets:is_element/2`, `sets:union/2`, `sets:intersection/2`,
`sets:subtract/2`. One subtlety: v2 sets use `=:=` for membership, so `1`
and `1.0` are different elements (which matches Mochi semantics; the type
system forbids mixing them in a `set<T>` anyway).

We deliberately do not lower to a raw `map<T, []>` (even though that is
what v2 is internally), because we want to keep the door open for OTP
improvements to the `sets` API without changing the emit pass.

### 3.5 `option<T>` (`?T`)

**Lowering:** tagged tuple `{some, V}` or atom `none`.

The two common BEAM idioms are:

- Erlang convention: `V | undefined`. Concise, but ambiguous (`undefined`
  is a valid value of many types) and asymmetric with `result<T, E>`.
- Tagged tuple: `{some, V} | none`. Two heap words for the `some` case,
  one tagged atom for the `none` case.

We pick the tagged-tuple variant. The reasons:

- It composes with `result<T, E>` (`{ok, V}` / `{error, E}`), the other
  Mochi sum that has an established BEAM convention.
- Pattern matching is unambiguous: `{some, X}` cannot be confused with a
  record (records are tagged with a module-named atom, never `some`).
- The wrapper is a one-element tuple plus a tag, which is the natural
  sum-type representation we use everywhere else.

The cost is two heap words per `Some`. This is acceptable; option is not a
hot-path memory consumer in practice.

### 3.6 Records

For a Mochi declaration

```mochi
record Point { x: int; y: int }
```

**Lowering:** tagged tuple `{point, X, Y}`.

This is the Erlang `-record` convention. The first element is an atom
equal to the lowercased record name. Field order is fixed at compile time.
Access `p.x` lowers to `element(2, P)` (which BEAM optimises to a direct
word load when the static type is known). Update `p with x: 7` lowers to
`setelement(2, P, 7)` or, when multiple fields change, to a direct tuple
constructor.

Alternative representations considered and rejected:

- **Map** (`#{x => X, y => Y}`). Loses the compile-time field position,
  costs at minimum five words plus the key/value pairs. For record-heavy
  code (datalog facts, AST nodes, query rows) the difference compounds
  quickly. Maps also lose the cheap discriminator that the tag atom
  provides for sum types.
- **Elixir-style struct** (`#{__struct__ => point, x => X, y => Y}`). Map
  plus extra key, no benefit on BEAM.
- **OTP 29 native records.** Promising but still experimental. Once stable
  we can switch the lowering with no source change.

At the FFI boundary, if an Erlang library expects a map, the compiler
inserts an explicit `mochi_record:to_map/1` call. The default
representation stays tuple.

For records appearing as variants of a sum type, the same tag-leading-tuple
convention nests cleanly: a variant `Add(int, int)` of `type Expr` is
`{add, A, B}`, distinguishable from `Mul(int, int)` `{mul, A, B}` by the
tag, and matched on by `case` in O(1) via the BEAM jump-table.

### 3.7 Sum types

For

```mochi
type Tree = Leaf | Node(int, Tree, Tree)
```

**Lowering:** atom for nullary constructors, tagged tuple for n-ary.
`Leaf` becomes `leaf`. `Node(v, l, r)` becomes `{node, V, L, R}`.

This is symmetric with records and with option. The tag is always an atom,
always lowercase, always equal to the source constructor name lowercased.
Pattern matching on `case T of leaf -> ...; {node, V, L, R} -> ... end`
falls into BEAM's jump-table optimisation.

We do not box nullary constructors in a one-tuple. `leaf` is the bare
atom; there is no `{leaf}`. This costs nothing extra and reads naturally
in Erlang.

### 3.8 `fun(A, B): C`

**Lowering:** BEAM fun (closure).

BEAM funs are first-class, capture free variables by value, and live on
the heap as a 9-13 word object plus the captured environment. Mochi
closures lower 1:1: a lambda `\x, y -> x + y` becomes
`fun(X, Y) -> X + Y end`. A closure over a free variable `n` becomes
`fun(X) -> X + N end` where `N` is the captured value at closure
construction.

Identity equality on funs is brittle on BEAM (two funs are `=:=` only if
they were produced at the same construction site with the same captured
environment). Mochi does not expose fun equality, so this corner does not
bite us.

Function values that name a top-level function lower to the more efficient
`fun mod:fname/Arity` (an "external fun"), which is an 8-byte handle and
not a heap closure.

### 3.9 `stream<T>` and `agent`

**Lowering:** PID (process identifier).

Streams and agents are inherently process-shaped in Mochi semantics. They
lower to OTP gen-server-like processes. The Mochi runtime ships
`mochi_stream` and `mochi_agent` behaviours; user-defined streams compile
to modules that implement the behaviour. A `stream<int>` value at runtime
is a PID; the type-system tracks the element type and the runtime does
not.

For streams that need subscription handles distinct from the producer
PID, we use a pair `{Pid, Ref}` where `Ref` is `make_ref()`. The pair is
treated by Mochi as an opaque handle; the runtime helpers know how to
destructure it.

## 4. Naming and mangling

### 4.1 Variable mangling

Erlang requires variables to start with an uppercase letter (or `_`).
Mochi variables are lowercase. The transpiler mangles by prepending `V_`
for ordinary variables and `_V_` for unused or `_`-prefixed Mochi
variables:

| Mochi | Erlang |
|-------|--------|
| `x` | `V_x` |
| `total_count` | `V_total_count` |
| `_unused` | `_V_unused` |
| `result` | `V_result` |
| `T` (type var at value site, rare) | `V_T` |

Special cases:

- Loop variables in comprehensions follow the same rule.
- Pattern variables in `match` arms follow the same rule.
- Wildcard `_` lowers to Erlang `_`.

This is verbose but unambiguous and survives any future addition of
reserved Erlang words at the value level. The prefix `V_` is never emitted
by Mochi for compiler-generated names; compiler-generated temporaries use
`T_` (e.g., `T_0`, `T_1`).

### 4.2 Module mangling

Erlang modules are atoms. Mochi packages have dotted paths. Three
conventions exist:

- Dotted atom: `'mochi.foo.bar'`. Legal Erlang, but requires single-quoting
  everywhere and is awkward in tools that grep on module names.
- Underscore: `mochi_foo_bar`. Plain atom, no quoting, but loses the
  package boundary visually.
- Hierarchical (no prefix): `foo_bar`. Same as underscore, drops the
  `mochi.` prefix.

We adopt **`mochi_foo_bar`**. The `mochi_` prefix prevents collisions with
OTP and user Erlang code, the underscores keep it a bare atom, and the
result is searchable. Compiler-emitted runtime helpers all live under
`mochi_`-prefixed modules (`mochi_str`, `mochi_list`, `mochi_map`,
`mochi_time`, `mochi_float`, `mochi_record`, `mochi_stream`,
`mochi_agent`).

User packages map deterministically: `foo/bar/baz` → `mochi_foo_bar_baz`.
Dots in package names (rare) become further underscores.

### 4.3 Function-name mangling

Mochi function names are already valid Erlang atoms (lowercase identifier).
They pass through unchanged. Overloaded names are not a problem because
Mochi rejects them at type-check time.

The one collision risk is Erlang reserved words used as function names
(`if`, `case`, `receive`, `try`, `catch`, `end`, `of`, `when`, `fun`,
`cond`, `let`, `query`, `do`, `or`, `xor`, `not`, `and`, `andalso`,
`orelse`, `band`, `bor`, `bxor`, `bnot`, `bsl`, `bsr`, `div`, `rem`).
Mochi does not reserve all of these. When a Mochi function name collides,
we suffix it with `_`: `if` → `if_`, `let` → `let_`.

## 5. Atom safety

BEAM atoms are not garbage-collected. The atom table has a hard upper bound
(1,048,576 by default, configurable via the emulator `+t` flag). Once an
atom enters the table it stays for the lifetime of the VM. If the table
fills, the VM crashes.

This forces a strict rule: **the Mochi → BEAM compiler never produces atoms
from user data**. All atoms emitted into compiled BEAM modules are one of:

- Tag atoms for known sum-type constructors (`leaf`, `node`, `some`,
  `none`, `ok`, `error`, plus record-name tags).
- Module names from the compiled program.
- Function names from the compiled program.
- A small fixed set of runtime atoms (`true`, `false`, `undefined`,
  `infinity`).

User strings stay as binaries. JSON parsing produces binary keys
(`mochi_json:decode/1` returns `#{<<"key">> => Value}`, never
`#{key => Value}`). When interop with `jsx`/`jiffy` is needed and those
libraries default to atom keys, the runtime wrapper forces
`{labels, binary}`.

The compiler also flags any user call to a hypothetical `to_atom(string)`
BIF as an error. If a Mochi program genuinely needs runtime atom interning,
the user must call `binary_to_existing_atom/2` (which throws `badarg`
rather than allocating) via the explicit FFI.

## 6. Pattern matching

Mochi `match` lowers to Erlang `case`. The BEAM kernel-compile pass
(`v3_kernel` followed by `beam_match_state` lowering) compiles a `case`
into a decision tree: each test compiles to a single guard, identical
prefixes are shared, the order of clauses determines fallback. This is
exactly what Mochi `match` semantics require.

Three jobs remain for the Mochi front end:

1. **Exhaustiveness.** Mochi's type checker already proves that every
   constructor of a sum is covered. The transpiler emits a trailing
   `_ -> erlang:error({mochi_unreachable, ?MODULE, ?LINE})` clause as a
   defence in depth.
2. **Tag normalisation.** Variant patterns in source (`Node(v, l, r)`)
   get rewritten to tagged-tuple patterns (`{node, V, L, R}`) so the
   BEAM matcher sees the canonical shape.
3. **Guard lowering.** Mochi `match` arms with `if` guards lower to
   Erlang `when` guards, restricted to the subset BEAM accepts (no user
   calls, only BIF guards: comparisons, type tests, arithmetic). When a
   Mochi guard exceeds that subset, the front end nests it as an `if`
   inside the arm body rather than as a `when`.

Destructuring `let` lowers to a `case` with one arm; the BEAM compiler
turns that back into a single match instruction.

## 7. Strings

`len(s)` is the question that decides everything else. Three reasonable
answers exist:

- **`byte_size(S)`**, counts bytes. O(1). Wrong for any non-ASCII string.
- **`string:length(S)`**, counts extended grapheme clusters per Unicode
  14. O(n). Matches end-user expectation (`"é"` is one character).
- **`length(unicode:characters_to_list(S))`**, counts code points. O(n).
  Matches Mochi's documented semantics of "code-point iteration".

Mochi documents `len(string)` as a code-point count. We expose:

- `len(s)` → `length(unicode:characters_to_list(S))`, code points.
- `s.byte_len()` → `byte_size(S)`, raw bytes.
- `s.grapheme_len()` → `string:length(S)`, grapheme clusters.

`s[i]` (single-code-point indexing) lowers to a helper
`mochi_str:char_at(S, I)` that walks code points and returns a one-grapheme
binary (using `string:slice/3`). The cost is O(i); we document that.

Concatenation `s + t` lowers to a binary construction
`<<S/binary, T/binary>>`. A chain `a + b + c` lowers as
`<<A/binary, B/binary, C/binary>>` in one allocation.

Iteration `for ch in s` lowers to a recursive helper around
`string:next_grapheme/1`:

```erlang
mochi_str:foreach(F, S) ->
    case string:next_grapheme(S) of
        [] -> ok;
        [G | Rest] -> F(unicode:characters_to_binary([G])), mochi_str:foreach(F, Rest)
    end.
```

Equality `s == t` lowers to `S =:= T`. Two UTF-8 binaries with identical
bytes are `=:=`; this matches Mochi semantics because both backends agree
on UTF-8 as the canonical encoding.

## 8. Lists

Default complexity must be known by users:

| Operation | Source | Erlang | Complexity |
|-----------|--------|--------|------------|
| `xs[0]` | head | `hd(Xs)` | O(1) |
| `xs[i]`, i > 0 | nth | `lists:nth(I+1, Xs)` | O(i) |
| `len(xs)` | length | `length(Xs)` | O(n) |
| `xs + ys` | append | `Xs ++ Ys` | O(len(Xs)) |
| `[v] + xs` | prepend | `[V | Xs]` | O(1) |
| `xs.reverse()` | reverse | `lists:reverse(Xs)` | O(n) |
| `for x in xs` | iter | list comprehension or `lists:foreach` | O(n) |

Programs that genuinely need O(log n) random access opt into `array<T>`
(§3.1). The compiler does not promote silently.

A future optimisation pass (not in MEP-46 scope) may detect read-only
random access on a `list<T>` known at compile time to be large and rewrite
it to an `array<T>` literal; that's a v2 concern.

## 9. Maps and omaps

`m[k]` → `maps:get(K, M)`. Throws `{badkey, K}` if absent; Mochi front end
either inserts a `maps:is_key` check first (if the type is `map<K, V>` with
`K` not guaranteed present) or trusts the type (if it is `omap<K, V>`
indexed within a known-present block).

`m[k] = v` → `M#{K => V}` for inline update, `maps:put(K, V, M)` for
dynamic K. Both BIFs allocate a new map sharing structure with the old
one.

`k in m` → `maps:is_key(K, M)`. Guard-safe; usable inside `when` clauses.

`for k, v in m` → `maps:fold/3` for accumulating loops, `maps:foreach/2`
for side-effecting loops. Iteration order undefined (§3.2).

`m.keys()` → `maps:keys(M)`. Order undefined.

`omap` operations route through a small wrapper. `om[k]` is
`maps:get(K, element(2, OM))`. `om[k] = v` checks whether `K` is already
in the value map; if yes it just updates the value side, if no it appends
to the key list and inserts into the value side. Iteration walks
`element(1, OM)` and indexes the value side; order is insertion order.

## 10. Tuple vs map: when to deviate from "always tuple for records"

The default is tagged tuple. Two situations push toward maps:

- **FFI boundary with library that expects maps.** Insert one conversion
  at the boundary.
- **Records with very many fields, most unset.** Rare in Mochi; if it
  becomes common, revisit.

For all other cases, datalog rows, AST nodes, query results, common
configuration records, tuples win on every dimension: less memory (1 tag
word + N value words vs 5+ words flat-map overhead + key/value words),
faster access (one `element/2`, optimised to a load when the tag and index
are known statically), faster construction (one BIF, no rehashing), and
better Dialyzer inference.

Benchmark estimates on OTP 27 for a record with three fields (typical):

| Representation | Memory (64-bit, words) | Access cost | Update cost |
|---|---|---|---|
| `{point, X, Y, Z}` | 1 (header) + 1 (tag) + 3 (fields) = 5 | `element/2`, ~1 ns | `setelement/3`, ~3 ns |
| `#{x => X, y => Y, z => Z}` | 5 (header) + 4 (key tuple) + 3 (values) = 12 | `maps:get/2`, ~5 ns | `maps:put/3`, ~15 ns |

The factor on memory is 2.4×; on update, ~5×. For a query that
materialises a million rows the difference is real.

## 11. Equality

Mochi `==` is structural equality on all types. BEAM offers two: `==`
(numeric coercion, so `1 == 1.0` is true) and `=:=` (no coercion).

We emit `=:=` everywhere. Mochi's type checker rejects mixed-numeric
comparisons at compile time, so the coercion behaviour of `==` is
unreachable; using `=:=` removes any temptation for the runtime to do a
float/int unification we did not authorise.

`<`, `>`, `=<`, `>=` follow BEAM's standard term order, which is
consistent with Mochi's order on numbers, strings, lists, and tuples. The
corner is that BEAM's term order is total (it compares across types:
number < atom < reference < fun < port < pid < tuple < map < nil < list
< bitstring). Mochi never asks for cross-type ordering; the type system
rules it out. So the corner is unreachable.

## 12. FFI marshalling

When Mochi code calls Erlang code (`import erlang "lists" as lists`),
values cross the boundary unchanged because the Mochi representation is
already the BEAM representation. The marshalling table is short:

| Mochi type | Crossing to Erlang | Crossing back |
|---|---|---|
| `int`, `float`, `bool` | identity | identity |
| `string` | UTF-8 binary | UTF-8 binary, validated by `unicode:characters_to_binary/1` if origin uncertain |
| `time`, `duration` | integer ns | integer ns |
| `list<T>` | proper list | proper list |
| `map<K, V>` | map | map |
| `set<T>` | `sets` v2 opaque | `sets` v2 opaque |
| `option<T>` | `{some, V} | none` | same |
| record | tagged tuple | tagged tuple, tag validated |
| sum variant | tagged tuple | tag validated against type |
| fun | BEAM fun | BEAM fun |
| `stream<T>`, agent | PID (or `{Pid, Ref}`) | same |

The only non-identity step is record/variant tag validation on the way
back. The compiler emits a runtime check at the boundary; if the tag is
wrong, it throws `{mochi_ffi_type, ExpectedTag, Got}`. Inside Mochi-only
code the check is omitted.

For interop with Erlang code that uses the `undefined` convention for
absent values, the Mochi `option` type provides `?from_undefined/1` and
`?to_undefined/1` in the standard library.

## 13. Dialyzer integration

Every Mochi function lowers with a `-spec`. Examples:

```mochi
fun add(x: int, y: int): int = x + y
```

lowers to

```erlang
-spec add(integer(), integer()) -> integer().
add(V_x, V_y) -> V_x + V_y.
```

Sum types map to `-type` declarations:

```mochi
type Tree = Leaf | Node(int, Tree, Tree)
```

lowers to

```erlang
-type tree() :: leaf | {node, integer(), tree(), tree()}.
```

Records map to type aliases over tuple types:

```mochi
record Point { x: int; y: int }
```

lowers to

```erlang
-type point() :: {point, integer(), integer()}.
```

Generic functions emit polymorphic specs with type variables:

```mochi
fun id<T>(x: T): T = x
```

lowers to

```erlang
-spec id(X) -> X.
id(V_x) -> V_x.
```

Once specs are emitted, Dialyzer can run on the compiled BEAM output and
find any remaining type errors that slipped through Mochi's checker, plus
any errors in hand-written Erlang FFI modules that interact with Mochi
types. We treat Dialyzer green as a release gate.

## 14. The boxed `mochi_value` term

For FFI calls into Erlang code that wants to operate on Mochi values
polymorphically (a serialiser, a debugger, an interpreter for embedded
queries), we provide a discriminated wrap/unwrap helper:

```erlang
-type mochi_value() ::
    {int, integer()}
  | {float, float()}
  | {bool, boolean()}
  | {string, binary()}
  | {time, integer()}
  | {duration, integer()}
  | {list, [mochi_value()]}
  | {map, #{mochi_value() => mochi_value()}}
  | {set, sets:set(mochi_value())}
  | {option, none | {some, mochi_value()}}
  | {record, atom(), [mochi_value()]}
  | {variant, atom(), [mochi_value()]}
  | {fun_, function()}
  | {pid, pid()}.
```

`mochi_value:wrap/2` takes a Mochi type witness and a raw value, returns
the boxed term. `mochi_value:unwrap/1` strips it. The wrap helper is only
emitted at explicit user request (`introspect(v)` in source, or at the
boundary of a generic serialiser); the default lowering never boxes.

## 15. Process and concurrency primitives

Mochi `spawn`, `send`, `receive`, `actor` map directly to BEAM `spawn`,
`!`, `receive`. The type system tracks message types per actor; the
runtime does not. Misdirected messages do not raise at runtime (BEAM has
no static mailbox typing); they simply remain in the mailbox unmatched.
The Mochi compiler emits a catch-all
`_ -> erlang:error({mochi_unexpected_message, ?MODULE, Msg})` clause at
the end of every `receive` to surface misrouted messages instead of
silently buffering them.

## 16. Lowering table, consolidated reference

| Mochi type / construct | BEAM representation | Typical BIFs |
|---|---|---|
| `int` | integer (small → bignum) | `+`, `-`, `*`, `div`, `rem` |
| `float` | float (3-word boxed) | `+`, `-`, `*`, `/`, `math:*` |
| `bool` | atom `true` / `false` | `andalso`, `orelse`, `not` |
| `string` | UTF-8 binary | `<<X/binary, Y/binary>>`, `byte_size/1`, `string:length/1` |
| `time` | integer (ns since epoch) | `erlang:system_time/1`, `mochi_time:to_calendar/1` |
| `duration` | integer (ns) | `+`, `-` |
| `list<T>` | proper list | `[H|T]`, `lists:*`, list comprehensions |
| `array<T>` | OTP `array` | `array:get/2`, `array:set/3` |
| `map<K, V>` | BEAM map | `maps:get/2`, `maps:put/3`, `maps:is_key/2` |
| `omap<K, V>` | `{[K], #{K => V}}` | runtime helper `mochi_omap:*` |
| `set<T>` | OTP `sets` v2 | `sets:add_element/2`, `sets:is_element/2` |
| `option<T>` | `{some, V} | none` | pattern match |
| record `R` | `{r, F1, ..., Fn}` | `element/2`, `setelement/3` |
| sum variant `C(args)` | `{c, Arg1, ...}` (atom if 0 args) | pattern match |
| `fun(A, B): C` | BEAM fun | direct application, `fun mod:f/2` |
| `stream<T>` | PID (or `{Pid, Ref}`) | `mochi_stream:*` |
| agent | PID | `mochi_agent:*` |
| `==` | `=:=` | guard-safe |
| ordering | BEAM term order | `<`, `=<`, `>`, `>=` |
| variable `name` | `V_name` | — |
| variable `_unused` | `_V_unused` | — |
| package `foo/bar` | atom `mochi_foo_bar` | — |
| function `name` | atom `name` (suffix `_` on Erlang keyword collision) | — |
| `match` | Erlang `case` | BEAM decision tree |
| destructuring `let` | single-arm `case` | — |
| sum `-type` | `-type variant() :: tag1 | {tag2, ...} | ...` | Dialyzer |
| record `-type` | `-type r() :: {r, T1, ..., Tn}` | Dialyzer |
| function `-spec` | `-spec f(T1, T2) -> T3` | Dialyzer |
| FFI atom from user data | **forbidden** | — |
| FFI Mochi → Erlang | identity (binary/tuple/map shapes unchanged) | optional tag validation on return |

This table is the source of truth. Anything not listed here is undefined
for the BEAM backend and must be added to this section before being
emitted.
