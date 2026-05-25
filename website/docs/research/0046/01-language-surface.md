# MEP-46 research note 01, Mochi language surface (Erlang/BEAM target)

Author: research pass for MEP-46 (Mochi → Erlang/BEAM transpiler).
Date: 2026-05-22 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`–`v0.7`, normative security
specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`, and
the companion MEP-45 note 01 (whose section structure this note deliberately
mirrors so the two backends can be diffed line-for-line).

This note records the user-visible language surface that the Erlang/BEAM
target must faithfully reproduce. It is deliberately written *from the spec
downward* and ignores the existing Go runtime, the vm3 bytecode, the C target
under MEP-45, and any other backend implementation. The goal is a transpiler
design that would be correct against the *language*, not against the present
implementation.

The surface decomposes into the same eight orthogonal sub-languages identified
in MEP-45 note 01: (1) the value core, (2) the function and method core, (3)
the collection core, (4) the algebraic-data-type core, (5) the query DSL, (6)
the stream / agent core, (7) the logic-programming core, and (8) the AI / FFI
shells. Each section below names every form a Mochi program can write, then
states a *lowering obligation* the BEAM backend must honour.

Where MEP-45 maps Mochi types to C struct + helper-function pairs, this note
maps them to BEAM terms (atoms, tuples, maps, binaries, funs, PIDs). The
target IR is Core Erlang (see note 05); the runtime is OTP plus a thin
`mochi_*` application (see note 04). Throughout, "BEAM" means the canonical
Ericsson reference implementation, OTP 27+ era. AtomVM (the embedded BEAM
re-implementation for microcontrollers) is a Phase-2 secondary target and is
called out where it constrains a choice.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time** error,
  not a runtime panic. BEAM is single-assignment by construction, so `let`
  lowers to a fresh Erlang variable with no extra work, modulo Mochi → Erlang
  name mangling (see §1.6).
- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope.

BEAM's single-assignment discipline means `var` is the interesting case. The
backend must rewrite a Mochi `var x` that is re-assigned N times into N+1
distinct Erlang variables (`X0`, `X1`, … `Xn`) via SSA-style numbering, with
each control-flow merge point getting a phi-equivalent (Erlang's `case` arms
binding to the same final variable). This is identical to the lowering most
imperative-to-Erlang shells use (Erlando, the Joxa frontend); see note 05 §4.

A binding may carry an explicit type: `let x: int = 0`. BEAM is dynamically
typed, so types do not survive into the emitted code. They survive into
`-spec` declarations (Dialyzer-readable) emitted per top-level function. See
note 06 §15.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

The list pattern lowers to an Erlang head/tail `case` since `list<T>` ⇒ proper
cons list (see note 06 §1). The map pattern lowers to an Erlang map pattern
`#{<<"name">> := N, <<"age">> := Age}` (with the binary key form because
Mochi strings ⇒ binaries; the `:=` is the "this key must exist" matcher).
Both bind fresh names; both are immutable.

Scoping is lexical and block-based. Inner blocks shadow outer bindings.
Erlang has no block-scoped variables (every variable lives until the end of
its function clause); the backend must rename shadowed names. See note 05 §4.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the BEAM-side representation:

| Mochi | Width / semantics | BEAM lowering |
|-------|------------------|---------------|
| `int` | 64-bit signed integer (inferred from integer literals) | BEAM integer (small int up to 60-bit, then bignum; native machine-word op on small; arbitrary precision on overflow) |
| `float` | 64-bit IEEE 754 double | BEAM float (always a boxed 8-byte heap object) |
| `bool` | `true` / `false` | atoms `true` / `false` (the BEAM idiom; reserved) |
| `string` | UTF-8 text, indexable as code points, immutable | UTF-8 binary `<<...>>` (the modern OTP idiom; legacy "string as list of codepoints" is rejected, see §1.4) |
| `time` | absolute timestamp (used by streams) | BEAM integer (ns since Unix epoch UTC; matches `erlang:system_time(nanosecond)`) |
| `duration` | time interval (`std/time` API) | BEAM integer (ns) |
| `image` (preview) | binary blob (`load "cat.png"`) | BEAM binary |

Implicit numeric conversions are **not** allowed (per the type-checker
discipline implied by MEP-4/5/6 referenced from the threat model). `int +
float` is a type error; the program must `float(x)` first. On BEAM this is
load-bearing: BEAM `==` coerces `1 == 1.0` to true, BEAM `=:=` does not. The
backend emits `=:=` and `=/=` exclusively to match Mochi's strict semantics
(see note 06 §12).

BEAM's `+` is polymorphic and accepts `int+int`, `int+float`, `float+float`.
Mochi forbids the mixed case at the type layer, so the emitted code never
exercises it, but the runtime still works if it ever does.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean `&& || !`;
membership `in`; string concatenation overloads `+`.

| Mochi | Erlang |
|-------|--------|
| `a + b` (int) | `A + B` |
| `a + b` (float) | `A + B` |
| `a + b` (string) | `<<A/binary, B/binary>>` |
| `a + b` (list) | `A ++ B` (note: O(length(A))) |
| `a - b` | `A - B` |
| `a * b` | `A * B` |
| `a / b` (float) | `A / B` |
| `a / b` (int) | `A div B` (Mochi's `/` on ints is truncating; document) |
| `a % b` | `A rem B` |
| `a == b` | `A =:= B` |
| `a != b` | `A =/= B` |
| `a < b` | `A < B` (works on any term; for Mochi's strict typing the operands are always the same scalar) |
| `a && b` | `A andalso B` (short-circuit) |
| `a \|\| b` | `A orelse B` |
| `!a` | `not A` |
| `x in xs` (list) | `lists:member(X, Xs)` |
| `x in m` (map) | `maps:is_key(X, M)` |
| `x in s` (set) | `maps:is_key(X, S)` (sets are maps with `[]` values, see note 06 §1) |

Integer overflow is silent on BEAM (small → bignum), unlike Mochi's documented
64-bit `int`. The backend exposes a `--strict-int` flag that wraps every
arithmetic op in a guard checking `is_integer(R) andalso R < 1 bsl 63`. Off by
default, on for security-sensitive builds. See note 06 §5.

### 1.4 Strings as read-only character sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a `char`). Iteration yields
1-character strings in **code-point** order, not byte order. The BEAM lowering
must therefore use the `string` and `unicode` modules:

- Mochi strings are UTF-8 **binaries** (not Erlang's legacy "string = list of
  integer codepoints" representation, which is space-inefficient and never
  what modern OTP code uses; OTP 20's `string` module redesign endorsed
  binaries as the canonical form).
- `text[1]` lowers to `string:slice(Text, 1, 1)`, which returns a binary
  containing the codepoint at index 1 (zero-based per the `string` module,
  matching Mochi's convention).
- `for ch in text` lowers to a recursive step using `string:next_grapheme/1`,
  which yields one grapheme cluster at a time (Mochi's "codepoint" semantic
  is actually grapheme-cluster in practice, since the existing vm3 uses
  Go's `[]rune`-style decoding; see note 06 §7 for the exactness discussion).
- `len(text)` lowers to `string:length/1` (counts grapheme clusters), not
  `byte_size/1` (counts bytes). The byte form is exposed as `byte_len` in the
  runtime for users who explicitly want it.

This is the area where the Erlang and C targets diverge most: in C we own a
`mochi_str` runtime; on BEAM we lean on the existing `string`/`unicode`
modules whose semantics are settled and documented.

### 1.5 Literals

Integer literals; floating literals (`3.14`); boolean; string with C-style
escapes; triple-quoted multi-line strings (`"""..."""`); list `[...]`;
map `{key: val, ...}`; set `{a, b, c}`; record constructor `T { field: val }`.

The set literal `{a, b, c}` is distinguished from the empty/map literal
`{}` by the absence of `:` after the first element. The grammar must keep
these unambiguous; the BEAM lowering picks the right constructor accordingly.

Lowering forms:

| Mochi | Erlang |
|-------|--------|
| `42` | `42` |
| `3.14` | `3.14` |
| `true` / `false` | `true` / `false` |
| `"hello"` | `<<"hello"/utf8>>` |
| `[1, 2, 3]` | `[1, 2, 3]` |
| `{"a": 1, "b": 2}` | `#{<<"a">> => 1, <<"b">> => 2}` |
| `{1, 2, 3}` (set) | `#{1 => [], 2 => [], 3 => []}` (sets v2 idiom) |
| `Book { title: "X", pages: 10 }` | `{book, <<"X">>, 10}` (tagged tuple per note 06 §1) |

### 1.6 Identifier mangling

Erlang variables MUST begin with an uppercase letter or `_`; Erlang atoms
(used as module names and function names) MUST begin with lowercase or be
quoted. Mochi identifiers follow neither convention strictly: a function
named `Hello` and a variable named `Hello` are both legal Mochi.

Mangling rules (full table in note 06 §2 and §3):

- Mochi variables `foo` ⇒ Erlang variable `Foo` (capitalise first letter);
  if the original starts uppercase, prefix with `V_` to disambiguate from a
  Mochi-uppercase name (`Foo` ⇒ `V_Foo`).
- Mochi local function references and method names ⇒ Erlang atoms in
  snake_case (`fooBar` ⇒ `'fooBar'` quoted, OR `foo_bar` normalised; we
  pick the **preserved** quoted form to keep error messages legible).
- Mochi package paths `mathutils/extra` ⇒ Erlang module atom
  `'mochi.mathutils.extra'` (dotted-atom convention, common in OTP code).
- Mochi record type names ⇒ Erlang tagged-tuple atoms in snake_case
  (`Book` ⇒ `book`; on collision with an Erlang BIF or reserved word, prefix
  with `mochi__`).
- Mochi sum-type variant constructors ⇒ atoms (`Leaf` ⇒ `leaf`; `Node` ⇒
  `node`).

The mangling is deterministic (note 05 §3) and reversible via the `#line`
analogue (Erlang's `-file/-line` attributes plus the abstract-format
annotations) so diagnostics can point back to Mochi source.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Lowers to an Erlang function clause:

```erlang
add(A, B) -> A + B.
```

with a `-spec add(integer(), integer()) -> integer().` declaration emitted
above for Dialyzer. The function head's pattern list is positional. Mochi
default-argument values (if any future spec adds them) lower to multiple
function clauses via the standard Erlang pattern.

Mochi `return` is the value of the last expression in the body, which is the
Erlang convention; no explicit `return` keyword is needed in Erlang. The
backend lifts `return e` to the tail position when possible, otherwise emits
a labelled jump construct (see note 05 §7 on the early-return lowering).

The docs warn there is **no implicit tail-call optimisation** in Mochi, but
BEAM does TCO unconditionally on calls in tail position. The emitted code
therefore gets TCO for free; the docs warning is about portability, not BEAM
behaviour.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Lower directly to BEAM funs (closures):

```erlang
Square = fun(X) -> X * X end.
apply(F, Value) -> F(Value).
```

BEAM funs are first-class, capture free variables automatically, and the JIT
handles indirect calls competently. There is no lowering work beyond name
mangling. Closures escape freely (BEAM does not distinguish heap vs stack
closures; everything lives in the per-process heap and is GC-traced).

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block are
unqualified. Lowering: every method becomes a top-level Erlang function
`circle_area(Self)` taking the record's tagged tuple as the first argument,
and field access desugars to a tuple-index lookup (`element(2, Self)` for a
record with one field).

For records-as-maps (the FFI-friendly alternative), field access lowers to
`maps:get(<<"radius">>, Self)`. The default is tagged tuples (faster); the
map form is opt-in per type via an `@as_map` annotation.

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end.

Lowers to a runtime call `mochi_io:print(List)` where `List` is the list of
arguments. The runtime walks the list, applies per-type formatting (via the
emitted formatter table, since BEAM has no compile-time reflection), inserts
single-space separators, and ends with `io:format("~n")`. See note 04 for
the `mochi_io` module.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. ⇒ BEAM proper list (cons cells).
- `map<K, V>`, keyed lookup. ⇒ BEAM map (`#{...}`).
- `set<T>`, unordered, unique members. ⇒ BEAM map with `[]` values
  (the OTP 24+ `sets` v2 idiom).
- `string`, read-only `list<char>`-ish (see §1.4). ⇒ UTF-8 binary.

Lowering obligations (full per-type details in note 06 §1):

- `list<T>` is the workhorse. BEAM's cons cell is the natural fit. The
  iteration form is recursive `[H|T] -> ... ; [] -> ...`. Random access
  `xs[i]` is O(n) via `lists:nth/2`; this is documented as a perf
  caveat. For random-access-heavy workloads, the runtime exposes an
  `array<T>` opt-in (Erlang `array` module, log-N access).
- `map<K, V>` defaults to a BEAM map. Small maps (≤32 entries) are stored
  as a sorted flat structure; large maps switch to a hash trie. This
  matters for performance but not for semantics. Iteration order on the
  large-map regime is hash order, not insertion order; for queries that
  need stability the `omap<K, V>` shape (note 06 §1) keeps a parallel
  insertion-order list.
- `set<T>` is a `map<T, []>` internally. The query layer (§5) needs the
  *insertion-ordered* semantics for `union`/`except` to be deterministic;
  use `omap`-backed sets when ordering matters.
- All collections are **value-semantically copied** at language level.
  BEAM data is immutable by construction, so the "copy" is logically a
  no-op; the VM enhancement spec 0951 §1 ("each function call must
  allocate a fresh copy of any list/map literal") is satisfied trivially
  because Erlang literal expressions allocate a fresh term per evaluation.
  The Mochi C target has to work for this; the BEAM target gets it free.

The "list of binary" pattern is the canonical BEAM idiom for "list of string"
and round-trips through the FFI cleanly.

## 4. Algebraic-data-type core

Two type-declaration shapes:

- **Records** (struct-like):
  ```mochi
  type Book { title: string, author: Person, pages: int }
  ```
  Lower to a tagged tuple in declaration order: `{book, Title, Author, Pages}`.
  Field access `b.title` ⇒ `element(2, B)` where the field index is fixed
  at codegen time (note 06 §1). Equality is structural via `=:=`, which
  matches Mochi semantics. The codegen also emits an Erlang `-record(book,
  {title, author, pages}).` declaration in the module's header so Dialyzer
  and the `?BOOK_TITLE` record-field accessor macro Just Work, even though
  Mochi-emitted code uses positional tuple access directly.
- **Sum types** with payload-carrying variants:
  ```mochi
  type Tree = Leaf | Node(left: Tree, value: int, right: Tree)
  ```
  Nullary variants ⇒ bare atoms (`leaf`). Positional variants ⇒ tagged
  tuples (`{node, L, V, R}`). The discriminator is always the first element
  (the atom tag).

Pattern matching deconstructs both:

```mochi
return match t {
  Leaf => 0
  Node(l, v, r) => sum(l) + v + sum(r)
}
```

lowers to an Erlang `case`:

```erlang
case T of
    leaf -> 0;
    {node, L, V, R} -> sum(L) + V + sum(R)
end
```

Exhaustiveness is checked at Mochi type-check time (already in MEP-13). The
emitted `case` does *not* need a catch-all clause; BEAM raises `case_clause`
on non-exhaustive match, which the Mochi runtime maps to a panic. In
`--debug` builds, the codegen adds a catch-all clause that calls
`mochi_runtime:non_exhaustive/2` with the source span for a precise error
message; in release builds, the BEAM exception fires.

Pattern matching is **native** to BEAM. The BEAM kernel pass already
compiles patterns to a decision tree (the Maranget-equivalent algorithm),
shares prefixes across clauses, and emits compact `select_val` /
`select_arity` instructions. The Mochi backend therefore does **no
match-to-tree work**; it emits the `case` and lets the BEAM compiler do
it. This is one of the headline wins of targeting BEAM vs C (where MEP-45
has to implement Maranget itself).

Type declarations may carry methods (§2.3) inside the block. Methods
dispatch by the principal type, so `Tree.fold(...)` ⇒ `tree_fold(Self,
...)`. Methods that need variant dispatch use `match self`, which becomes
the same `case` lowering.

## 5. Query DSL

The full Mochi query grammar (`from / join / where / group by / sort by / skip
/ take / select` plus set operations `union / intersect / except`) lowers to
a pipeline of Erlang list comprehensions or, for the more complex cases, a
fused iterator chain in the `mochi_query` runtime module.

Mapping table (full details in note 08):

| Mochi clause | Erlang lowering |
|--------------|-----------------|
| `from x in xs select e` | `[E\|\| X <- Xs]` (list comprehension) |
| `from x in xs where p select e` | `[E \|\| X <- Xs, P]` |
| `from x in xs, y in ys select ...` | nested generator in one comprehension |
| `from x in xs join y in ys on cond select ...` | nested comprehension with `=:=` join predicate (small N) OR hash-join via `mochi_query:hash_join/4` (large N) |
| `group by` | `mochi_query:group_by/2` collecting into `omap<Key, list<T>>` |
| `sort by e1, -e2` | `lists:sort/2` with a generated comparator fun |
| `take N` / `skip N` | `lists:sublist` / `lists:nthtail` |
| `union` / `intersect` / `except` | `mochi_query:union/2` etc. with insertion-order preservation |

The simple cases (`from / where / select` with at most one join) inline as
plain list comprehensions in the emitted Erlang, which the BEAM compiler
already optimises competently. The complex cases delegate to
`mochi_query`. See note 08 §3 for the inlining heuristic.

External sources are loaded with `load PATH as T`. The BEAM lowering invokes
the `mochi_data` runtime module which uses `jsone` / `jsx` / OTP 27's stdlib
`json` module for JSON, `yamerl` for YAML, a hand-rolled CSV parser for CSV.
`save expr to PATH` is symmetric. See note 08 §6.

## 6. Stream / agent core

### 6.1 Streams

A `stream T { fields }` declaration introduces a global event channel keyed
by the type name. Events are emitted with `emit T { ... }` and consumed by
`on T as x { body }` handler blocks.

BEAM lowering: each stream type lowers to one **stream-manager process**
(`mochi_stream:start_link(StreamName)`), modelled as a thin gen_event-like
hub. `emit T { ... }` sends a `{stream_event, T, Value}` message to the hub;
the hub broadcasts to all currently subscribed handler PIDs. The roadmap's
"concurrently per docs" obligation is satisfied because BEAM processes are
inherently concurrent; the deterministic-replay obligation is satisfied
because the hub serialises messages through its mailbox in arrival order.

Each `on T as x { body }` block lowers to a **handler process** that calls
`mochi_stream:subscribe(T)` then enters a receive loop. Multiple handlers
for the same stream get separate PIDs, all subscribed to the same hub.

The dispatch contract (from `docs/features/streams.md`):

- Events are queued and replayed deterministically ⇒ hub serialises via its
  mailbox.
- Multiple `on` blocks for the same stream are all invoked concurrently ⇒
  hub broadcasts via `!` to each subscriber's PID, which runs them on its
  own scheduler thread.
- Optional `timestamp: time` field; auto-assigned via `now()` if absent ⇒
  hub fills in `erlang:system_time(nanosecond)` before broadcast.
- Events emitted from inside a handler are queued (FIFO) and processed
  after the current handler returns ⇒ falls out for free because BEAM
  message sends are async and BEAM processes are single-threaded
  internally.

See note 09 §2 for the full hub protocol.

### 6.2 Agents

An `agent T { ... }` block bundles state, handlers, and exposed methods.

BEAM lowering: each agent instance is a **gen_server** process holding the
state record. Construction `let m = T {}` lowers to `mochi_agent:start(T)`
which returns a PID. Intent calls `m.status()` lower to `gen_server:call(M,
{intent, status, []})`. Handler blocks `on Sensor as s { ... }` inside an
agent compile to additional clauses in the agent's `handle_info/2`
callback, reacting to broadcast messages from the stream hub.

State updates inside an intent are reflected in the gen_server `State`
return value; mutation of `self.field` is rewritten by the codegen into
state-record replacement at the end of the call.

See note 09 §4 for the agent state machine.

### 6.3 Channels

If Mochi grows a `chan<T>` primitive (the C target's MEP-45 §9.1
already plans it), the BEAM lowering is a process pair with a bounded
mailbox emulated via a counter-protected `gen_server`. See note 09 §3.

## 7. Logic-programming core

```mochi
fact parent("Alice", "Bob")
rule grandparent(x, z) :- parent(x, y), parent(y, z)
let gp = query grandparent(x, z)
```

Bottom-up Datalog. BEAM lowering: each relation lives in an ETS table
(`set` type) keyed by the relation name; facts insert rows; rules compile to
nested table scans with deduplication via the ETS set semantics. Semi-naive
evaluation iterates until no new tuples are added.

ETS gives us:
- O(1) insert, O(1) point lookup, O(N) scan
- per-process or named ownership (we use named, owned by `mochi_datalog`)
- atomic operations (no inter-process locking needed for single-writer
  workloads)

Queries return `list<map<string, T>>` keyed by the variable names in the
head. The codegen emits the result-shape map at compile time so the runtime
doesn't need reflection.

See note 08 §7 (datalog sub-section).

## 8. AI and FFI shells

### 8.1 Generative AI

`generate text { prompt: ..., temperature: ..., max_tokens: ..., stop: ...,
model: ..., tools: [...] }` returns a string;
`generate embedding { text: ..., normalize: bool, model: ... }` returns
`list<float>`; `generate T { prompt: ... }` returns a `T`.

BEAM lowering: `generate` is a runtime call dispatched to `mochi_llm:call/2`.
The runtime uses `hackney` or `gun` (the cowboy team's HTTP client) for
HTTP; the stdlib `json` module (OTP 27+) for parsing the response. Models
declared with `model name { ... }` are populated into a persistent_term
table at application startup so the lookup at call time is allocation-free.

Tools are ordinary `fun` references with optional metadata. The codegen
captures their reference and arity at compile time so `mochi_llm` can
dispatch tool calls back into Mochi code without reflection.

### 8.2 HTTP fetch

`fetch "url" as T` and the `with { method, headers, body }` long form.
Errors propagate as exceptions; `try { ... } catch err { ... }` catches.

BEAM lowering: `fetch` ⇒ `mochi_fetch:get/2`, which wraps the chosen HTTP
client. JSON decoding into a typed `T` uses the same record-from-JSON shim
as `load`. The `try/catch` form lowers to Erlang's `try X of ... catch
Class:Reason:Stack -> ... end`, with `mochi_runtime:exception/3` converting
the BEAM exception tuple into a `mochi_error` record visible to user code.

### 8.3 FFI

```mochi
import go "math" as math
extern fun math.Sqrt(x: float): float
```

Three host languages explicitly named: `go`, `python`, `typescript`. For the
BEAM target, "import go" cannot mean linking against Go objects; the
universal interop story is **subprocess RPC over a port**.

- `import go` ⇒ spawn a Go sidecar via `erlang:open_port({spawn_executable,
  GoBin}, [...])`, marshal calls as JSON over the port's stdin/stdout pipe.
- `import python` ⇒ same, with a Python sidecar.
- `import typescript` ⇒ same, with a Deno/Node sidecar.
- `import erlang "lists" as lists` (the BEAM-target-specific shell) ⇒
  direct call to the named Erlang module; no marshalling needed.

A fifth implicit case is `import c`, exposed as a NIF: an `import c
"header.h" as foo` declaration with a corresponding `.c` file compiled into
a `.so` and loaded via `erlang:load_nif/2`. NIFs are the closest BEAM
analogue to C's direct linkage; they require careful authoring (a NIF crash
takes down the BEAM node). See note 04 §8.

## 9. Tests

```mochi
test "name" { expect bool_expr ; ... }
```

`test` blocks are top-level. Each `expect` is a boolean expression. On
failure, the reported diagnostic carries the line and the rendered
expression text.

BEAM lowering: every `test` becomes an `eunit` test function in a generated
test module. Every `expect e` lowers to a call `mochi_test:expect(File,
Line, ExprText, E)` that throws on false. The build driver wires the test
suite into `rebar3 eunit` so the standard OTP test runner picks them up. A
parallel "vm3-diff" gate (note 11 §3) runs the same Mochi source through
vm3, captures stdout, and diffs against the BEAM output for byte equality.

## 10. Module and package system

A directory is a package. Files share a namespace. `package foo` at the top
of a file sets the package name; `import "path"` brings another package in;
aliasing via `as`. `export` makes a name visible; unmarked names are
package-private.

BEAM lowering: each Mochi package becomes one Erlang module per file (NOT
one module per package — Erlang modules are file-scoped). Cross-file
references inside a package go through `?MODULE` if same-file, else through
the sibling module's fully-qualified name. The mangling rules of §1.6
guarantee no collisions; the codegen emits an `-export([...])` list
listing only the names the Mochi source marked `export`.

Cross-package references: `import "mathutils" as mu; mu.add(...)` ⇒
`'mochi.mathutils':add(...)`.

The transpiler produces a deterministic mangling (note 05 §3) so the same
Mochi input produces byte-identical Erlang output across hosts. This is
load-bearing for the reproducibility gate (note 07 §7).

## 11. Error model

Two distinct error paths:

- **Compile time**: type errors, exhaustiveness, re-assignment of `let`,
  module-cycle, undeclared `extern`, schema mismatch in `load`, etc.
- **Runtime**: `fetch` failure, parse failure, division by zero, integer
  overflow (per the threat-model "logic bugs trap deterministically"
  clause).

Runtime errors are recovered with `try { ... } catch err { ... }`. The
caught `err` has at least `.message: string`.

BEAM lowering: `try`/`catch` maps directly to Erlang's `try ... of ...
catch Class:Reason:Stack -> ... end`. `Class` is one of `error`, `exit`,
`throw`; the Mochi catch binds a `mochi_error` record built from
`(Class, Reason, Stack)` by `mochi_runtime:exception/3`. The `.message`
field is the result of `mochi_runtime:format_reason/2`. The `.code` field
is one of the `MOCHI_ERR_*` codes (see note 04 §9), mapped from `Reason`'s
shape.

Division by zero on BEAM raises `error:badarith`. The Mochi runtime catches
this in the divide site (the codegen wraps integer divides in a try) and
re-raises as `MOCHI_ERR_DIVZERO` so user code sees the documented error.

## 12. Concurrency semantics summary

Distilled from §6:

- Streams: M handlers per stream type, all invoked per event, concurrently
  per the docs but replayable deterministically in test mode.
- Agents: own per-agent state. Handlers inside an agent are serialised
  against that agent's state (single-thread per agent is the BEAM
  guarantee: each gen_server handles one message at a time). Inter-agent
  dispatch can be concurrent.
- The threat model excludes "concurrent / multi-actor Mochi" from the vm3
  memory-safety claim, which means the language *does* admit concurrency
  but the safety story is best-effort there.

For the BEAM target this resolves cleanly because **the BEAM is the actor
model**. Agents ⇒ processes; streams ⇒ broadcast hubs; channels ⇒ bounded
process pairs. Concurrency is the native idiom; no scheduler, no fiber
library, no work-stealing M:N runtime to import. See note 09.

## 13. Reflection / introspection

Nothing in the language surface requires runtime reflection. `print(x)`
needs a per-type formatter, which the transpiler emits statically by walking
the record's field list at compile time. The BEAM lowering emits a
`mochi_format_<type_id>/1` function per Mochi type at compile time;
`print(x)` calls the right one via the static type from the type checker.

For the catch-all "print any value" case (the `?MODULE`-aware `io:format
"~p"`), BEAM's built-in `~p` printer handles tuples, lists, maps, atoms,
binaries adequately, and the Mochi runtime can call `io:format("~p", [X])`
as a fallback.

## 14. Lowering-obligation summary

Compressed checklist that the MEP body uses as the source of truth for the
codegen design:

1. Preserve let/var, structural records, sum types, methods, pattern
   matching — every form has a clean Erlang AST analogue (note 05 §4-§6).
2. Preserve UTF-8 string semantics for indexing and iteration — use binary
   strings + `string` / `unicode` modules (note 06 §7).
3. Preserve copy-on-allocate semantics for list/map/set literals inside
   function bodies — BEAM gives this for free.
4. Preserve LINQ query semantics including set operations and group-by —
   inline simple cases as list comprehensions, delegate complex cases to
   `mochi_query` (note 08).
5. Preserve concurrent multi-handler stream dispatch with deterministic
   test replay — stream-hub gen_event-like process (note 09 §2).
6. Preserve agent encapsulation and per-instance state — agent ⇒
   gen_server (note 09 §4).
7. Preserve `try/catch` with stack unwinding — Erlang `try` (note 11 §3).
8. Preserve Datalog `fact`/`rule`/`query` evaluation with deduplication —
   ETS-backed semi-naive evaluator (note 08 §7).
9. Preserve `load`/`save` for CSV/JSON/JSONL/YAML — `mochi_data` (note 08
   §6).
10. Preserve `fetch` with header/body/method customisation and JSON
    decoding — `mochi_fetch` (note 04 §10).
11. Preserve `generate text`/`generate T`/`generate embedding` with per-model
    defaults and tool-callbacks — `mochi_llm` (note 04 §11).
12. Preserve test discoverability and pretty-printed failures —
    eunit-compatible test functions (note 11 §2).
13. Preserve package-private symbol scoping with deterministic mangling —
    note 05 §3.
14. Preserve `extern` interop with Go / Python / TypeScript / Erlang / C —
    ports for the first three, direct calls for Erlang, NIF for C (note 04
    §8).

Each obligation maps to a specific Erlang/BEAM construct documented in
`05-codegen-design.md` and `04-runtime.md`.
