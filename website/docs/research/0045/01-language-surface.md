# MEP-45 research note 01, Mochi language surface

Author: research pass for MEP-45 (Mochi → C transpiler).
Date: 2026-05-22 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`–`v0.7`, normative security
specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`.

This note records the user-visible language surface that the C target must
faithfully reproduce. It is deliberately written **from the spec downward**
and ignores the existing Go runtime, the vm3 bytecode, and any backend codegen
files. The goal is a transpiler design that would be correct against the
*language*, not against the present implementation.

The surface decomposes into eight orthogonal sub-languages: (1) the value
core, (2) the function and method core, (3) the collection core, (4) the
algebraic-data-type core, (5) the query DSL, (6) the stream / agent core,
(7) the logic-programming core, and (8) the AI / FFI shells. Each section
below names every form a Mochi program can write, then states a *lowering
obligation* the C backend must honour.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time** error,
  not a runtime panic. The compiler must emit the binding as a `const`-ish
  C l-value: either `const T name = ...;` for primitive types, or a flag in
  the symbol table that forbids `=` lowering after first definition.
- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope.

A binding may carry an explicit type: `let x: int = 0`. When the annotation
is absent the type is inferred from the initializer. The inference is
unidirectional (left-to-right, expression-down), so `var xs: list<int> = []`
is the canonical idiom for an empty mutable list. (See VM enhance spec
0951 §1 for the "mutable literal in a function body must be cloned" rule;
this is a *semantic* rule, not a backend hack, and the C lowering must
respect it.)

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

The list pattern is positional; the map pattern is key-driven. Both bind
fresh names; both are immutable. C lowering: emit a temp for the source
expression and then bind each name to a field/index access of the temp.

Scoping is lexical and block-based. Inner blocks shadow outer bindings.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet:

| Mochi | Width / semantics | C lowering |
|-------|------------------|------------|
| `int` | 64-bit signed integer (inferred from integer literals) | `int64_t` |
| `float` | 64-bit IEEE 754 double | `double` |
| `bool` | `true` / `false` | `bool` (C23 keyword) |
| `string` | UTF-8 text, indexable as code points, immutable | tagged `mochi_str` (see runtime note) |
| `time` | absolute timestamp (used by streams) | `mochi_time` (i64 ns since Unix epoch, UTC) |
| `duration` | time interval (`std/time` API) | `mochi_dur` (i64 ns) |
| `image` (preview) | binary blob (`load "cat.png"`) | `mochi_bytes` |

Implicit numeric conversions are **not** allowed (per the type-checker
discipline implied by MEP-4/5/6 referenced from the threat model). `int +
float` is a type error; the program must `float(x)` first. This shapes the
C lowering: emit no implicit C conversions; the C output is dense with
explicit cast helpers that the type-checker has already proven safe.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean `&& || !`;
membership `in`; string concatenation overloads `+`. Lowering: 1-for-1 to C
operators *for integer types* (with overflow trap in debug builds);
floating-point uses C's `/` (IEEE 754); `==` on records is field-wise
structural; `==` on collections is element-wise; `in` on map dispatches on
the map's hash table; `in` on string is `strstr`-style substring.

### 1.4 Strings as read-only character sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a `char`). Iteration yields
1-character strings in **code-point** order, not byte order. This forces a
UTF-8 cursor in the runtime (not raw `char *`).

### 1.5 Literals

Integer literals; floating literals (`3.14`); boolean; string with C-style
escapes; triple-quoted multi-line strings (`"""..."""`); list `[...]`;
map `{key: val, ...}`; set `{a, b, c}`; record constructor `T { field: val }`.

The set literal `{a, b, c}` is distinguished from the empty/map literal
`{}` by the absence of `:` after the first element. The grammar must keep
these unambiguous; the C lowering picks the right constructor accordingly.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Parameters are typed; return type is required (even `void` is implicit when
no return-type clause appears, as in `fun describe() { print(...) }` from
`examples/v0.3/method.mochi`). Recursion is supported. The docs explicitly
warn there is **no implicit tail-call optimisation**, so the C lowering is
free to (and should) emit ordinary C calls with no obligation to TCO.
Optional opt-in via `[[gnu::musttail]]` / `[[clang::musttail]]` is a Phase
2 enhancement.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Function values are first-class. They have a structural type
`fun(int): int`. Lowering: closures need an environment struct
because the arrow form can capture lexical variables
(`make_adder` in `mcp/cheatsheet.mochi` closes over `n`). The natural
lowering is a 2-word fat pointer `{ void (*code)(env*, args...); env* env; }`.
See codegen note for the alternative monomorphisation strategies.

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block are
unqualified (`radius`, not `self.radius`). Lowering: every method becomes
a C function `Circle_area(struct Circle *self)` and field access desugars to
`self->radius`.

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end. No format-string interface
at the source layer.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. Access by integer index. Append via
  `xs.push(v)` or `append(xs, v)`. Concatenation with `+`. Iteration with
  `for x in xs`.
- `map<K, V>`, keyed lookup with `m[k]`. Iteration over keys: `for k in
  m.keys()`. Membership: `k in m`.
- `set<T>`, unordered, unique members. Membership: `x in s`. Set ops via
  the query layer (`union`, `intersect`, `except`).
- `string`, read-only `list<char>`-ish (see §1.4).

Lowering obligations:

- `list<T>` is the workhorse. The runtime needs a small-buffer-optimised
  growable vector: pointer + len + cap, with the first ~4–8 elements inline.
- `map<K, V>` defaults to a hash table for `K: int | string`; an ordered
  variant is needed for query stability. Swiss-table style is the modern
  default; see runtime note.
- `set<T>` is a `map<T, unit>` internally. The query layer (§5) needs the
  *insertion-ordered* semantics for `union`/`except` to be deterministic.
- All collections are **value-semantically copied** at language level; the
  VM enhancement spec 0951 §1 makes this explicit ("each function call must
  allocate a fresh copy of any list/map literal"). The C lowering must
  therefore use copy-on-write or per-call allocation; persistent data
  structures (HAMT) are a candidate.

## 4. Algebraic-data-type core

Two type-declaration shapes:

- **Records** (struct-like):
  ```mochi
  type Book { title: string, author: Person, pages: int }
  ```
  Construction `Book { title: ..., author: ..., pages: ... }`. Field
  access `b.title`. Equality is structural.
- **Sum types** with payload-carrying variants:
  ```mochi
  type Tree = Leaf | Node(left: Tree, value: int, right: Tree)
  ```
  Lowering: a tagged union. Nullary variants like `Leaf` can be folded
  into the tag word; positional variants get a payload struct.

Pattern matching deconstructs both:

```mochi
return match t {
  Leaf => 0
  Node(l, v, r) => sum(l) + v + sum(r)
}
```

The C lowering must implement an **exhaustive** decision-tree compilation
(Maranget's algorithm) and reject non-exhaustive matches at compile time;
the wildcard `_` arm makes exhaustiveness explicit. Variable bindings
inside a pattern (`Node(l, v, r)`) are immutable in the arm body.

Type declarations may carry methods (§2.3) inside the block. Methods are
indexed by the principal type, so `Tree` cannot carry methods that depend on
which variant `self` is; methods that need that dispatch internally via
`match self`.

## 5. Query DSL

A complete in-language LINQ. The grammar of a single query is:

```
query := 'from' x 'in' src
         ( 'from' y 'in' src2
         | 'join' alias 'in' src 'on' expr
         | 'left' 'join' alias 'in' src 'on' expr
         | 'right' 'join' alias 'in' src 'on' expr
         | 'outer' 'join' alias 'in' src 'on' expr
         )*
         ( 'where' expr )?
         ( 'group' 'by' expr 'into' g )?
         ( 'sort' 'by' expr (',' expr)* )?
         ( 'skip' expr )?
         ( 'take' expr )?
         'select' expr
```

Set operations operate on whole queries:

```mochi
(from x in a select x) union (from y in b select y)
(from x in a select x) union all ...
(from x in a select x) intersect ...
(from x in a select x) except ...
```

Sort direction is encoded by leading `-`: `sort by -p.age`.

`group by` returns a synthetic `g` per group, with `.key` and aggregation
functions `count(g)`, `sum(g, e)`, `avg(g, e)`, `min(g, e)`, `max(g, e)`.

External sources are loaded with `load PATH as T` (CSV / JSON / JSONL /
YAML auto-detected by extension, override with `with { format: "..." }`).
Results are written with `save expr to PATH`. `extern type T { ... }`
declares a record shape that the loader is allowed to populate from an
externally-defined schema (the `load_with.mochi` example).

Lowering: the query is *not* a function call to a library; it is a
**source-level DSL** that the compiler translates into a pipeline of
iterator combinators. The naive lowering is a nested loop; the modern
lowering is a fused stream (Csharp LINQ-to-objects style) that respects
short-circuiting (`take` cuts off the upstream). The C target must support
both: small queries inline as nested loops, large queries materialise into
the runtime's `mochi_dataset` type.

## 6. Stream / agent core

### 6.1 Streams

A `stream T { fields }` declaration introduces a global event channel
keyed by the type name. Events are emitted with `emit T { ... }` and
consumed by `on T as x { body }` handler blocks. Handlers can be either
top-level (the cheatsheet example) or nested inside an agent (§6.2).

The roadmap pins the dispatch contract:

- Events are queued and replayed deterministically.
- Multiple `on` blocks for the same stream are all invoked, **concurrently**
  per the docs (`docs/features/streams.md`).
- Optional `timestamp: time` field; auto-assigned via `now()` if absent.
- Events emitted from inside a handler are queued (FIFO) and processed
  after the current handler returns.

The "concurrently" claim is a real semantic obligation. The C lowering
needs an M:N scheduler with a per-stream FIFO and a worker pool. Test
runs require the scheduler to *also* support deterministic single-threaded
replay (the cheatsheet's `test` blocks use `emit` and `expect` and require
ordering by timestamp).

### 6.2 Agents

An `agent T { ... }` block bundles state, handlers, and exposed methods.
State is per-instance (`var count: int = 0`); handlers (`on Sensor as s
{ ... }`) react to streams; `intent foo(): U { ... }` methods are
externally callable. Construction is `let m = T {}`; intents are invoked as
methods: `m.status()`.

Lowering: an agent is a record-with-handlers. The C output is a struct
with the state fields plus a fixed-size handler table that the scheduler
walks on event delivery. Intent methods compile like ordinary methods
(§2.3). The agent struct is heap-allocated and lives for the duration of
the program (roadmap: "single instance per agent definition" was the v0.5
contract; future versions may allow many).

## 7. Logic-programming core

```mochi
fact parent("Alice", "Bob")
rule grandparent(x, z) :- parent(x, y), parent(y, z)
let gp = query grandparent(x, z)
```

Bottom-up Datalog. Facts are stored in an indexed relation. Rules are
non-recursive in syntax but the engine must support transitive closures
(grandparent over parent). Queries return a `list<map<string, T>>` keyed by
the variable names in the head, so `gp[0].x` and `gp[0].z` are accessible.

Lowering: the modern path is to emit a semi-naive evaluator. The pragmatic
path for a transpiler is to translate each rule into a nested loop and a
result accumulator, with deduplication via a per-relation hash set. Sliding
window: emit *both*, gate on relation size at runtime.

## 8. AI and FFI shells

### 8.1 Generative AI

`generate text { prompt: ..., temperature: ..., max_tokens: ..., stop: ..., model: ..., tools: [...] }` returns a string;
`generate embedding { text: ..., normalize: bool, model: ... }` returns `list<float>`;
`generate T { prompt: ... }` (where `T` is a user record) returns a `T`,
with the model coerced to emit JSON matching the record schema (cheatsheet
§6, `examples/v0.3/generate-struct.mochi`).

Models are declared once:

```mochi
model quick { provider: "openai", name: "gpt-3.5-turbo", temperature: 0.7 }
```

and referenced by name from `generate` blocks. Tools are ordinary `fun`
references with optional `{ description: "..." }` metadata.

Lowering: `generate` is a **runtime call** dispatched to a `mochi_llm_*`
shim in the C runtime. The shim talks HTTP (libcurl) to the provider; the
record-return form uses the same JSON-driven loader as `load`. Models live
in a global symbol table populated at static-init time.

### 8.2 HTTP fetch

`fetch "url" as T` and the `with { method, headers, body }` long form.
Errors propagate as exceptions; `try { ... } catch err { ... }` catches.
Lowering: libcurl + JSON parser. The `try/catch` form needs an
exception-like construct in C; setjmp/longjmp is the pragmatic choice (see
runtime note for the algebraic-effects-vs-setjmp tradeoff).

### 8.3 FFI

```mochi
import go "math" as math
extern fun math.Sqrt(x: float): float
```

Three host languages are explicitly named: `go`, `python`, `typescript`.
At the C target, "import go" and "import typescript" cannot mean linking
against Go or Deno objects directly; the only universal interop story is
**subprocess RPC**. The transpiler must generate stubs that marshall the
call to a sidecar process for each host, using a JSON-over-pipe protocol
matching the existing `runtime/ffi/*` shape (named in the docs but not
read here).

A fourth host implicit in the design is "import c", direct linkage
against a C library. The transpiler should add this case explicitly: an
`import c "header.h" as foo` declaration that maps to a `#include` plus
direct call generation, no marshalling.

## 9. Tests

```mochi
test "name" { expect bool_expr ; ... }
```

`test` blocks are top-level. Each `expect` is a boolean expression. On
failure, the reported diagnostic carries the line and the rendered
expression text (so the compiler has to keep the source span for every
`expect`). Tests participate in build via `mochi test`.

Lowering: every `test` becomes a C function `mochi_test_<n>(test_ctx*)`;
every `expect` becomes a macro that records pass/fail and the source-text
expansion. A small driver iterates the test table at `main()` time and
prints results in a stable format.

## 10. Module and package system

A directory is a package. Files share a namespace. `package foo` at the
top of a file sets the package name; `import "path"` (relative `./...`)
brings another package in; aliasing via `as`. `export` makes a name
visible; unmarked names are package-private.

Lowering: each package becomes a C translation unit (or a header+source
pair). Cross-package references go through the alias as a C namespace
prefix: `import "mathutils" as mu; mu.add(...)` → `mathutils_add(...)`.
The transpiler must produce a deterministic mangling scheme; see codegen
note for the rules.

## 11. Error model

Two distinct error paths:

- **Compile time**: type errors, exhaustiveness, re-assignment of `let`,
  module-cycle, undeclared `extern`, schema mismatch in `load`, etc.
- **Runtime**: `fetch` failure, parse failure, division by zero, integer
  overflow (per the threat-model "logic bugs trap deterministically"
  clause).

Runtime errors are recovered with `try { ... } catch err { ... }`. The
caught `err` is a value with at least `.message: string`. There is no
explicit error-type hierarchy in the docs; the C lowering uses a single
`mochi_error` discriminated value with a string message plus an optional
machine-readable code, threaded through `try / catch` via a stacked
longjmp buffer.

## 12. Concurrency semantics summary

Distilled from §6:

- Streams: M handlers per stream type, all invoked per event,
  concurrently per the docs but *replayable deterministically* in test
  mode.
- Agents: own per-agent state. Handlers inside an agent are serialised
  against that agent's state (single-thread per agent is the simplest
  reading); inter-agent dispatch can be concurrent.
- The threat model excludes "concurrent / multi-actor Mochi" from the
  vm3 memory-safety claim, which means the language *does* admit
  concurrency but the safety story is best-effort there.

For the C target this resolves to: per-agent mailbox, one fiber per agent,
M:N scheduler over OS threads, channels for inter-agent events. See the
runtime note for the libdill / minicoro / boost.context tradeoff.

## 13. Reflection / introspection

Nothing in the language surface requires runtime reflection. `print(x)`
needs a per-type formatter, which the transpiler can emit statically by
walking the record's field list at compile time (no runtime type
descriptors needed beyond what `match` already requires for sum types).
The C lowering can therefore avoid a full RTTI system, keeping the
runtime small.

## 14. Lowering-obligation summary

Compressed checklist that the MEP body uses as the source of truth for
the codegen design:

1. Preserve let/var, structural records, sum types, methods, pattern matching.
2. Preserve UTF-8 string semantics for indexing and iteration.
3. Preserve copy-on-allocate semantics for list/map/set literals inside
   function bodies (VM-enhance §1).
4. Preserve LINQ query semantics including set operations and group-by.
5. Preserve concurrent multi-handler stream dispatch with deterministic
   test replay.
6. Preserve agent encapsulation and per-instance state.
7. Preserve `try/catch` with stack unwinding.
8. Preserve Datalog `fact`/`rule`/`query` evaluation with deduplication.
9. Preserve `load`/`save` for CSV/JSON/JSONL/YAML.
10. Preserve `fetch` with header/body/method customisation and JSON
    decoding.
11. Preserve `generate text`/`generate T`/`generate embedding` with
    per-model defaults and tool-callbacks.
12. Preserve test discoverability and pretty-printed failures.
13. Preserve package-private symbol scoping with deterministic mangling.
14. Preserve `extern` interop with Go / Python / TypeScript / C.

Each obligation maps to a specific C-output construct documented in
`05-codegen-design.md`.
