# MEP-48 research note 01, Mochi language surface (.NET target)

Author: research pass for MEP-48 (Mochi to .NET/CLR transpiler).
Date: 2026-05-23 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`-`v0.7`, the normative
security specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`,
and the companion MEP-45 note 01 (C target), MEP-46 note 01 (Erlang/BEAM),
and MEP-47 note 01 (JVM), whose section structure this note deliberately
mirrors so all four backends can be diffed line for line.

This note records the user-visible language surface that the .NET target must
faithfully reproduce. It is deliberately written *from the spec downward* and
ignores the existing Go runtime (vm3), the vm3 bytecode, the C target under
MEP-45, the Erlang/BEAM target under MEP-46, the JVM target under MEP-47, and
any other backend implementation. The goal is a transpiler design that would
be correct against the *language*, not against the present implementations.

The surface decomposes into the same eight orthogonal sub-languages identified
in MEP-45 note 01, MEP-46 note 01, and MEP-47 note 01: (1) the value core,
(2) the function and method core, (3) the collection core, (4) the
algebraic-data-type core, (5) the query DSL, (6) the stream / agent core,
(7) the logic-programming core, and (8) the AI / FFI shells. Each section
below names every form a Mochi program can write, then states a *lowering
obligation* the .NET backend must honour.

Where MEP-45 maps Mochi types to C struct plus helper-function pairs, MEP-46
maps them to BEAM terms (atoms, tagged tuples, maps, binaries, funs, PIDs),
and MEP-47 maps them to JVM values (Java primitives, boxed numerics, records,
sealed interfaces, lambdas, Loom virtual threads), this note maps them to .NET
values: CLR primitives (`long`, `double`, `bool`), boxed numerics, `string`,
`System.Collections.Immutable.ImmutableList<T>` and `ImmutableDictionary<K,V>`
for the immutable defaults, `List<T>` / `Dictionary<K,V>` for mutable views,
C# 12 records (both `record class` for ADTs and `record struct` for small
value types), discriminated unions encoded as sealed record class hierarchies
under a `[JsonPolymorphic]` base, F#-style `Option<T>` and `Result<T,E>`
realised as record structs, async lambdas (`Func<Task<T>>`), and
`System.Threading.Channels` for the agent core. The target IR is discussed in
note 05 (a hybrid: Roslyn `SyntaxFactory` source emission for the default
path, with `System.Reflection.Emit` direct IL for a small set of hot lowerings
that benefit from skipping a C# round-trip); the runtime is the .NET BCL plus
a thin `Mochi.Runtime` NuGet package (see note 04). Throughout, ".NET" means
.NET 8 LTS (released 2023-11) for the primary CI gate and .NET 10 LTS
(released 2025-11) for the rolling secondary gate; both must stay green for a
phase to ship. Mono is a Phase-3 secondary target and is called out where its
JIT or AOT constraints force a divergence; NativeAOT is the Phase-2 deployable
artefact and is called out where it forbids reflection or runtime codegen.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time** error,
  not a runtime panic. The .NET lowering emits a C# `readonly` local with the
  C# 7.0+ `ref readonly` discipline for value types, and a `readonly` field
  for module-level lets, so the C# compiler itself rejects any accidental
  re-assignment leak that escapes the Mochi type checker. For locals C# does
  not have a `readonly local` keyword pre-C# 12; the backend uses C# 12's
  primary constructors and `readonly` semantics for fields, and a `var`
  local that is provably single-assignment via Roslyn's flow analysis (any
  attempted re-assignment in the lowered source would be a Mochi compiler
  bug, caught by the C# compiler itself once it sees a second assignment).
- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope. Lowers to a plain C# `var` local (no modifier)
  or a private mutable field.

Mochi blocks are expressions in the sense that the last expression is the
block's value. C# blocks are statements. The backend lowers a block whose
value is consumed using a local function or a switch expression:
`var ___t = ((Func<T>)(() => { ...; return e; }))();` for the general case,
and the C# 8 switch expression `_ => { ...; yield ...; }` style is not
supported (C# switch expressions are pure expressions). For the common
`let x = if cond { a } else { b }` form the backend emits the C# 9 ternary
`var x = cond ? a : b` directly, and for `let x = match v { ... }` it emits
a C# 8 switch expression. See note 05 §6 for the full block-lowering table.

A binding may carry an explicit type: `let x: int = 0`. C# is statically
typed; the type survives end-to-end. `int` lowers to C# `long` (see §1.2 on
why), so `let x: int = 0` becomes `long x = 0L;`.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

C# 7+ has tuple deconstruction and C# 11 has list patterns, but they apply
to tuples, arrays, and types that implement `Deconstruct`, not to
`ImmutableList<T>` or `ImmutableDictionary<K,V>` directly. The list pattern
lowers to a positional read with a runtime length check:

```csharp
var ___tmp = ImmutableList.Create<long>(1L, 2L);
if (___tmp.Count != 2) throw new MochiPatternError(...);
long a = ___tmp[0];
long b = ___tmp[1];
```

The map pattern lowers to `Dictionary.TryGetValue` per key with a null
check (since the pattern asserts the keys exist). For records the backend
can emit C# 9 positional record deconstruction directly
(`if (point is Point(long x, long y))`), which is the cleanest fit, taking
advantage of the auto-generated `Deconstruct` method. See note 05 §11 for
the full destructuring strategy.

Scoping is lexical and block-based. Inner blocks shadow outer bindings.
C# allows block-scoped locals but is more permissive than Java about
shadowing in nested blocks (C# 9+ allows variable shadowing within
nested local function scopes but not in nested ordinary blocks for the
same name). The backend must rename shadowed names with a suffix
(`x`, `x__1`, `x__2`) for cross-block shadowing within the same method.
See note 05 §4.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the .NET-side representation:

| Mochi | Width / semantics | .NET lowering |
|-------|------------------|----------------|
| `int` | 64-bit signed integer (inferred from integer literals) | C# `long` primitive (unboxed); `long?` for nullable; boxed as `object` only at variance boundaries |
| `float` | 64-bit IEEE 754 double | C# `double` primitive; `double?` nullable |
| `bool` | `true` / `false` | C# `bool` primitive; `bool?` nullable |
| `string` | UTF-8 text (spec) | C# `string`, which is UTF-16 at runtime; encoding boundary at FFI and `print` (UTF-8) per note 04 |
| `bigint` | arbitrary-precision integer (opt-in) | `System.Numerics.BigInteger` (value struct, immutable) |
| `bigrat` | arbitrary-precision rational (opt-in) | `Mochi.Runtime.BigRat` record struct over two `BigInteger` operands |
| `null` | unit / absence | C# `null`, statically typed as either nullable reference (`string?`) or `Mochi.Option<T>.None` for nullable value cells |

The choice of `long` over `int` for Mochi `int` is load-bearing: Mochi
specifies 64-bit integer semantics, and C# `int` is 32-bit (the C# spelling
of `long` is the 64-bit type, opposite to Mochi). Silent narrowing in the
lowering would be a correctness bug. `BigInteger` lives in
`System.Numerics`, ships in the BCL since .NET Framework 4.0, and supports
all standard arithmetic operators including `**` via `BigInteger.Pow`. The
backend lifts a `int` value to `BigInteger` on overflow only when the user
has annotated the binding as `bigint`; silent promotion is a Mochi spec
violation.

### 1.3 Operators

Arithmetic: `+`, `-`, `*`, `/`, `%`, `**` (power). C# does not have a `**`
operator; the lowering picks `Math.Pow` for `float`, `Mochi.Runtime.IntPow`
for `int` (binary exponentiation, no allocations), and `BigInteger.Pow`
for `bigint`. Integer `/` on negative dividends follows Mochi's truncated
semantics, matching C# integer division.

Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`. For value types the backend
emits `==` directly (C# operator overloading covers `BigInteger` and
records). For `string`, C# `==` is value equality (overloaded on `string`),
which matches Mochi semantics. For `ImmutableList<T>` and
`ImmutableDictionary<K,V>` the backend emits a structural-equality helper
since their `==` is reference equality; see note 04 §3.

Logical: `&&`, `||`, `!`. Short-circuiting, lowered as C# `&&`, `||`, `!`.

String: `+` for concatenation, lowered to `string.Concat` (C#'s compiler
already turns `"a" + "b"` into `string.Concat` calls; the backend just
emits the surface form and trusts the compiler).

Membership: `in`. For `list`: `list.Contains(x)`. For `map`:
`dict.ContainsKey(x)`. For `string`: `s.Contains(substr)`.

### 1.4 Literals

Integer literal: `42` → `42L`. Float literal: `3.14` → `3.14d`. String
literal: `"hello"` → `"hello"`. Boolean: `true` / `false` → `true` /
`false`. List: `[1, 2, 3]` → `ImmutableList.Create<long>(1L, 2L, 3L)` (or
`ImmutableArray.Create` for typed dense lists, decision in note 04 §2).
Map: `{"a": 1}` → `ImmutableDictionary<string, long>.Empty.Add("a", 1L)`
(or the `ImmutableDictionary.CreateBuilder` route for many-key literals,
see note 04 §3).

Tuple: Mochi supports tuples via records (no anonymous tuple type at the
language surface), so `(a, b)` parses as a 2-record literal. The .NET
backend can lower 2-to-7-arity record tuples to `System.ValueTuple<...>`
for hot paths, but the canonical lowering is a named C# record struct
because Mochi tuples carry field names. See note 06 §4.

### 1.5 Control flow

Conditionals: `if cond { ... } else { ... }`, lowering to C# `if`/`else`
when used as a statement, and to the C# ternary `cond ? a : b` or a switch
expression when used as an expression.

`match`: pattern matching on shape, lowering to C# 9+ pattern matching
(`switch` expression with `is` patterns, record deconstruction, list
patterns from C# 11). For sum types the backend emits a sealed record
class hierarchy and matches with the `is` operator. See §4 and note 06.

Loops: `for i in 0..n`, `for x in xs`, `while cond { ... }`, `break`,
`continue`. `for x in xs` lowers to a C# `foreach (var x in xs)` over the
`IEnumerable<T>` of the source collection. `for i in 0..n` lowers to a
`for (long i = 0; i < n; i++)` since Mochi `int` is `long`. The half-open
nature of `0..n` matches the canonical C# `for` form. There is no Mochi
`do/while`; back-edges always lower to `while`.

Exception-style flow: Mochi has no checked exceptions and no
`try`/`catch` in the spec; runtime errors are panics that abort. The .NET
backend translates panics to `System.InvalidOperationException` with a
`Mochi.Runtime.MochiPanic` subtype carrying source-location metadata.

### 1.6 Equality and hashing

Mochi equality is structural across all built-in types: two lists are
equal iff they are element-wise equal, two maps iff their key sets and
per-key values are equal, two records iff their fields are equal.

C# records (both `class` and `struct`) generate structural `Equals`,
`GetHashCode`, and `==`/`!=` automatically, so user-defined records get
the right semantics for free. For collections the backend wraps
`ImmutableList<T>` and `ImmutableDictionary<K,V>` in a thin
`Mochi.Runtime.MochiList<T>` and `Mochi.Runtime.MochiMap<K,V>` only when
the user requires structural equality at the equality operator; the
default lowering keeps the raw immutable type and emits a
`Mochi.Runtime.Eq.Equal(a, b)` helper at comparison sites. See note 04 §3.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { a + b }
```

Lowers to a `public static long Add(long a, long b)` in a generated
`global::Mochi.Program` class for the entry module, and to
`public static` methods in module-named classes for non-entry modules
(`Mochi.Modules.ModuleName`). Single-expression function bodies lower to
C# 6 expression-bodied methods: `public static long Add(long a, long b) =>
a + b;`. Block bodies become full method bodies.

Name mangling: Mochi `snake_case` is preserved in C# PascalCase by
default (idiomatic C#), but the Mochi-to-C# name map is stored in a
sidecar so debugger displays, stack traces, and reflection (used by the
agent supervisor) can recover the source name. See note 06 §8.

### 2.2 Closures and first-class functions

```mochi
let inc = fun(x: int) { x + 1 }
let twice = fun(f: fun(int): int, x: int) { f(f(x)) }
```

Closures lower to C# `Func<T,...,TR>` and `Action<T,...>` instances
(constructed via lambda expression `(x) => x + 1`), and the higher-order
parameter `f: fun(int): int` becomes `Func<long, long>`. Mochi closures
can capture mutable variables; C# lambdas can also capture mutable
locals (the compiler boxes them into a generated closure class), so
this is a direct match. The backend should prefer C# lambdas for
single-method functional interfaces because the C# compiler emits them
into `static readonly` cache fields where possible (lambdas with no
captures), avoiding repeated allocations.

### 2.3 Generic functions

```mochi
fun first<T>(xs: list<T>): T { xs[0] }
```

Lowers to `public static T First<T>(ImmutableList<T> xs) => xs[0];`. C#
has true reified generics (no type erasure, unlike Java), which means:

- A Mochi `list<int>` lowers to `ImmutableList<long>` *as a runtime
  type*. No boxing of `long` into `Long` at element access sites.
- `T` can be instantiated at primitive `long`, `double`, `bool`
  directly without auto-boxing.
- Reflection over generic types yields the actual type arguments, which
  makes the agent supervisor's runtime introspection trivial compared
  to the JVM (MEP-47 note 06 §3 had a workaround).

This is one of the largest *positive* deltas relative to the JVM
target. See note 06 §3 for the full discussion.

### 2.4 Methods on records

```mochi
type Point { x: int, y: int }
fun (p: Point) distance(): float { sqrt(p.x * p.x + p.y * p.y) }
```

Lowers to a C# instance method on the record:

```csharp
public sealed record Point(long X, long Y) {
    public double Distance() => Math.Sqrt(X * X + Y * Y);
}
```

The backend prefers the partial-record-class form (`partial record
Point`) so that auto-generated boilerplate (`Equals`, `GetHashCode`) and
user-supplied methods can live in separate emitted files. See note 05 §7.

### 2.5 Method dispatch

Mochi has no virtual dispatch in the spec (no method overriding across
types). All methods are statically resolved. The .NET lowering uses
non-virtual sealed `record` members, which JIT and AOT can both inline
aggressively. Interface dispatch is not surfaced; sum-type cases are
realised as record class hierarchies with `is`-pattern matching, not
virtual method tables. See §4.

### 2.6 Recursion and tail calls

Mochi spec is silent on TCO. The .NET CLR does not guarantee TCO for C#
(F# has `tailcall` opcode emission via the F# compiler, but C# does
not). For self-recursive functions the backend rewrites tail calls into
loops as a Roslyn pass over the lowered AST, matching the strategy used
for the JVM target. Mutual recursion gets a trampoline only when the
analysis pass detects a tail cycle; see note 05 §13.

## 3. Collection core

### 3.1 List

Mochi `list<T>` is an ordered, indexed, growable sequence. Operations:
literal `[a, b]`, index `xs[i]`, slice `xs[i..j]`, length `len(xs)`,
push `xs + [x]` (immutable; produces a new list), concat `xs + ys`,
in-place mutation under `var`: `xs[i] = v`, `xs.append(v)` (when `xs` is
mutable).

The default lowering is `System.Collections.Immutable.ImmutableList<T>`
(B-tree backed, O(log n) most ops). For dense numeric workloads the
backend may upgrade to `ImmutableArray<T>` (array-backed, O(1) index,
O(n) update). The choice is value-dependent and lives in note 04 §2;
the language surface does not surface it.

`len(xs)` lowers to `xs.Count`. `xs[i]` is `xs[i]` (C# indexer). Slice
`xs[i..j]` lowers to `xs.GetRange(i, j-i)` for `ImmutableList<T>` (returns
a sub-list view) or `xs.Slice(i, j-i)` for `ImmutableArray<T>` (returns a
sub-array). `xs + ys` becomes `xs.AddRange(ys)`.

Mutation under `var xs = ...`: the backend tracks per-binding mutability
and switches the runtime representation to `List<T>` (mutable
`System.Collections.Generic.List<T>`), or keeps `ImmutableList<T>` and
rebinds the local each iteration (e.g., `xs = xs.Add(v)`). See note 04 §2
for the heuristic.

### 3.2 Map

Mochi `map<K, V>` is an unordered (since v0.7, ordered-on-insert in the
spec note) key-to-value table.

The default lowering is `ImmutableDictionary<K, V>`. For the insertion-
order guarantee the backend uses `System.Collections.Generic.OrderedDictionary`
(added in .NET 9; on .NET 8 the backend uses a small wrapper around
`Dictionary<K,V>` plus a parallel `List<K>` of insertion order, defined in
`Mochi.Runtime.OrderedMap<K,V>`). The mutable variant is `Dictionary<K,V>`.
For .NET 8 the spec-required iteration order is provided by the runtime
wrapper; for .NET 10 it can rely on `OrderedDictionary<K,V>` directly. The
language surface is uniform; the lowering target depends on the floor TFM
(`net8.0` vs `net10.0`).

Operations: literal `{"a": 1}`, lookup `m["a"]`, contains `"a" in m`,
size `len(m)`, keys `m.keys()`, values `m.values()`, mutation under
`var`: `m["a"] = 1`, `m.delete("a")`.

`len(m)` lowers to `m.Count`. `m["a"]` lowers to `m["a"]` (C# indexer)
with a `TryGetValue`-based check site for missing keys when the type
checker can't prove presence. `m.keys()` is `m.Keys`. `m.delete("a")`
under `var` lowers to `m.Remove("a")`.

### 3.3 Set

Mochi `set<T>` is unordered, unique-element. Lowers to
`ImmutableHashSet<T>` for the immutable default and `HashSet<T>` for the
mutable variant. Operations are direct: `s + {x}` becomes `s.Add(x)`,
`x in s` becomes `s.Contains(x)`, `len(s)` becomes `s.Count`.

### 3.4 String

Mochi strings are UTF-8 in the spec. .NET strings are UTF-16 at runtime.
The boundary policy:

- Source-level string literals lower verbatim (Roslyn handles the
  Unicode escapes); the C# compiler emits them as UTF-16.
- `len(s)` lowers to a Mochi runtime helper that returns the UTF-8 byte
  count, not `s.Length` (which is UTF-16 code-unit count). For ASCII
  hot paths the backend may specialise to `s.Length` after a type
  refinement, but the default is the helper.
- String indexing `s[i]` lowers to a UTF-8 code-point access helper,
  not the C# UTF-16 `s[i]` indexer (which returns a `char`, half a
  surrogate pair on supplementary planes). See note 04 §5.
- The FFI boundary (`print`, file I/O, JSON, network) encodes/decodes
  UTF-8 explicitly.

### 3.5 Range

Mochi `0..n` (half-open) is not a first-class value in the spec; it
only appears in `for i in 0..n`. The lowering is a C# `for` loop. If a
future spec revision makes ranges first-class, the lowering target
would be a `Mochi.Runtime.Range` record struct that implements
`IEnumerable<long>` (Mochi int is 64-bit, so C#'s `System.Range` which
uses `int` indices is not usable directly).

### 3.6 Iteration order

Mochi spec guarantees insertion order for lists, maps, and sets in
iteration contexts (`for`, `keys()`, `values()`, query DSL). The .NET
mapping:

- `ImmutableList<T>`, `List<T>`: natural insertion order, direct match.
- `ImmutableDictionary<K,V>`: *unordered*; the backend wraps it in
  `Mochi.Runtime.OrderedMap<K,V>` when iteration order is observable
  (any `for` over a map, any `keys()`/`values()` call). On .NET 10 the
  backend can use BCL `OrderedDictionary<K,V>` directly.
- `ImmutableHashSet<T>`, `HashSet<T>`: *unordered*; the backend wraps
  in `Mochi.Runtime.OrderedSet<T>` when iteration order is observable.

The wrapper choice is per-allocation, decided at the lowering site by
the dataflow pass in note 05 §9.

## 4. Algebraic-data-type core

### 4.1 Record types

```mochi
type Point { x: int, y: int }
type User  { name: string, age: int }
```

Lower to C# 9+ `record class` (positional or nominal):

```csharp
public sealed record Point(long X, long Y);
public sealed record User(string Name, long Age);
```

The backend emits *positional* records by default since they auto-
generate `Deconstruct`, structural `Equals`, `GetHashCode`, and a `with`
expression for non-destructive update. `with` is a perfect match for
Mochi's record update syntax `{ p, x: 5 }`, which lowers to `p with { X
= 5 }`. Records are `sealed` (Mochi records are not extensible).

For small records (3 or fewer fields, all value types) the backend may
emit `record struct` instead of `record class` to avoid heap allocation.
The decision lives in note 06 §4; the language surface is uniform.

### 4.2 Sum types (discriminated unions)

```mochi
type Shape =
  | Circle { r: float }
  | Square { side: float }
  | Rect   { w: float, h: float }
```

Lower to a sealed abstract base record class plus one record class per
variant:

```csharp
[JsonPolymorphic(TypeDiscriminatorPropertyName = "$kind")]
[JsonDerivedType(typeof(Circle), "Circle")]
[JsonDerivedType(typeof(Square), "Square")]
[JsonDerivedType(typeof(Rect),   "Rect")]
public abstract record Shape;
public sealed record Circle(double R)            : Shape;
public sealed record Square(double Side)         : Shape;
public sealed record Rect  (double W, double H)  : Shape;
```

The `sealed` plus `abstract` combination makes the type closed: the C#
compiler does not enforce exhaustiveness on `switch` for arbitrary
sealed hierarchies (unlike Java's `JEP 441` sealed interfaces with
pattern switch exhaustiveness), so the backend emits an `_ => throw new
MochiMatchExhaustivityError()` default arm in every emitted switch.
Roslyn does flag unreachable arms, which catches typos in case names.

`match` on a sum type lowers to a C# 8 switch expression:

```csharp
var area = shape switch {
    Circle(var r)      => Math.PI * r * r,
    Square(var s)      => s * s,
    Rect  (var w, var h) => w * h,
    _ => throw new MochiMatchExhaustivityError(shape),
};
```

The positional deconstruction binds case parameters by name, matching
the Mochi surface. See note 06 §5 for the exhaustiveness analysis (a
Roslyn analyzer is added to surface non-exhaustive matches as compile
errors).

### 4.3 Generic ADTs

```mochi
type Option<T> = | Some { value: T } | None
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Lower to generic sealed hierarchies:

```csharp
public abstract record Option<T>;
public sealed record Some<T>(T Value) : Option<T>;
public sealed record None<T>()         : Option<T>;

public abstract record Result<T, E>;
public sealed record Ok<T, E>(T Value)     : Result<T, E>;
public sealed record Err<T, E>(E Error)    : Result<T, E>;
```

Because C# generics are reified, `Option<int>` and `Option<string>`
are distinct runtime types. The backend ships a hand-written
`Mochi.Runtime.Option<T>` and `Mochi.Runtime.Result<T, E>` in the
runtime NuGet package so that user code (and Mochi stdlib code) can
import a single canonical generic ADT without re-declaring per module.
See note 06 §6.

For very small Option-like cells over value types, the backend may
lower `Option<int>` to `long?` (nullable value type), trading off
pattern uniformity for zero allocation. The decision is per-call-site
and is documented in note 06 §6.

## 5. Query DSL

```mochi
let big = from u in users
          where u.age > 30
          orderby u.age desc
          select u.name
```

The query DSL is a first-class language form; it is not LINQ syntax
sugar at the Mochi source level, but it lowers to LINQ in the .NET
target almost mechanically. The above becomes:

```csharp
var big = users
    .Where(u => u.Age > 30)
    .OrderByDescending(u => u.Age)
    .Select(u => u.Name)
    .ToImmutableList();
```

The .NET target has a major advantage here: LINQ is the canonical .NET
query API, every `IEnumerable<T>` has the full operator set, and the
JIT inlines them well. The backend does *not* emit C# query syntax
(`from u in users where u.age > 30 ...`) because the query syntax
restricts the operator set; method-syntax LINQ covers more operators
including `Aggregate`, `GroupJoin`, `Zip`, `SelectMany` with index, etc.

Operators that lower directly:

| Mochi clause | LINQ |
|--------------|------|
| `where p` | `.Where(x => p)` |
| `select e` | `.Select(x => e)` |
| `orderby k` / `orderby k desc` | `.OrderBy(k)` / `.OrderByDescending(k)` |
| `take n` | `.Take(n)` |
| `drop n` | `.Skip(n)` |
| `distinct` | `.Distinct()` |
| `group by k` | `.GroupBy(k)` |
| `join` | `.Join(...)` or `.GroupJoin(...)` |

For parallel execution the backend can switch the source to
`AsParallel()` and use PLINQ. The dataflow detection rules live in
note 08.

For very large datasets the backend lowers to `IAsyncEnumerable<T>`
plus `System.Linq.Async` extension methods (from the
`System.Linq.Async` NuGet package), giving back-pressure-friendly
streaming. See note 08 §4.

## 6. Stream and agent core

```mochi
agent Counter {
  state n: int = 0
  on tick() { n = n + 1 }
  on get() -> int { n }
}
```

Mochi agents are isolated, mailbox-driven actors with sequential
per-agent semantics. The .NET lowering:

- One `System.Threading.Channels.Channel<TMessage>` per agent, where
  `TMessage` is a sealed record class union over the on-handlers.
- A dispatch loop running on `Task.Run` (or a dedicated long-running
  task scheduled via `TaskCreationOptions.LongRunning`) that
  `await`s `channel.Reader.ReadAsync()`.
- Each `on handler(args)` call from a Mochi caller becomes
  `channel.Writer.WriteAsync(new TickMsg(...))`. For request/response
  handlers (`on get() -> int`), the message carries a
  `TaskCompletionSource<int>` and the caller awaits its `Task`.

This is the canonical .NET pattern for actor-style concurrency since
`Channels` shipped (.NET Core 3.0). It is not as tightly integrated as
Erlang/OTP (no link/monitor in the BCL; the backend builds a small
supervisor in `Mochi.Runtime.Agents`) but it is more ergonomic than the
JVM Loom approach used in MEP-47 because async/await is a first-class
language feature in C#.

The agent core is the topic of note 09.

### 6.1 Streams

Mochi streams (`stream<T>`) are cold, pull-based, demand-driven
sequences. Lower to `IAsyncEnumerable<T>` (C# 8). The Mochi stream
combinators (`.map`, `.filter`, `.take`, `.window`) lower to extension
methods on `IAsyncEnumerable<T>` from the `System.Linq.Async` package
or hand-written in `Mochi.Runtime.Streams` if the operator is not in
the canonical set.

## 7. Logic-programming core

Mochi has a Prolog-flavoured logic sub-language (predicates,
unification, backtracking). The .NET backend lowers it the same way
MEP-45 and MEP-47 do: a small WAM-style runtime in
`Mochi.Runtime.Logic` plus per-predicate generated dispatch tables.
The CLR's reified generics make the term representation cleaner than
on the JVM (no type erasure casts at every unification site), but the
overall structure is identical to MEP-47 note 01 §7. See note 06 §9 for
the term representation.

## 8. AI and FFI shells

### 8.1 AI shell

`ai.chat`, `ai.embed`, and friends are Mochi spec surfaces with
runtime-side implementations that hit external HTTP services. The
.NET backend lowers them to async calls into
`Mochi.Runtime.Ai.IAiClient`, which has implementations in
`Mochi.Runtime.Ai.Anthropic`, `Mochi.Runtime.Ai.OpenAi`, etc. The
client interface is `Task<...>`-based; Mochi `await ai.chat(...)` is a
direct match. Authentication and key material are read from
environment variables at runtime, never embedded in the emitted
assembly. See note 04 §7.

### 8.2 FFI

Mochi FFI calls (`extern fun foo(...) -> ...`) lower to:

- For .NET-native libraries: a direct method call on a referenced
  assembly. The `extern` declaration carries the assembly name and
  type as attributes.
- For native C libraries: `[DllImport]` P/Invoke. The Mochi type
  surface is mapped through `Mochi.Runtime.Marshal`; UTF-8 strings
  cross the boundary as `byte*` plus length, not as `LPStr`.
- For JS interop (Blazor WASM target only): `[JSImport]` from
  `System.Runtime.InteropServices.JavaScript`. This is a Phase-3
  capability; see note 07.

The FFI boundary is the security-critical surface. Per the threat
model, the default lowering refuses to load arbitrary native libraries
without a manifest entry; see note 04 §8 and the security spec.

## 9. Module system

Mochi modules are file-scoped, with `import` and `from m import x`.
Lower to C# namespaces under `Mochi.User.<ModuleName>` and to C# `using
static Mochi.User.ModuleName;` for the `from m import x` form when `x`
is a static member.

Public-by-default in Mochi; the lowering emits `public` modifiers
everywhere except for compiler-internal helpers, which are `internal`.

## 10. Diagnostics and source mapping

The .NET backend must preserve Mochi source locations through to
runtime stack traces. Two mechanisms:

- C# `#line` directives in the emitted Roslyn syntax tree, so that
  `Exception.StackTrace` shows `mochi.mc:42` rather than the synthetic
  `Mochi.User.Foo.cs:120`.
- A sidecar `.mochi-pdb.json` map that the runtime supervisor uses to
  resolve frames for the agent debugger and `Mochi.Runtime.Diag`.

For NativeAOT the `#line` directives are emitted but Roslyn's resulting
PDB is consumed by the AOT compiler; stack traces in AOT binaries
require the PDB to be shipped alongside the binary or embedded. See
note 07 §5.

## 11. Lowering obligations summary

Every form in the surface above induces an obligation on the .NET
backend:

- Mochi `int` is `long` everywhere; never silently 32-bit.
- Mochi `string` is UTF-8 at boundaries, UTF-16 inside; explicit
  encoding at every FFI hop.
- Mochi structural equality on collections is a runtime helper, not
  the C# `==` operator on `ImmutableList<T>` / `ImmutableDictionary<K,V>`.
- Mochi iteration order on maps/sets requires a wrapper (or .NET 10
  `OrderedDictionary<K,V>`); never raw `ImmutableDictionary<K,V>` in
  an observable iteration context.
- Mochi `match` exhaustiveness is enforced by an analyzer and a
  default-arm throw, not by the C# compiler alone.
- Mochi agents are channel-backed; per-agent sequentiality is
  enforced by a single dispatch loop, never by locks.
- Mochi tail-self-recursion is rewritten to loops; the CLR does not
  guarantee TCO.
- Mochi panics are `MochiPanic`, not arbitrary `Exception`; the
  supervisor relies on that subtype.

These obligations are restated in note 11 as test gates and in the
MEP-48 spec body as normative requirements.

## 12. Out of scope for this note

- Bytecode-level codegen choices (note 05).
- Per-collection representation choices (note 04).
- Generic specialisation, sum-type encoding tradeoffs (note 06).
- LINQ vs PLINQ vs IAsyncEnumerable selection (note 08).
- NativeAOT compatibility caveats (note 07).
- Per-phase test gates (note 11).
- Risks and alternative backends (note 12).

The language surface above is the contract; every other note in
MEP-48 explains *how* the backend honours it.
