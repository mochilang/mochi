# MEP-48 research note 06, Type-system lowering

Author: research pass for MEP-48 (Mochi to .NET/CLR transpiler).
Date: 2026-05-23 (GMT+7).

This note specifies how every Mochi type maps to a CLR type. It is the
companion to [[01-language-surface]] (which lists the forms) and to
[[05-codegen-design]] (which lists the IR-emission strategy). The
section ordering mirrors MEP-47 note 06 so the two notes can be diffed
to highlight CLR-vs-JVM deltas.

The single largest delta versus the JVM target is that **CLR generics
are reified**. JVM type erasure forces MEP-47 to ship per-instance
type tags and bridge methods; the CLR carries the type arguments at
runtime, so a Mochi `list<int>` is literally `List<long>` with no
boxing. This note records where that simplification shows up.

The single largest delta versus the C target is that **the CLR has a
managed heap and a GC**. We do not write a custom allocator; Mochi
heap-allocated types are CLR reference types and the runtime tracks
liveness. Value types (`struct`, `record struct`) are stack-allocated
where possible.

## 1. Primitive types

| Mochi | CLR | Boxed form | Default literal |
|-------|-----|------------|------------------|
| `int` | `System.Int64` (C# `long`) | `object` boxed `long` | `42L` |
| `float` | `System.Double` (C# `double`) | `object` boxed `double` | `3.14d` |
| `bool` | `System.Boolean` (C# `bool`) | `object` boxed `bool` | `true` |
| `string` | `System.String` | itself (reference type) | `"abc"` |
| `bigint` | `System.Numerics.BigInteger` (value struct) | itself (immutable struct) | `BigInteger.Parse("...")` |
| `bigrat` | `Mochi.Runtime.BigRat` (record struct) | itself | runtime constructor |
| `null` | `null` literal | n/a | `null` |

The key choice is `long` (64-bit) for Mochi `int`. This is *opposite
to C#'s naming*: in C# the `int` keyword is the 32-bit type. The
transpiler must never lower Mochi `int` to C# `int`; that is a silent
narrowing bug. Even at literal level: `let x: int = 0` lowers to
`long x = 0L;`, never `int x = 0;`.

`BigInteger` is `System.Numerics.BigInteger`, a value struct that
ships in the BCL since .NET Framework 4.0 / .NET Core 1.0. It is
immutable. All arithmetic operators are overloaded, including `^`,
`>>`, `<<`, and there is a `BigInteger.Pow(BigInteger, int)` static
method. Mochi `**` lowers to `BigInteger.Pow` for `bigint`,
`Math.Pow` for `float`, and the helper `Mochi.Runtime.IntPow` for
`int` (binary exponentiation, no allocations).

`BigRat` is a Mochi-supplied record struct: `record struct BigRat(BigInteger Num, BigInteger Den)`,
normalised at construction (sign on numerator, gcd reduction). Two
`BigRat` values are structurally equal iff their normalised numerator
and denominator are equal; the record-struct auto-generated `Equals`
covers this for free.

## 2. Boxing and unboxing

CLR has full reified generics, so a `List<long>` stores unboxed `long`
slots (8 bytes per element, contiguous). Boxing only happens at:

- `object`-typed parameters (rare in Mochi-generated code).
- Variance-incompatible interface boundaries (e.g., storing a `long`
  in `IList` non-generic).
- Reflection-based invocation.

The Mochi codegen avoids all three by emitting strongly-typed
generic types end-to-end. The exception is the agent dispatch
trampoline (see [[09-agent-streams]] §3), which uses a sealed record
class union over message types; the union dispatch is a `switch`
expression, not reflection.

This is the largest performance delta versus the JVM target. On
JVM, MEP-47 must box `long` into `Long` whenever it crosses a
generic boundary because of type erasure. On CLR, no such crossing
exists; `List<long>.this[i]` returns an unboxed `long`.

## 3. Generic type instantiations

CLR generics are reified: every concrete instantiation
(`List<int>`, `List<string>`, `List<Point>`) is a distinct runtime
type with its own JIT-compiled method bodies (or in NativeAOT, its
own pre-compiled IL).

Implications for Mochi:

- **No type-tag threading.** MEP-47 had to add a `Class<T> token`
  parameter to every Mochi generic function to recover the type
  argument at runtime; MEP-48 does not. `Mochi.Runtime.Eq.Equal<T>(a, b)`
  recovers `T` via `typeof(T)` at zero cost.
- **No bridge methods.** JVM type erasure forces synthetic bridge
  methods at method overrides involving generic parameters; the
  CLR has no such requirement.
- **Specialized JIT code.** The CLR JIT specialises generic method
  bodies per value-type instantiation (`List<int>`, `List<long>`,
  `List<MyStruct>`) and shares one body across reference-type
  instantiations (`List<string>`, `List<MyClass>`). This is the
  right tradeoff for Mochi: value-typed instantiations get the
  inlined-primitive performance, reference-typed instantiations
  share code to avoid bloat.
- **Variance.** CLR has covariant/contravariant generic interfaces
  (`IEnumerable<out T>`, `Action<in T>`). The Mochi type checker is
  invariant; the lowering does not surface variance to Mochi code.
  The runtime uses variance where the BCL exposes it (e.g.,
  `IEnumerable<DerivedShape>` flowing where `IEnumerable<Shape>`
  is expected).

The Mochi runtime ships generic types: `Mochi.Runtime.Option<T>`,
`Mochi.Runtime.Result<T, E>`, `Mochi.Runtime.OrderedMap<K, V>`,
`Mochi.Runtime.OrderedSet<T>`. Their `T`, `E`, `K`, `V` carry through
to runtime.

## 4. Records and tuples

### 4.1 Mochi records

```mochi
type Point { x: int, y: int }
```

Lowers to a positional C# record class:

```csharp
public sealed record Point(long X, long Y);
```

The `sealed` modifier closes the record (Mochi records are not
extensible). Positional means `Deconstruct(out long x, out long y)`
is auto-generated, which feeds Mochi destructuring at `let
{x, y} = p`.

The auto-generated members are:

- A primary constructor accepting all fields.
- `public long X { get; init; }` per field (init-only since C# 9).
- `Equals(Point)` doing field-wise comparison.
- `GetHashCode()` doing field-wise combine.
- `==`/`!=` operators delegating to `Equals`.
- `ToString()` returning `Point { X = 1, Y = 2 }` (Mochi printer
  overrides this).
- `Deconstruct(out long X, out long Y)`.

`with` expressions create non-destructive updates: Mochi
`{ p, x: 5 }` lowers to `p with { X = 5 }`. The `with` syntax
desugars to a `<Clone>$()` call followed by setting the `init` setter
on the clone.

### 4.2 Small records as value structs

For records that are:

- ≤ 4 fields,
- all field types are value types,
- no mutation observed by the type checker,

the backend emits `record struct` instead of `record class`:

```csharp
public readonly record struct Point(long X, long Y);
```

`record struct` is stack-allocatable, has the same auto-generated
members, but lives in the stack frame when used as a local and is
copied by value when passed. For tight numeric code (geometry,
linear algebra, dataset processing) this eliminates the per-record
heap allocation.

The threshold (4 fields) is tunable; the cutoff is where the
struct exceeds two cache lines (~64 bytes per line). The decision
is per-record-type at codegen time.

`readonly record struct` (the `readonly` keyword on the struct
declaration) is preferred: it guarantees all fields are immutable
and lets the JIT skip defensive copies. Mochi records are immutable
by spec, so `readonly` is always safe.

### 4.3 ValueTuple for ad-hoc tuples

C# has `System.ValueTuple<...>` (the underlying type behind
`(int, string)` syntax) which is a value struct with named fields.
Mochi tuples carry field names, so they lower to a named record
struct, not raw `ValueTuple`. ValueTuple is used internally by the
runtime for short-lived intermediates (e.g., the dataset hash-join
key builder returns `ValueTuple<...>` for the join key).

## 5. Sum types (discriminated unions)

```mochi
type Shape =
  | Circle { r: float }
  | Square { side: float }
  | Rect   { w: float, h: float }
```

Lowers to a sealed abstract base record plus per-variant sealed
records:

```csharp
[JsonPolymorphic(TypeDiscriminatorPropertyName = "$kind")]
[JsonDerivedType(typeof(Circle), "Circle")]
[JsonDerivedType(typeof(Square), "Square")]
[JsonDerivedType(typeof(Rect),   "Rect")]
public abstract record Shape;
public sealed record Circle(double R)               : Shape;
public sealed record Square(double Side)            : Shape;
public sealed record Rect  (double W, double H)     : Shape;
```

### 5.1 Exhaustiveness

C# does not enforce exhaustiveness on switch expressions over
sealed hierarchies (unlike Java's JEP 441 sealed-interface
exhaustiveness). The backend uses two mechanisms:

1. **A Roslyn analyzer** shipped in the `Mochi.Runtime.Analyzers`
   NuGet package. The analyzer reads the Mochi-generated
   `[MochiUnion]` attribute on the base type, walks the switch
   expression's arms, and reports `MOCHI001: non-exhaustive
   match` as a compile error (severity Error, not Warning, to
   match Mochi spec semantics).
2. **A runtime fall-through arm** in every emitted switch:

   ```csharp
   _ => throw new MochiMatchExhaustivityError(shape),
   ```

   This is a defence in depth: if a downstream user adds a new
   variant by hand-editing the generated code (which they
   shouldn't), the runtime catches it.

The analyzer also flags unreachable arms (a `Rect` arm after
`_` arm).

### 5.2 Pattern matching

Mochi `match` lowers to C# 8 switch expressions:

```csharp
var area = shape switch
{
    Circle(var r)        => Math.PI * r * r,
    Square(var s)        => s * s,
    Rect  (var w, var h) => w * h,
    _ => throw new MochiMatchExhaustivityError(shape),
};
```

For guards (`Circle { r: var r } if r > 0`) the lowering uses C#'s
`when` clause:

```csharp
Circle(var r) when r > 0 => ...,
```

For nested patterns (e.g., a `Result<Option<int>, string>`), the
deconstruction nests:

```csharp
result switch
{
    Ok<Option<long>, string>(Some<long>(var v)) => v,
    Ok<Option<long>, string>(None<long>())      => 0L,
    Err<Option<long>, string>(var e)            => throw new MochiError(e),
};
```

C# 9+ supports nested record-pattern deconstruction, so this works
out of the box.

### 5.3 The "None" generic-arity problem

Mochi `type Option<T> = | Some { value: T } | None` has a tricky
encoding: `None` has no fields, but it must still be parameterised
on `T` so that `None: Option<int>` and `None: Option<string>` are
distinct runtime types (matching reified-generic semantics).

The lowering:

```csharp
public abstract record Option<T>;
public sealed record Some<T>(T Value) : Option<T>;
public sealed record None<T>()        : Option<T>;
```

Mochi `None` (with inferred `T`) becomes `new None<long>()` (or
whichever `T` the inference resolves). To avoid per-call allocation
for `None`, the backend lazily caches a `static readonly None<T>
Instance` per `T` instantiation in a `Mochi.Runtime.NoneCache<T>`
generic class. (CLR generic statics are per-instantiation by
default, which is exactly what we want.)

### 5.4 Option<T> as nullable value type fast path

For `Option<int>`, `Option<long>`, `Option<double>`, `Option<bool>`
where the inner type is a value type, the backend may lower to
`long?`, `long?`, `double?`, `bool?` instead of the
`Option<long>` record hierarchy. The decision is per-call-site,
driven by a flow analysis pass:

- If the value flows only through pattern-matching arms that are
  expressible as null-checks (`is null` / `is not null`), use
  nullable.
- If the value flows into a generic context expecting `Option<T>`
  (e.g., passed to `Mochi.Runtime.Option.Map`), use the record
  hierarchy.

The lowering inserts adapter code at the boundary
(`x is long v ? new Some<long>(v) : new None<long>()` and the
reverse).

## 6. Result<T, E>

```mochi
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Lowers analogously:

```csharp
public abstract record Result<T, E>;
public sealed record Ok<T, E>(T Value)    : Result<T, E>;
public sealed record Err<T, E>(E Error)   : Result<T, E>;
```

The runtime ships extension methods:
`Result<T,E>.Map<U>(Func<T, U>) -> Result<U, E>`,
`Result<T,E>.MapErr<F>(Func<E, F>) -> Result<T, F>`,
`Result<T,E>.AndThen<U>(Func<T, Result<U, E>>) -> Result<U, E>`,
`Result<T,E>.Unwrap() -> T (throws on Err)`.

For `Result<T, MochiPanic>` (the most common shape), the runtime
provides a `?` operator analogue via a `Try` source generator that
rewrites `let v = expr?` into the lowered try-pattern form. This is
a Phase-2 sugar; v1 requires explicit `match` on Result.

## 7. Closures and function types

Mochi function type `fun(int, string): bool` lowers to
`Func<long, string, bool>`. Mochi `fun(int)` (no return) lowers to
`Action<long>`. Mochi `fun(): T` lowers to `Func<T>`.

For arity > 16 (the max BCL `Func`/`Action` arity), the runtime
ships `Mochi.Runtime.Func17<...>` etc. up to arity 32. Beyond 32
the codegen emits a record class with a single `Invoke(args)` method;
practically no Mochi program hits this.

Closures with no captures lower to lambda expressions that the C#
compiler caches in `static readonly Func<...> <>9__0_0` fields,
allocated once per process. The compiler does this automatically
for lambdas with no captures since C# 7.

Closures with captures allocate a closure class (`<>c__DisplayClass0_0`)
that holds the captured locals as fields. Mochi's "capture mutable"
semantics match C# closure semantics (the closure shares the
mutable cell with the enclosing scope).

## 8. Collections

### 8.1 list<T> immutable default

`list<T>` lowers to `System.Collections.Immutable.ImmutableList<T>`
(B-tree backed, O(log n) most ops). Operations:

| Mochi | .NET |
|-------|------|
| `[a, b, c]` | `ImmutableList.Create<long>(a, b, c)` |
| `xs[i]` | `xs[i]` (C# indexer, O(log n)) |
| `xs[i..j]` | `xs.GetRange(i, j - i)` |
| `len(xs)` | `xs.Count` |
| `xs + ys` | `xs.AddRange(ys)` |
| `xs + [v]` | `xs.Add(v)` |
| `len(xs)` | `xs.Count` |
| `for x in xs` | `foreach (var x in xs)` |

For dense numeric workloads the backend may upgrade to
`ImmutableArray<T>` (array-backed, O(1) index, O(n) update). The
heuristic: lists declared inside a hot loop that are never modified
post-construction. Detected by the dataflow pass in
[[05-codegen-design]] §9. The choice is per-allocation, not per-type.

### 8.2 list<T> mutable under var

If a list binding is `var xs`, the backend has two options:

1. Keep `ImmutableList<T>` and rebind: `xs = xs.Add(v);` per
   modification. O(log n) per add, no allocation-amortisation.
2. Switch to `List<T>` (mutable) for the binding. O(1) amortised
   add, allocates a backing array.

The heuristic: if the list has more than ~8 modifications in the
binding's lifetime, switch to mutable `List<T>`. Otherwise keep
immutable. See [[04-runtime]] §2.

The lowering wraps the choice in a `Mochi.Runtime.MochiListBuilder<T>`
type when the choice is ambiguous (e.g., a list inside a method that
may be called in either context).

### 8.3 map<K, V>

Immutable default: `ImmutableDictionary<K, V>`. Mutable under `var`:
`Dictionary<K, V>`.

**Iteration order.** Mochi spec requires insertion-order iteration
over maps. `ImmutableDictionary<K, V>` is unordered. The lowering:

- On .NET 10+, use `System.Collections.Generic.OrderedDictionary<K, V>`
  (added in .NET 9 preview, GA in .NET 9). This is a mutable
  insertion-ordered dictionary with O(1) lookup.
- On .NET 8, ship `Mochi.Runtime.OrderedMap<K, V>` in the runtime
  package. Internal layout: a `Dictionary<K, int>` for key-to-
  index, a `List<KeyValuePair<K, V>>` for ordered storage.

For pure-immutable insertion-ordered maps the runtime ships
`Mochi.Runtime.ImmutableOrderedMap<K, V>` (a hand-written immutable
variant since the BCL has no `ImmutableOrderedDictionary` as of .NET 10).

When iteration order is NOT observable (the map is only used for
membership tests), the backend falls back to plain
`ImmutableDictionary<K, V>` or `Dictionary<K, V>` for performance.
The flow analysis pass detects this; see [[05-codegen-design]] §9.

### 8.4 set<T>

`ImmutableHashSet<T>` for immutable default; `HashSet<T>` for mutable
under `var`. Insertion-order parallel: `Mochi.Runtime.OrderedSet<T>`.

### 8.5 FrozenDictionary / FrozenSet

For maps/sets that are constructed once at module init and never
modified (e.g., a lookup table built from a literal), the backend
upgrades to `System.Collections.Frozen.FrozenDictionary<K, V>` or
`FrozenSet<T>` (added .NET 8). These are read-only collections
optimised for lookup throughput (sub-`Dictionary` lookup time).

The detection is conservative: only literals of all-constant keys
qualify. See [[04-runtime]] §3.

## 9. Strings

Mochi strings are UTF-8 in the spec. C# `string` is UTF-16 at
runtime. The boundary policy:

- **Source-level string literals** lower verbatim. The C# compiler
  emits them as UTF-16 in the metadata; the Mochi runtime converts
  at I/O boundaries.
- **`len(s)` returns the UTF-8 byte count**, not the UTF-16
  code-unit count. The lowering is
  `Encoding.UTF8.GetByteCount(s)` cached on the string when
  beneficial (via a `ConditionalWeakTable<string, int>` in the
  runtime). For ASCII hot paths the compiler optimisation pass
  detects `s.All(c => c < 128)` and falls back to `s.Length`.
- **`s[i]` is UTF-8 code-point access**. Lowers to
  `Mochi.Runtime.Str.CodepointAt(s, i)`, which decodes UTF-8 from
  the UTF-16 representation (since the BCL stores UTF-16) and
  returns the i-th codepoint. For ASCII the optimisation reduces
  to `(long)s[i]`.
- **`s + t`** lowers to `string.Concat(s, t)`, which the C#
  compiler already emits.
- **`for c in s`** lowers to a `foreach` over
  `Mochi.Runtime.Str.Codepoints(s)`, which yields one `long` per
  UTF-8 codepoint.

The boundary at I/O (`print`, file read, file write, JSON, HTTP)
explicitly encodes/decodes UTF-8 via `Encoding.UTF8.GetBytes` and
`Encoding.UTF8.GetString`. The `print` function writes
`Console.OpenStandardOutput()` directly with UTF-8 bytes, avoiding
the default `Console.Out` encoding (which is platform-dependent).

For hot paths the runtime exposes `ReadOnlySpan<byte>` over the
UTF-8 byte sequence, avoiding allocation. The Mochi surface does
not expose Span directly; the optimisation is transparent.

## 10. Span<T>, Memory<T>, ref struct constraints

`Span<T>` (`ref struct`) and `Memory<T>` (regular struct, points
into managed or unmanaged memory) are used by the runtime for
zero-copy hot paths. The Mochi surface does not expose them.

Constraints the runtime respects:

- A `Span<T>` cannot be a field of a class.
- A `Span<T>` cannot be captured by a lambda.
- A `Span<T>` cannot cross an `await` boundary.
- A `Span<T>` cannot be stored in a generic `T` slot (since `T`
  could be a reference type).

Mochi-generated code never produces a `Span<T>` directly. The
runtime uses Span internally (e.g., string parsing, hash-join key
construction) and converts to/from heap types at the boundary.

## 11. Logic-programming terms

The Mochi logic sub-language uses Prolog-style terms (atoms,
integers, lists, compound terms, variables). The runtime
representation:

```csharp
public abstract record Term;
public sealed record Atom(string Name)               : Term;
public sealed record IntTerm(long Value)             : Term;
public sealed record StrTerm(string Value)           : Term;
public sealed record Compound(string F, ImmutableArray<Term> Args) : Term;
public sealed record Var(int Id, string Hint)         : Term;
public sealed record ListTerm(ImmutableArray<Term> Items) : Term;
```

Unification operates on these records. The `Var` carries a hint
name for debugging only; identity is via `Id`. The substitution is
an `ImmutableDictionary<int, Term>`.

Reified generics let us write the unification algorithm directly
without per-term type tags. The MEP-47 note 06 §9 workaround
(threading a `Class<Term>` token) is unnecessary.

## 12. Type erasure of phantom parameters

A few Mochi patterns produce phantom type parameters (parameters
that appear in the type but not the value, e.g., `type Tagged<T> { value: long }`).
CLR reified generics make these zero-cost at runtime, but at the
codegen level the backend emits the phantom `T` and lets the JIT
specialise. No erasure pass needed.

## 13. Nullable annotation (C# Nullable Reference Types)

C# 8 added Nullable Reference Types (NRT), a flow-sensitive
nullability tracker. The Mochi codegen runs in
`#nullable enable` mode and emits explicit nullability:

- Mochi `T` (non-null in the type system) lowers to non-nullable
  `T` in C# (e.g., `string`, `Point`).
- Mochi `Option<T>` (the explicit option type) lowers to the
  `Option<T>` record hierarchy.
- Mochi `T?` (sugar for `Option<T>`) where `T` is a value type
  may lower to `T?` (nullable value type) for the fast path; for
  reference types it always lowers to `Option<T>` to avoid
  conflating "absent" with "null".

The lowering does not emit C# `T?` for reference types, because
that would surface NRT semantics (a `string?` is "may be null", not
"is an Option of string"). Mochi semantics require explicit
Option, not implicit null.

## 14. Async/await colouring

The async colouring pass (described in [[02-design-philosophy]] §13
and [[05-codegen-design]] §12) determines whether a Mochi function
lowers to a sync or async C# method. The decision propagates
transitively: any function that calls a red (async) function is
itself red.

The lowering shape:

- **Sync function:** `public static T Foo(args) { body }`.
- **Async function:** `public static async Task<T> FooAsync(args) { body }`
  (or `ValueTask<T>` for hot paths).
- **Sync void:** `public static void Bar(args) { body }`.
- **Async void-returning:** `public static async Task Baz(args) { body }`.
  Never `async void` (which would suppress exception propagation).

At call sites:

- sync-to-sync: ordinary call.
- async-to-async: `await Foo(args)`.
- sync-to-async: not allowed by colouring; either the caller is
  async, or the callee is sync.
- async-to-sync: ordinary call (sync callee inside async caller).

The "Async" suffix follows BCL convention (.NET ecosystem expects
`FooAsync` for `Task`-returning methods). The Mochi-to-C# name map
in the PDB sidecar records both names.

## 15. Module-level lowering

Each Mochi module lowers to one C# file containing one or more
classes:

- A `public static class <ModuleName>` for the module's top-level
  functions and constants.
- One `public sealed record <TypeName>` per Mochi type definition.
- One sealed-hierarchy group per sum type.

The module class is `static` (cannot be instantiated) and lives in
the `Mochi.User.<PackageName>` namespace. Imports map to `using
Mochi.User.<OtherModule>;` or `using static
Mochi.User.<OtherModule>;` for the `from m import f` form.

Module-level `let` lowers to `public static readonly T Name = expr;`.
Module-level `var` lowers to `public static T Name = expr;`. Both
are init-once in the static constructor.

## 16. Lowering obligations summary

- Mochi `int` is C# `long`; never C# `int`.
- Mochi `string` is UTF-8 at boundaries; the runtime converts
  to/from UTF-16 at I/O.
- Mochi small records (≤ 4 value-type fields) become
  `readonly record struct`.
- Mochi large records become `sealed record class`.
- Mochi sum types become `abstract record` + `sealed record` per
  variant; exhaustiveness via analyzer plus default-arm throw.
- Mochi generic types lower directly (reified generics, no type-
  tag threading, no bridge methods).
- Mochi maps with observable iteration order use
  `OrderedDictionary<K,V>` on .NET 10 or `Mochi.Runtime.OrderedMap`
  on .NET 8.
- Mochi `Option<T>` over value types may lower to `T?` for the
  fast path; otherwise to the `Option<T>` record hierarchy.
- Mochi closures with no captures lower to compiler-cached static
  delegate fields; with captures, to a per-allocation closure
  class.
- The async colouring pass decides sync vs async per function;
  callers and callees stay synchronised by transitive propagation.

These obligations are restated in [[11-testing-gates]] as test gates
and in the MEP-48 spec body as normative requirements.
