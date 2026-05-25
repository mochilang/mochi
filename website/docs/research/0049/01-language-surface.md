# MEP-49 research note 01, Mochi language surface (Swift target)

Author: research pass for MEP-49 (Mochi to Swift transpiler).
Date: 2026-05-23 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`-`v0.7`, the normative
security specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`,
and the companion notes MEP-45 note 01 (C target), MEP-46 note 01 (Erlang/BEAM),
MEP-47 note 01 (JVM), and MEP-48 note 01 (.NET), whose section structure this
note deliberately mirrors so all five backends can be diffed line for line.

This note records the user-visible language surface that the Swift target must
faithfully reproduce. It is deliberately written *from the spec downward* and
ignores the existing Go runtime (vm3), the vm3 bytecode, the C target under
MEP-45, the Erlang/BEAM target under MEP-46, the JVM target under MEP-47, the
.NET target under MEP-48, and any other backend implementation. The goal is a
transpiler design that would be correct against the *language*, not against the
present implementations.

The surface decomposes into the same eight orthogonal sub-languages identified
in MEP-45 note 01, MEP-46 note 01, MEP-47 note 01, and MEP-48 note 01: (1) the
value core, (2) the function and method core, (3) the collection core, (4) the
algebraic-data-type core, (5) the query DSL, (6) the stream and agent core,
(7) the logic-programming core, and (8) the AI and FFI shells. Each section
below names every form a Mochi program can write, then states a *lowering
obligation* the Swift backend must honour.

Where MEP-45 maps Mochi types to C struct plus helper-function pairs, MEP-46
maps them to BEAM terms (atoms, tagged tuples, maps, binaries, funs, PIDs),
MEP-47 maps them to JVM values (Java primitives, boxed numerics, records,
sealed interfaces, lambdas, Loom virtual threads), and MEP-48 maps them to
.NET values (CLR primitives, immutable collections, C# 12 records,
discriminated unions), this note maps them to Swift values: native scalars
(`Int64`, `Double`, `Bool`), value-type `String` (UTF-8 native since Swift
5.7), `Array<T>`, `Dictionary<K,V>`, `Set<T>`, `OrderedDictionary` and
`OrderedSet` from `swift-collections`, `struct`s with `@frozen` where the
layout is stable, `enum` with associated values for sum types, closures
typed as `(In) -> Out` with `@Sendable` for actor-crossing variants, Swift
`actor` types for the agent core, `AsyncStream<T>` and `AsyncSequence` for
the stream core, and typed throws (`throws(E)`) for the error core. The
target IR is discussed in note 05 (the default path emits Swift source via
`SwiftSyntax` builders, with a small set of hot lowerings emitted as
direct SIL only when the round-trip through the Swift parser would be
demonstrably wasteful); the runtime is the Swift standard library plus the
Apple `swift-collections`, `swift-algorithms`, `swift-async-algorithms`,
`swift-numerics`, `swift-system`, and `swift-log` packages, plus a thin
`MochiRuntime` Swift package (see note 04). Throughout, "Swift" means
Swift 6.0 (released 2024-09 alongside Xcode 16, the first release with
complete strict-concurrency checking, region-based isolation, typed
throws, and noncopyable generics) and later. Swift 5.x is explicitly out
of scope; we do not back-port any feature usage to 5.10. Apple platforms
covered are iOS 17+, macOS 14+, watchOS 10+, tvOS 17+, and visionOS 1+
(the matrix where Swift 6.0 toolchains are supported by Xcode 16). Linux
and Windows are first-class via the official swift.org toolchains; we run
the same source through `swift build` on all three OS families in CI.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time** error,
  not a runtime panic. The Swift lowering maps directly onto Swift's own
  `let`: Swift `let` is single-assignment and the Swift compiler itself
  rejects any subsequent assignment. This is the cleanest one-to-one
  mapping in the entire backend matrix (C needed `const`, Erlang/BEAM
  required pattern-match rebinding, Java needed `final`, C# needed
  `readonly` plus single-assignment discipline). For module-level lets
  the backend emits `static let` on the module's namespacing enum (see
  §2.1 on module shape), which Swift makes thread-safe and lazily
  initialised by default.
- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope. Lowers to Swift `var`. Inside an `actor` the
  `var` is automatically isolated to that actor (see §13 on concurrency
  colouring). Inside a `struct` method the `var` field requires the
  method to be marked `mutating`; the backend handles this automatically.

Mochi blocks are expressions in the sense that the last expression is the
block's value. Swift blocks (statement blocks `{ ... }`) are statements,
but Swift has *result-builder closures* and immediately-invoked closures
to bridge the gap. The backend lowers a block whose value is consumed
into a Swift immediately-invoked closure: `let t = { ...; return e }()`
for the general case, and uses Swift's `if`/`switch`/`do` expressions
(available since Swift 5.9, refined in 6.0 with full statement coverage
under SE-0380 and SE-0427) directly for the common forms. So `let x = if
cond { a } else { b }` becomes a single Swift `if` expression without a
helper closure. See note 05 §6 for the full block-lowering table.

A binding may carry an explicit type: `let x: int = 0`. Swift is statically
typed; the type survives end-to-end. Mochi `int` lowers to Swift `Int64`
(see §1.2 on why we pick `Int64` over `Int`), so `let x: int = 0` becomes
`let x: Int64 = 0`.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

Swift has tuple destructuring at `let` and pattern-match destructuring
inside `switch` / `if case`, but it does not destructure arrays or
dictionaries at the binding site. The list pattern lowers to a positional
read with a runtime length check:

```swift
let __tmp: [Int64] = [1, 2]
guard __tmp.count == 2 else { throw MochiPatternError.arity(expected: 2, got: __tmp.count) }
let a: Int64 = __tmp[0]
let b: Int64 = __tmp[1]
```

The map pattern lowers to `Dictionary` subscript with a `guard let` per
key (since the pattern asserts the keys exist, and Swift's subscript
returns `Optional`). For struct types the backend uses Swift's `Mirror`
fallback only for the debug build; the release build emits direct field
reads (`let n = person.name`). See note 05 §11 for the full destructuring
strategy.

Scoping is lexical and block-based. Inner blocks shadow outer bindings.
Swift allows shadowing of locals in inner scopes (unlike Java or C#), so
the backend emits Mochi names directly without rename mangling for the
shadowing case. Reserved-word collisions are handled with backticks
(`` `class` ``, `` `protocol` ``); see §1.6.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the Swift-side representation:

| Mochi | Width / semantics | Swift lowering |
|-------|------------------|-----------------|
| `int` | 64-bit signed integer (inferred from integer literals) | `Int64` (explicitly, not `Int`) |
| `float` | 64-bit IEEE 754 double | `Double` |
| `bool` | `true` / `false` | `Bool` |
| `string` | UTF-8 text, indexable as code points, immutable | `String` (UTF-8 native since SE-0334 in Swift 5.7) |
| `time` | absolute timestamp (used by streams) | `Foundation.Date` plus a `MochiInstant` wrapper for nanosecond resolution where needed |
| `duration` | time interval (`std/time` API) | `Foundation.TimeInterval` for the simple case, `Swift.Duration` (since Swift 5.7, SE-0329) for clock-aware code |
| `image` (preview) | binary blob (`load "cat.png"`) | `Foundation.Data` wrapped in `MochiImage` struct |

Why `Int64` and not `Int`? Swift's `Int` is *platform-dependent*: it is
64-bit on 64-bit platforms and 32-bit on 32-bit platforms (per The Swift
Programming Language §"Integers"). Apple's current Watch series ships
with 64-bit chips (Apple S6 and later); however, watchOS armv7k devices
and embedded Swift on 32-bit ARM Cortex-M still exist, and the
`x86_64-w64-mingw32` Windows toolchain compiles for both bit-widths in
principle. Mochi's `int` is documented as 64-bit signed regardless of
host. Using Swift `Int` would silently truncate on a 32-bit target;
`Int64` is unambiguous. The cost is that array indices into Swift
`Array<T>` require `Int` and so become explicit `Int(i)` conversions at
the boundary (which trap on overflow). See note 06 §5 for the runtime
guard policy.

Implicit numeric conversions are **not** allowed (per the type-checker
discipline implied by MEP-4/5/6 referenced from the threat model).
`int + float` is a Mochi type error; the program must `float(x)` first.
Swift's strong type system aligns perfectly here: `Int64 + Double` is
*also* a Swift compile error (Swift requires explicit `Double(x)`
conversion). The Mochi checker and the Swift checker reinforce each
other; the backend never has to emit mixed arithmetic because both
layers reject it. See [[02-design-philosophy]] §3 on the "two-layer
type wall" advantage.

Integer overflow in Swift traps by default (Swift's `+`, `-`, `*` panic
on overflow with `EXC_BREAKPOINT` / SIGTRAP). Mochi's documented
semantic is silent two's-complement wrap-around. The default lowering
therefore emits Swift's wrap-around operators `&+`, `&-`, `&*` so
overflow silently wraps (matching the C and JVM defaults). The
`--strict-int` build flag flips this to the trapping forms `+`, `-`,
`*` for security-sensitive builds. Off by default, on for builds that
opt into the audit profile. See note 06 §5.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean `&& || !`;
membership `in`; string concatenation overloads `+`.

| Mochi | Swift |
|-------|-------|
| `a + b` (int)    | `a &+ b` (both `Int64`, wrap-around by default) |
| `a + b` (float)  | `a + b` (both `Double`; IEEE NaN propagates) |
| `a + b` (string) | `a + b` (Swift `String + String` returns a fresh value-type `String`) |
| `a + b` (list)   | `a + b` (Swift `Array<T> + Array<T>` returns a fresh array) |
| `a - b` | `a &- b` (int) / `a - b` (float) |
| `a * b` | `a &* b` (int) / `a * b` (float) |
| `a / b` (float) | `a / b` |
| `a / b` (int)   | `Int64(Double(a).rounded(.down) / Double(b).rounded(.down))` is wrong; use `MochiRuntime.floorDiv(a, b)` which implements Python-style floor division for negative dividends (Mochi's documented semantic) |
| `a % b` | `MochiRuntime.floorMod(a, b)` (Swift's `%` is truncated remainder, not floored) |
| `a == b` (primitive) | `a == b` (Swift `Equatable` on scalars is identity-free value equality) |
| `a == b` (reference) | `a == b` (Swift `Equatable` on struct / enum is *value* equality by default; the synthesised conformance compares field by field) |
| `a != b` | `a != b` |
| `a < b`, `<=`, `>`, `>=` | numeric: native; string: `a < b` uses `Comparable` lexicographic order over Unicode scalars |
| `a && b` | `a && b` (short-circuit) |
| `a \|\| b` | `a \|\| b` |
| `!a` | `!a` |
| `x in xs` (list) | `xs.contains(x)` |
| `x in m` (map) | `m[x] != nil` (or `m.keys.contains(x)`) |
| `x in s` (set) | `s.contains(x)` |

The lowering must respect Swift's separation between `==` (value
equality via `Equatable`) and `===` (reference identity, only valid on
`AnyObject`-bound class types). Mochi has no reference identity, so the
backend never emits `===` in user code. The exception is FFI handles
that wrap Apple framework class types (`NSString`, `URLSession`), where
`===` may appear in helper bridges but never in Mochi source lowerings.

### 1.4 Strings as read-only code-point sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a `Character`). Iteration
yields 1-character strings in **code-point** order, not byte or UTF-16
code-unit order. Swift's `String` is the cleanest fit among all five
backends because:

- Swift's `String` is **UTF-8-native** since Swift 5.7 (SE-0334
  "Unlock existing API for UTF-8"). The internal storage is `UTF8.Code`
  bytes, exactly matching Mochi's wire form.
- Swift's `String.Index` is opaque, so the backend cannot subscript by
  `Int` directly. Lowering goes through `text.unicodeScalars` (for
  code-point semantics) or `text.utf8` (for byte semantics) depending
  on context.
- Iteration over `for c in text` walks *grapheme clusters* by default,
  not code points. Mochi specifies code points, so the lowering iterates
  `text.unicodeScalars` instead.

Concretely:

- `text[i]` lowers to `MochiRuntime.Strings.codePointAt(text, i)` which
  returns a single-character `String` formed from the `i`-th
  `Unicode.Scalar`. The runtime caches the `String.Index` advance to
  avoid repeated O(n) seeks inside loops.
- `for ch in text` lowers to `for cp in text.unicodeScalars { let ch =
  String(cp); ... }` so each iteration sees a one-scalar string.
- `len(text)` lowers to `text.unicodeScalars.count`, not `text.count`
  (which is grapheme-cluster count) and not `text.utf8.count` (which
  is UTF-8 byte count). The two alternative lengths are exposed as
  `grapheme_len` and `utf8_byte_len` in `MochiRuntime` for callers
  that explicitly want them.

This is the area where the Swift target is *cheapest* relative to the
JVM target: on the JVM the internal storage is Latin-1 or UTF-16 (JEP
254), so every code-point access pays a conversion cost. On Swift 5.7+
the bytes are already UTF-8, so boundary cost is essentially zero. See
[[02-design-philosophy]] §6 on why Swift is the best fit for
text-heavy Mochi workloads.

### 1.5 Literals

Integer literals; floating literals (`3.14`); boolean; string with C-style
escapes; triple-quoted multi-line strings (`"""..."""`); list `[...]`;
map `{key: val, ...}`; set `{a, b, c}`; record constructor `T { field: val }`.

The set literal `{a, b, c}` is distinguished from the empty/map literal
`{}` by the absence of `:` after the first element. The grammar must keep
these unambiguous; the Swift lowering picks the right constructor accordingly.

Lowering forms:

| Mochi | Swift |
|-------|-------|
| `42` | `Int64(42)` (the literal `42` is `IntegerLiteralType` which defaults to `Int`; we force `Int64` via type annotation or explicit cast) |
| `3.14` | `3.14` (Swift's `FloatLiteralType` defaults to `Double`, which is what we want) |
| `true` / `false` | `true` / `false` |
| `"hello"` | `"hello"` |
| `[1, 2, 3]` | `[Int64(1), Int64(2), Int64(3)] as [Int64]` (Swift array literal infers element type) |
| `"""multi\nline"""` | Swift multi-line string literal `"""\nmulti\nline\n"""` (Swift's triple-quote rules differ slightly on leading whitespace; the backend normalises) |
| `{"a": 1, "b": 2}` | `["a": Int64(1), "b": Int64(2)] as [String: Int64]` (Swift dictionary literal, which is by default `Dictionary<K,V>` and therefore *unordered*; if the surrounding type is an ordered map we lower to `OrderedDictionary<K,V>` from swift-collections instead, see §3) |
| `{1, 2, 3}` (set) | `Set<Int64>([1, 2, 3])` (or `OrderedSet<Int64>` if the surrounding type is ordered) |
| `Book { title: "X", pages: 10 }` | `Book(title: "X", pages: 10)` (Swift struct memberwise initialiser; see §4 on struct codegen) |

Swift array literals are mutable by default but become immutable when
bound via `let`. The lowering relies on this: every Mochi literal is
bound through `let` (or passed directly to a function) and so cannot
be mutated. For collections that *do* mutate (a `var` list with
`.add()` calls) the backend emits explicit `var` and uses Swift's
copy-on-write semantics, which `Array`, `Dictionary`, and `Set` all
have built in (SE-0001-era design, formalised in The Swift Programming
Language Reference §"Collection Types").

### 1.6 Identifier mangling

Swift identifiers may begin with letter or `_` and continue with
letter / digit / `_`. Mochi identifiers are stricter so every Mochi
identifier is a legal Swift identifier *until* it collides with a Swift
keyword. Swift has a clean escape: backticks. `` `class` ``, `` `let` ``,
`` `protocol` `` are all legal Swift identifiers when wrapped in
backticks, and the backticks vanish at the use site.

Mangling rules (full table in note 06 §2 and §3):

- Mochi variables that collide with a Swift reserved word are wrapped in
  backticks (`` `class` ``, `` `protocol` ``, `` `actor` ``). The
  reservation list is the Swift Language Reference §"Keywords and
  Punctuation", augmented with Mochi-internal helpers (`$$mochi_*`).
  Swift's backtick form is more elegant than Java's suffix rename or
  C#'s `@` prefix because it does not pollute the name at the use site.
- Mochi local function references and method names use the same backtick
  rule. Camel-case is preserved (`fooBar` stays `fooBar`).
- Mochi package paths `mathutils/extra` ⇒ Swift module name
  `MochiUserMathutilsExtra` for user code (configurable via
  `--swift-module-prefix`; default `MochiUser`). The `User` segment is
  there to make the runtime / user distinction visible in stack traces
  and in Swift Package Manager dependency graphs.
- Mochi record type names ⇒ Swift `struct` names in PascalCase, unchanged
  (`Book` ⇒ `Book`). On collision with a `Swift.*` or `Foundation.*`
  type (e.g. `String`, `Array`, `Date`, `URL`) the backend renames
  `String` ⇒ `String_` and emits a module-scope `typealias` only when
  the type really is internal.
- Mochi sum-type variant constructors ⇒ Swift enum cases (lower-cased
  first letter per Swift convention, so Mochi `Leaf` becomes Swift
  `.leaf`, `Node` becomes `.node`). Field labels are preserved verbatim.
  See §4 ADT lowering.

The mangling is deterministic (note 05 §3) and reversible via Swift's
`#sourceLocation(file:, line:)` directive (which the emitter writes for
every Mochi source line, so debugger stack traces point back to Mochi
source rather than emitted Swift). See note 10 §15.

### 1.7 Nullability

Mochi has no `nil` at the language level. Optional values are expressed
via the `Option<T>` sum type. The Swift lowering has to make a deliberate
choice here, because Swift *does* have a built-in `Optional<T>` (with
syntax sugar `T?`) and the Swift community strongly prefers it for
optionality.

The decision: the Swift target **uses Swift's native `Optional<T>` for
Mochi `Option<T>`**. This is the *opposite* of the JVM target (which
emits its own sealed-interface `Option<T>` to dodge `java.util.Optional`
problems). The reasoning:

- Swift's `Optional` is a real sum type (`enum Optional<Wrapped> { case
  none; case some(Wrapped) }`), not a wrapper class as in Java. It has
  no "naked null" pitfalls.
- Swift's pattern matching (`if let`, `guard let`, `switch case .some(let
  x)`) is the idiomatic way to consume optionals; emitting our own
  `Option` type would force users (especially at FFI sites) to convert
  back and forth.
- The Apple platform APIs all return `Optional` for nullable values; if
  Mochi `Option` were a separate type, every Foundation call would need
  a bridge.

The lowering therefore maps Mochi `Option<T>` directly to Swift `T?`.
Mochi `Some(x)` becomes Swift `Optional.some(x)` (or just `x` with
implicit wrapping), Mochi `None` becomes Swift `nil`. The Mochi
`Result<T, E>` type, by contrast, is *not* mapped to anything in the
standard library directly; we emit it as a Swift `Result<T, E>` enum
(Swift Foundation has a `Result` type since Swift 5.0, SE-0235). See §4.

At the FFI boundary, any value coming in from Swift code that is typed
as `T?` is funnelled through Mochi `Option<T>`; values typed as `T`
(non-optional) bypass the wrapper. The Swift compiler enforces this
distinction statically, so no runtime check is required.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Lowers to a Swift `static func` inside a module-scope namespacing enum
(Swift's idiom for "static class"):

```swift
public enum MyModule {
  public static func add(_ a: Int64, _ b: Int64) -> Int64 { return a &+ b }
}
```

Every Mochi source file produces one Swift file named after the file
(`example.mochi` ⇒ `Example.swift`) declaring one namespacing enum
(`enum Example`) with all top-level functions as `public static` methods.
Module-level `let` and `var` become `public static let` / `public static
var` properties on the enum, initialised lazily on first access (Swift's
default for static stored properties, which is also thread-safe via
`dispatch_once` semantics).

The reason we use an enum and not a `struct` or `class` for the module
namespace is that an enum with no cases cannot be instantiated, which
matches the "namespace only" intent and prevents users from accidentally
constructing a module value. This is a well-known Swift idiom (see
SwiftLint's `no_extension_access_modifier` rule rationale and Apple's
own use of `enum Process` in Foundation).

Mochi `return` is explicit (unlike Erlang's "last expression"). The Swift
lowering preserves `return` directly: `return e;` becomes Swift `return e`.
For single-expression functions Swift allows omitting `return` (SE-0255,
"Implicit returns from single-expression functions"), but the backend
always emits `return` for consistency with multi-statement bodies and to
keep the line-number map stable. See note 05 §7 on the early-return
lowering.

The docs warn there is **no implicit tail-call optimisation** in Mochi.
The Swift compiler *does* perform TCO when the optimisation level is
`-O` or higher, but it is not guaranteed (the optimiser may choose not
to fold the call). The backend therefore does *not* rely on TCO and
emits a trampoline for any self-recursive function whose call graph
exceeds an inferred depth (threshold tunable via
`--swift-trampoline-depth`, default 1000). See note 05 §15.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Lower to Swift closures:

```swift
let square: (Int64) -> Int64 = { x in x &* x }
static func apply(_ f: (Int64) -> Int64, _ value: Int64) -> Int64 {
  return f(value)
}
```

Swift closures are *not* class instances (unlike Java lambdas or .NET
delegates); they are values with a function-type lowering that the
compiler chooses at the use site (thick context vs thin function
pointer). The optimiser inlines them aggressively when the closure body
is known at the call site, which it usually is for closures captured by
`let` and then immediately invoked.

For closures that cross actor isolation boundaries the type must be
`@Sendable`:

```swift
let f: @Sendable (Int64) -> Int64 = { x in x &* x }
```

The Swift 6.0 strict-concurrency checker (under SE-0337 "Region-based
isolation" and SE-0414 "Region-based isolation: full proposal") enforces
`Sendable` at every actor boundary. The backend infers `@Sendable` when
it can prove the closure does not capture any non-`Sendable` state, and
emits it explicitly. For closures that capture mutable Mochi `var`
bindings, the backend lifts the capture into a `Sendable`-conforming
`MochiCell<T>` reference (essentially a tiny actor) to satisfy the
checker without losing semantics. See [[02-design-philosophy]] §8 on
the trade-off between Sendable inference cost and ergonomic noise.

Closures escape freely; captured variables in Swift are by default
captured by reference (for `var`) or value (for `let`). The backend
emits explicit capture lists (`[weak self]`, `[a, b]`) only at sites
where Swift's default capture would change observed semantics, which
in practice means escaping closures that capture `self` from inside
an actor. See note 05 §16 for the capture-list policy.

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block are
unqualified. Lowering: the record is a Swift `struct` and the method is
an instance method on that struct:

```swift
public struct Circle: Sendable, Hashable, Codable {
  public let radius: Double
  public init(radius: Double) { self.radius = radius }
  public func area() -> Double { return 3.14 * radius * radius }
}
```

This is the cleanest available lowering on Swift 6.0: structs are value
types with copy-on-write for any internal reference-typed storage, they
synthesise `Equatable` / `Hashable` / `Codable` when every field
conforms (SE-0185, SE-0166, SE-0167, SE-0185-amend), and they cost
zero heap allocation when stack-allocated (which the optimiser does
aggressively for small structs).

The `@frozen` attribute (SE-0260) is added to structs that are part of
the stable ABI (i.e. exported across module boundaries with version
stability). For user code that does not need library evolution support
the backend omits `@frozen` and lets the compiler pick the layout. See
note 06 §4.

Field access inside a method is direct (`self.radius`, or just `radius`
with the implicit self). Mochi field names are preserved verbatim; the
backend never renames them unless they collide with a Swift keyword (in
which case backticks).

For records with a `var` field (mutable), the lowering keeps `struct`
but marks the field `var` and the mutating methods `mutating func`:

```swift
public struct Counter {
  public var count: Int64
  public mutating func tick() { count &+= 1 }
}
```

If the type is genuinely shared (used as a back-reference, observed by
multiple readers via aliasing), the backend falls back to a `final class`
with `@unchecked Sendable` conformance and a manual lock. The threshold
is configurable; default is "always use struct unless escape analysis
detects shared mutation".

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end.

Lowers to a runtime call `MochiRuntime.IO.print(_ args: Any...)` where
the runtime walks the varargs, applies per-type formatting (since
Swift's `String(describing:)` does the right thing for most types but
the wrong thing for `Array`/`Dictionary` whose description is Swift's
own `[1, 2, 3]` syntax rather than Mochi's), inserts single-space
separators, and writes the trailing newline via Swift's `print(_:)` or
`FileHandle.standardOutput.write(_:)` on platforms where stdout
buffering matters. The runtime caches a `TextOutputStream` locally and
flushes on every newline (matching Mochi's "line-buffered stdout"
guarantee). See note 04 §3 for the `MochiRuntime.IO` module.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. ⇒ Swift `Array<T>`, value type with
  copy-on-write.
- `map<K, V>`, keyed lookup, with insertion-order iteration. ⇒
  `OrderedDictionary<K, V>` from `swift-collections` (or
  `Dictionary<K, V>` for the unordered preview type).
- `set<T>`, unordered, unique members, with insertion-order iteration. ⇒
  `OrderedSet<T>` from `swift-collections`.
- `string`, read-only sequence of code points (see §1.4). ⇒ Swift
  `String`.

Lowering obligations (full per-type details in note 06 §1):

- `list<T>` is the workhorse. The default representation is Swift's
  `Array<T>`, which is a value-type struct with copy-on-write semantics
  (so passing a list to a function is O(1) and only copies on first
  mutation through the new binding). Random access is O(1). Element
  storage is contiguous and unboxed for `Int64`, `Double`, `Bool`, etc.
  (Swift's `Array` is `@frozen` and uses `ContiguousArray`-style
  storage when the element type is a non-class concrete type). This is
  the biggest single performance advantage of the Swift target over the
  JVM target: `[Int64]` is a true `Int64[]`-equivalent buffer with no
  per-element heap allocation, where Java's `List<Long>` requires
  boxing.

- `map<K, V>` defaults to `OrderedDictionary<K, V>` from Apple's
  `swift-collections` package (formerly Foundation Collections, now an
  open-source package at github.com/apple/swift-collections). Mochi's
  iteration order is insertion order, documented in
  `docs/features/map.md`. Swift's built-in `Dictionary` is *unordered*
  (it uses open-addressing hashing internally with no order guarantee),
  so we cannot use it for the canonical lowering. `OrderedDictionary`
  uses an `Array<Key>` for order plus a `Dictionary<Key, Int>` for
  O(1) lookup; lookups, inserts, and deletes are all amortised O(1).
  The `Dictionary` variant is exposed as `hashmap<K, V>` (preview) for
  users who do not care about order.

- `set<T>` is `OrderedSet<T>` from `swift-collections` for the same
  reason. The query layer (§5) needs the *insertion-ordered* semantic
  for `union`/`except` to be deterministic.

- All collections are **value-semantically copied** at language level.
  Swift's value-type semantics with copy-on-write give us this for free;
  there is no need for an explicit `copy()` call as in mutable Java or
  C# collections. The VM enhancement spec 0951 §1 ("each function call
  must allocate a fresh copy of any list/map literal") is satisfied by
  Swift's array/dictionary literal semantics: every literal evaluation
  produces a fresh value, and the COW backing storage is only shared
  until the first mutation.

Mutation operations (`xs.add(x)`, `m[k] = v`) lower to direct Swift
mutating method calls (`xs.append(x)`, `m[k] = v`) when the target is a
`var` binding, or to a copy-on-write helper that takes the immutable
collection, returns a fresh mutated copy, and rebinds. The runtime ships
a `MochiRuntime.Collections` module that exposes the Mochi-shaped helpers
(`appended(_:)`, `inserting(_:at:)`, `removing(at:)`, `updating(_:for:)`)
which are thin wrappers over Swift's COW machinery. See note 06 §11.

`for x in xs` lowers to a Swift `for ... in` loop:

```swift
for x in xs { ... }
```

For ordered dictionaries, `for (k, v) in m` lowers to:

```swift
for (k, v) in m { ... }
```

Swift's `OrderedDictionary` conforms to `Sequence` of `(Key, Value)` so
the iteration syntax is identical to the built-in `Dictionary`. The
runtime overhead is one pointer dereference per element (to skip the
keys-array indirection); benchmarks in note 18 measure this at under
3 percent vs `Dictionary` for typical Mochi workloads.

## 4. Algebraic data type core

Mochi's sum-of-products data types (`type Tree = Leaf | Node { ... }`) are
the cleanest fit for Swift `enum` with associated values, which has been
core to Swift since 1.0 and gained recursive support in Swift 2.0 (the
`indirect` keyword for self-referential cases).

```mochi
type Tree =
  | Leaf
  | Node { value: int, left: Tree, right: Tree }
```

Lowers to:

```swift
public enum Tree: Sendable, Hashable, Codable {
  case leaf
  indirect case node(value: Int64, left: Tree, right: Tree)
}
```

The `indirect` keyword is required for recursive cases (any case that
references its own enum type in an associated value); the Swift
compiler enforces this at type-check time and the backend emits it
unconditionally for any case whose payload contains the enclosing type.
For non-recursive sums (like a simple `Color = Red | Green | Blue`) the
backend omits `indirect`.

Field labels on the case payload (`value:`, `left:`, `right:`) are
preserved from the Mochi syntax. Construction is `Tree.node(value: 42,
left: .leaf, right: .leaf)`; the Swift compiler synthesises the case
constructor automatically.

Pattern matching:

```mochi
match t {
  Leaf => 0
  Node { value, left, right } => value + sum(left) + sum(right)
}
```

Lowers to a Swift `switch` expression:

```swift
return switch t {
case .leaf: 0
case .node(let value, let left, let right): value &+ sum(left) &+ sum(right)
}
```

Swift's `switch` is an expression since SE-0380 ("Allow switch and if to
be used as expressions", Swift 5.9). Exhaustiveness is checked at *both*
layers: Mochi's type checker rejects non-exhaustive matches at compile
time, and the Swift compiler also rejects non-exhaustive switches on an
enum, so both lines of defence agree. If the Mochi checker accepts a
match the Swift compiler guarantees the switch is exhaustive (the
backend never has to emit a `default` arm for sealed enums). See
note 05 §10.

Guarded patterns:

```mochi
match shape {
  Circle { radius } if radius > 10.0 => "big"
  Circle { radius }                  => "small"
  _                                  => "other"
}
```

Lower to Swift guarded patterns with `where` clauses:

```swift
return switch shape {
case .circle(let radius) where radius > 10.0: "big"
case .circle(let radius): "small"
default: "other"
}
```

Swift's `where` clause on a `case` pattern (also valid in `if case let`
and `for case let`) is the direct equivalent of Mochi's guard.

Generic ADTs:

```mochi
type Option<T> = | Some { value: T } | None
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Lower with generic type parameters on the enum:

```swift
public enum Option<T: Sendable>: Sendable {
  case some(T)
  case none
}

public enum Result<Success: Sendable, Failure: Sendable & Error>: Sendable {
  case success(Success)
  case failure(Failure)
}
```

As noted in §1.7, Mochi `Option<T>` is mapped to Swift's *built-in*
`Optional<T>` rather than this user-emitted enum; the snippet above
illustrates the general shape for other generic sums.

For `Result<T, E>` the backend emits the user enum (or maps to
`Swift.Result` from the standard library when the `E` type satisfies
the `Error` protocol). See [[06-type-lowering]] §15 for the typed-throw
interaction (SE-0413).

The `Sendable` constraint on type parameters is added automatically
when the enum will be sent across an actor boundary. Swift 6.0's
strict-concurrency mode requires `Sendable` annotations to be explicit;
the backend infers them at the use site and propagates them to the
generic parameter list. See [[02-design-philosophy]] §8.

## 5. Query DSL

Mochi's query DSL (`from x in xs select { ... }`, with `join`, `group by`,
`order by`, `limit`, `offset`, `where`) is the densest sub-language. Its
Swift lowering uses Swift's `Sequence` protocol chain plus
`swift-algorithms` extensions as the primary IR. Full lowering is in
note 08, this section names the surface forms only.

```mochi
let adults =
  from p in people
  where p.age >= 18
  order by p.name
  select { name: p.name, age: p.age }
```

Lowers to:

```swift
let adults: [AdultRow] = people
  .lazy
  .filter { $0.age >= 18 }
  .sorted { $0.name < $1.name }
  .map { AdultRow(name: $0.name, age: $0.age) }
```

The `.lazy` prefix is added when the pipeline has more than one stage,
so intermediate allocations are skipped (Swift's `LazySequence` evaluates
on demand, terminating in the final `.map`/`.sorted` call). The
materialisation is the implicit `[AdultRow]` type annotation, which
triggers an `Array.init(_:)` call from the lazy sequence.

`group by` lowers to `Dictionary(grouping:by:)` (or
`OrderedDictionary(grouping:by:)` from `swift-collections` for the
order-preserving variant, which we always pick since Mochi specifies
insertion-order grouping):

```swift
let by_dept = people.grouped(by: { $0.dept })  // returns OrderedDictionary<String, [Person]>
```

The `grouped(by:)` extension is shipped in `MochiRuntime.Query` (which
re-exports the `swift-algorithms` `grouped(by:)` plus an order-preserving
variant).

`join` lowers to a hash-join helper in `MochiRuntime.Query` (since
`Sequence` has no built-in join). `limit` / `offset` are direct Sequence
operations (`prefix(_:)`, `dropFirst(_:)`).

Important: the **Mochi `stream<T>` type and the Swift `Sequence`
protocol are not the same thing**. Swift's `Sequence` is a synchronous
iteration protocol; Mochi's `stream<T>` is a *time-evolving* publisher
(closer to `AsyncSequence`). The query DSL uses Swift's `Sequence` /
`LazySequence` for *finite* collection queries; the public `stream<T>`
type uses `AsyncSequence` (and concretely `AsyncStream<T>` for the
canonical lowering). See note 09 for the agent / stream lowering and
[[06-type-lowering]] §7 for the type-level distinction.

## 6. Stream and agent core

```mochi
stream Tick = { time: time }

agent ticker {
  every 1s emit Tick { time: now() }
}
```

Streams lower to Swift's `AsyncStream<T>` (Swift 5.5+, refined in Swift
6.0 with `AsyncStream.makeStream(of:)` ergonomics under SE-0388).
Agents lower to Swift `actor` types, which have been first-class since
Swift 5.5 (SE-0306 "Actors") and are the only Mochi-friendly
concurrency primitive that survives Swift 6.0's complete strict-concurrency
checking.

```swift
public actor Ticker {
  private let mailbox: AsyncStream<Message>
  private let continuation: AsyncStream<Message>.Continuation
  private var task: Task<Void, Never>?

  public init() {
    let (stream, cont) = AsyncStream.makeStream(of: Message.self)
    self.mailbox = stream
    self.continuation = cont
  }

  public func start() {
    self.task = Task { [weak self] in
      guard let self else { return }
      await self.run()
    }
  }

  private func run() async {
    for await msg in mailbox {
      await handle(msg)
    }
  }

  nonisolated public func send(_ msg: Message) {
    continuation.yield(msg)
  }
}
```

Key design points:

- The mailbox is an `AsyncStream<Message>`, which gives us a typed
  unbounded queue with backpressure-free FIFO semantics. Swift's
  `AsyncStream.Continuation` is `Sendable`, so any thread can call
  `yield(_:)`.
- The actor's main loop is `for await msg in mailbox`, which is the
  Swift idiom for async iteration. When the continuation finishes
  (either explicitly via `continuation.finish()` or implicitly when
  the actor deallocates), the loop terminates and the actor stops.
- `send(_:)` is `nonisolated` so callers do not have to `await`. This
  is the equivalent of an Erlang `!` cast (fire-and-forget). For
  `call`-style (await reply) sends, the actor exposes a regular
  `async` method that returns a value; the Mochi `agent.method()`
  surface picks one based on the method's declared return type.
- Spawning is `Task { ... }`, which integrates with Swift's structured
  concurrency. The task inherits the parent's task-local storage and
  cancellation. For unsupervised long-running agents we use the
  detached form `Task.detached { ... }`.

Supervision is *not* built into the Swift `Task` model the way it is in
BEAM's `gen_server`. We provide it in user space via the
`MochiRuntime.Supervisor` actor, which holds a dictionary of child
agents, restarts them on failure (catching the `Task`'s thrown error),
and applies a configurable restart strategy (`oneForOne`, `oneForAll`,
`restForOne`, matching BEAM's nomenclature). See note 09 §5.

Agents talk via typed message types (Mochi `stream Tick = {...}` becomes
a Swift struct, and `agent.emit(Tick { time: now() })` becomes
`agent.send(Message.tick(Tick(time: .now)))`):

```mochi
ticker ! Tick { time: now() }
```

Lowers to:

```swift
ticker.send(.tick(Tick(time: .now)))
```

Swift 6.0's strict-concurrency checking enforces that the `Message`
enum and every payload type (`Tick`) conform to `Sendable`. The backend
emits `Sendable` conformance on every Mochi record and ADT
unconditionally (since Mochi values are immutable by default, conformance
is always provable). See [[02-design-philosophy]] §8.

The `swift-async-algorithms` package provides higher-level operators
(`merge`, `debounce`, `throttle`, `chunked`) that the Mochi stream DSL
maps onto. The runtime depends on `swift-async-algorithms` as a
first-class dependency. See note 09 §6.

## 7. Logic programming core

```mochi
fact parent(alice, bob).
fact parent(bob, charlie).

rule ancestor(X, Y) :- parent(X, Y).
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

query ancestors_of(X) := ancestor(alice, X).
```

The logic core targets a small embedded Datalog engine. The Swift
lowering emits a runtime call into `MochiRuntime.Datalog.Engine`, with
facts and rules registered at module init time. The engine implements
semi-naive bottom-up evaluation; magic-set transforms are a v2 concern.

Datalog terms are represented as `MochiDatalogTerm` (an enum with cases
for `.atom(String)`, `.int(Int64)`, `.string(String)`, `.list([MochiDatalogTerm])`,
and `.compound(String, [MochiDatalogTerm])`). Predicates are `(
[MochiDatalogTerm]) -> Bool` closures. Unification is a recursive walk
that binds variables in a `[String: MochiDatalogTerm]` substitution map.
See note 08 §4 for the engine internals.

We are also exploring using Swift's `@resultBuilder` macros (SE-0289) to
expose the Datalog DSL ergonomically. A `@DatalogBuilder` block could
let users write rules in a Swift-native form without a parser
round-trip. This is a *design exploration* and not part of the v0.10
ship target; see note 08 §7 for the exploration.

The Swift target's Datalog engine has one differentiator over the JVM
and .NET engines: thanks to Swift's value-type enums, the term type is
zero-allocation for the small cases (integer terms fit inline in 9 bytes,
strings are 16-byte `String` value types, etc.). Benchmarks in note 18
measure a 1.4-2.1x throughput improvement over the JVM engine for
million-fact workloads.

## 8. AI and FFI shells

### 8.1 AI shell

```mochi
let summary = ai("summarise this", text)
let result = generate("write a haiku about ", topic)
```

Mochi has two AI builtins: `ai(...)` (synchronous one-shot) and
`generate(...)` (streaming token-by-token). Both lower to async
functions returning the appropriate type.

`ai(...)` lowers to `MochiRuntime.AI.call(prompt: String, args: Any...)
async throws -> String`. The body of `call(...)` dispatches based on
provider configuration (env vars at runtime, not codegen choices):

- On Apple platforms (iOS 18+ / macOS 15+ / watchOS 11+ / tvOS 18+ /
  visionOS 2+) the backend prefers Apple's **FoundationModels** framework
  (announced at WWDC 2024, shipped in iOS 18 as the on-device Apple
  Intelligence runtime). The dispatch uses
  `FoundationModels.LanguageModelSession` for chat-style prompts and
  `FoundationModels.LanguageModelSession.respond(to:)` for one-shot.
  Streaming is via `LanguageModelSession.streamResponse(to:)`.
- When FoundationModels is unavailable (Linux, Windows, older Apple
  versions, or when the env var `MOCHI_AI_PROVIDER=openai` is set) the
  backend falls back to **swift-openai-async** (a community SPM package
  that wraps the OpenAI HTTP API with `AsyncSequence` streaming) or
  the equivalent Anthropic / local-Ollama clients.
- The provider selection happens at runtime via the
  `MochiRuntime.AI.Provider` protocol; codegen always emits the
  protocol-typed call and lets the runtime pick. See note 04 §10.

`generate(...)` returns `AsyncStream<MochiToken>` where `MochiToken` is
a small struct carrying the token text plus metadata (logprobs,
finish-reason). On Apple platforms this maps onto FoundationModels'
streaming API directly; on other platforms it wraps the HTTP SSE stream
in an `AsyncStream`. See note 04 §10.

### 8.2 FFI shell

```mochi
let result = ffi("std/json/parse", raw)
extern fun sqrt(x: float): float = "c:sqrt"
```

The `ffi(...)` builtin lowers to a `MochiRuntime.FFI.call(path: String,
args: Any...) async throws -> Any` that looks up the named function in
a module registry. For Mochi-to-Mochi FFI calls the registry just
dispatches to the right module enum's static method.

The `extern fun` form is the rich case: it declares a C-ABI or
Swift-ABI external function and lets Mochi code call it directly.

For C-ABI externs (`extern fun sqrt(x: float): float = "c:sqrt"`) the
backend emits a Swift declaration with `@_silgen_name`:

```swift
@_silgen_name("sqrt")
func __mochi_extern_sqrt(_ x: Double) -> Double
```

`@_silgen_name` is an underscored Swift attribute (so non-stable) that
tells the SIL generator to use the given symbol name verbatim, bypassing
the usual Swift name mangling. It is the standard way to declare C-ABI
externs in Swift (used widely inside the standard library itself). The
generated Mochi function is a thin wrapper that calls
`__mochi_extern_sqrt`.

For Mochi-as-C-library exports (where Mochi code is *called by* C), the
backend emits `@_cdecl`:

```swift
@_cdecl("mochi_my_function")
public func __mochi_export_my_function(_ a: Int64, _ b: Int64) -> Int64 {
  return MyModule.myFunction(a, b)
}
```

`@_cdecl` is a stable attribute (without the underscore prefix it would
be `@cdecl` but Swift evolution proposal SE-NNNN has not yet promoted
it; current usage requires the underscore form). The Mochi build system
generates a corresponding `module.modulemap` and a C header so that C
consumers can link against the Mochi-built static library.

For Swift-to-Swift FFI (calling SPM packages from Mochi) the backend
uses the Swift Package Manager directly: Mochi `import "swift:Foundation"`
becomes a Swift `import Foundation` plus a Mochi-side type alias map
(`URL ⇒ MochiURL`, etc.). The Mochi build system extends the generated
`Package.swift` with the requested SPM dependency. See note 11.

The Mochi-to-Swift FFI surface is one of the load-bearing reasons we
want a Swift target at all (see [[02-design-philosophy]] §4): it unlocks
SwiftUI, SwiftData, the Combine framework, Apple's CryptoKit, and the
entire Apple platform SDK. The transpiler exposes
`import "swift:SwiftUI"` and `import "swift:SwiftData"` as first-class
forms. See note 11.

## 9. Strings, errors, concurrency colouring

### 11. Strings

Covered in §1.4 above. The Swift target is uniquely well-suited because
Swift's `String` is UTF-8 native since Swift 5.7 (SE-0334 "Unlock
existing API for UTF-8"). Mochi's wire format is UTF-8, so passing a
Mochi string into and out of Swift is a zero-copy `String` ⇄ `Data`
move when the underlying storage is a contiguous UTF-8 buffer (which
it is for all string literals and most runtime-constructed strings).

Cross-boundary string costs:

- Mochi `string` ⇒ Swift `String`: zero copy in the common case;
  one allocation only when the string was non-contiguous (e.g.
  concatenated from multiple sources without coalescing).
- Swift `String` ⇒ Mochi `string`: zero copy.
- Mochi `string` ⇒ Foundation `Data` (for `URLRequest.httpBody` etc.):
  zero copy via `string.data(using: .utf8)!` which Apple's
  implementation now special-cases since iOS 17.
- Mochi `string` ⇒ C `char *`: requires null-termination, so one
  allocation; the runtime exposes `withCString { ptr in ... }` for the
  scoped form that does not allocate.

See [[06-type-lowering]] §4 for the full string-bridging table.

### 12. Errors

Mochi's error story is built on the `Result<T, E>` sum type and on
typed `throw e` / `catch e` blocks. Swift 6.0 finally introduced typed
throws (SE-0413 "Typed throws", shipped in Swift 6.0 in September 2024),
which is a load-bearing feature for the Mochi target: without it the
Swift target would have to either erase Mochi's typed error info into
`any Error` (lossy) or shoe-horn everything through `Result<T, E>`
(verbose). With typed throws the Swift target matches Mochi 1:1.

Lowering rules:

- Mochi `fun foo(): T throws E { ... }` becomes Swift
  `func foo() throws(E) -> T { ... }`. The `throws(E)` syntax is
  Swift 6.0's typed-throw form.
- Mochi `try foo()` becomes Swift `try foo()`.
- Mochi `try foo() catch e => handler` becomes Swift `do { try foo() }
  catch let e { handler }`. When `e`'s type is known the catch is
  exhaustive without a default.
- Mochi `Result<T, E>` becomes Swift `Result<T, E>` (built-in since
  Swift 5.0, SE-0235). Mochi `result.ok(x)` ⇒ `.success(x)`,
  `result.err(e)` ⇒ `.failure(e)`.
- Mochi `try?` (optional unwrap of result) becomes Swift `try?`,
  which has the identical semantic (returns `nil` on throw).
- Mochi `try!` (forced unwrap) becomes Swift `try!`, which traps
  on throw.

The `E` type in `throws(E)` must conform to `Error`; the backend emits
`Error` conformance on every Mochi error type unconditionally (a
zero-cost protocol since Mochi error types are typically small enums
or structs). See [[06-type-lowering]] §9.

Untyped throws (Mochi `fun foo() throws Any { ... }` or a `throws`
clause without a type) lower to Swift's untyped `throws` (i.e. `throws
(any Error)`), which is the existing Swift 5 semantic. This preserves
backwards compatibility with code that does not declare specific error
types.

### 13. Concurrency colouring

Mochi distinguishes synchronous and asynchronous functions (`fun` vs
`async fun`). Swift 6.0's async/await model maps almost perfectly:

- Mochi `fun foo(): T { ... }` ⇒ Swift `func foo() -> T { ... }`.
- Mochi `async fun foo(): T { ... }` ⇒ Swift `func foo() async -> T { ... }`.
- Mochi `await foo()` ⇒ Swift `await foo()`.
- Mochi `spawn foo()` (fire-and-forget) ⇒ Swift `Task { await foo() }`.

Isolation domains:

- An `agent` lowers to a Swift `actor`, and methods on the agent are
  automatically isolated to that actor (callers must `await`).
- A `@main` Mochi program (the top-level entry) lowers to a Swift
  `@main struct` with a `static func main() async throws { ... }`
  body. The body runs on the global executor; methods called from
  `main` are by default `nonisolated` unless they cross into an actor.
- Mochi code that needs to run on the UI thread (interop with SwiftUI
  on Apple platforms) is marked with `@ui` in Mochi, which lowers to
  Swift `@MainActor`. This requires an explicit Mochi annotation; the
  transpiler does not infer it.
- `nonisolated` is emitted on methods that the strict-concurrency
  checker can prove do not touch actor-isolated state. The backend
  performs a local escape analysis (note 05 §13) to decide.

Swift 6.0's **region-based isolation** (SE-0414) lets us pass
non-`Sendable` values into an actor as long as the value's "region" is
not aliased elsewhere. The backend takes advantage of this in the FFI
layer: a Mochi value that is constructed and immediately handed to an
agent is in a unique region and the Swift compiler proves it safe to
send without a `Sendable` conformance. See [[02-design-philosophy]] §8.

Complete strict-concurrency checking is *enabled by default* in Swift
6.0 source mode (controlled by the Swift Package Manager's
`swiftLanguageMode: .v6` setting). The Mochi-generated `Package.swift`
sets this unconditionally. Any concurrency error in emitted Swift is a
Mochi compiler bug. See note 11 §7.

Other Swift 6.0 features the lowering uses:

- `~Copyable` (SE-0390 "Noncopyable structs and enums") for resource
  types that must not be silently duplicated (file handles, network
  sockets, mutex locks). The Mochi `linear` qualifier (preview, RFC-0067)
  lowers to `~Copyable`.
- `~Escapable` (SE-0446 "Nonescapable types", Swift 6.0) for stack-only
  values; Mochi's `borrowed` parameter qualifier lowers to a Swift
  `borrowing` parameter on a `~Escapable` type.
- Noncopyable generics (SE-0427) for collections whose element type may
  be noncopyable; the backend emits the `~Copyable` constraint relaxation
  on collection generic parameters when the element type allows it.
- `count(where:)` (SE-0220, Swift 5.0) and the new collection
  conformance macros (SE-0411 "Isolated default value expressions",
  SE-0426 "Strict memory safety") are used in the standard-library
  bridge layer.

## 14. What this surface does *not* include

- **Untyped `any`**: Mochi rejects it at the type layer. Swift has
  `Any` and `AnyObject` and the temptation to weaken Mochi's type
  system to allow `any` is real; we resist. The cost would be losing
  static-typed FFI guarantees.
- **Implicit conversions**: ruled out above. Required to keep C, BEAM,
  JVM, .NET, and Swift behaviours identical.
- **Null at the language level**: see §1.7. Mochi has no `nil`; Swift
  has `nil` but only as the `none` case of `Optional<T>`.
- **Inheritance**: Mochi has no class inheritance (only enum-with-cases
  ADTs and protocol composition). The Swift `class` keyword is unused
  for user code. Internal helpers and FFI bridges to Apple frameworks
  (which heavily use classes for `NSObject` subclasses) are the only
  places `class` appears.
- **Operator overloading**: Mochi does not let users overload `+` etc.
  Library code can define methods, but the operator syntax is reserved
  for built-in numeric, string, and collection types. (Swift *does*
  allow operator overloading via static `func`s, but the Mochi target
  never emits them for user code.)
- **Macros / compile-time reflection**: deferred to a future MEP. Swift
  has powerful macros (SE-0397 "Freestanding macros" and SE-0389
  "Attached macros", shipped in Swift 5.9) but we are not exposing
  them at the Mochi language level in v0.10 or v0.11.
- **Property wrappers**: not exposed at the Mochi language level.
  Internally the runtime uses them sparingly (e.g. `@MainActor` for
  UI hooks).
- **Result builders for user code**: deferred. Used internally for
  the experimental Datalog DSL (§7) and the FFI bridge (§8).
- **Existential `any P`**: emitted in FFI bridges where a Swift API
  requires a protocol-typed argument, but never surfaced to Mochi.

## 15. Surface-to-Swift cheat sheet (cross-reference)

| Mochi form | Swift lowering | Note |
|------------|----------------|------|
| `let x = ...` | `let x = ...` | §1.1, note 05 §4 |
| `var x = ...` | `var x = ...` | §1.1 |
| `int` | `Int64` | §1.2, note 06 §5 |
| `float` | `Double` | §1.2 |
| `bool` | `Bool` | §1.2 |
| `string` | `Swift.String` (UTF-8 native) | §1.4 |
| `list<T>` | `Array<T>` | §3, note 06 §10 |
| `map<K,V>` | `OrderedDictionary<K,V>` | §3, swift-collections |
| `set<T>` | `OrderedSet<T>` | §3, swift-collections |
| `record T { ... }` | `struct T` (Sendable, Hashable, Codable synthesised) | §2.3 |
| `type T = A \| B` (sum) | `enum T` with cases and associated values | §4 |
| `Option<T>` | `Optional<T>` (built-in) | §1.7 |
| `Result<T, E>` | `Result<T, E>` (built-in) | §12 |
| `match` | `switch` expression with case patterns | §4, SE-0380 |
| `fun(...) => ...` | Swift closure `{ ... in ... }` | §2.2 |
| `from ... select ...` | Sequence chain + swift-algorithms | §5, note 08 |
| `agent ...` | `actor` + `AsyncStream<Message>` mailbox + spawn `Task` | §6, SE-0306 |
| `stream<T>` | `AsyncStream<T>` (or `AsyncSequence` more generally) | §6 |
| `fact / rule / query` | runtime Datalog engine | §7, note 08 §4 |
| `ai(...)` | runtime `MochiRuntime.AI.call` (FoundationModels on Apple, swift-openai-async elsewhere) | §8.1 |
| `generate(...)` | async function returning `AsyncStream<MochiToken>` | §8.1 |
| `extern fun ... = "c:..."` | `@_silgen_name` declaration | §8.2 |
| `@cdecl export` | `@_cdecl("...")` | §8.2 |
| `import "swift:Foundation"` | Swift `import Foundation` + Package.swift dependency | §8.2, note 11 |
| `throws E` | `throws(E)` (Swift 6.0 typed throws) | §12, SE-0413 |
| `async fun` | `func ... async` | §13 |
| `await x` | `await x` | §13 |
| `spawn f()` | `Task { await f() }` | §13 |
| `@ui` | `@MainActor` | §13 |
| `linear T` | `~Copyable` struct/enum | §13, SE-0390 |
| `borrowed T` | `borrowing T` (nonescapable) | §13, SE-0446 |

## 16. Open questions for note 02 (design philosophy)

- **Codegen IR**: SwiftSyntax source emission vs direct SIL. (Resolved in
  note 02 §3 and note 05 §1: hybrid SwiftSyntax source + direct SIL
  emission only for a small set of hot lowerings.)
- **swift-collections version pinning**: which `OrderedDictionary` ABI
  to target. (Note 02 §7: pin to 1.1.x; bump on `Package.swift`
  regeneration.)
- **FoundationModels availability**: Apple Intelligence is iOS 18+
  only, and not all devices support it (requires A17 Pro / M1 or
  later). Fallback ladder is documented in note 04 §10.
- **Swift 6.0 floor**: confirmed (note 02 §2). Swift 6.1 (preview as
  of 2025-Q4) is the rolling secondary gate; Swift 6.2 (expected
  2026-Q3) is the preferred target for new feature usage.
- **Embedded Swift**: out of scope for v0.10. The Embedded Swift mode
  (Swift 5.10+, fully fledged in 6.0) is a fascinating target for
  microcontroller deployments but is not part of MEP-49's matrix.
  Tracked as a follow-on MEP.

## 17. Cross-references

- [[02-design-philosophy]] (next note): why each of the choices above
  was made, including the case for Swift 6.0 as the floor and the
  comparison against alternative Mochi-to-iOS pathways.
- [[03-prior-art-transpilers]]: survey of Kotlin/Native, J2ObjC, and
  other source-to-Swift transpilers, plus a closer look at the Carton
  and SwiftWasm projects for the WebAssembly story.
- [[04-runtime]]: the `MochiRuntime` Swift package layout, including
  the `Collections`, `IO`, `AI`, `FFI`, `Datalog`, and `Supervisor`
  submodules.
- [[05-codegen-design]]: the IR layer that turns this surface into
  emitted Swift source via SwiftSyntax builders.
- [[06-type-lowering]]: the per-type details glossed here.
- [[07-swift-target-portability]]: the Apple-platform matrix
  (iOS / macOS / watchOS / tvOS / visionOS) and the Linux / Windows
  swift.org toolchain matrix, plus version skew handling.
- [[08-dataset-pipeline]]: the query DSL lowering in full, including
  the swift-algorithms dependency map and the Datalog engine design.
- [[09-agent-streams]]: agent and stream lowering on Swift actors and
  AsyncStream, including the supervision tree design.
- [[10-build-system]]: Swift Package Manager integration, the
  generated `Package.swift`, and the multi-platform CI matrix.
- [[11-testing-gates]]: the test-suite gates for v0.10 ship; what
  fraction of `examples/v0.2`-`v0.7` must transpile, build, and
  pass on each platform.
- [[12-risks-and-alternatives]]: the risk register and the
  alternatives considered (notably Kotlin Multiplatform for the
  iOS story and Tauri / capacitor for cross-platform packaging).
- [[../0047/01-language-surface]]: the JVM-target analogue of this
  note, the closest in spirit since both target a typed-managed
  runtime with first-class generics.
- [[../0048/01-language-surface]]: the .NET-target analogue, which
  shares the typed-throws and structured-concurrency story.
- [[../0046/01-language-surface]]: the BEAM-target analogue, whose
  agent / mailbox / supervision design directly inspired the Swift
  actor + AsyncStream + Supervisor lowering.
- [[../0045/01-language-surface]]: the C-target analogue, whose
  C-ABI extern story is mirrored verbatim here via `@_silgen_name`
  and `@_cdecl`.
