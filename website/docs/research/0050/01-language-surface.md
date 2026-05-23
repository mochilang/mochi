# MEP-50 research note 01, Mochi language surface (Kotlin target)

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`-`v0.7`, the normative
security specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`,
the JetBrains KEEP proposal stream at github.com/Kotlin/KEEP, the Kotlin
Multiplatform documentation at kotlinlang.org/docs/multiplatform.html, and
the companion notes MEP-45 note 01 (C target), MEP-46 note 01 (Erlang/BEAM),
MEP-47 note 01 (JVM), MEP-48 note 01 (.NET), and MEP-49 note 01 (Swift),
whose section structure this note deliberately mirrors so all six backends
can be diffed line for line.

This note records the user-visible language surface that the Kotlin target
must faithfully reproduce. It is deliberately written *from the spec downward*
and ignores the existing Go runtime (vm3), the vm3 bytecode, the C target
under MEP-45, the Erlang/BEAM target under MEP-46, the JVM target under
MEP-47 (which emits bytecode directly, bypassing kotlinc), the .NET target
under MEP-48, the Swift target under MEP-49, and any other backend
implementation. The goal is a transpiler design that would be correct against
the *language*, not against the present implementations.

The surface decomposes into the same eight orthogonal sub-languages identified
in the prior notes: (1) the value core, (2) the function and method core,
(3) the collection core, (4) the algebraic-data-type core, (5) the query DSL,
(6) the stream and agent core, (7) the logic-programming core, and (8) the
AI and FFI shells. Each section below names every form a Mochi program can
write, then states a *lowering obligation* the Kotlin backend must honour.

Where MEP-45 maps Mochi types to C struct plus helper-function pairs, MEP-46
maps them to BEAM terms (atoms, tagged tuples, maps, binaries, funs, PIDs),
MEP-47 maps them to JVM values directly via bytecode, MEP-48 maps them to
.NET values (CLR primitives, immutable collections, C# records, discriminated
unions), and MEP-49 maps them to Swift values (`Int64`, `Double`, structs,
enums with associated values, actors, AsyncStream), this note maps them to
Kotlin values: boxed and unboxed primitives (`Long`, `Double`, `Boolean`),
`String` (UTF-16 internal across all KMP targets), `kotlin.collections.List<T>`
(`ArrayList` underneath), `LinkedHashMap<K,V>` and `LinkedHashSet<T>` for
insertion-ordered collections, `data class` for records, `sealed interface`
with `data class` and `data object` variants for sum types, function types
`(In) -> Out` (and `suspend (In) -> Out` for async closures), custom actor
classes wrapping `Channel<Message>` for the agent core, `Flow<T>` (and
`SharedFlow`/`StateFlow` for hot variants) for the stream core, and
return-typed `MochiResult<T, E>` for the error core. The target IR is
discussed in note 05 (the default path emits Kotlin source via KotlinPoet,
JetBrains's first-party Kotlin source emitter library); the runtime is the
Kotlin standard library plus kotlinx.coroutines, kotlinx.serialization,
kotlinx.datetime, kotlinx.collections.immutable, and a thin `MochiRuntime`
KMP module (see note 04).

Throughout, "Kotlin" means Kotlin 2.1 (released 2024-11-27, the K2-compiler
baseline with stable Kotlin Multiplatform, multidollar string interpolation,
and smart-cast improvements for `when` expressions on sealed hierarchies)
and later. Kotlin 1.9.x is explicitly out of scope; we do not back-port any
feature usage to the K1 frontend. The platform matrix covered is full Kotlin
Multiplatform: JVM (Java 17+), Android (API 24+), Kotlin/Native (iOS arm64
and simulator, macOS, Linux x64 and arm64, Windows mingw, watchOS, tvOS),
Kotlin/JS (browser and Node.js, IR backend only), and Kotlin/Wasm (Alpha,
GC target via wasmJs). Kotlin/JS via the legacy backend is unsupported.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time** error,
  not a runtime panic. The Kotlin lowering maps directly onto Kotlin `val`:
  Kotlin `val` is single-assignment, and the Kotlin compiler itself rejects
  any subsequent assignment to a `val` binding. This is one of the cleanest
  one-to-one mappings in the backend matrix, second only to the Swift
  target's `let` (since `let` and `val` mean exactly the same thing on
  both targets). For top-level Mochi `let` the backend emits Kotlin
  top-level `val` (Kotlin allows top-level declarations without a wrapping
  class or object, unlike Java).
- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope. Lowers to Kotlin `var`. Inside an actor class
  (see §13) the `var` is implicitly serialised by the single dispatcher
  consuming the actor's channel; Kotlin has no field-level isolation
  annotation (unlike Swift's `actor`-isolated property), so the discipline
  is enforced at the call-site level by funnelling all reads and writes
  through the channel.

Mochi blocks are expressions in the sense that the last expression is the
block's value. Kotlin blocks (`{ ... }` after `if`/`when`/`run`/`let`) are
expressions when the body is enclosed in an `if`/`when`/`try` head or in
a scope function (`run { ... }`, `let { ... }`, `apply { ... }`). The
backend lowers a block whose value is consumed into a Kotlin `run { ... }`
block for the general case, and uses Kotlin's `if` / `when` / `try`
expressions directly for the common forms. So `let x = if cond { a } else { b }`
becomes a single Kotlin `val x = if (cond) a else b` without a helper
lambda. See note 05 §6 for the full block-lowering table.

A binding may carry an explicit type: `let x: int = 0`. Kotlin is statically
typed; the type survives end-to-end. Mochi `int` lowers to Kotlin `Long`
(see §1.2 on why we pick `Long` over `Int`), so `let x: int = 0` becomes
`val x: Long = 0L`.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

Kotlin has a *positional* destructuring mechanism via `componentN()`
operators (since Kotlin 1.0) that works with data classes and certain
stdlib types (`Pair`, `Triple`, `Map.Entry`). It does **not** destructure
lists or maps at the binding site in the way Mochi does. The list pattern
lowers to a positional read with a runtime length check:

```kotlin
val __tmp: List<Long> = listOf(1L, 2L)
check(__tmp.size == 2) { "MochiPatternError: arity expected 2, got ${__tmp.size}" }
val a: Long = __tmp[0]
val b: Long = __tmp[1]
```

The map pattern lowers to `Map` subscript with a `requireNotNull` per key
(since Kotlin's `Map[k]` returns `V?`). For record types the backend uses
Kotlin's `componentN()` destructuring directly, which Kotlin synthesises
for every `data class`:

```kotlin
val (n, age) = person
```

See note 05 §11 for the full destructuring strategy.

Scoping is lexical and block-based. Inner blocks shadow outer bindings.
Kotlin allows shadowing of locals in inner scopes (unlike Java's "duplicate
local variable" error), so the backend emits Mochi names directly without
rename mangling for the shadowing case. Reserved-word collisions are
handled with backticks (`` `class` ``, `` `object` ``, `` `fun` ``); see §1.6.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the Kotlin-side representation:

| Mochi | Width / semantics | Kotlin lowering |
|-------|------------------|-----------------|
| `int` | 64-bit signed integer (inferred from integer literals) | `Long` (explicitly, not `Int`) |
| `float` | 64-bit IEEE 754 double | `Double` |
| `bool` | `true` / `false` | `Boolean` |
| `string` | UTF-8 text, indexable as code points, immutable | `String` (UTF-16 internally on every KMP target) |
| `time` | absolute timestamp (used by streams) | `kotlin.time.Instant` (stable since Kotlin 2.1, KEEP-371), falling back to `kotlinx.datetime.Instant` for pre-2.1 modules |
| `duration` | time interval (`std/time` API) | `kotlin.time.Duration` (stable since Kotlin 1.6) |
| `image` (preview) | binary blob (`load "cat.png"`) | `ByteArray` wrapped in `MochiImage` data class |

Why `Long` and not `Int`? Kotlin's `Int` is *unambiguously 32-bit* on every
target (JVM, Android, Native, JS, Wasm). This differs from C's `int` (which
is at least 16 bits and typically 32) and from Swift's `Int` (which is
platform-word-sized). Mochi's `int` is documented as 64-bit signed
regardless of host. Using Kotlin `Int` would silently truncate on every
Mochi program that handled values above 2^31; `Long` is unambiguous. The
cost is that array indices into Kotlin's `List<T>` and `Array<T>` require
`Int`, so the backend emits `i.toInt()` at the boundary (which throws on
overflow only when running under `Math.toIntExact`-equivalent guards,
otherwise it silently wraps; see note 06 §5 for the runtime guard policy).
The performance cost of `Long` over `Int` on JVM is small (modern HotSpot
specialises `Long` arithmetic to single x86-64 instructions), and on
Kotlin/Native it is identical. On Kotlin/JS, `Long` is more expensive
because JavaScript has no native 64-bit integer (Kotlin/JS represents
`Long` as a two-word `Long` class with method-level arithmetic). We accept
this cost; the alternative (silent 32-bit truncation) would be a Mochi
semantic violation.

Implicit numeric conversions are **not** allowed (per the type-checker
discipline implied by MEP-4/5/6 referenced from the threat model).
`int + float` is a Mochi type error; the program must `float(x)` first.
Kotlin's strong type system aligns perfectly here: `Long + Double` is also
a Kotlin compile error (Kotlin requires explicit `x.toDouble()` conversion,
in contrast to Java's silent widening). The Mochi checker and the Kotlin
checker reinforce each other; the backend never has to emit mixed
arithmetic because both layers reject it. See [[02-design-philosophy]] §16
on the "two-layer type wall" advantage.

Integer overflow in Kotlin **wraps silently** by default (the JVM specifies
two's-complement wrap on `+`, `-`, `*` for `int` and `long`, and Kotlin
inherits this on every target). Mochi's documented semantic is also silent
two's-complement wrap-around. The default lowering therefore emits Kotlin's
ordinary operators `+`, `-`, `*` and inherits wrap-around semantics for
free. The `--strict-int` build flag flips this to `Math.addExact(a, b)`,
`Math.subtractExact(a, b)`, `Math.multiplyExact(a, b)` (JVM 8+, polyfilled
on K/Native via inline `MochiRuntime.Math.addExact`) for security-sensitive
builds. Off by default, on for builds that opt into the audit profile. See
note 06 §5.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean `&& || !`;
membership `in`; string concatenation overloads `+`.

| Mochi | Kotlin |
|-------|--------|
| `a + b` (int)    | `a + b` (both `Long`, wrap-around by default) |
| `a + b` (float)  | `a + b` (both `Double`; IEEE NaN propagates) |
| `a + b` (string) | `a + b` (Kotlin `String + String` returns a fresh `String`; Kotlin compiles this to `StringBuilder.append` chains for hot loops via `StringConcatFactory.makeConcatWithConstants` on JVM 9+) |
| `a + b` (list)   | `a + b` (Kotlin `List<T> + List<T>` returns a fresh `List<T>` via the `plus` operator function on `Iterable<T>`) |
| `a - b` | `a - b` |
| `a * b` | `a * b` |
| `a / b` (float) | `a / b` |
| `a / b` (int)   | `Math.floorDiv(a, b)` (Kotlin's `Long / Long` is truncated division, not floored; Mochi's documented semantic is Python-style floor division) |
| `a % b` | `Math.floorMod(a, b)` (Kotlin's `%` is truncated remainder, not floored) |
| `a == b` (primitive) | `a == b` (Kotlin's `==` calls `equals`; for `Long`, `Double`, `Boolean`, this is identity-free value equality) |
| `a == b` (data class) | `a == b` (Kotlin synthesises `equals` field-by-field for `data class`) |
| `a != b` | `a != b` |
| `a < b`, `<=`, `>`, `>=` | numeric: native; string: `a < b` uses `Comparable<String>` lexicographic UTF-16 code-unit order (which differs from Mochi's specified code-point order for surrogate pairs; see §1.4 and note 06 §4 for the bridging helper) |
| `a && b` | `a && b` (short-circuit) |
| `a \|\| b` | `a \|\| b` |
| `!a` | `!a` |
| `x in xs` (list) | `x in xs` (Kotlin `in` calls `contains`) |
| `x in m` (map) | `x in m` (calls `containsKey`) |
| `x in s` (set) | `x in s` (calls `contains`) |

The lowering must respect Kotlin's separation between `==` (value equality
via `equals`) and `===` (reference identity, returns true iff the two
operands are the same object). Mochi has no reference identity, so the
backend never emits `===` in user code. The exception is FFI handles
that wrap JVM class instances (e.g., `java.net.URL`), where `===` may
appear in helper bridges but never in Mochi source lowerings.

### 1.4 Strings as read-only code-point sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a `Char`). Iteration yields
1-character strings in **code-point** order, not UTF-16 code-unit order.
Kotlin's `String` is the *thorniest* fit among all six backends because:

- Kotlin's `String` is **UTF-16 internally** on every KMP target. On JVM
  this is mandated by the JLS (java.lang.String backs onto `char[]` or,
  since JEP 254 in Java 9, a packed Latin-1 byte array when all code
  points are below 256). On Android the same JLS applies. On Kotlin/Native
  the runtime stores `String` as a UTF-16 buffer. On Kotlin/JS, JavaScript
  strings are themselves UTF-16, so Kotlin/JS `String` is the underlying
  JS string. On Kotlin/Wasm, the Wasm GC string proposal is still in flux,
  and the current implementation uses a UTF-16 buffer in the Wasm linear
  memory (or in a GC-managed object once Wasm GC strings land).
- Kotlin's `String.length` returns the UTF-16 *code-unit* count, not the
  code-point count and not the grapheme count.
- Kotlin's `text[i]` returns a `Char` (a 16-bit UTF-16 code unit). For
  characters outside the Basic Multilingual Plane (code points above
  U+FFFF, e.g. most emoji) a single Mochi code point is two Kotlin
  `Char`s.
- Iteration over `for c in text` walks UTF-16 code units by default
  (since Kotlin's `String` implements `CharSequence` which iterates
  `Char`s).

Concretely:

- `text[i]` lowers to `MochiRuntime.Strings.codePointAt(text, i)` which
  returns a single-character `String` formed from the `i`-th code point.
  On JVM this is `String(Character.toChars(text.codePointAt(text.offsetByCodePoints(0, i))))`.
  On Kotlin/Native the runtime ships a polyfill of the same shape. On
  Kotlin/JS, `String.codePointAt(i)` exists on modern JS engines
  (since ES2015) and the polyfill uses it. The runtime caches the
  UTF-16 offset for each Mochi index to avoid repeated O(n) seeks inside
  loops.
- `for ch in text` lowers to `for (cp in MochiRuntime.Strings.codePoints(text)) { val ch = MochiRuntime.Strings.fromCodePoint(cp); ... }`
  so each iteration sees a one-code-point string. On JVM the `codePoints()`
  method on `String` (JDK 8+) returns an `IntStream`; the runtime adapter
  wraps it as a Kotlin `Sequence<Int>`. On K/Native and K/JS the runtime
  walks the UTF-16 buffer manually.
- `len(text)` lowers to `MochiRuntime.Strings.codePointLength(text)`, not
  `text.length` (which is UTF-16 code-unit count) and not `text.toByteArray(Charsets.UTF_8).size`
  (which is UTF-8 byte count). The two alternative lengths are exposed
  as `code_unit_len` and `utf8_byte_len` in `MochiRuntime` for callers
  that explicitly want them.

This is the area where the Kotlin target is *most expensive* relative to
the Swift target: on Swift 5.7+ the internal storage is UTF-8 and code-point
access is cheap, while on Kotlin every code-point operation pays a UTF-16
walk. The cost is asymptotically the same (O(n) for full-string operations,
O(1) for fixed-position seeks after caching), but the constant factor is
higher. We accept this as the price of targeting the existing Kotlin
ecosystem; reimplementing `String` as a UTF-8 type would break interop
with every Kotlin library. See [[02-design-philosophy]] §6 on the UTF-16
cost analysis.

### 1.5 Literals

Integer literals; floating literals (`3.14`); boolean; string with C-style
escapes; triple-quoted multi-line strings (`"""..."""`); list `[...]`;
map `{key: val, ...}`; set `{a, b, c}`; record constructor `T { field: val }`.

The set literal `{a, b, c}` is distinguished from the empty/map literal
`{}` by the absence of `:` after the first element. The grammar must keep
these unambiguous; the Kotlin lowering picks the right constructor
accordingly.

Lowering forms:

| Mochi | Kotlin |
|-------|--------|
| `42` | `42L` (the literal `42` in Kotlin is `Int`; we force `Long` via the `L` suffix or an explicit type annotation) |
| `3.14` | `3.14` (Kotlin's floating-point literal defaults to `Double`, which is what we want) |
| `true` / `false` | `true` / `false` |
| `"hello"` | `"hello"` |
| `[1, 2, 3]` | `mutableListOf<Long>(1L, 2L, 3L)` (Kotlin's `listOf` returns an immutable `List<T>`; we use `mutableListOf` because Mochi list values are mutable by default) |
| `"""multi\nline"""` | Kotlin raw string literal `"""\nmulti\nline\n""".trimIndent()` (Kotlin's triple-quote rules differ on leading whitespace; the backend normalises with `trimIndent()`) |
| `{"a": 1, "b": 2}` | `linkedMapOf<String, Long>("a" to 1L, "b" to 2L)` (Kotlin's `mutableMapOf` on JVM defaults to `LinkedHashMap` which is insertion-ordered; we always use the explicit `linkedMapOf` to make insertion order a documented contract rather than an implementation detail) |
| `{1, 2, 3}` (set) | `linkedSetOf<Long>(1L, 2L, 3L)` (Kotlin's `linkedSetOf` returns a `LinkedHashSet<T>`, insertion-ordered) |
| `Book { title: "X", pages: 10 }` | `Book(title = "X", pages = 10L)` (Kotlin data class constructor with named arguments; see §4 on data class codegen) |

Kotlin collection literals are *not* a language feature (unlike Swift's
`[1, 2, 3]` syntax). Kotlin uses constructor calls (`listOf`, `mutableListOf`,
`mapOf`, `linkedMapOf`, `setOf`, `linkedSetOf`). The backend always emits
the appropriate constructor; there is no syntactic sugar for collection
construction.

Kotlin collection-bound `val` produces an *immutable reference* to a
mutable collection (the collection itself can still be mutated via its
mutating methods, but the reference cannot be reassigned). This is the
opposite of Swift where `let` on an array makes the array itself immutable.
For collections that *do* mutate (a `var` list with `.add()` calls) the
backend emits `var` and uses Kotlin's mutable collection types directly.
For Mochi's value-semantics guarantee that "each function call must
allocate a fresh copy of any list/map literal" (VM enhancement spec 0951),
the backend emits `toMutableList()` / `toMutableMap()` at the call boundary
to defensively copy. See note 06 §11.

### 1.6 Identifier mangling

Kotlin identifiers may begin with letter or `_` and continue with letter /
digit / `_`. Mochi identifiers are stricter, so every Mochi identifier is
a legal Kotlin identifier *until* it collides with a Kotlin keyword. Kotlin
has a clean escape: backticks. `` `class` ``, `` `fun` ``, `` `object` ``
are all legal Kotlin identifiers when wrapped in backticks, and the
backticks vanish at the use site (similar to Swift but with one subtle
difference: Kotlin backtick identifiers can contain any character except
backtick, newline, and forward slash, so they can also be used to name
JVM methods with arbitrary characters from Java interop).

Mangling rules (full table in note 06 §2 and §3):

- Mochi variables that collide with a Kotlin reserved word are wrapped in
  backticks (`` `class` ``, `` `object` ``, `` `fun` ``, `` `interface` ``,
  `` `val` ``, `` `var` ``). The reservation list is the Kotlin Language
  Reference §"Keywords and operators", augmented with Mochi-internal
  helpers (`$$mochi_*`).
- Mochi local function references and method names use the same backtick
  rule. Camel-case is preserved (`fooBar` stays `fooBar`).
- Mochi package paths `mathutils/extra` ⇒ Kotlin package
  `mochi.user.mathutils.extra` for user code (configurable via
  `--kotlin-package-prefix`; default `mochi.user`). The `user` segment is
  there to make the runtime / user distinction visible in stack traces
  and in Gradle dependency graphs.
- Mochi record type names ⇒ Kotlin `data class` names in PascalCase,
  unchanged (`Book` ⇒ `Book`). On collision with a `kotlin.*` or
  `java.lang.*` type (e.g. `String`, `Array`, `Date`, `URL`) the backend
  renames `String` ⇒ `String_` and emits a file-scope `typealias` only
  when the type really is internal.
- Mochi sum-type variant constructors ⇒ Kotlin sealed interface variants
  (PascalCase preserved, so Mochi `Leaf` becomes Kotlin `Leaf` as a
  `data object`, `Node` becomes Kotlin `Node` as a `data class`). Field
  labels are preserved verbatim. See §4 ADT lowering.

The mangling is deterministic (note 05 §3) and reversible via Kotlin's
`// $$mochi_source: file.mochi:line` line directives (which the emitter
writes for every Mochi source line, so debugger stack traces point back
to Mochi source rather than emitted Kotlin via the standard JVM
SourceDebugExtension SMAP mechanism on JVM, and via inline source maps
on Kotlin/JS). See note 10 §15.

### 1.7 Nullability

Mochi has no `null` at the language level. Optional values are expressed
via the `Option<T>` sum type. The Kotlin lowering has to make a deliberate
choice here, because Kotlin *does* have a built-in nullable type system
(every type `T` is non-nullable; `T?` is nullable) and the Kotlin community
overwhelmingly prefers it for optionality.

The decision: the Kotlin target **uses Kotlin's native nullable types
for Mochi `Option<T>`**. This is the same choice the Swift target made
(both targets have a built-in nullable type system that is too good to
ignore), and the opposite of the JVM bytecode target (MEP-47) which emits
its own sealed-interface `Option<T>` to dodge `java.util.Optional`
problems. The reasoning:

- Kotlin's nullable types are a real part of the type system: `T?` and
  `T` are distinct types, the compiler enforces null safety statically
  via flow-sensitive type narrowing (smart casts), and the JVM's NPE
  trap is the only runtime cost.
- Kotlin's null-safety operators (`?.`, `?:`, `!!`, `let { }`, `?.let { }`)
  are the idiomatic way to consume optionals; emitting our own `Option`
  type would force users (especially at FFI sites and Java interop sites)
  to convert back and forth.
- The Java platform APIs that Kotlin interops with use `null` for
  nullable values (annotated `@Nullable` for the few APIs that have
  nullness annotations); if Mochi `Option` were a separate type, every
  Java call would need a bridge.
- Kotlin/Native and Kotlin/JS preserve the same null-safety semantic.
  Kotlin/Native lowers `T?` to a tagged-pointer representation; Kotlin/JS
  uses JavaScript `null` and `undefined` (distinguished as both being
  the `null` member of `T?`).

The lowering therefore maps Mochi `Option<T>` directly to Kotlin `T?`.
Mochi `Some(x)` becomes Kotlin `x` (the implicit wrap when assigning to
`T?`), Mochi `None` becomes Kotlin `null`. The Mochi `Result<T, E>` type,
by contrast, is *not* mapped to Kotlin's built-in `kotlin.Result<T>`
because the latter is invariant in `T`, lacks an `E` type parameter, was
originally internal-only (made public in Kotlin 1.5 with the
`@Suppress("ResultOptInSurfaces")` opt-in), and does not preserve typed
error info. The backend emits a custom `MochiResult<T, E>` sealed class
instead. See §4 and §12.

At the FFI boundary, any value coming in from Kotlin code that is typed
as `T?` is funnelled through Mochi `Option<T>`; values typed as `T`
(non-nullable) bypass the wrapper. The Kotlin compiler enforces this
distinction statically through its null-safety checker, so no runtime
check is required.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Lowers to a Kotlin top-level function (Kotlin supports top-level
declarations natively, unlike Java which requires a wrapping class):

```kotlin
public fun add(a: Long, b: Long): Long {
  return a + b
}
```

Every Mochi source file produces one Kotlin file named after the file
(`example.mochi` ⇒ `Example.kt`) declaring all top-level functions,
top-level `val` / `var`, and any helper data classes at file scope.
There is no wrapping `object` or `class`. On the JVM, Kotlin top-level
functions are compiled into a synthetic class named `${FileNameKt}`
(e.g., `ExampleKt`), and the Kotlin compiler emits the appropriate
`@JvmName("...")` annotations to control the public Java name. The
backend emits `@file:JvmName("Example")` at the top of each emitted
Kotlin file so Java consumers see the cleaner name without the `Kt`
suffix.

The reason we use top-level declarations and not a wrapping `object` for
the module namespace is that Kotlin's top-level declarations are the
idiomatic way to express a module-level namespace, and they reduce the
nesting depth in generated code by one level compared to the
`object Example { ... }` form. This is the Kotlin equivalent of Swift's
"namespacing enum" idiom (MEP-49 §2.1), but expressed more directly
since Kotlin's compilation model already has the concept of a file-level
class.

Mochi `return` is explicit (unlike Erlang's "last expression"). The Kotlin
lowering preserves `return` directly: `return e;` becomes Kotlin `return e`.
For single-expression functions Kotlin allows the assignment-style form
(`fun foo() = expr` instead of `fun foo(): T { return expr }`), but the
backend always emits the block-body form for consistency with multi-
statement bodies and to keep the line-number map stable. See note 05 §7
on the early-return lowering.

The docs warn there is **no implicit tail-call optimisation** in Mochi.
The Kotlin compiler *does* perform tail-call optimisation, but only for
functions marked with the `tailrec` modifier (Kotlin 1.0+) and only for
direct self-recursion (not mutual recursion). The backend therefore
inspects the call graph at the Mochi IR level (note 05 §15): if a Mochi
function is directly self-recursive in tail position, the backend emits
`tailrec fun` and lets the Kotlin compiler convert it to a `while` loop.
For mutual recursion or non-tail recursion, the backend emits a
trampoline. The threshold is tunable via `--kotlin-trampoline-depth`,
default 1000.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Lower to Kotlin function types and lambdas:

```kotlin
val square: (Long) -> Long = { x -> x * x }
public fun apply(f: (Long) -> Long, value: Long): Long {
  return f(value)
}
```

Kotlin function types `(In) -> Out` are first-class types in the language
(unlike Java where they are erased to `kotlin.jvm.functions.FunctionN`
interfaces under the hood). On JVM, a lambda compiles to an anonymous
class implementing `FunctionN`, with `invoke` as the entry point. Since
Kotlin 1.4, lambdas are compiled to `invokedynamic` call sites via
`LambdaMetafactory` (the same mechanism Java 8 uses), reducing both
classfile size and runtime overhead.

For closures that must be invoked from a coroutine (i.e., that may suspend),
the type is `suspend (In) -> Out`:

```kotlin
val process: suspend (Long) -> Long = { x -> delay(10); x * x }
```

The Kotlin compiler enforces the `suspend` colour (cannot call `suspend`
from non-suspend context), which gives Mochi the same async-colour
enforcement that Swift provides via `async`. See [[02-design-philosophy]]
§12 on the coroutine model.

Closures escape freely; captured variables in Kotlin are by default
captured by reference for `var` and by value for `val`. The backend emits
Mochi `var` captures as Kotlin `var`s and lets the Kotlin compiler box
them into `Ref.ObjectRef` / `Ref.LongRef` automatically when they cross
a lambda boundary. For closures that escape a function and capture
`this`, the backend does not need explicit capture lists (unlike Swift's
`[weak self]` for ARC cycle breaking) because Kotlin's GC handles cycles
automatically. See note 05 §16 for the capture policy.

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block are
unqualified. Lowering: the record is a Kotlin `data class` and the method
is an instance method on that class:

```kotlin
public data class Circle(
  val radius: Double,
) {
  public fun area(): Double {
    return 3.14 * radius * radius
  }
}
```

This is the cleanest available lowering on Kotlin 2.1: data classes are
reference types (allocated on the heap, GC-managed) but they synthesise
`equals` / `hashCode` / `toString` / `copy` / `componentN` automatically
from the primary constructor parameters. They are intentionally analogous
to Scala case classes and Swift structs (though without value semantics
on the heap).

Kotlin data classes do *not* have value semantics in the language model;
they are reference types like every other Kotlin class. To preserve
Mochi's value-semantics contract (records are copied by value when
assigned or passed), the backend either: (a) relies on the synthesised
`copy()` function for explicit copying when Mochi semantics demand a
defensive copy, or (b) ensures all data class fields are themselves
immutable (`val`, not `var`) so that aliasing without mutation is safe.
The default codegen uses (b): every data class field is `val`. For
records with a `var` field (mutable), the lowering keeps `data class`
but marks the field `var`:

```kotlin
public data class Counter(
  var count: Long,
)
```

The backend then emits an explicit `.copy(...)` call at every Mochi
assignment site to preserve value semantics. See note 06 §4.

For records that need to participate in Kotlin's `Comparable<T>` (e.g.,
used as sort keys), the backend emits the `Comparable` implementation
explicitly since data class does not synthesise it. For records that
need `Serializable` (for `java.io.Serializable`-based interop on JVM)
or `kotlinx.serialization.Serializable` (for JSON / CBOR / ProtoBuf
serialization), the backend emits the `@Serializable` annotation from
`kotlinx.serialization` unconditionally on every Mochi record. The cost
is one extra annotation per data class; the benefit is that any Mochi
record can be serialised without further user action.

Field access inside a method is direct (`this.radius`, or just `radius`
with implicit this). Mochi field names are preserved verbatim; the
backend never renames them unless they collide with a Kotlin keyword (in
which case backticks).

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end.

Lowers to a runtime call `MochiRuntime.IO.print(vararg args: Any?)` where
the runtime walks the varargs, applies per-type formatting (since Kotlin's
`Any.toString()` does the right thing for primitives but the wrong thing
for `List`/`Map` whose `toString()` produces Kotlin's own `[1, 2, 3]` or
`{a=1, b=2}` syntax rather than Mochi's), inserts single-space separators,
and writes the trailing newline via Kotlin's `println(...)` on JVM /
Android / Native / JS / Wasm. On Kotlin/JS the runtime targets
`console.log` rather than stdout (which does not exist in the browser);
on Kotlin/Wasm it targets the WASI `fd_write` syscall (when running
under wasmtime) or `console.log` (when running under a JS host). The
runtime caches a `PrintStream` reference locally and flushes on every
newline (matching Mochi's "line-buffered stdout" guarantee). See note
04 §3 for the `MochiRuntime.IO` module.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. ⇒ Kotlin `MutableList<T>` (backed by
  `ArrayList<T>`), value-shaped via defensive copy at call boundaries.
- `map<K, V>`, keyed lookup, with insertion-order iteration. ⇒
  `LinkedHashMap<K, V>`, the JVM and KMP default for `mutableMapOf` since
  Kotlin 1.0.
- `set<T>`, unordered (in the type-theoretic sense), unique members, with
  insertion-order iteration. ⇒ `LinkedHashSet<T>`.
- `string`, read-only sequence of code points (see §1.4). ⇒ Kotlin
  `String`.

Lowering obligations (full per-type details in note 06 §1):

- `list<T>` is the workhorse. The default representation is Kotlin's
  `MutableList<T>`, which is backed by `ArrayList<T>` (a wrapper around
  Java's `java.util.ArrayList` on JVM, a Kotlin-native `ArrayList`
  implementation on K/Native, a JS array on K/JS, a Wasm-backed array
  on K/Wasm). Random access is O(1). Element storage is *boxed* for
  `Long`, `Double`, `Boolean`, etc. (Kotlin's `MutableList<Long>` is
  `MutableList<java.lang.Long>` on JVM at runtime; the boxing is the
  classic JVM cost). This is the biggest single performance disadvantage
  of the Kotlin target compared to the Swift target: `Array<Long>` on
  Swift is a true `Int64[]`-equivalent unboxed buffer, while Kotlin's
  `List<Long>` always boxes. For the performance-sensitive cases,
  Mochi exposes `intlist<int>` / `floatlist<float>` types that lower to
  Kotlin `LongArray` / `DoubleArray` (Kotlin's primitive-array types
  that store unboxed values). The default `list<T>` lowering uses the
  boxed form; the typed-array form is opt-in.

- `map<K, V>` defaults to `LinkedHashMap<K, V>` (referenced via the
  `MutableMap<K, V>` interface). Mochi's iteration order is insertion
  order, documented in `docs/features/map.md`. Kotlin's `mutableMapOf()`
  constructor returns a `LinkedHashMap` by default on JVM (verified via
  the implementation of `kotlin.collections.mutableMapOf`), so the
  default lowering is already insertion-ordered. The backend uses
  `linkedMapOf()` explicitly rather than `mutableMapOf()` to make this
  a documented contract rather than an implementation detail (the JVM's
  `HashMap` is *not* insertion-ordered, and a future Kotlin version could
  theoretically change the default; the explicit `LinkedHashMap` removes
  this risk).

- `set<T>` is `LinkedHashSet<T>` for the same reason. The query layer
  (§5) needs the *insertion-ordered* semantic for `union` / `except` to
  be deterministic.

- All collections are **value-semantically copied** at language level.
  Kotlin's reference types do not provide this for free (unlike Swift's
  value types with copy-on-write), so the backend emits explicit defensive
  copies at function-call boundaries. The VM enhancement spec 0951 §1
  ("each function call must allocate a fresh copy of any list/map
  literal") is satisfied by emitting `arg.toMutableList()` /
  `arg.toMutableMap()` at every callsite where a list / map is passed
  to a function. The cost is one O(n) copy per call; for hot loops the
  optimiser can sometimes elide the copy via escape analysis (HotSpot's
  C2 does this routinely), but the worst case is one copy per call. This
  is the largest performance cost of the Kotlin target relative to
  immutable-by-default targets like Swift. See [[02-design-philosophy]] §7.

Mutation operations (`xs.add(x)`, `m[k] = v`) lower to direct Kotlin
mutating method calls (`xs.add(x)`, `m[k] = v`) when the target is a
`var` binding, or to a copy-and-mutate helper that takes the immutable
collection, returns a fresh mutated copy, and rebinds. The runtime ships
a `MochiRuntime.Collections` module that exposes the Mochi-shaped helpers
(`appended(x)`, `inserting(x, at = i)`, `removing(at = i)`, `updating(k, with = v)`)
as extension functions on `List<T>` / `Map<K,V>`. See note 06 §11.

`for x in xs` lowers to a Kotlin `for ... in` loop:

```kotlin
for (x in xs) { ... }
```

For maps, `for (k, v) in m` lowers to:

```kotlin
for ((k, v) in m) { ... }
```

Kotlin's `Map` iteration produces `Map.Entry<K, V>` values, and the
destructuring `(k, v)` uses the `componentN` operators on `Map.Entry`
(synthesised by the Kotlin stdlib). The runtime overhead is negligible
(one allocation per entry on JVM, which HotSpot routinely scalarises).

## 4. Algebraic data type core

Mochi's sum-of-products data types (`type Tree = Leaf | Node { ... }`) are
the cleanest fit for Kotlin `sealed interface` with `data class` and
`data object` variants. `sealed class` has been in Kotlin since 1.0;
`sealed interface` arrived in Kotlin 1.5 (KEEP-213, 2021-05) and is
preferred because variants can also implement other interfaces. `data object`
arrived in Kotlin 1.9 (2023-07) and is the idiomatic form for nullary
variants (singletons with synthesised `toString`, `equals`, `hashCode`).

```mochi
type Tree =
  | Leaf
  | Node { value: int, left: Tree, right: Tree }
```

Lowers to:

```kotlin
public sealed interface Tree {
  public data object Leaf : Tree
  public data class Node(
    val value: Long,
    val left: Tree,
    val right: Tree,
  ) : Tree
}
```

The `sealed interface` declaration constrains the set of permitted
subtypes to those declared in the same file (or in the same compilation
unit on the JVM). This gives the Kotlin compiler the information it
needs to verify exhaustiveness on `when` expressions. Unlike Swift's
recursive enum cases (`indirect case`), Kotlin's sealed hierarchy
automatically supports recursive type references because the variants
are reference types and their fields are reference-typed.

Field labels on the case payload (`value`, `left`, `right`) are
preserved from the Mochi syntax. Construction is `Tree.Node(value = 42L, left = Tree.Leaf, right = Tree.Leaf)`;
the Kotlin compiler synthesises the data class constructor and
`Tree.Leaf` is a singleton object accessible by name.

Pattern matching:

```mochi
match t {
  Leaf => 0
  Node { value, left, right } => value + sum(left) + sum(right)
}
```

Lowers to a Kotlin `when` expression with smart casts:

```kotlin
return when (t) {
  is Tree.Leaf -> 0L
  is Tree.Node -> t.value + sum(t.left) + sum(t.right)
}
```

Kotlin's `when` is an expression since Kotlin 1.0. Exhaustiveness on a
sealed hierarchy is enforced by the compiler since Kotlin 1.7 (the
"exhaustive when" warning was promoted to an error in 1.7, KEEP-213-followup);
Kotlin 2.1's K2 frontend further improves smart casts on sealed `when`
to correctly narrow within complex flow patterns (KEEP-358). The Mochi
checker's exhaustiveness check and the Kotlin checker's exhaustiveness
check agree; the backend never has to emit an `else` arm for sealed
when. See note 05 §10.

Field destructuring on the matched value uses Kotlin's data class
destructuring inside the arm body:

```kotlin
is Tree.Node -> {
  val (value, left, right) = t
  value + sum(left) + sum(right)
}
```

The backend emits the destructuring form for matches that consume more
than two fields; for one- or two-field matches it accesses fields
directly (`t.value`, `t.left`). The choice is a readability heuristic;
both lowerings produce identical bytecode on JVM after inlining.

Guarded patterns:

```mochi
match shape {
  Circle { radius } if radius > 10.0 => "big"
  Circle { radius }                  => "small"
  _                                  => "other"
}
```

Lower to Kotlin `when` with guards:

```kotlin
return when {
  shape is Shape.Circle && shape.radius > 10.0 -> "big"
  shape is Shape.Circle -> "small"
  else -> "other"
}
```

Kotlin 1.4 added the *subject-less* `when` (just `when { ... }` with
boolean conditions on each arm), which is the natural fit for guarded
patterns. The smart cast on `shape is Shape.Circle` makes `shape.radius`
accessible without an explicit cast inside the guarded arm. Kotlin's
upcoming KEEP-323 (`when` with guard clauses, status: in design as of
2025-Q3) proposes a syntactic guard form `is Foo if cond ->` but is not
yet in the language; the subject-less form is the working substitute.

Generic ADTs:

```mochi
type Option<T> = | Some { value: T } | None
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Lower with generic type parameters on the sealed hierarchy. For
`Option<T>` the backend does NOT emit a custom type (it maps to Kotlin
`T?` per §1.7). For other generic sums:

```kotlin
public sealed interface Result<out T, out E> {
  public data class Ok<T>(val value: T) : Result<T, Nothing>
  public data class Err<E>(val error: E) : Result<Nothing, E>
}
```

The `out` variance annotations (KEEP-29, since Kotlin 1.0) make the
sealed interface covariant in both type parameters, mirroring Swift's
default and matching Mochi's covariant Result semantics. The `Nothing`
bottom type is used in each variant to express that the *other* type
parameter is unconstrained from that variant's perspective. This is the
idiomatic Kotlin pattern for variant-aware Result types and is documented
in the Kotlin standard library's own `kotlin.Result` (though that type
is invariant, not what we want).

For `Result<T, E>` the backend emits the user sealed interface (always
named `MochiResult<T, E>` to avoid collision with `kotlin.Result<T>`).
See the errors discussion section §12 for the typed-throw interaction.

## 5. Query DSL

Mochi's query DSL (`from x in xs select { ... }`, with `join`, `group by`,
`order by`, `limit`, `offset`, `where`) is the densest sub-language. Its
Kotlin lowering uses Kotlin's `Sequence<T>` chain (for synchronous
finite collections) as the primary IR, with `Flow<T>` chains for
streaming queries. Full lowering is in note 08; this section names the
surface forms only.

```mochi
let adults =
  from p in people
  where p.age >= 18
  order by p.name
  select { name: p.name, age: p.age }
```

Lowers to:

```kotlin
val adults: List<AdultRow> = people
  .asSequence()
  .filter { it.age >= 18 }
  .sortedBy { it.name }
  .map { AdultRow(name = it.name, age = it.age) }
  .toList()
```

The `.asSequence()` prefix is added when the pipeline has more than one
stage, so intermediate allocations are skipped (Kotlin's `Sequence<T>`
evaluates lazily, similar to Java 8 `Stream` but without the parallel
flag and without the auto-close semantic). The materialisation is the
`.toList()` terminal operator.

`group by` lowers to `groupBy`:

```kotlin
val byDept: Map<String, List<Person>> = people.groupBy { it.dept }
```

Kotlin's `groupBy` returns a `LinkedHashMap<K, List<V>>`, which preserves
insertion order (the iteration order is the order in which keys first
appeared). This matches Mochi's documented semantic for `group by`. See
note 08.

`join` lowers to a hash-join helper in `MochiRuntime.Query` (since
Kotlin's `Sequence` has no built-in cross-collection join). `limit` /
`offset` are direct Sequence operations (`take(n)`, `drop(n)`).

Important: the **Mochi `stream<T>` type and the Kotlin `Sequence<T>`
protocol are not the same thing**. Kotlin's `Sequence` is a synchronous
iteration protocol (with lazy evaluation); Mochi's `stream<T>` is a
*time-evolving* publisher (closer to `Flow<T>` from kotlinx.coroutines).
The query DSL uses Kotlin's `Sequence` for *finite* collection queries;
the public `stream<T>` type uses `Flow<T>` (and concretely `SharedFlow<T>`
/ `StateFlow<T>` for hot variants). See note 09 for the agent / stream
lowering and [[06-type-lowering]] §7 for the type-level distinction.

## 6. Stream and agent core

```mochi
stream Tick = { time: time }

agent ticker {
  every 1s emit Tick { time: now() }
}
```

Streams lower to Kotlin `Flow<T>` (cold flow, from kotlinx.coroutines
1.3+, stable since 1.4 in 2020-08). Agents lower to a custom actor class
wrapping a `Channel<Message>` (from kotlinx.coroutines). The kotlinx.coroutines
`actor { }` builder has been **deprecated** in the library since 2018
and marked obsolete in the 1.7 release (2023-05); the canonical actor
shape since then is a custom class with a private channel and a launched
receive loop. See [[02-design-philosophy]] §8.

```kotlin
public class Ticker(
  private val scope: CoroutineScope = CoroutineScope(SupervisorJob() + Dispatchers.Default),
) {
  private val mailbox: Channel<Message> = Channel(capacity = Channel.UNLIMITED)
  private var job: Job? = null

  public fun start() {
    job = scope.launch {
      for (msg in mailbox) {
        handle(msg)
      }
    }
  }

  public fun send(msg: Message) {
    mailbox.trySend(msg)
  }

  public suspend fun sendAndWait(msg: Message) {
    mailbox.send(msg)
  }

  public fun stop() {
    mailbox.close()
    job?.cancel()
  }

  private suspend fun handle(msg: Message) {
    // dispatch to per-message handler
  }
}
```

Key design points:

- The mailbox is a `Channel<Message>` with `Channel.UNLIMITED` capacity,
  giving an unbounded FIFO queue. Mochi's `bounded(N)` qualifier lowers
  to `Channel(capacity = N)` with the default `BufferOverflow.SUSPEND`
  policy (sender suspends when full).
- The actor's main loop is `for (msg in mailbox) { ... }`, which is the
  Kotlin idiom for channel iteration. When the channel is closed
  (either explicitly via `mailbox.close()` or when the actor's scope is
  cancelled), the loop terminates and the actor stops.
- `send(msg)` is `trySend`, returning a `ChannelResult` that callers can
  inspect for backpressure handling. For fire-and-forget (Mochi's `!`
  cast operator) the result is ignored. For `sendAndWait` the call
  suspends until the value is buffered or accepted by the receiver,
  matching Mochi's `agent.method() -> reply` form.
- Spawning child coroutines is `scope.launch { ... }`, which integrates
  with structured concurrency. The launched coroutine inherits the
  parent's `Job` (under the `SupervisorJob` so a failure in one child
  does not cancel siblings) and dispatcher.
- Cancellation propagates via `scope.cancel()`. Cooperative cancellation
  checkpoints (`yield()`, `ensureActive()`) are inserted at every loop
  back-edge to keep cancellation responsive. See the shared-decisions anchor
  §"Concurrency model".

Supervision is *not* built into Kotlin's coroutine model the way it is in
BEAM's `gen_server`. We provide it in user space via the
`MochiRuntime.Supervisor` class, which holds a map of child agents,
restarts them on failure (catching the `Job`'s thrown exception via
`CoroutineExceptionHandler`), and applies a configurable restart
strategy (`oneForOne`, `oneForAll`, `restForOne`, matching BEAM's
nomenclature). See note 09 §5.

Agents talk via typed message types (Mochi `stream Tick = {...}` becomes
a Kotlin data class, and `agent.emit(Tick { time: now() })` becomes
`agent.send(Message.Tick(Tick(time = Clock.System.now())))`):

```mochi
ticker ! Tick { time: now() }
```

Lowers to:

```kotlin
ticker.send(Message.Tick(Tick(time = Clock.System.now())))
```

Kotlin has no `Sendable`-equivalent annotation or compile-time check.
Structured concurrency provides isolation via the channel boundary: a
value sent through a `Channel<T>` is captured by the channel and the
receiver gets the same reference, so the sender must voluntarily not
mutate it after sending. Mochi's value-semantics contract on records
(immutable `val` fields by default) makes this safe in the common case.
For records with `var` fields, the backend either: (a) emits a defensive
`.copy()` at the send site, or (b) relies on the user to discipline
themselves (the default; the cost of universal defensive copy is high).
See [[02-design-philosophy]] §8 for the Sendability discussion.

The `kotlinx-coroutines-core` library provides higher-level Flow
operators (`merge`, `combine`, `debounce`, `sample`, `throttleFirst`,
`buffer`, `conflate`, `flatMapConcat`, `flatMapMerge`, `flatMapLatest`)
that the Mochi stream DSL maps onto. The runtime depends on
`kotlinx-coroutines-core` as a first-class dependency. See note 09 §6.

## 7. Logic programming core

```mochi
fact parent(alice, bob).
fact parent(bob, charlie).

rule ancestor(X, Y) :- parent(X, Y).
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

query ancestors_of(X) := ancestor(alice, X).
```

The logic core targets a small embedded Datalog engine. The Kotlin
lowering emits a runtime call into `MochiRuntime.Datalog.Engine`, with
facts and rules registered at module init time. The engine implements
semi-naive bottom-up evaluation; magic-set transforms are a v2 concern.

Datalog terms are represented as `MochiDatalogTerm` (a sealed interface
with `data class Atom(val name: String)`, `data class IntTerm(val value: Long)`,
`data class StringTerm(val value: String)`, `data class ListTerm(val values: List<MochiDatalogTerm>)`,
and `data class Compound(val head: String, val args: List<MochiDatalogTerm>)`).
Predicates are `(List<MochiDatalogTerm>) -> Boolean` lambdas. Unification
is a recursive walk that binds variables in a
`MutableMap<String, MochiDatalogTerm>` substitution map. See note 08 §4
for the engine internals.

The Kotlin target's Datalog engine has one differentiator over the C and
BEAM engines: thanks to Kotlin's reified generics on JVM and the
specialised primitive-array types (`LongArray`, `IntArray`), tight inner
loops in the engine can run on unboxed long-keyed indices, reducing GC
pressure for million-fact workloads. Benchmarks in note 18 measure a
1.1-1.5x throughput improvement over the .NET engine for typical
Datalog programs.

## 8. AI and FFI shells

### 8.1 AI shell

```mochi
let summary = ai("summarise this", text)
let result = generate("write a haiku about ", topic)
```

Mochi has two AI builtins: `ai(...)` (synchronous one-shot) and
`generate(...)` (streaming token-by-token). Both lower to suspend
functions returning the appropriate type.

`ai(...)` lowers to `MochiRuntime.AI.call(prompt: String, vararg args: Any?): String`
as a suspend function. The body of `call(...)` dispatches based on
provider configuration (env vars at runtime, not codegen choices):

- On Android (API 31+, Pixel 8+ with Gemini Nano) the backend prefers
  Google's **AICore SDK** (in beta as of 2025, integrating Gemini Nano
  for on-device inference). The dispatch uses
  `com.google.ai.edge.aicore.LanguageModel.generateContent(prompt)`.
- On Apple platforms via Kotlin/Native iOS targets, we can call into
  Apple's `FoundationModels` framework via cinterop (see §8.2).
- When no on-device provider is available (Linux, Windows, server JVM,
  or when the env var `MOCHI_AI_PROVIDER=openai` is set) the backend
  falls back to **OpenAI / Anthropic / local Ollama** HTTP clients
  built on Ktor (`io.ktor:ktor-client-core` 3.0+, with platform-specific
  engines: `ktor-client-okhttp` on JVM/Android, `ktor-client-darwin`
  on K/Native iOS/macOS, `ktor-client-curl` on K/Native Linux/Windows,
  `ktor-client-js` on K/JS, `ktor-client-cio` as a pure-Kotlin
  fallback).
- The provider selection happens at runtime via the
  `MochiRuntime.AI.Provider` interface; codegen always emits the
  interface-typed call and lets the runtime pick. See note 04 §10.

`generate(...)` returns `Flow<MochiToken>` where `MochiToken` is a small
data class carrying the token text plus metadata (logprobs,
finish-reason). On Android with AICore the backend wraps the streaming
callback in a `callbackFlow`; on other platforms it wraps the HTTP SSE
stream in a `flow { ... emit(...) }` builder using Ktor's
`ChannelFlow` adapter. See note 04 §10.

### 8.2 FFI shell

```mochi
let result = ffi("std/json/parse", raw)
extern fun sqrt(x: float): float = "c:sqrt"
```

The `ffi(...)` builtin lowers to a `MochiRuntime.FFI.call(path: String, vararg args: Any?): Any?`
suspend function that looks up the named function in a module registry.
For Mochi-to-Mochi FFI calls the registry just dispatches to the right
module's top-level function.

The `extern fun` form is the rich case: it declares a foreign function
and lets Mochi code call it directly. The lowering depends on the target:

For JVM and Android (`extern fun sqrt(x: float): float = "java:java.lang.Math.sqrt"`)
the backend emits a Kotlin top-level function that calls the JVM method:

```kotlin
public fun sqrt(x: Double): Double {
  return java.lang.Math.sqrt(x)
}
```

For JNI bindings (`extern fun sqrt(x: float): float = "jni:libm.sqrt"`)
the backend emits a Kotlin `external` declaration plus a `System.loadLibrary`
init block:

```kotlin
public external fun sqrt(x: Double): Double

private val __mochi_jni_load = run {
  System.loadLibrary("m")
  Unit
}
```

`external` is Kotlin's keyword for JNI declarations (since Kotlin 1.0),
equivalent to Java's `native` modifier. The companion C-side stub uses
the standard JNI naming convention (`Java_mochi_user_module_sqrt`).

For Kotlin/Native cinterop (`extern fun sqrt(x: float): float = "c:sqrt"`)
the backend generates a `.def` file for the cinterop tool and emits a
direct call:

```kotlin
import platform.posix.sqrt as cSqrt

public fun sqrt(x: Double): Double {
  return cSqrt(x)
}
```

Kotlin/Native's cinterop tool (`klib-cinterop`) consumes a `.def` file
that names C headers and library dependencies; it produces a Kotlin
library binding the C declarations. The backend writes the `.def` file
alongside the Mochi-generated Kotlin source.

For Kotlin/JS external declarations (`extern fun sqrt(x: float): float = "js:Math.sqrt"`)
the backend emits a Kotlin `external` declaration in the `jsMain` source
set:

```kotlin
// in jsMain
@JsName("Math")
external object MathJs {
  fun sqrt(x: Double): Double
}

public actual fun sqrt(x: Double): Double = MathJs.sqrt(x)
```

`external` declarations in Kotlin/JS map directly to JavaScript references
without any conversion shim. The `@JsName` annotation controls the JS name
seen at the boundary.

For Kotlin/Wasm interop the story is shifting as the Wasm GC and
component-model proposals stabilise. The current approach uses
`@JsFun` for Wasm-JS bindings (when the Wasm module is hosted in a JS
runtime) and standard Wasm imports for the host-agnostic case. See
note 11 for the full FFI matrix.

For Mochi-as-library exports (Mochi code called by Java, Kotlin, Swift,
or C consumers), the backend emits the appropriate platform-specific
annotation:

- JVM/Android: `@JvmStatic` on `object` companions, `@JvmName` for clean
  symbol names, `@JvmOverloads` for default-argument variants.
- Native: `@CName("mochi_my_function")` for C-ABI exports (Kotlin/Native
  attribute since 1.4).
- JS: `@JsExport` for ES module exports (since Kotlin 1.4).
- Wasm: `@JsExport` for the wasmJs target, `@WasmExport` for the
  wasmWasi target.

The Mochi-to-Kotlin FFI surface is one of the load-bearing reasons we
want a Kotlin target at all (see [[02-design-philosophy]] §1): it unlocks
the entire JVM ecosystem (Maven Central, Gradle plugins, Android), the
Apple ecosystem via Kotlin/Native (XCFramework export consumed by Swift),
and the JS / Wasm ecosystems. See note 11.

## 9. Strings, errors, concurrency colouring

### 11. Strings

Covered in §1.4 above. The Kotlin target is the *opposite* of the Swift
target's UTF-8-native cleanliness: Kotlin's `String` is UTF-16 on every
target, so every Mochi code-point operation pays a UTF-16-to-code-point
conversion. The cost is well-understood (the JDK has been UTF-16 since
1.0 in 1996; JEP 254 in Java 9 added Latin-1 compaction for ASCII-heavy
strings) and the JIT optimises the common cases (ASCII-only loops, BMP
characters) to near-zero overhead.

Cross-boundary string costs:

- Mochi `string` ⇒ Kotlin `String`: zero copy (both internal representations
  are the same `String` object).
- Kotlin `String` ⇒ Mochi `string`: zero copy.
- Mochi `string` ⇒ Java `byte[]` (UTF-8 encoded for HTTP, JSON, file I/O):
  one O(n) encoding pass via `string.encodeToByteArray()` or
  `string.toByteArray(Charsets.UTF_8)` on JVM.
- Mochi `string` ⇒ C `char *` via JNI: one O(n) encoding pass plus one
  allocation (the JNI `GetStringUTFChars` family copies into a UTF-8
  buffer). The cost is the same on Kotlin/Native via the cinterop
  `String.cstr` extension, which copies into a stable C buffer.
- Mochi `string` ⇒ Kotlin/JS string: zero copy (both are JS strings,
  i.e., UTF-16 buffers).

See [[06-type-lowering]] §4 for the full string-bridging table.

### 12. Errors

Mochi's error story is built on the `Result<T, E>` sum type and on
typed `throw e` / `catch e` blocks. Kotlin has **no checked exceptions**
(a deliberate language design choice; Kotlin's stance is that Java's
checked-exception model has been a failed experiment). Kotlin functions
that throw use the unchecked `throw e` form, where `e` is any
`Throwable`. To preserve Mochi's typed-error semantic, we **do not use
exceptions** for Mochi error reporting; instead, every Mochi function
that declares `throws E` lowers to a Kotlin function returning
`MochiResult<T, E>`.

Lowering rules:

- Mochi `fun foo(): T throws E { ... }` becomes Kotlin
  `fun foo(): MochiResult<T, E> { ... }`. The body returns
  `MochiResult.Ok(value)` for the success path and `MochiResult.Err(error)`
  for the failure path.
- Mochi `try foo()` becomes Kotlin `foo().getOrThrow()` (where
  `getOrThrow` rethrows wrapping `Err.error` in a Kotlin
  `MochiThrownException(error)` for callers that want exception-style
  propagation), or `foo().getOrNull()` for the `T?` variant.
- Mochi `try foo() catch e => handler` becomes Kotlin
  ```kotlin
  when (val r = foo()) {
    is MochiResult.Ok -> r.value
    is MochiResult.Err -> { val e = r.error; handler }
  }
  ```
- Mochi `Result<T, E>` becomes Kotlin `MochiResult<T, E>` (the custom
  sealed class). Mochi `result.ok(x)` ⇒ `MochiResult.Ok(x)`,
  `result.err(e)` ⇒ `MochiResult.Err(e)`.
- Mochi `try?` (optional unwrap of result) becomes Kotlin
  `foo().getOrNull()`, returning `T?`.
- Mochi `try!` (forced unwrap) becomes Kotlin `foo().getOrThrow()`,
  throwing on `Err`.

The `MochiResult<T, E>` sealed class is defined in `MochiRuntime`:

```kotlin
public sealed interface MochiResult<out T, out E> {
  public data class Ok<T>(val value: T) : MochiResult<T, Nothing>
  public data class Err<E>(val error: E) : MochiResult<Nothing, E>

  public fun getOrNull(): T? = when (this) {
    is Ok -> value
    is Err -> null
  }

  public fun getOrThrow(): T = when (this) {
    is Ok -> value
    is Err -> throw MochiThrownException(error as Any)
  }
}
```

We do *not* use Kotlin's built-in `kotlin.Result<T>` because it is
invariant in `T`, lacks the `E` parameter, and was originally restricted
to internal use (the restriction was lifted in Kotlin 1.5 but the type
signature is still a poor fit). See [[06-type-lowering]] §9.

For interop with Java code that does throw, the backend emits
`@Throws(MyException::class)` annotations on Kotlin functions whose
Java contract throws checked exceptions. The `@Throws` annotation
(KEEP-7, since Kotlin 1.0) is the bridge between Kotlin's no-checked-
exception model and Java's checked-exception model. See note 06 §9.

### 13. Concurrency colouring

Mochi distinguishes synchronous and asynchronous functions (`fun` vs
`async fun`). Kotlin's `suspend` modifier and coroutine machinery maps
almost perfectly:

- Mochi `fun foo(): T { ... }` ⇒ Kotlin `fun foo(): T { ... }`.
- Mochi `async fun foo(): T { ... }` ⇒ Kotlin `suspend fun foo(): T { ... }`.
- Mochi `await foo()` ⇒ Kotlin `foo()` (the `await` is implicit; calling
  a `suspend` function from a `suspend` context suspends automatically).
- Mochi `spawn foo()` (fire-and-forget) ⇒ Kotlin `scope.launch { foo() }`
  where `scope` is the enclosing `CoroutineScope`.

Isolation domains:

- An `agent` lowers to a custom actor class (see §6), and the methods on
  the agent are conceptually isolated to the actor's single-threaded
  receiver dispatcher.
- A `@main` Mochi program (the top-level entry) lowers to a Kotlin
  `fun main()` (top-level), with the body wrapped in `runBlocking { ... }`
  for synchronous-blocking execution or just `suspend fun main()` for
  the natural suspend entry (Kotlin 1.3+ supports `suspend fun main`).
- Mochi code that needs to run on the UI thread (interop with Compose
  on Android) is marked with `@ui` in Mochi, which lowers to a
  `withContext(Dispatchers.Main) { ... }` wrapper. There is no
  `@MainActor` equivalent annotation in Kotlin; isolation is by
  dispatcher choice, not by type annotation. This requires an explicit
  Mochi annotation; the transpiler does not infer it.
- `nonisolated` is irrelevant in Kotlin (there is no `Sendable`-style
  check to bypass). The backend emits no equivalent annotation.

Kotlin has no compile-time data-race check (the analogue of Swift 6's
strict-concurrency mode). Structural protection comes from:
- Channels: values sent through `Channel<T>` are owned by the receiver
  on receive.
- Coroutine context: each coroutine has its own `CoroutineContext`,
  including its dispatcher; concurrent coroutines on different
  dispatchers share state only through explicit shared references.
- Mochi's value-semantics contract: records are immutable by default,
  collections are defensively copied at call boundaries, so the natural
  Mochi style produces code that is already free of data races.

This is the largest semantic gap between MEP-50 (Kotlin) and MEP-49
(Swift): Swift's strict-concurrency catches sharing bugs at compile
time, Kotlin catches none of them. The mitigation is: (a) Mochi's own
type checker rejects sharing patterns at the Mochi level (so the
Kotlin codegen never emits unsafe sharing), (b) the Mochi runtime's
collection wrappers defensively copy at boundaries, and (c) Mochi
agents serialise via channel so cross-agent state is naturally isolated.
See [[02-design-philosophy]] §8 for the Sendability discussion.

Other Kotlin 2.1 features the lowering uses:

- `data object` (Kotlin 1.9, KEEP-317) for singleton ADT variants with
  synthesised `toString` / `equals` / `hashCode`.
- `sealed interface` (Kotlin 1.5, KEEP-213) for non-recursive sum types.
- `value class` (KEEP-237, formerly `inline class`, since Kotlin 1.5)
  for zero-allocation wrapper types where the Mochi spec calls for a
  newtype. Currently unused in the core lowering but reserved for
  Mochi `newtype` declarations.
- Context receivers (KEEP-259, status: experimental in Kotlin 2.0, may
  reach stable in 2.2 or be replaced by KEEP-374 "context parameters")
  for ergonomic dependency injection. Not used in core lowering.
- Multidollar string interpolation (`$$...$$` for nested interpolation,
  Kotlin 2.1+) for the multi-line string lowering when the inner
  string contains `${}` syntax.
- Kotlin's `Result<T>` / `runCatching` are NOT used (see §12 on why
  we emit our own `MochiResult<T, E>`).

## 14. What this surface does *not* include

- **Untyped `any`**: Mochi rejects it at the type layer. Kotlin has
  `Any` and `Any?` and the temptation to weaken Mochi's type system to
  allow `any` is real; we resist. The cost would be losing static-typed
  FFI guarantees.
- **Implicit conversions**: ruled out above. Required to keep C, BEAM,
  JVM, .NET, Swift, and Kotlin behaviours identical.
- **Null at the language level**: see §1.7. Mochi has no `null`; Kotlin
  has `null` but only as the nullable variant of `T?`.
- **Inheritance**: Mochi has no class inheritance (only sealed-hierarchy
  ADTs and interface composition). The Kotlin `open class` is unused
  for user code. Internal helpers and FFI bridges to JVM frameworks
  (which use class hierarchies heavily) are the only places `open` and
  inheritance appear.
- **Operator overloading**: Mochi does not let users overload `+` etc.
  Kotlin allows operator overloading via specific `operator fun`
  declarations, but the Mochi target never emits them for user code.
- **Reified generics in user code**: deferred. Kotlin's `reified` keyword
  (since 1.0) requires the function to be `inline`, which has its own
  trade-offs. The Mochi target uses reified generics internally in
  `MochiRuntime` for serialization helpers but does not expose them at
  the Mochi language level.
- **Property delegates** (`val x by lazy { ... }`, etc.): not exposed
  at the Mochi language level. Internally the runtime uses `by lazy`
  for module-init helpers.
- **Coroutine flow operators in user code**: Mochi exposes `stream<T>`
  but does not directly surface every Flow operator (e.g., `flatMapMerge`
  with custom concurrency); those are accessed via Mochi's stream DSL.
- **Inline functions / inline classes for user code**: deferred. The
  runtime uses them internally.
- **DSL builders (`@DslMarker`)**: deferred. The query DSL is parsed
  at the Mochi layer, not built via Kotlin's DSL machinery.

## 15. Surface-to-Kotlin cheat sheet (cross-reference)

| Mochi form | Kotlin lowering | Note |
|------------|-----------------|------|
| `let x = ...` | `val x = ...` | §1.1, note 05 §4 |
| `var x = ...` | `var x = ...` | §1.1 |
| `int` | `Long` | §1.2, note 06 §5 |
| `float` | `Double` | §1.2 |
| `bool` | `Boolean` | §1.2 |
| `string` | `kotlin.String` (UTF-16 internal) | §1.4 |
| `list<T>` | `MutableList<T>` (ArrayList) | §3, note 06 §10 |
| `map<K,V>` | `LinkedHashMap<K,V>` | §3 |
| `set<T>` | `LinkedHashSet<T>` | §3 |
| `record T { ... }` | `data class T(val ...)` | §2.3 |
| `type T = A \| B` (sum) | `sealed interface T` with `data class` / `data object` variants | §4 |
| `Option<T>` | Kotlin `T?` (nullable, built-in) | §1.7 |
| `Result<T, E>` | custom `MochiResult<T, E>` sealed interface | §12 |
| `match` | `when` expression with smart casts on sealed hierarchy | §4 |
| `fun(...) => ...` | Kotlin lambda `{ ... -> ... }` | §2.2 |
| `from ... select ...` | Sequence chain (or Flow chain for streams) | §5, note 08 |
| `agent ...` | custom actor class with `Channel<Message>` + `SupervisorJob` scope | §6 |
| `stream<T>` | `Flow<T>` (or `SharedFlow<T>` / `StateFlow<T>`) | §6 |
| `fact / rule / query` | runtime Datalog engine | §7, note 08 §4 |
| `ai(...)` | runtime `MochiRuntime.AI.call` (AICore on Android, Ktor HTTP elsewhere) | §8.1 |
| `generate(...)` | suspend function returning `Flow<MochiToken>` | §8.1 |
| `extern fun ... = "c:..."` | `external fun` (JVM JNI) / cinterop binding (Native) / `external` (JS) | §8.2 |
| `@cdecl export` | `@CName("...")` (Native) / `@JvmStatic` (JVM) / `@JsExport` (JS) | §8.2 |
| `import "kotlin:Foundation"` | not applicable (Foundation is Apple-only); for KMP use `kotlinx-datetime`, etc. | §8.2 |
| `throws E` | return `MochiResult<T, E>` (no Kotlin checked exceptions) | §12 |
| `async fun` | `suspend fun` | §13 |
| `await x` | implicit (calling suspend in suspend ctx) | §13 |
| `spawn f()` | `scope.launch { f() }` | §13 |
| `@ui` | `withContext(Dispatchers.Main) { ... }` | §13 |
| `linear T` | not yet supported (Kotlin has no `~Copyable` equivalent) | §13 |
| `borrowed T` | not yet supported | §13 |

## 16. Open questions for note 02 (design philosophy)

- **Codegen IR**: KotlinPoet (JetBrains's first-party Kotlin source
  emitter library) vs raw string emit vs KSP-based emit. (Resolved in
  note 02 §3 and note 05 §1: KotlinPoet for the source emitter.)
- **kotlinx-coroutines version pinning**: which `Flow` ABI to target.
  (Note 02 §13: pin to 1.10.x; bump on `libs.versions.toml`
  regeneration.)
- **Compose Multiplatform availability**: Compose is the canonical
  UI framework for KMP shared UI. Mochi's UI story is currently
  deferred to v2; the question is whether the `view` keyword (when
  introduced) lowers to Compose `@Composable` functions.
- **Kotlin 2.1 floor**: confirmed (note 02 §2). Kotlin 2.2 (expected
  2025-Q3) is the rolling secondary gate; Kotlin 2.3 is the preferred
  target for new feature usage.
- **Kotlin/Wasm Alpha status**: the Wasm GC target is marked Alpha as
  of Kotlin 2.1; ABI stability and browser support are not yet
  production-grade. We ship as Alpha with a clear caveat.
- **iOS via K/Native vs going through MEP-49 Swift**: see note 02
  §14 for the deep comparison.

## 17. Cross-references

- [[02-design-philosophy]] (next note): why each of the choices above
  was made, including the case for Kotlin 2.1 as the floor and the
  comparison against alternative Mochi-to-Android pathways.
- [[03-prior-art-transpilers]]: survey of J2K, ts2kt, dukat, KotlinPoet,
  J2CL, TeaVM, Skip, and other source-to-Kotlin transpilers, plus a
  closer look at the Kotlin/Wasm and Wasm GC story for the WebAssembly
  target.
- [[04-runtime]]: the `MochiRuntime` Kotlin module layout, including
  the `Collections`, `IO`, `AI`, `FFI`, `Datalog`, and `Supervisor`
  submodules.
- [[05-codegen-design]]: the IR layer that turns this surface into
  emitted Kotlin source via KotlinPoet builders.
- [[06-type-lowering]]: the per-type details glossed here.
- [[07-kotlin-target-portability]]: the KMP target matrix (JVM /
  Android / iOS / macOS / Linux / Windows / JS / Wasm) and the version
  skew handling.
- [[08-dataset-pipeline]]: the query DSL lowering in full, including
  the kotlinx-coroutines Flow dependency map and the Datalog engine
  design.
- [[09-agent-streams]]: agent and stream lowering on Kotlin coroutines
  and channels, including the supervision tree design.
- [[10-build-system]]: Gradle + AGP + KGP integration, the generated
  `build.gradle.kts`, and the multi-platform CI matrix.
- [[11-testing-gates]]: the test-suite gates for v0.10 ship; what
  fraction of `examples/v0.2`-`v0.7` must transpile, build, and pass
  on each platform.
- [[12-risks-and-alternatives]]: the risk register and the
  alternatives considered (notably Skip-style Swift-to-Kotlin
  transpilation and direct JVM bytecode emission as in MEP-47).
- [[../0049/01-language-surface]]: the Swift-target analogue of this
  note, the closest in spirit since both target a typed-managed
  runtime with first-class generics, sealed types, and async/await.
- [[../0047/01-language-surface]]: the JVM-bytecode-target analogue,
  which shares the typed-Result error story and structured-concurrency
  shape but bypasses kotlinc.
- [[../0048/01-language-surface]]: the .NET-target analogue, which
  shares the typed-throws-vs-Result tension.
- [[../0046/01-language-surface]]: the BEAM-target analogue, whose
  agent / mailbox / supervision design directly inspired the Kotlin
  actor + Channel + Supervisor lowering.
- [[../0045/01-language-surface]]: the C-target analogue, whose
  C-ABI extern story is mirrored via Kotlin/Native cinterop.
