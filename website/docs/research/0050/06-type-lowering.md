# MEP-50 research note 06, Per-type Kotlin lowering

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).
Sources: Kotlin Language Specification (kotlinlang.org/spec), Kotlin
1.7-2.1 release notes, kotlinx.coroutines 1.10.x documentation,
kotlinx.serialization 1.7.x documentation, kotlinx.collections.immutable
0.3.8 documentation, kotlin.time stabilisation notes (Kotlin 2.1),
JVM Specification §2.11 (primitive types), the K/Native memory model
documentation, and the Mochi-side anchor file at
the shared-decisions anchor.

This note documents the per-type lowering of every Mochi type to its
Kotlin commonMain representation, plus the per-target variations
(JVM, Android, K/Native, K/JS, K/Wasm). It is the type-system
counterpart to [[01-language-surface]]: the language-surface note
states the lowerings as a one-line cheatsheet; this note gives the
implementation detail, the boundary costs, the boxing decisions, the
encoding cost, and the FFI-cross-platform mapping.

## 1. The primitive triangle: `int`, `float`, `bool`

### 1.1 Mochi `int` to Kotlin `Long`

Mochi `int` is a 64-bit signed integer. Kotlin has two integer types
to choose from:

- `Int`, **32-bit signed**, mapped to JVM `int` (primitive) and to
  K/Native `int32_t` and to JS `Number` (52-bit mantissa, exact for
  ±2^31).
- `Long`, **64-bit signed**, mapped to JVM `long` (primitive) and to
  K/Native `int64_t`. On K/JS, `Long` is *boxed* into a custom
  `kotlin.Long` class because JS `Number` cannot exactly represent
  64-bit integers (the JS spec's `Number` is IEEE 754 double, exact
  only to 53 bits).

Mochi specifies `int` as 64-bit. We therefore unconditionally lower
Mochi `int` to Kotlin `Long`. The cost analysis:

- **JVM**: `Long` is a primitive `long` (8 bytes, stack-allocated when
  not boxed). Boxing happens only when the `Long` is stored in a
  `List<Long>` or any other generic collection. The transpiler avoids
  boxing by using primitive specialisations where they exist
  (`LongArray`, `kotlin.collections.LongIterator`).
- **K/Native**: `Long` is `int64_t` (8 bytes, value type). Zero
  boxing overhead.
- **K/JS**: `Long` is a boxed class wrapper. A `Long` field on a data
  class adds one object indirection per field. This is the unavoidable
  JS-target overhead.
- **K/Wasm**: `Long` is the Wasm `i64` type. Zero boxing in user code;
  one boxing layer only when crossing into JS interop.

Conversions: `Long.toInt()` truncates to 32 bits (silent overflow);
`Int.toLong()` widens. Kotlin requires explicit conversions both ways;
implicit conversions are not allowed (matching Mochi). See
[[01-language-surface]] §1.2 on the matching strictness.

Array indices in Kotlin's `List<T>` and `Array<T>` require `Int`, not
`Long`. Mochi `list<T>` index expressions therefore lower to
`xs[i.toInt()]` with a runtime range check (Kotlin's index access
throws `IndexOutOfBoundsException` automatically). For very large
indices (greater than `Int.MAX_VALUE = 2^31 - 1`) the conversion
truncates silently; the transpiler emits a guard
`require(i in Int.MIN_VALUE..Int.MAX_VALUE) { "index out of Int range" }`
when `--strict-int` is set. Default is permissive (matching the
silent-wrap convention).

### 1.2 Mochi `float` to Kotlin `Double`

Mochi `float` is IEEE 754 64-bit double. Kotlin has `Float` (32-bit)
and `Double` (64-bit); we map Mochi `float` to Kotlin `Double`
unconditionally.

- **JVM**: `Double` is primitive `double` (8 bytes).
- **K/Native**: `Double` is `double` (8 bytes).
- **K/JS**: `Double` is JS `Number` (the native JS number type).
  Zero boxing because JS `Number` *is* a 64-bit double.
- **K/Wasm**: `Double` is Wasm `f64`.

NaN propagation follows IEEE 754. `Double.NaN` is not equal to itself
(`Double.NaN == Double.NaN` is `false`); the runtime exposes
`isNaN()` for explicit checks. Mochi `nan` literal lowers to
`Double.NaN`.

### 1.3 Mochi `bool` to Kotlin `Boolean`

Direct mapping. JVM: `boolean` (1 byte); K/Native: `bool` (1 byte);
K/JS: JS boolean; K/Wasm: Wasm `i32` (0 or 1).

## 2. Integer overflow and the strict-int flag

Mochi documents `int + int` as silent two's-complement wrap-around.
Kotlin's default `Long + Long` also wraps silently (JVM `ladd`
opcode); no exception is thrown on overflow. The lowering therefore
uses Kotlin's native operators directly:

```kotlin
val sum: Long = a + b  // silent wrap on overflow
```

The `--strict-int` build flag flips this to checked arithmetic:

- **JVM**: emit `Math.addExact(a, b)` and `Math.multiplyExact(a, b)`,
  which throw `ArithmeticException` on overflow.
- **K/Native**: emit a manual check, `if (a > 0 && b > Long.MAX_VALUE - a) throw ArithmeticException("overflow")`.
- **K/JS, K/Wasm**: same manual check.

Strict mode is off by default. See
[[02-design-philosophy]] §3 on the wrap-vs-trap decision.

## 3. Floor division and floor modulo

Mochi specifies floor division (Python-style): `(-7) / 2 == -4`,
`(-7) % 2 == 1`. Kotlin's `Long / Long` is *truncated* division
(C-style): `(-7L) / 2L == -3L`, `(-7L) % 2L == -1L`. The lowering
must emit `Math.floorDiv` (JVM) or an explicit helper:

```kotlin
fun mochiFloorDiv(a: Long, b: Long): Long = Math.floorDiv(a, b)
fun mochiFloorMod(a: Long, b: Long): Long = Math.floorMod(a, b)
```

`Math.floorDiv` and `Math.floorMod` are available on JVM since Java
8. K/Native, K/JS, K/Wasm lack them; the transpiler emits a
commonMain implementation in `MochiRuntime.Arithmetic`:

```kotlin
inline fun mochiFloorDiv(a: Long, b: Long): Long {
    val q = a / b
    return if ((a xor b) < 0L && q * b != a) q - 1L else q
}

inline fun mochiFloorMod(a: Long, b: Long): Long {
    val r = a % b
    return if ((r xor b) < 0L && r != 0L) r + b else r
}
```

The JVM target uses `Math.floorDiv` directly via the `actual` mechanism
to avoid the helper overhead.

## 4. Strings

### 4.1 The UTF-16 reality

Mochi `string` is documented as UTF-8 code-point sequences. Kotlin
`String` is *UTF-16 internally* across every target (JVM heritage):

- **JVM**: `String` is the JVM's `java.lang.String`, internally
  `char[]` (UTF-16) or in Java 9+ optimised to `byte[]` (Latin-1 or
  UTF-16, the JEP 254 compact strings).
- **Android**: same as JVM but with the dx/d8 bytecode rewriter
  preserving `String` semantics.
- **K/Native**: `String` is a UTF-16 sequence stored in a Kotlin-allocated
  heap object.
- **K/JS**: `String` is the JS string (UTF-16 internally, exposed via
  the JS string API).
- **K/Wasm**: `String` is UTF-16 in the Wasm heap (managed via Wasm GC
  reference types).

This is the biggest single performance pain point of the Kotlin target
compared to MEP-49 (Swift, UTF-8 native) and MEP-45 (C, raw UTF-8 bytes).
Every Mochi string operation that needs code-point semantics has to
walk the UTF-16 sequence, which can mean two `char` reads per code
point for surrogate-pair characters (emojis, CJK supplementary
characters).

### 4.2 Indexing

Mochi `text[i]` returns a 1-character string containing the i-th
*code point*. Kotlin `text[i]` returns a `Char` (16-bit UTF-16 code
*unit*, not code *point*). The transpiler emits a runtime call:

```kotlin
fun mochiStringIndex(text: String, i: Long): String {
    val cp = text.codePointAt(text.offsetByCodePoints(0, i.toInt()))
    return String(Character.toChars(cp))
}
```

`codePointAt`, `offsetByCodePoints`, `Character.toChars` are JVM APIs;
on K/Native we implement them via manual surrogate-pair walking in
`MochiRuntime.Strings`. On K/JS we use `String.fromCodePoint` and
`String.codePointAt`. On K/Wasm same as JVM (via Wasm-GC bindings).

The cost: O(i) per indexing call. Loops that index repeatedly are
quadratic. The transpiler emits a code-point iterator for
`for ch in text` loops to amortise:

```kotlin
text.codePoints().forEach { cp ->
    val ch = String(Character.toChars(cp))
    // body
}
```

`text.codePoints()` returns an `IntStream` on JVM (Java 8+); on
K/Native we expose `text.codePointSequence()` as a Sequence<Int>.

### 4.3 Length

Mochi `len(text)` returns code-point count. Kotlin `text.length`
returns code-unit count (UTF-16 chars). The transpiler emits:

```kotlin
fun mochiStringLen(text: String): Long = text.codePointCount(0, text.length).toLong()
```

For ASCII-only strings the code-point count equals the code-unit
count, so callers that know they are ASCII-only can use
`text.length.toLong()` directly. The runtime exposes both via
`mochiStringLen(text)` (code points) and `mochiStringLenUtf16(text)`
(code units) and `mochiStringLenUtf8(text)` (UTF-8 byte count, via
`text.toByteArray(Charsets.UTF_8).size`).

### 4.4 UTF-8 boundary

When Mochi code crosses to a UTF-8 boundary (writing to a file,
sending over HTTP, calling a C function expecting UTF-8), the
transpiler emits `text.toByteArray(Charsets.UTF_8)` which produces a
`ByteArray`. The round-trip cost is one allocation plus one walk.

The runtime caches the UTF-8 byte array for `val`-bound strings that
cross a UTF-8 boundary repeatedly, via a thread-local `WeakHashMap`
in JVM target and a Mochi-side `MochiUtf8Cache` on other targets.
See [[04-runtime]] §6.

## 5. Mochi `list<T>` to Kotlin `List<T>`

Kotlin separates `List<T>` (read-only) from `MutableList<T>`. Mochi
lists are value-semantically immutable at language level (mutating
operations produce a fresh list); the transpiler emits `List<T>` for
the immutable case and `MutableList<T>` only when local mutation is
proven safe (the variable is `var`-bound and not aliased).

Default backing: `kotlin.collections.ArrayList<T>` (which on JVM is
`java.util.ArrayList`, on K/Native and K/JS is a custom array-backed
implementation). Random access is O(1); append is amortised O(1).

### 5.1 Construction

```kotlin
val xs: List<Long> = listOf(1L, 2L, 3L)
```

`listOf` returns an immutable wrapper. For mutable cases:

```kotlin
val ys: MutableList<Long> = mutableListOf(1L, 2L, 3L)
ys.add(4L)
```

### 5.2 Iteration

Mochi `for x in xs` lowers to Kotlin `for (x in xs) { ... }`. The
iteration walks the underlying `Iterator<T>`.

### 5.3 Persistent variant

For functional code that needs structural sharing (e.g., Datalog rule
substitution maps), the transpiler offers
`kotlinx.collections.immutable.PersistentList<T>` as a non-default
variant, triggered by the Mochi `persistent` qualifier (preview, not
in v1).

### 5.4 Primitive-specialised arrays

For `list<int>`, Kotlin offers `LongArray` (primitive-array specialised,
no boxing). The transpiler emits `LongArray` when the list type is
proven `list<int>` (or `list<float>` → `DoubleArray`, `list<bool>` →
`BooleanArray`). This avoids one boxing per element on JVM, halving
memory for large numeric lists.

When the list is generic (`list<T>` with `T` unbounded), the transpiler
emits `Array<T>` (boxed reference array), which boxes primitive
elements. This is the unavoidable JVM-erasure cost.

### 5.5 Per-target cost

- **JVM**: `ArrayList<Long>` boxes elements. The transpiler prefers
  `LongArray` for numeric lists.
- **K/Native**: `ArrayList<Long>` does not box (Native generics are
  reified). No primitive-array preference needed.
- **K/JS**: similar to JVM (boxed). The transpiler prefers typed
  arrays (`LongArray` → JS `BigInt64Array`) where possible.
- **K/Wasm**: typed arrays in Wasm GC.

## 6. Mochi `map<K, V>` to Kotlin `Map<K, V>`

Kotlin separates `Map<K, V>` (read-only) from `MutableMap<K, V>`.
Mochi maps are value-semantically immutable; the transpiler emits
`Map<K, V>` by default and `MutableMap<K, V>` for proven-safe `var`
cases.

Default backing: `java.util.LinkedHashMap<K, V>` on JVM (and the
K/Native / K/JS / K/Wasm equivalents that preserve insertion order).
This is what `mutableMapOf()` and `linkedMapOf()` return.

Why insertion-ordered, not hash-ordered: Mochi documents map iteration
as insertion-ordered. Kotlin's `HashMap` does not guarantee order;
`LinkedHashMap` does. We always use `LinkedHashMap` for the canonical
`map<K, V>` lowering.

### 6.1 Construction

```kotlin
val m: Map<String, Long> = linkedMapOf("a" to 1L, "b" to 2L)
```

`linkedMapOf` returns a `MutableMap` view that is up-cast to `Map`
when bound via `val` to an immutable type. The transpiler emits
explicit type annotations to make the immutability visible.

### 6.2 Subscript

Mochi `m["a"]` returns `Option<V>`. Kotlin `m["a"]` returns `V?`
(nullable). The mapping is one-to-one because Mochi `Option<T>` ⇒
Kotlin `T?`.

### 6.3 Mutation

Mochi `m["a"] = 1` is sugar for the assignment expression. The
transpiler emits `(m as MutableMap)["a"] = 1L` when the cast is
safe (i.e., the underlying is `MutableMap`); when it is not, the
transpiler emits a fresh copy:

```kotlin
val m2 = LinkedHashMap(m).apply { this["a"] = 1L }
```

### 6.4 Persistent variant

`kotlinx.collections.immutable.PersistentMap<K, V>` for structural
sharing; non-default, triggered by `persistent` qualifier.

## 7. Mochi `set<T>` to Kotlin `Set<T>`

Same pattern as `map<K, V>`. Default backing:
`java.util.LinkedHashSet<T>`. Insertion order preserved.

```kotlin
val s: Set<Long> = linkedSetOf(1L, 2L, 3L)
```

## 8. Mochi record types to Kotlin `data class`

Mochi `type Book { title: string, pages: int }` lowers to:

```kotlin
@Serializable
public data class Book(
    public val title: String,
    public val pages: Long
)
```

Kotlin's `data class` synthesises:
- `equals(other: Any?)` (field-by-field equality)
- `hashCode()` (combined hash)
- `toString()` (debug-friendly `Book(title=X, pages=10)`)
- `copy(title=..., pages=...)` (for the Mochi `with` expression)
- `componentN()` (for destructuring: `val (title, pages) = book`)

The `@Serializable` annotation is from `kotlinx.serialization` and
enables JSON/CBOR/ProtoBuf round-tripping. The transpiler emits it
unconditionally for Mochi records; users who do not want serialization
can suppress it with `--no-serializable` (rare).

Field types are `val` (read-only). Mutation goes through `copy()`:

```kotlin
val b2 = b.copy(pages = 200L)
```

which matches Mochi's `b with { pages: 200 }` semantic.

### 8.1 Methods on records

Mochi `type Circle { radius: float, fun area(): float { ... } }` lowers
to a data class with the method on the class body:

```kotlin
@Serializable
public data class Circle(public val radius: Double) {
    public fun area(): Double = 3.14 * radius * radius
}
```

### 8.2 With mutable fields

Mochi `type Counter { var count: int }` lowers to a data class with
`var` fields:

```kotlin
@Serializable
public data class Counter(public var count: Long)
```

Data class with `var` fields is legal Kotlin but breaks the value
semantics expected of records. The transpiler warns when a Mochi `var`
field is declared inside a `type` and recommends extracting to a
separate mutable holder.

## 9. Mochi sum types to Kotlin `sealed interface`

Mochi `type Tree = Leaf | Node { value: int, left: Tree, right: Tree }`
lowers to:

```kotlin
@Serializable
public sealed interface Tree {
    @Serializable
    public data object Leaf : Tree

    @Serializable
    public data class Node(
        public val value: Long,
        public val left: Tree,
        public val right: Tree
    ) : Tree
}
```

Key choices:
- **`sealed interface`, not `sealed class`** (Kotlin 1.7+, KEEP-213).
  Interfaces allow variants to implement other interfaces too. Classes
  do not (Kotlin's single-inheritance rule).
- **`data object` for unit variants** (Kotlin 1.9+, KEEP-317). Cleaner
  than `object Leaf : Tree` because `data object` synthesises
  `toString()` and `equals()`/`hashCode()` consistent with `data class`.
- **Recursive types**. Kotlin handles recursive sealed types directly;
  no `indirect` keyword as in Swift.
- **`@Serializable` on the interface and each variant**. Required for
  polymorphic serialization in kotlinx.serialization.

### 9.1 Pattern matching

Mochi `match` lowers to Kotlin `when`:

```kotlin
val result: Long = when (t) {
    is Tree.Leaf -> 0L
    is Tree.Node -> t.value + sum(t.left) + sum(t.right)
}
```

Kotlin's `when` is exhaustive on sealed types since Kotlin 1.6 (warning)
and since Kotlin 1.7 (error). The transpiler relies on this to catch
non-exhaustive matches at both layers (Mochi checker + Kotlin compiler).

For payload destructuring, Kotlin needs explicit smart-cast or
component destructuring:

```kotlin
val result = when (t) {
    is Tree.Leaf -> 0L
    is Tree.Node -> {
        val (value, left, right) = t
        value + sum(left) + sum(right)
    }
}
```

The transpiler emits the explicit smart-cast form (no destructuring)
for clarity and to allow Kotlin's smart-cast tracker to infer types
without unpacking.

### 9.2 Guards

Mochi `match shape { Circle { radius } if radius > 10.0 => ... }`
lowers to Kotlin `when` with conditional pattern:

```kotlin
val label = when {
    shape is Shape.Circle && shape.radius > 10.0 -> "big"
    shape is Shape.Circle -> "small"
    else -> "other"
}
```

### 9.3 Generic sum types

Mochi `type Option<T> = Some { value: T } | None` could lower to:

```kotlin
public sealed interface MochiOption<out T> {
    public data class Some<out T>(public val value: T) : MochiOption<T>
    public data object None : MochiOption<Nothing>
}
```

with `out T` covariance. But as documented in [[01-language-surface]]
§1.7, we **do not** emit `MochiOption`; we map Mochi `Option<T>` to
Kotlin nullable `T?` directly. The above is shown only for illustration.

Mochi `Result<T, E>` *does* emit a custom sealed class because
`kotlin.Result<T>` is invariant in `T` and missing the `E` parameter:

```kotlin
public sealed interface MochiResult<out T, out E> {
    public data class Ok<out T>(public val value: T) : MochiResult<T, Nothing>
    public data class Err<out E>(public val error: E) : MochiResult<Nothing, E>
}
```

## 10. Optionals (Mochi `Option<T>` to Kotlin `T?`)

Mochi `Option<T>` maps to Kotlin nullable `T?`. None becomes `null`;
Some(x) becomes `x` (implicit wrap into the nullable). Pattern matching:

```kotlin
when (val o = opt) {
    null -> handleNone()
    else -> handleSome(o)
}
```

or, more idiomatically:

```kotlin
opt?.let { handleSome(it) } ?: handleNone()
```

Kotlin's null-safety operator chain (`?.`, `?:`, `!!`, `?.let { }`,
`?.also { }`) gives ergonomic optional handling.

`x?.field` is the equivalent of Swift's `x?.field` (optional chaining);
the transpiler emits these directly when Mochi code accesses an
optional field.

`!!` (force-unwrap, NullPointerException on null) is the Kotlin
equivalent of Swift's `!`. The transpiler **never emits `!!` for user
code**, matching the Swift target's discipline. The runtime library may
use it for invariants the runtime author has proven (e.g., a
`require(...) { ... }`-guarded value).

## 11. Result<T, E>: custom MochiResult

As noted in §9.3 and [[01-language-surface]] §1.7, we emit our own
sealed interface rather than reusing `kotlin.Result<T>`:

```kotlin
@Serializable
public sealed interface MochiResult<out T, out E> {
    @Serializable
    public data class Ok<out T>(public val value: T) : MochiResult<T, Nothing>

    @Serializable
    public data class Err<out E>(public val error: E) : MochiResult<Nothing, E>
}

public fun <T, E> ok(value: T): MochiResult<T, E> = MochiResult.Ok(value)
public fun <T, E> err(error: E): MochiResult<T, E> = MochiResult.Err(error)
```

Why not `kotlin.Result<T>`:

1. **Invariant in T**: `kotlin.Result<T>` is `Result<T>`, no covariance.
   This blocks `Result<Cat>` from being used where `Result<Animal>` is
   expected. Mochi's `Result<T, E>` is conventionally covariant in both.
2. **No E type parameter**: `kotlin.Result<T>` wraps `Throwable` only.
   Mochi's `Result<T, E>` lets `E` be any type (often a sealed enum of
   domain errors).
3. **Originally internal**: `kotlin.Result<T>` was internal-only when
   first introduced (Kotlin 1.3); it was opened up for public use in
   1.5 but the API still has compromises from that history.
4. **No KSP/Serialization friendliness**: `kotlin.Result<T>` does not
   compose with `@Serializable`.

The cost of emitting our own: one extra type in the runtime library,
one extra import per file. Acceptable.

## 12. Mochi typed throws to MochiResult return

Mochi `fun foo(): T throws E` lowers to:

```kotlin
public fun foo(): MochiResult<T, E> { ... }
```

Mochi `try foo()` (propagate) lowers to:

```kotlin
val r = foo()
when (r) {
    is MochiResult.Err -> return MochiResult.Err(r.error)
    is MochiResult.Ok -> r.value
}
```

This pattern is verbose; the runtime exposes a helper:

```kotlin
inline fun <T, E> MochiResult<T, E>.unwrapOrPropagate(): T = when (this) {
    is MochiResult.Ok -> value
    is MochiResult.Err -> throw MochiPropagateException(error)
}
```

with a matched `try-catch` at the function boundary. Or, more
idiomatically for Mochi:

```kotlin
public fun foo(): MochiResult<T, E> {
    val v = bar().getOrElse { return MochiResult.Err(it) }
    // use v
    return MochiResult.Ok(v)
}
```

We do **not** use Kotlin exceptions for Mochi typed throws because:
1. Kotlin has no checked exceptions (unlike Java); typed throws would
   need an unchecked exception mechanism that defeats type-checking.
2. Exception cost on JVM is non-trivial (stack trace capture); on
   K/Native it is also non-zero.
3. The Mochi type system already encodes the error path; using
   exceptions duplicates the information.

See [[02-design-philosophy]] §15 on the no-exceptions rule.

## 13. Mochi `agent` type

Mochi `agent T { ... }` lowers to a final class (see
[[09-agent-streams]] for full detail):

```kotlin
public class T(private val scope: CoroutineScope) {
    private val mailbox = Channel<Message>(Channel.UNLIMITED)
    // ...
}
```

Not a `data class` (agents have identity and mutable mailbox state, not
value semantics). The transpiler emits `class`, not `data class`.

## 14. Mochi `stream<T>` to Kotlin `Flow<T>`

Mochi stream lowers to `kotlinx.coroutines.flow.Flow<T>`. Cold flow by
default; hot flow via `MutableSharedFlow<T>` or `MutableStateFlow<T>`.

```kotlin
public fun tickerStream(): Flow<Tick> = flow {
    while (true) {
        delay(1000L)
        emit(Tick(time = Clock.System.now()))
    }
}
```

Consumption:

```kotlin
tickerStream().collect { tick ->
    // body
}
```

See [[09-agent-streams]] for the full lowering.

## 15. Functions and closures

Mochi `fun(int) -> int` lowers to Kotlin `(Long) -> Long`. The closure
literal `fun(x: int): int => x * x` lowers to Kotlin `{ x: Long -> x * x }`.

Suspended functions: Mochi `async fun(int) -> int` lowers to Kotlin
`suspend (Long) -> Long`. The `suspend` modifier is part of the
function type (Kotlin compiler tracks it).

Capture: Kotlin closures capture by reference for `var` and by value
for `val`. The transpiler emits `val` for captured Mochi `let`
bindings and `var` for captured `var` bindings.

## 16. Time and duration

Mochi `time` lowers to `kotlin.time.Instant` (stable since Kotlin 2.1).
Pre-2.1 modules fall back to `kotlinx.datetime.Instant`.

Mochi `duration` lowers to `kotlin.time.Duration` (stable since
Kotlin 1.6).

```kotlin
val t: Instant = Clock.System.now()
val d: Duration = 5.seconds
val later: Instant = t + d
```

`kotlin.time.Duration` is *value-class* implemented (zero allocation
on JVM via inline class semantics).

## 17. JSON values

The Mochi `Json` type (preview) lowers to `MochiRuntime.JSON.JSONValue`:

```kotlin
@Serializable
public sealed interface JSONValue {
    @Serializable @SerialName("null") public data object Null : JSONValue
    @Serializable @SerialName("bool") public data class Bool(val value: Boolean) : JSONValue
    @Serializable @SerialName("int") public data class Int_(val value: Long) : JSONValue
    @Serializable @SerialName("float") public data class Float_(val value: Double) : JSONValue
    @Serializable @SerialName("string") public data class Str(val value: String) : JSONValue
    @Serializable @SerialName("array") public data class Arr(val items: List<JSONValue>) : JSONValue
    @Serializable @SerialName("object") public data class Obj(val fields: Map<String, JSONValue>) : JSONValue
}
```

The names `Int_` and `Float_` use trailing underscore because `Int`
and `Float` are Kotlin reserved type names; backticks would also work
but underscore is cleaner in production code.

## 18. FFI type bridging per target

The Mochi-to-Kotlin FFI surface varies by target:

| Target | Mochi `int` | Mochi `string` | Mochi `list<T>` | FFI annotation |
|--------|-------------|----------------|------------------|----------------|
| JVM | `long` | `String` | `List<T>` | `@JvmStatic`, `external` (JNI) |
| Android | same as JVM | same as JVM | same as JVM | same + `@Keep` for R8 |
| K/Native iOS | `Long` (bridged to Swift `Int64`) | `String` (bridged to NSString) | `List<T>` (bridged to NSArray) | `@CName`, `objc` annotations |
| K/Native macOS | same as iOS | same as iOS | same as iOS | same |
| K/Native Linux | `Long` (bridged to C `int64_t`) | `String` (bridged to UTF-8 C string) | `List<T>` (manual) | `@CName`, cinterop bindings |
| K/Native Windows | same as Linux | same as Linux | same as Linux | same |
| K/JS | `Long` (boxed) | `String` (JS string) | `List<T>` (JS array via interop) | `external`, `@JsExport`, `@JsName` |
| K/Wasm | `Long` (Wasm i64) | `String` (Wasm GC string) | `List<T>` (Wasm GC array) | `external`, Wasm import |

Detailed per-target FFI bridging is in [[07-kotlin-target-portability]]
§17 and [[04-runtime]] §13.

## 19. Boxing cost summary

JVM-target boxing per type when stored in a generic container:

| Mochi type | Storage type | Boxing? | Cost |
|-----------|--------------|---------|------|
| `int` | `Long` in `List<Long>` | yes | 24 bytes per element |
| `int` | `Long` in `LongArray` | no | 8 bytes per element |
| `float` | `Double` in `List<Double>` | yes | 24 bytes per element |
| `float` | `Double` in `DoubleArray` | no | 8 bytes per element |
| `bool` | `Boolean` in `List<Boolean>` | yes | 16 bytes per element |
| `bool` | `Boolean` in `BooleanArray` | no | 1 byte per element |
| `string` | `String` in `List<String>` | no (already reference) | 16 bytes per ref |
| record | `Book` in `List<Book>` | no (already reference) | 16 bytes per ref |

The transpiler prefers primitive-array specialisations when the element
type is known statically. For generic containers, boxing is unavoidable
on JVM (erasure cost).

K/Native does *not* box because Native generics are reified.
K/JS boxes Long but not Double/Boolean (Double is JS Number directly).
K/Wasm boxes nothing (Wasm GC has typed arrays for primitives).

## 20. Cross-references

- [[01-language-surface]]: the user-visible surface that maps onto
  these types.
- [[02-design-philosophy]]: the rationale behind the per-type
  choices (especially `Long` over `Int`, MochiResult over
  `kotlin.Result`, T? over MochiOption).
- [[04-runtime]]: the MochiRuntime helpers that implement the
  type-bridging functions referenced here.
- [[05-codegen-design]]: the codegen pass that emits the type
  lowerings.
- [[07-kotlin-target-portability]]: per-target type cost analysis.
- [[09-agent-streams]]: the `agent` and `stream<T>` lowerings.
- [[10-build-system]]: the Gradle setup that ships the type-aware
  runtime.
- [[../0049/06-type-lowering]]: the Swift sibling note, with the
  matching type table.
- [[../0048/06-type-lowering]]: the .NET sibling note.
- [[../0047/06-type-lowering]]: the JVM-bytecode sibling note,
  which uses different lowerings because it skips the Kotlin source layer.
