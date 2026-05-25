# MEP-47 research note 06, Lowering Mochi's static type system onto JVM types

Author: research pass for MEP-47 (Mochi to JVM bytecode transpiler).
Date: 2026-05-23 (GMT+7).

Mochi is a statically typed language. The JVM is a hybrid: two primitive
families (the eight JLS primitives, plus `void`) live below the reference
universe of object types, with autoboxing as the bridge. Method bodies in
classfiles carry a typed operand stack and typed local slots; generic
parameters are erased at the bytecode level to their bounds. Every Mochi
type therefore has a lowering decision with two faces: the surface Java
type used in field signatures and method descriptors (what `javac` would
emit for a hand-written equivalent), and the actual JVM descriptor pair
that the verifier sees after erasure.

This note fixes both faces of every Mochi type and operator. It is the
contract that every later phase of MEP-47 (lower, emit, classfile writer,
runtime, FFI, [[10-build-system]]) is allowed to assume.

Target platform baseline: JDK 21 LTS minimum, JDK 25 LTS supported, with
opt-in flags for JDK 26 preview features (value classes, JEP 401). The
companion BEAM mapping in `[[../0046/06-type-lowering]]` is the structural
template; where MEP-47's choices diverge from MEP-46's, §16 records the
reason.

## 1. Design principles

Four principles drive every decision below.

**P1, Prefer JVM primitives where the static type permits.** A Mochi
`int` is `long`, not `java.lang.Long`. A Mochi `float` is `double`, not
`java.lang.Double`. The JIT optimises primitive arithmetic into a single
machine op; boxing into `Long`/`Double` forces a heap allocation per
intermediate result and burns the L1 cache. Mochi's static type checker
already gives us the type, so we should never lose it at the bytecode
level except where the JVM forces our hand (§7, generics).

**P2, One representation per Mochi type, everywhere.** A `list<int>` is
always `ArrayList<Long>` at the Java level (and `List` of erased
references inside method descriptors). A record `Point` is always a JVM
`record`, never sometimes a class. Two representations would force every
call site to branch.

**P3, Use Java 21 idiomatic features even when older alternatives exist.**
Sealed interfaces (JEP 409, GA in 17), records (JEP 395, GA in 16),
pattern matching for `switch` (JEP 441, GA in 21), record patterns
(JEP 440, GA in 21), sequenced collections (JEP 431, GA in 21),
`invokedynamic` for string concat (JEP 280, GA in 9) and lambda capture
(JSR 335, GA in 8). The combined toolkit gives us exhaustive
algebraic-data-type matching, structural equality, and minimal-overhead
closures without any custom runtime classes.

**P4, Forbid silent boxing at hot-path boundaries.** Every place where a
Mochi `int` would have to cross into a generic parameter slot is a
candidate for monomorphisation in MEP-45 style. We default to boxed
generic, but the lower pass emits an attribute on the call site that the
JIT, or a later specialisation pass, can read to specialise.

These principles produce a few non-obvious recommendations later, notably:
sum types lower to sealed interfaces with record cases (not enums), strings
go to `java.lang.String` even though Mochi indexes by code point, and
function values use `invokedynamic` LambdaMetafactory call sites rather
than anonymous inner classes.

## 2. Primitive types

### 2.1 `int`

**Lowering:** JVM `long` (primitive, 64-bit signed two's-complement).
Boxes to `java.lang.Long` only at generic-position boundaries.

Mochi documents `int` as 64-bit signed. The JVM offers four integer
primitives (`byte`, `short`, `int`, `long`); only `long` matches Mochi's
width. Arithmetic uses the `ladd`, `lsub`, `lmul`, `ldiv`, `lrem`
opcodes, each a single machine op on 64-bit hardware. The classfile
descriptor character is `J`; locals occupy two slots; the operand stack
also uses two slots per `long`.

Boxing crosses to `java.lang.Long` (descriptor `Ljava/lang/Long;`,
8-byte object header plus an 8-byte value field, 24 bytes total on a
64-bit JVM with compressed oops, or 32 bytes without) in exactly these
situations:

| Boxing site | Trigger | Cost |
|---|---|---|
| Generic parameter slot | `list<int>` element accessed as `List<Long>.get` | one `Long.valueOf` per autobox |
| `Object` return from FFI | reflective dispatch | one allocation per call |
| Map key / value | `HashMap<Long, V>` | per entry |
| Optional<T> wrap | `Optional<Long>` | one allocation per wrap |
| Stream element | `Stream<Long>` (not `LongStream`) | per element |

`Long.valueOf` uses a cached range of `[-128, 127]` per JLS 5.1.7, so
small integers hit the cache and do not allocate. Outside that range
each boxing allocates a fresh `Long`.

For tight numeric loops the lower pass detects `for x in xs` over a
`list<int>` and emits the loop body against a `long[]` (with descriptor
`[J`) when the type is locally known to be exclusively `list<int>`. This
is the same idea as MEP-45's monomorphisation, applied selectively at
the boundary into a closed scope.

Future: once JEP 401 (value classes) reaches preview-default in JDK 26
or GA later, `Long` itself becomes a value class, removing the heap
boxing for the boxed form. We design the type table so that this is a
zero-touch upgrade.

### 2.2 `float`

**Lowering:** JVM `double` (primitive, 64-bit IEEE 754). Boxes to
`java.lang.Double` at generic boundaries.

Descriptor `D`. Operates with `dadd`, `dsub`, `dmul`, `ddiv`. NaN
propagation is IEEE 754, which means `Double.NaN != Double.NaN` returns
`true`, and `Double.compare(NaN, NaN)` returns `0` (total order). Mochi
inherits IEEE NaN semantics directly from the JVM; we do not paper over
it.

Boxing thresholds match §2.1. `Double.valueOf` does not cache; every
autobox outside an interned-by-the-runtime path allocates.

### 2.3 `bool`

**Lowering:** JVM `boolean` (primitive, 1 bit stored as a byte).
Boxes to `java.lang.Boolean`.

Descriptor `Z`. The JVM has no boolean-specific arithmetic ops; the
bytecode uses `iconst_0`/`iconst_1` plus the integer comparison ops
(`ifeq`, `ifne`). `Boolean.valueOf` returns the singletons
`Boolean.TRUE` and `Boolean.FALSE`, so autoboxing never allocates.

Mochi `&&` and `||` lower to short-circuit branches in bytecode (an
`ifeq` followed by either the right operand or a `goto` past it). They
are not method calls.

### 2.4 `string`

**Lowering:** `java.lang.String` (descriptor `Ljava/lang/String;`).

Java strings are immutable, interned-as-literals at classfile load time,
and stored internally as a `byte[]` plus a `coder` field that selects
LATIN1 (one byte per character) or UTF16 (two bytes per character). The
compact-strings optimisation (JEP 254, GA in 9) means an ASCII-only
Mochi string costs roughly `byte_count + 16` bytes of heap.

The tension with Mochi: Mochi indexes strings by Unicode code point,
not by UTF-16 code unit. Java's `String.charAt(int)` returns a `char`
(UTF-16 code unit), which is wrong for any code point above U+FFFF
(emoji, supplementary CJK, etc.). We handle this in §3.

UTF-8 is the source encoding for Mochi files but is not the in-memory
encoding on the JVM. Conversion to and from `byte[]` UTF-8 happens at
FFI boundaries via `String.getBytes(StandardCharsets.UTF_8)` and the
`new String(bytes, StandardCharsets.UTF_8)` constructor.

### 2.5 `time`

**Lowering:** `java.time.Instant`.

`Instant` represents a point on the timeline as `(seconds since epoch,
nanos within second)`. It is immutable, has nanosecond resolution, and
its `compareTo` and `equals` semantics match what Mochi documents.
Construction goes through `Instant.now()`, `Instant.ofEpochMilli(long)`,
`Instant.ofEpochSecond(long, long)`. Conversion to and from a single
`long` of nanos-since-epoch (the BEAM representation) is one
multiplication or division by `1_000_000_000` plus a `nano` field
adjustment.

Alternatives considered:

- `long` nanos-since-epoch (the BEAM choice). Saves a heap object per
  timestamp but loses the JDK ecosystem (DateTimeFormatter, ZonedDateTime
  conversions, Duration arithmetic). For Mochi on JVM, ecosystem wins.
- `ZonedDateTime`. Carries a timezone, which Mochi `time` does not. The
  type would carry information the source type cannot.
- `LocalDateTime`. No timezone at all. The opposite mistake.

`Instant` is the right choice and matches Java idiom.

Under JEP 401 (preview), `Instant` is one of the JDK classes migrating
to a value class, which makes the heap object cost go away. We benefit
from this transition without changing source.

### 2.6 `duration`

**Lowering:** `java.time.Duration`.

`Duration` represents a span of seconds + nanos. Pairs with `Instant`:
`t1.minus(t0)` returns a `Duration`; `t.plus(d)` returns an `Instant`.
The type system tracks this; the runtime gets it for free via JDK
overloads.

`Duration` is also slated to become a value class under JEP 401.

### 2.7 `image`

**Lowering:** `byte[]` (descriptor `[B`).

Mochi `image` is opaque to the language; the runtime sees raw bytes.
`byte[]` is the natural JVM container. We expose helpers in
`mochi.runtime.image` that wrap common operations (load from path,
decode PNG/JPEG via `javax.imageio.ImageIO`, encode back).

For zero-copy paths (large images, GPU upload), the runtime supports
`java.lang.foreign.MemorySegment` (Foreign Function and Memory API,
JEP 442 GA in 22) as an alternate carrier. Mochi code does not see the
distinction; `image` is one type. The lower pass picks `MemorySegment`
for images larger than a tunable threshold (default 1 MiB) and `byte[]`
otherwise, similar to how `String` picks LATIN1 vs UTF16.

## 3. Operators

### 3.1 Numeric

| Mochi op | Mochi types | JVM op | Notes |
|---|---|---|---|
| `+` | `int, int` | `ladd` | wraps on overflow |
| `-` | `int, int` | `lsub` | wraps |
| `*` | `int, int` | `lmul` | wraps |
| `/` | `int, int` | `ldiv` | truncates toward zero |
| `%` | `int, int` | `lrem` | sign follows dividend |
| `+` | `float, float` | `dadd` | IEEE 754 |
| `-` | `float, float` | `dsub` | IEEE 754 |
| `*` | `float, float` | `dmul` | IEEE 754 |
| `/` | `float, float` | `ddiv` | IEEE 754, no exception on /0 |

Mochi forbids implicit numeric coercion. `int + float` is rejected by
the type checker. There is no `intToDouble` widening conversion in
generated bytecode unless the user wrote `float(x)` explicitly.

`int / int` truncates toward zero on the JVM (`ldiv` is defined by the
JLS to do exactly this). Mochi adopts the same semantics. Floor division
is available as a library function `Math.floorDiv(long, long)`.

Division by zero: `ldiv` and `lrem` throw `ArithmeticException` with
message `"/ by zero"`. `ddiv` returns `Infinity`, `-Infinity`, or `NaN`
per IEEE 754 and does not throw. Mochi inherits both behaviours
directly. A user who wants Mochi to throw on float div-by-zero must call
a library function that checks.

Overflow: Mochi `int` wraps on overflow (matches JVM `long`). Programs
that want overflow detection call `Math.addExact`, `Math.multiplyExact`,
etc., which throw `ArithmeticException` on wrap. The compiler exposes
these as `int.addExact(x, y)` etc. but does not insert them by default.

### 3.2 Comparison

| Mochi op | Mochi types | JVM lowering |
|---|---|---|
| `==` | primitives | `lcmp` / `dcmpl` / direct branch |
| `==` | references | `Objects.equals(a, b)` |
| `!=` | primitives | negated primitive compare |
| `!=` | references | negated `Objects.equals` |
| `<` | numeric | `lcmp` / `dcmpl` |
| `<` | string | `a.compareTo(b) < 0` |
| `<` | record / sum | `a.compareTo(b) < 0` if record implements `Comparable` |

Reference equality uses `Objects.equals(Object, Object)` rather than
raw `==` because Mochi `==` is structural, not identity. For records,
`Objects.equals` invokes the auto-generated `equals` method, which is
structural over all components. For sum types (sealed interfaces with
record cases), the same applies. For `String`, `Objects.equals` invokes
`String.equals` which is content comparison; this matches Mochi.

Primitive equality bypasses `Objects.equals` (no autoboxing).

NaN: `Double.NaN == Double.NaN` returns `false` (IEEE). Mochi's spec
matches. For sorting and use in tree maps, we use `Double.compare`
which gives a total order treating NaN as larger than `+Infinity`.

### 3.3 Logical

`a && b` lowers to short-circuit: evaluate `a`, branch on `ifeq` to
push `false` if `a` is false, otherwise evaluate `b`. `a || b`
symmetric with `ifne`. `!a` lowers to `iconst_1 ixor` (XOR with 1).

### 3.4 String concatenation

`a + b` for strings lowers to an `invokedynamic` instruction targeting
`java.lang.invoke.StringConcatFactory.makeConcatWithConstants` (since
JDK 9, JEP 280). The bootstrap method receives the recipe at link time
and the JIT specialises the concat to a direct `byte[]` allocation +
copy. This is faster than `StringBuilder.append` chains for short
strings and equivalent for long chains.

Multi-operand concat `a + b + c + d` lowers to a single `invokedynamic`
with all four operands on the stack, one allocation total, not three.

`StringBuilder` remains available for loops that build incrementally;
Mochi's runtime library exposes it as `string.builder()`.

## 4. Strings, in detail

### 4.1 Length and indexing

`len(s)` returns the count of Unicode code points, not the count of
UTF-16 code units. Java's `String.length()` returns the UTF-16 unit
count, which differs for any string containing a supplementary
character (code point > U+FFFF). We expose two functions:

- `len(s)` lowers to `s.codePointCount(0, s.length())`. O(n) on the
  number of UTF-16 units.
- `s.byte_len()` lowers to `s.getBytes(StandardCharsets.UTF_8).length`.
  O(n) plus an allocation; the runtime caches it where possible.
- `s.utf16_len()` lowers to `s.length()`. O(1).

`s[i]` for a Mochi index `i` (code points) lowers to a helper
`mochi.runtime.Strings.codePointAt(String, int)` that walks the string
via `String.offsetByCodePoints(0, i)` and then reads `codePointAt(off)`.
The cost is O(i); we document it. A bulk-iteration helper is faster
(§4.3).

### 4.2 Slicing

`s[a..b]` lowers to `s.substring(start, end)` where `start` and `end`
are computed via `s.offsetByCodePoints` from the Mochi indices. Since
JDK 7u6, `String.substring` allocates a fresh `byte[]` and does not
share with the source (the old shared-array behaviour was removed to
fix a memory-retention bug). This is safe but no longer free; a long
source string survives only as long as some `substring` retains it.

### 4.3 Iteration

`for ch in s` lowers to:

```java
s.codePoints().forEach(cp -> { /* body */ });
```

where `codePoints()` returns an `IntStream` of code-point values. The
iteration is O(n) total, not O(n^2) the way naive `for i in 0..len`
would be.

The runtime also provides `Strings.eachGrapheme(s, cb)` for cases where
the user wants extended grapheme clusters per Unicode 16 (matches
Mochi's `s.graphemes()` library function). Implementation goes through
`java.text.BreakIterator.getCharacterInstance()`.

### 4.4 Interpolation

Mochi has interpolated strings (`"hello \(name)"`). The original plan
was to lower to JEP 459 String Templates (preview in JDK 21 and 22).
That feature has been **withdrawn** as of JDK 23 (see [Java Almanac, String Templates withdrawn](https://javaalmanac.io/features/stringtemplates/)
and the [OpenJDK April 2024 update](https://mail.openjdk.org/pipermail/amber-spec-experts/2024-April/004106.html)).
A successor JEP exists but has not yet landed in any LTS.

Mochi's transpiler instead lowers interpolation to the
`invokedynamic StringConcatFactory` path (§3.4). This works on every
JVM from 9 onward, has no preview flag, and is faster than a string
template would have been once the processor abstraction was bypassed.
Once a stable string-template successor lands (post-JDK 26), we can
revisit; until then, `StringConcatFactory` is the right answer.

### 4.5 Equality and hashing

`String.equals` is content comparison. `String.hashCode` is the cached
polynomial hash (`s[0]*31^(n-1) + s[1]*31^(n-2) + ...`). Mochi
inherits both directly. Two strings with identical code points are
`equal()` and have identical `hashCode()`, regardless of internal
LATIN1/UTF16 split.

## 5. Collections

### 5.1 `list<T>`, mutable

**Lowering:** `java.util.ArrayList<T>` for the default mutable list.

`ArrayList` is a dynamically-resized backing array of `Object[]`. Random
access is O(1), append amortised O(1), prepend O(n). The Java level
sees `List<Long>` (boxed) for `list<int>`; the underlying array is
`Object[]`. Each `Long` is a separate heap object.

For lists known at compile time to hold a fixed primitive type
(`list<int>` in a tight scope), the lower pass emits a `long[]` backing
array instead. This is opt-in monomorphisation, gated on:

- The list type is closed (`list<int>`, `list<float>`, `list<bool>`).
- The list does not escape into a generic parameter slot.
- The size is either fixed at literal time or grows by `append` only.

When all three hold, we get one allocation (the `long[]`) plus zero
boxing per element, matching C-backend performance for hot loops.

### 5.2 `list<T>`, immutable

**Lowering:** `java.util.List.of(elements...)` factory.

`List.of` produces an immutable, value-based, possibly-shared list
implementation (`ImmutableCollections.ListN` and friends). It is
faster to construct, smaller in memory, and signals to the reader (and
to optimisers) that the list cannot mutate. Mochi literals
`[1, 2, 3]` lower to `List.of(1L, 2L, 3L)` boxed.

The 12-element fast path (`List.of` with up to 10 inline elements goes
through specialised classes) is a microoptimisation we get for free.

### 5.3 `map<K, V>`

**Lowering:** `java.util.LinkedHashMap<K, V>` for the default mutable
map. `java.util.Map.of` for immutable map literals.

Why `LinkedHashMap` rather than `HashMap`: Mochi documents map
iteration order as **insertion order**. `HashMap` iteration is bucket
order, which depends on hash codes and rehashing thresholds and is
effectively random. `LinkedHashMap` adds a doubly-linked list spine to
preserve insertion order at the cost of ~16 extra bytes per entry.

JEP 431 (Sequenced Collections, GA in 21) retrofits `LinkedHashMap` to
implement `SequencedMap`, which gives us `putFirst`, `putLast`,
`firstEntry`, `lastEntry`, `reversed()`, and `sequencedKeySet()` for
free. Mochi `m.first()`, `m.last()`, `m.reversed()` lower to these
directly. See [Oracle's JDK 21 docs on sequenced collections](https://docs.oracle.com/en/java/javase/21/core/creating-sequenced-collections-sets-and-maps.html).

For maps that need concurrent mutation from multiple virtual threads
(agent state shared across grain boundaries), the lower pass picks
`java.util.concurrent.ConcurrentHashMap`. `ConcurrentHashMap` does not
preserve insertion order; agents that need ordered iteration must
either lock externally or use a different structure.

### 5.4 `set<T>`

**Lowering:** `java.util.LinkedHashSet<T>` for the mutable set.
`java.util.Set.of` for immutable literals.

Same rationale as map: insertion order matters. `LinkedHashSet`
implements `SequencedSet` since JDK 21.

### 5.5 Empty and singleton optimisations

Literal `[]` lowers to `List.of()` (singleton empty). Literal `{}` for
a map lowers to `Map.of()`. Literal `set()` lowers to `Set.of()`. All
three are zero-allocation singletons.

Single-element literals `[x]` lower to `List.of(x)` (a specialised
single-element class).

### 5.6 Generic erasure and primitive specialisation

At the bytecode level, `List<Long>` is just `List` (raw, erased to its
bound `Object`). The element type is enforced only by checkcasts at
return sites. Mochi's compile-time type checker rules out misuse
before bytecode emission, so the checkcasts are correctness defence,
not safety load-bearing.

For `list<int>` in monomorphisation-eligible scopes (§5.1), the
bytecode descriptor changes from `Ljava/util/List;` to `[J` and the
operations change from `List.get(I)` (returning `Object` then
cast/unbox) to `laload` (returning `long` directly).

### 5.7 Lowering table for collections

| Mochi | Default JVM type | Literal factory | Concurrent variant |
|---|---|---|---|
| `list<T>` | `ArrayList<T>` | `List.of(...)` | `CopyOnWriteArrayList<T>` |
| `list<int>` (hot) | `long[]` | inline array literal | none |
| `map<K, V>` | `LinkedHashMap<K, V>` | `Map.of(...)` | `ConcurrentHashMap<K, V>` |
| `set<T>` | `LinkedHashSet<T>` | `Set.of(...)` | `ConcurrentHashMap.newKeySet()` |
| `omap<K, V>` | `LinkedHashMap` (always) | `Map.of(...)` (ordered as inserted) | not yet supported |

## 6. Records

Mochi declaration:

```mochi
type Point { x: int, y: int }
```

**Lowering:** JVM record (JEP 395, GA in 16).

```java
public record Point(long x, long y) {}
```

Records auto-generate:

- A canonical constructor `Point(long, long)`.
- Accessor methods `x()` and `y()`.
- Structural `equals(Object)` over both components.
- Structural `hashCode()` combining both components.
- A `toString()` of the form `Point[x=1, y=2]`.

These exactly match Mochi semantics: structural equality, structural
hashing, components accessed positionally and by name.

### 6.1 Methods on records

A Mochi method on `Point`:

```mochi
type Point { x: int, y: int }
method Point distance_from(other: Point) -> float {
    let dx = float(self.x - other.x)
    let dy = float(self.y - other.y)
    return sqrt(dx*dx + dy*dy)
}
```

lowers to an instance method on the record:

```java
public record Point(long x, long y) {
    public double distance_from(Point other) {
        double dx = (double)(this.x - other.x);
        double dy = (double)(this.y - other.y);
        return Math.sqrt(dx*dx + dy*dy);
    }
}
```

Records can implement interfaces (any number), extend nothing (records
are implicitly final and extend `java.lang.Record` directly). Mochi
methods on records translate 1:1.

### 6.2 Record patterns

JEP 440 (GA in 21) gives us record patterns inside `instanceof` and
`switch`:

```java
if (p instanceof Point(long x, long y)) {
    // x and y bound to components
}
```

Mochi destructuring:

```mochi
let Point(x, y) = p
```

lowers to either a record pattern (if the surrounding scope is a
`switch` or `if instanceof`) or to two accessor calls
`var x = p.x(); var y = p.y();` when used at top level. We prefer the
accessor form for simplicity since record patterns compile to the same
bytecode.

### 6.3 Default and update syntax

Mochi `p with { x: 5 }` lowers to a constructor call with all
components, substituting the named one:

```java
new Point(5L, p.y())
```

This is the canonical record-update idiom in Java; there is no
record-update syntax sugar in the JVM yet (a JEP for "with" expressions
has been discussed but not landed).

### 6.4 Record memory layout

A two-field record of two `long`s on a 64-bit JVM with compressed oops
occupies 24 bytes: 12-byte object header (8 bytes mark + 4 bytes class
pointer) + 16 bytes for the two `long` fields, rounded up to 32 bytes
due to 8-byte alignment.

JEP 401 (value classes, targeting JDK 26 preview, see
[OpenJDK JEP 401](https://openjdk.org/jeps/401) and the
[Inside.java JEP 401 article](https://inside.java/2025/10/27/try-jep-401-value-classes/))
will allow records to be declared `value record`, dropping identity and
the header. A `value record Point(long x, long y)` would flatten into
16 bytes inside arrays and fields, removing the header overhead. We
design our lowering so that once JEP 401 stabilises, we can flip records
to value records without source change. Until then, identity-record
overhead is the price; it is still cheaper than the BEAM 5-word tuple
mapping.

## 7. Sum types

Mochi declaration:

```mochi
type Result<T> = Ok(T) | Err(string)
```

**Lowering:** Sealed interface (JEP 409, GA in 17) with record cases
(JEP 395, GA in 16).

```java
public sealed interface Result<T> permits Result.Ok, Result.Err {
    record Ok<T>(T value) implements Result<T> {}
    record Err<T>(String msg) implements Result<T> {}
}
```

### 7.1 Pattern matching

Mochi `match`:

```mochi
match r {
    Ok(v) -> use(v)
    Err(msg) -> log(msg)
}
```

lowers to JEP 441 pattern-matching switch (GA in 21):

```java
switch (r) {
    case Result.Ok<T>(T value) -> use(value);
    case Result.Err<T>(String msg) -> log(msg);
}
```

The compiler verifies exhaustiveness against the sealed hierarchy at
`javac` level. Since the interface is sealed and all cases are covered,
no default clause is required and the `switch` is total.

The Mochi front end emits the cases in source order. `javac` compiles
the `switch` to a `tableswitch` or `lookupswitch` on the case's class
identity (via `invokedynamic` to a bootstrap that hashes the class
constant), so dispatch is O(1) regardless of case count.

### 7.2 Exhaustiveness

Sealed interfaces give compile-time exhaustiveness checking. If a Mochi
sum gains a new variant, every `match` must add a clause or the
generated Java will fail `javac`. This is the precise behaviour we want
and matches Mochi's source-level guarantee.

We do **not** emit a default `throw new MatchError(r)` clause unless
the source `match` is non-exhaustive (which Mochi's type checker
rejects). For partial matches that exit on failure, the front end
inserts the default explicitly.

### 7.3 Nullary constructors

For nullary variants:

```mochi
type Maybe<T> = Some(T) | None
```

`None` lowers to a record with no components, which is allocation-free
per call only if the JVM detects identity-elision (escape analysis +
scalar replacement). Without that, every `None` is a fresh object. We
optimise by emitting a singleton:

```java
public sealed interface Maybe<T> permits Maybe.Some, Maybe.None {
    record Some<T>(T value) implements Maybe<T> {}
    record None<T>() implements Maybe<T> {
        private static final None<?> INSTANCE = new None<>();
        @SuppressWarnings("unchecked")
        public static <T> None<T> instance() { return (None<T>) INSTANCE; }
    }
}
```

Mochi `None` lowers to `Maybe.None.<T>instance()`. Pattern matching
still works against the record case (record patterns match by class
identity, not by instance identity).

### 7.4 Generic sum types and erasure

`Result<T>` at the JVM level is `Result<Object>` after erasure. Pattern
matching against `Result.Ok<T>` is checked at runtime by classcheck
against `Result$Ok`, and the component type `T` is unchecked (Mochi's
type system is what guarantees it). This matches Java's standard
generic-erasure regime.

We emit `@SuppressWarnings("unchecked")` on the few sites where the
warning would fire, and `javac` is silent on the rest.

## 8. Generics

Mochi generics monomorphise in MEP-45 for the C backend. The JVM does
not need monomorphisation because erasure makes one generic class
serve all reference instantiations. However, primitive instantiations
benefit from monomorphisation just as on the C side.

### 8.1 Erasure baseline

The default lowering for a Mochi generic function:

```mochi
fun id<T>(x: T) -> T { return x }
```

is one method:

```java
public static <T> T id(T x) { return x; }
```

descriptor `(Ljava/lang/Object;)Ljava/lang/Object;`. Every primitive
instantiation autoboxes at the call site. For reference instantiations
this is free; for `int`/`float`/`bool` it adds a `Long.valueOf` or
similar.

### 8.2 Selective monomorphisation

For generic functions called with primitive arguments inside hot scopes,
the lower pass emits primitive-specialised variants:

```java
public static long id_long(long x) { return x; }
public static double id_double(double x) { return x; }
```

The call site picks the specialised variant when the static type is
primitive. Mochi's monorphisation pass from MEP-45 reports which
specialisations are needed; we reuse the same call graph analysis.

This is a hybrid strategy:

| Instantiation kind | Strategy | Cost |
|---|---|---|
| Reference types (`list<string>`, `Result<Point>`) | shared erased class | zero |
| Primitive types (`list<int>`, `Result<long>`) | monomorphised variant | one extra method per (T, primitive) pair |
| Mixed (`Map<string, int>`) | erased class + boxed primitives at boundary | one autobox per primitive op |

The third row is the unavoidable cost of erasure-plus-boxing. JEP 218
(JVM class and method specialisation, post-JEP 401) will eventually let
us specialise the class itself; until then, monomorphised entry points
plus erased internal class is the best we can do.

### 8.3 Variance

Mochi generics are invariant by default with explicit `out` (covariant)
and `in` (contravariant) markers. At the JVM level:

- Invariant `T` becomes plain `T` in the method signature.
- `out T` (covariant) becomes `? extends T` only at API boundaries that
  return a `T` to the user.
- `in T` (contravariant) becomes `? super T` at parameter positions.

Inside a single Mochi function body, all uses are concrete; wildcards
appear only at the public signature. This minimises wildcard
proliferation, which Java tooling does not always render well.

## 9. Function types

Mochi function values:

```mochi
let add: (int, int) -> int = (x, y) -> x + y
```

**Lowering:** `java.util.function.LongBinaryOperator` (primitive
specialisation, descriptor
`(JJ)J`).

The JDK provides primitive functional interfaces for arity up to 2 with
common element types:

| Mochi type | JDK interface |
|---|---|
| `() -> ()` | `Runnable` |
| `() -> T` | `Supplier<T>` |
| `(T) -> ()` | `Consumer<T>` |
| `(T) -> R` | `Function<T, R>` |
| `(T, U) -> R` | `BiFunction<T, U, R>` |
| `(int) -> int` | `LongUnaryOperator` |
| `(int, int) -> int` | `LongBinaryOperator` |
| `(float, float) -> float` | `DoubleBinaryOperator` |
| `(int) -> bool` | `LongPredicate` |
| `(int) -> ()` | `LongConsumer` |
| `(int) -> R` | `LongFunction<R>` |
| `() -> int` | `LongSupplier` |
| `(T) -> int` | `ToLongFunction<T>` |

### 9.1 Higher arity

Mochi function types of arity > 2 have no JDK equivalent. The runtime
ships `mochi.runtime.MochiFunction3<A, B, C, R>` through
`MochiFunction8`, plus primitive specialisations as needed.

### 9.2 Closures and `invokedynamic`

A lambda expression in source:

```mochi
let n = 10
let add_n: (int) -> int = (x) -> x + n
```

lowers to an `invokedynamic` instruction with bootstrap
`java.lang.invoke.LambdaMetafactory.metafactory` (JSR 335, GA in 8).
The captured variable `n` becomes a constructor argument to a synthetic
class generated by the runtime, identical to what `javac` produces for
Java lambdas.

We do **not** emit anonymous inner classes. The `invokedynamic` path
defers class generation until first use, may share class objects across
identical lambdas, and benefits from JIT inlining via the
LambdaMetafactory call site's `MethodHandle` linkage.

### 9.3 Method references

Where the Mochi source uses a top-level function name as a value:

```mochi
let f = greet
```

we lower to an `invokedynamic` with the method-handle to the named
function. The result is a `Function<T, R>` (or arity-matched) singleton
per source site, again identical to Java's `Class::method` lowering.

### 9.4 Identity equality on funs

Mochi does not expose fun equality at the language level (and Java's
lambda identity is implementation-defined: two `(x) -> x + 1` may or
may not be `==`). We forbid `==` on fun values in the type checker.
The JVM lowering never relies on fun identity.

## 10. Optional and nullability

Mochi has no `null`. Java does. The boundary needs explicit handling.

### 10.1 Mochi optional

Mochi `option<T>` (sugar `?T`) is a sum:

```mochi
type Option<T> = Some(T) | None
```

which lowers exactly per §7. It is **not** lowered to
`java.util.Optional<T>`, for two reasons:

- `Optional` cannot hold primitives without boxing (`OptionalLong`,
  `OptionalDouble`, `OptionalInt` exist but are separate types,
  splitting our type table).
- `Optional` cannot be nested usefully (`Optional<Optional<T>>` is
  legal but the inner empty is indistinguishable from the outer empty
  in many APIs).

Mochi `option<T>` stays a sealed interface with `Some<T>` and `None`.

### 10.2 FFI boundary

When Mochi code calls Java code that returns `null`, the FFI layer
inserts a guard:

```java
// Mochi sees: option<String> get_user_name(int id)
public static Option<String> get_user_name(long id) {
    String result = JavaApi.lookupName(id);
    return result == null ? Option.None.<String>instance() : new Option.Some<>(result);
}
```

When Mochi code passes a value to Java code that expects `null`able
parameters, the FFI binding accepts an `option<T>` and unwraps:

```java
public static long save(Option<String> name) {
    String javaName = (name instanceof Option.Some<String>(String v)) ? v : null;
    return JavaApi.save(javaName);
}
```

Mochi-to-Mochi calls never see `null`; the type system guarantees it.
The boundary is the only place `null` appears in generated code, and
it is always behind an explicit FFI signature.

### 10.3 `Optional<T>` at FFI to JDK

For Java APIs that already use `Optional<T>` (`Stream.findFirst`,
`Map.entry`, etc.), the FFI layer does:

```java
Option<T> result = jdkResult.map(Option::some).orElse(Option.None.<T>instance());
```

The Mochi side sees `option<T>` uniformly.

## 11. Numeric edge cases

### 11.1 Overflow

Mochi `int` wraps on overflow (matches JVM `long`). Bytecode `ladd`
wraps. To opt into checked arithmetic, the user calls
`int.addExact(x, y)`, which lowers to `Math.addExact(long, long)` and
throws `ArithmeticException` on wrap.

The Mochi compiler does not insert overflow checks. A `--strict-int`
flag could (future work).

### 11.2 Division by zero

`int / 0` throws `ArithmeticException("/ by zero")` per JVM spec.
Mochi propagates this. `float / 0.0` returns IEEE `Infinity` or `NaN`
without throwing.

### 11.3 NaN

`Double.NaN != Double.NaN` is `true`. `Double.NaN < x` and
`Double.NaN > x` are both `false` for any `x`. Mochi inherits IEEE
NaN semantics. For sorting, the JDK provides `Double.compare(a, b)`
which produces a total order: NaN sorts after `+Infinity`. Mochi's
default ordering on `float` uses `Double.compare`, which is consistent
across `Arrays.sort`, `TreeMap`, and the BEAM term-order convention.

### 11.4 Negative zero

`0.0 == -0.0` is `true`, `1.0 / 0.0` is `+Infinity`, `1.0 / -0.0` is
`-Infinity`. Mochi inherits IEEE behaviour. `Double.compare(0.0, -0.0)`
is `1`, treating `0.0 > -0.0` in the total order (this matters only
for sorting, not equality).

### 11.5 Integer division semantics

`ldiv` truncates toward zero: `(-7) / 2 == -3` (not `-4`). `lrem`
follows: `(-7) % 2 == -1`. Mochi adopts both. Floor division and
modulo (always non-negative result) are available as
`Math.floorDiv(a, b)` and `Math.floorMod(a, b)`.

## 12. Equality and hashing

| Type | `equals` | `hashCode` | Source |
|---|---|---|---|
| `int`, `float`, `bool` | primitive `==` | not applicable | JVM ops |
| `string` | `String.equals` (content) | `String.hashCode` (cached) | JDK |
| `time` | `Instant.equals` | `Instant.hashCode` | JDK |
| `duration` | `Duration.equals` | `Duration.hashCode` | JDK |
| `list<T>` | element-wise via `List.equals` | element-combining via `List.hashCode` | JDK |
| `map<K, V>` | entry-wise via `Map.equals` | entry-combining via `Map.hashCode` | JDK |
| `set<T>` | element-wise via `Set.equals` | element-combining via `Set.hashCode` | JDK |
| record `R` | structural per record contract | structural per record contract | JEP 395 |
| sum variant | record contract on the case | record contract on the case | JEP 395 |
| fun | identity (use of equality is rejected by type checker) | identity | JVM |
| `option<T>` | per record contract | per record contract | JEP 395 |

The Mochi `==` operator lowers to `Objects.equals(a, b)` for reference
types, which dispatches through the runtime `equals` and matches every
row above. For primitive types, `==` lowers to the JVM compare op
directly.

Records' auto-equals is structural over all components, recursively.
This matches Mochi semantics exactly. Records are the cleanest part of
the entire type lowering.

## 13. Comparable / Ord

Mochi has an `Ord` ability on `int`, `float`, `string`, `time`,
`duration`, and any user-defined record annotated `@Ord`.

**Lowering:** Mochi types with `Ord` lower to JVM types that implement
`Comparable<Self>`:

- `int` is comparable via `Long.compare`.
- `float` via `Double.compare`.
- `string` via `String.compareTo` (lexicographic on UTF-16 units, which
  matches lexicographic on code points for the BMP).
- `time` via `Instant.compareTo`.
- `duration` via `Duration.compareTo`.

User-declared `@Ord` records lower to records that implement
`Comparable<Self>`, with the auto-generated `compareTo` doing
component-wise comparison in source-declaration order:

```mochi
@Ord
type Point { x: int, y: int }
```

lowers to:

```java
public record Point(long x, long y) implements Comparable<Point> {
    @Override
    public int compareTo(Point other) {
        int c = Long.compare(this.x, other.x);
        if (c != 0) return c;
        return Long.compare(this.y, other.y);
    }
}
```

Sum types do not auto-derive `Ord`; the user must annotate and provide
an explicit clause per variant.

## 14. Threading safety

Mochi has no type-level "thread-safe" annotation. The language model is
that all Mochi values are safe to share between virtual threads
because they are either:

- **Immutable.** Records, sum types, primitives, strings, `time`,
  `duration` are immutable by construction. Safe to share with no
  synchronisation.
- **Owned by an agent.** Mutable collections (`list<T>`, `map<K, V>`,
  `set<T>`) when held inside an agent's private state are accessed
  only through the agent's message-receive loop, which serialises
  access by definition. No external sharing.
- **Sent across an agent boundary.** Values passed in agent messages
  are deep-immutable per Mochi semantics (the type system rejects
  sending a mutable collection that has aliases outside the
  message).

The JVM target runs every Mochi agent on a virtual thread (JEP 444, GA
in 21). Virtual threads share heap memory; the JVM's normal happens-before
rules apply. Mochi's immutability invariant means we never need
synchronisation primitives in generated code; the runtime mailbox
(implemented over `java.util.concurrent.LinkedTransferQueue`) provides
the necessary happens-before edge between sender and receiver.

The exception: a shared `ConcurrentHashMap` used for agent-registry
lookup. That is internal to the runtime, not exposed to source.

## 15. Memory layout, today and Valhalla-future

Approximate sizes on a 64-bit JVM with compressed oops:

| Type | Today (JDK 21/25) | With JEP 401 value classes (JDK 26+) |
|---|---|---|
| `long` primitive | 8 bytes | 8 bytes (no change) |
| `Long` boxed | 24 bytes (header 12 + value 8 + pad 4) | 8 bytes inline |
| `Point` record (2 long) | 32 bytes (header 12 + 16 + pad 4) | 16 bytes inline |
| `String` (10 ASCII) | ~32 bytes (object + coder + 10 byte payload) | similar (String is not value-class) |
| `Optional<T>` | 24 bytes + boxed value | 8 bytes inline (Optional is on the migration list) |
| `Instant` | 24 bytes (header 12 + seconds 8 + nanos 4) | 16 bytes inline |

The Valhalla transition (JEP 401 to JDK 26 preview, see
[OpenJDK JEP 401](https://openjdk.org/jeps/401)
and [Inside Java's JEP 401 article](https://inside.java/2025/10/27/try-jep-401-value-classes/))
flips many JDK classes to value classes including `Integer`, `Long`,
`Float`, `Double`, `Optional*`, `LocalDate`, `Instant`, and `Duration`.
Mochi benefits without source change. We track JEP 401 status across
JDK 26 and 27 and update the build matrix in [[10-build-system]] as
support stabilises.

## 16. Cross-tier consistency with MEP-45 and MEP-46

MEP-47 diverges from MEP-45 (C) and MEP-46 (BEAM) in several places.
Each divergence is documented below with the reason.

### 16.1 Sum-type tags: JVM class identity, BEAM atoms, C int discriminant

| Tier | Tag representation |
|---|---|
| MEP-45 (C) | `int32_t` discriminant in tagged union |
| MEP-46 (BEAM) | atom in the first position of a tagged tuple |
| MEP-47 (JVM) | class identity (one final class per case) |

The JVM choice is forced: pattern-matching switch dispatches on class
identity (the case label is a class literal). Reusing the BEAM atom
trick (a string field at index 0) would defeat JEP 441's compile-time
optimisation. Using an `int` discriminant would lose Java tooling
support (debugger, profiler, IDE).

### 16.2 Records: JVM record, BEAM tagged tuple, C struct

| Tier | Record representation |
|---|---|
| MEP-45 (C) | `struct` with named fields, owned alloc |
| MEP-46 (BEAM) | tagged tuple `{point, X, Y}` |
| MEP-47 (JVM) | `record` (JEP 395) |

All three have structural equality, all three carry a runtime tag (the
C struct via a `type_id` field on heterogeneous-collection cases, the
BEAM tuple via the leading atom, the JVM via class identity). The JVM
record is the most Mochi-faithful of the three: same syntax, same
auto-generated members, same nesting behaviour.

### 16.3 Maps: insertion order

| Tier | Default map type | Insertion order |
|---|---|---|
| MEP-45 (C) | open-addressing hash table | not preserved |
| MEP-46 (BEAM) | BEAM map | not preserved (sorted-key or hash order) |
| MEP-47 (JVM) | `LinkedHashMap` | preserved |

This is a real divergence. Mochi's spec says iteration order is
**unspecified** for `map<K, V>` and explicit via a separate
`omap<K, V>` type. MEP-46 honours the spec strictly. MEP-47 chooses to
preserve insertion order anyway, because:

1. `LinkedHashMap` is already the JVM idiom for predictable iteration.
2. The cost (16 extra bytes per entry) is negligible against JVM record
   header overhead.
3. Test reproducibility benefits from deterministic iteration.
4. JEP 431 (SequencedMap) makes the order-aware API ergonomic.

Programs that need raw `HashMap` performance can request it via
`@unordered` on the map declaration; the lower pass then picks
`HashMap`. This is the inverse default from MEP-46.

### 16.4 Strings: encoding

| Tier | Native encoding | Indexed by |
|---|---|---|
| MEP-45 (C) | UTF-8 | byte (with code-point helpers) |
| MEP-46 (BEAM) | UTF-8 binary | byte (with code-point helpers) |
| MEP-47 (JVM) | LATIN1 or UTF-16 (compact strings) | UTF-16 unit (with code-point helpers) |

Mochi indexes strings by code point. All three tiers have to walk the
encoding to find a code-point boundary. The JVM is the only one where
the in-memory encoding is not UTF-8, but the cost difference is
negligible (Java's compact strings make ASCII storage 1 byte per
character, same as UTF-8 for ASCII; multi-byte code points cost a
different number of code units in UTF-8 vs UTF-16 but iteration is
linear in either case).

UTF-8 round-trips at FFI boundaries via
`String.getBytes(StandardCharsets.UTF_8)` and the matching constructor.

### 16.5 Time: JDK type vs integer

| Tier | `time` representation |
|---|---|
| MEP-45 (C) | `int64_t` nanos since epoch |
| MEP-46 (BEAM) | integer nanos since epoch |
| MEP-47 (JVM) | `java.time.Instant` |

JVM chooses the JDK type because the JDK time library is rich,
well-tested, and Valhalla-ready. The cost (one heap allocation per
timestamp, vanishing under JEP 401) is acceptable against the loss of
`DateTimeFormatter`, `ZonedDateTime`, etc. The C and BEAM tiers do not
have an equivalent stdlib, so the integer-nanos choice there is
optimal for those backends.

Conversion between tiers is one division (Instant.toEpochMilli * 10^6
+ Instant.getNano) for cross-runtime data exchange.

## 17. Lowering table, consolidated reference

| Mochi type / construct | JVM representation | Boxing kind | Typical ops |
|---|---|---|---|
| `int` | `long` | `Long` at generic | `ladd`, `lsub`, `lmul`, `ldiv`, `lrem` |
| `float` | `double` | `Double` at generic | `dadd`, `dsub`, `dmul`, `ddiv` |
| `bool` | `boolean` | `Boolean` (cached) | `ifeq`, `ifne` |
| `string` | `String` | none | `+` via StringConcatFactory |
| `time` | `Instant` | none | `Instant.plus`, `compareTo` |
| `duration` | `Duration` | none | `Duration.plus`, `compareTo` |
| `image` | `byte[]` or `MemorySegment` | none | Imageio, FFM |
| `list<T>` mutable | `ArrayList<T>` | boxed elements | `List.add`, `List.get` |
| `list<T>` immutable literal | `List.of(...)` | boxed elements | `List.get` |
| `list<int>` hot | `long[]` | none | `laload`, `lastore` |
| `map<K, V>` | `LinkedHashMap<K, V>` | boxed | `Map.get`, `Map.put` |
| `map<K, V>` immutable literal | `Map.of(...)` | boxed | `Map.get` |
| `set<T>` | `LinkedHashSet<T>` | boxed | `Set.add`, `Set.contains` |
| record `R` | JVM `record R(...)` | structural | accessor methods, `equals`, `hashCode` |
| sum `T = A \| B` | `sealed interface T permits A, B` with records | structural | `switch` pattern match |
| sum nullary case | singleton instance via static field | identity | `==` against singleton |
| `option<T>` | `Option<T>` sealed (Some, None) | structural | record pattern |
| `(int, int) -> int` | `LongBinaryOperator` | none | indy LambdaMetafactory |
| `(T) -> R` | `Function<T, R>` | boxed args | indy LambdaMetafactory |
| `(A,B,C) -> R` arity 3 | `MochiFunction3<A,B,C,R>` runtime | boxed args | indy |
| `==` primitives | `lcmp` / `dcmpl` / direct | none | branch |
| `==` references | `Objects.equals` | n/a | structural dispatch |
| ordering | `Comparable.compareTo` | n/a | record default |
| string concat | invokedynamic StringConcatFactory | none | JDK 9+ |
| pattern match | `switch` with case patterns (JEP 441) | n/a | sealed + records |
| record destructuring | record pattern (JEP 440) | n/a | inline |
| closure | invokedynamic LambdaMetafactory | captured boxed | JDK 8+ |
| iteration `for x in xs` | enhanced for loop + Iterator | boxed unless monomorphised | JDK standard |
| FFI null | `Option.None` at boundary | one allocation | one-time at boundary |

This table is the source of truth. Anything not listed here is
undefined for the JVM backend and must be added to this section before
being emitted.

## 18. Open questions

Items deferred to later phases or to follow-up MEPs:

- **Valhalla rollout strategy.** Once JEP 401 reaches stable preview in
  JDK 26, do we emit `value record` for all Mochi records, or only for
  those known to live in arrays / hot fields? The cost of emitting a
  value record where the JVM cannot flatten it (because of escape into
  identity-sensitive APIs) is zero; the benefit where it can flatten is
  large. Default to value record everywhere, fall back to identity
  record only on Valhalla-incompatible scenarios (FFI to legacy APIs).
  Tracked in [[10-build-system]].
- **String template successor.** A redesigned string-template JEP is
  expected post-JDK 26. When it lands, evaluate whether to switch
  interpolation lowering from `StringConcatFactory` to the new template
  mechanism. Until then, no change.
- **Stream gatherers (JEP 485, GA in JDK 25).** Mochi pipeline operators
  may lower to stream gatherers for non-trivial pipelines. Deferred to
  [[08-dataset-pipeline]].
- **Pattern matching for `Map.Entry` and arbitrary classes (JEP 488,
  preview JDK 24).** When stable, simplifies map iteration patterns
  further.
- **Foreign function and memory API stability.** FFM is GA as of JDK 22
  (JEP 442). Use for `image` and any large-buffer FFI; track per-LTS
  status in [[07-jvm-target-portability]].

This note is the lowering contract. Updates require a paired update to
the matching section of `mep-0047.md` and a passing
TestPhase47TypeLowering gate.
