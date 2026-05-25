# MEP-47 research note 01, Mochi language surface (JVM target)

Author: research pass for MEP-47 (Mochi → JVM transpiler).
Date: 2026-05-23 (GMT+7).
Sources: `docs/features/*.md`, `docs/index.md`, `docs/common-language-errors.md`,
`mcp/cheatsheet.mochi`, `ROADMAP.md`, `examples/v0.2`-`v0.7`, normative security
specs `docs/security/threat-model.md` and `docs/security/memory-safety.md`, and
the companion MEP-45 note 01 and MEP-46 note 01 (whose section structure this
note deliberately mirrors so all three backends can be diffed line-for-line).

This note records the user-visible language surface that the JVM target must
faithfully reproduce. It is deliberately written *from the spec downward* and
ignores the existing Go runtime (vm3), the vm3 bytecode, the C target under
MEP-45, the Erlang/BEAM target under MEP-46, and any other backend
implementation. The goal is a transpiler design that would be correct against
the *language*, not against the present implementations.

The surface decomposes into the same eight orthogonal sub-languages identified
in MEP-45 note 01 and MEP-46 note 01: (1) the value core, (2) the function and
method core, (3) the collection core, (4) the algebraic-data-type core, (5) the
query DSL, (6) the stream / agent core, (7) the logic-programming core, and
(8) the AI / FFI shells. Each section below names every form a Mochi program
can write, then states a *lowering obligation* the JVM backend must honour.

Where MEP-45 maps Mochi types to C struct + helper-function pairs, and MEP-46
maps them to BEAM terms (atoms, tagged tuples, maps, binaries, funs, PIDs),
this note maps them to JVM values: primitives (`long`, `double`, `boolean`),
boxed numerics, `java.lang.String`, `java.util.List`/`Map`/`Set`, Java records,
sealed interfaces, lambdas, and Loom virtual threads. The target IR is
discussed in note 05 (Java source emitted through `javax.tools.JavaCompiler`
for the default path, with a ClassFile-API direct-bytecode fallback for
hot lowerings); the runtime is the JDK plus a thin `dev.mochi.runtime.*`
support jar (see note 04). Throughout, "JVM" means OpenJDK 21 LTS or later;
Android Runtime (ART) is a Phase-2 secondary target and is called out where
its dex constraints force a divergence.

## 1. Value core

### 1.1 Bindings

Mochi has exactly two binding forms.

- `let name = expr`, immutable. Re-assignment is a **compile-time** error,
  not a runtime panic. The JVM lowering emits a Java `final` local (and a
  `final` field for module-level lets) so that javac itself rejects any
  accidental re-assignment leak that escapes the Mochi type checker.
- `var name = expr`, mutable. Re-assignment is unrestricted within the
  variable's lexical scope. Lowers to a plain Java local (no modifier) or a
  private mutable field.

Mochi blocks are expressions in the sense that the last expression is the
block's value. Java blocks are statements. The backend lowers a block whose
value is consumed into a `var ___t = switch (1) { default -> { ...; yield e; }
};` synthesis (JEP 361 expression switch) so that a Mochi `let x = if cond
{ a } else { b }` becomes a single-expression Java construct without
materialising a helper method. See note 05 §6 for the full block-lowering
table.

A binding may carry an explicit type: `let x: int = 0`. Java is statically
typed; the type survives end-to-end. `int` lowers to Java `long` (see §1.2
on why), so `let x: int = 0` becomes `final long x = 0L;`.

Mochi supports destructuring at `let`:

```mochi
let [a, b] = [1, 2]
let {"name": n, "age": age} = {"name": "Ana", "age": 22}
```

JDK 21 has record-deconstruction patterns (JEP 440) but they only apply to
record types, not to `List` / `Map`. The list pattern lowers to a positional
read with a runtime length check:

```java
final var ___tmp = List.of(1L, 2L);
if (___tmp.size() != 2) throw new MochiPatternError(...);
final long a = (Long) ___tmp.get(0);
final long b = (Long) ___tmp.get(1);
```

The map pattern lowers to `Map.get` per key with a null check (since the
pattern asserts the keys exist). For records the backend can emit JEP 440
record patterns directly (`if (point instanceof Point(long x, long y))`),
which is the cleanest fit. See note 05 §11 for the full destructuring
strategy.

Scoping is lexical and block-based. Inner blocks shadow outer bindings.
Java allows block-scoped locals but forbids name shadowing of locals in
inner blocks; the backend must rename shadowed names with a suffix
(`x`, `x__1`, `x__2`). See note 05 §4.

### 1.2 Primitive types

Surfaced by the docs and the cheatsheet, with the JVM-side representation:

| Mochi | Width / semantics | JVM lowering |
|-------|------------------|---------------|
| `int` | 64-bit signed integer (inferred from integer literals) | Java `long` primitive (unboxed); `Long` when boxed for generics or `Object` slots |
| `float` | 64-bit IEEE 754 double | Java `double` primitive; `Double` boxed |
| `bool` | `true` / `false` | Java `boolean`; `Boolean` boxed |
| `string` | UTF-8 text, indexable as code points, immutable | `java.lang.String` (note: internal `byte[]` since JEP 254 Compact Strings, may be Latin-1 or UTF-16; the lowering treats `String` opaquely and goes through `codePoints()` / `indexOf` rather than `charAt`) |
| `time` | absolute timestamp (used by streams) | `java.time.Instant` |
| `duration` | time interval (`std/time` API) | `java.time.Duration` |
| `image` (preview) | binary blob (`load "cat.png"`) | `byte[]` wrapped in a `dev.mochi.runtime.MochiImage` record |

Why `long` and not `int`? Mochi's documented `int` is 64-bit. Java's `int`
is 32-bit; using it would silently truncate. `long` is the right primitive.
The cost is that array indices into Java collections require `int` and so
become explicit `Math.toIntExact(i)` calls (which throw on overflow) at the
boundary; see note 06 §5 for the runtime guard policy.

Implicit numeric conversions are **not** allowed (per the type-checker
discipline implied by MEP-4/5/6 referenced from the threat model). `int +
float` is a type error; the program must `float(x)` first. On the JVM
unlike BEAM this matters less for correctness (Java's `long + double` is
unambiguous, the long is widened to double following IEEE), but the
emitted code never exercises mixed arithmetic because Mochi rejects it at
the type layer.

Integer overflow on the JVM is silent wrap-around (two's complement), which
matches Mochi's documented semantic. No guard is needed for the default
build. The `--strict-int` build flag wraps every arithmetic op in
`Math.addExact` / `Math.multiplyExact` etc., which throw `ArithmeticException`
on overflow. Off by default, on for security-sensitive builds. See note
06 §5.

### 1.3 Operators

Arithmetic `+ - * / %`; comparison `== != < <= > >=`; boolean `&& || !`;
membership `in`; string concatenation overloads `+`.

| Mochi | Java |
|-------|------|
| `a + b` (int)    | `a + b` (both `long`) |
| `a + b` (float)  | `a + b` (both `double`) |
| `a + b` (string) | `a + b` (Java `String + String`; the javac emits an `invokedynamic` to `StringConcatFactory.makeConcatWithConstants` per JEP 280) |
| `a + b` (list)   | `dev.mochi.runtime.Lists.concat(a, b)` (returns a fresh immutable list) |
| `a - b` | `a - b` |
| `a * b` | `a * b` |
| `a / b` (float) | `a / b` |
| `a / b` (int)   | `Math.floorDiv(a, b)` (Mochi's `/` on ints is floor-division for negative dividends; document) |
| `a % b` | `Math.floorMod(a, b)` |
| `a == b` (primitive) | `a == b` |
| `a == b` (reference) | `Objects.equals(a, b)` |
| `a != b` | logical negation of the above |
| `a < b`, `<=`, `>`, `>=` | numeric: native; string: `a.compareTo(b) < 0` |
| `a && b` | `a && b` (short-circuit) |
| `a \|\| b` | `a \|\| b` |
| `!a` | `!a` |
| `x in xs` (list) | `xs.contains(x)` |
| `x in m` (map) | `m.containsKey(x)` |
| `x in s` (set) | `s.contains(x)` |

The lowering must distinguish primitive `==` (value identity for `long`,
`double`, `boolean`) from reference `==` (identity for `String`, `List`,
records). Mochi's `==` is *value* equality everywhere. For Java, primitive
`==` already does value equality; reference `==` does identity, which is
wrong; the backend emits `Objects.equals(a, b)` instead. The exception is
sealed-interface ADT variants where the backend can prove both sides have
the same concrete record type, in which case `record.equals` is auto-generated
and `Objects.equals` is still semantically correct (and the JIT will inline).

### 1.4 Strings as read-only code-point sequences

```mochi
let text = "hello"
print(text[1])     // "e"
for ch in text { ... }
```

Indexing yields a 1-character string (not a `char`). Iteration yields
1-character strings in **code-point** order, not byte or UTF-16-code-unit
order. The JVM lowering must therefore avoid `String.charAt` (which returns
a UTF-16 code unit) for any code point above U+FFFF:

- `text[i]` lowers to `dev.mochi.runtime.Strings.codePointAt(text, i)` which
  returns a 1-character `String` (formed by `new String(Character.toChars(cp))`).
- `for ch in text` lowers to a `text.codePoints().forEach(cp -> ...)` Stream
  loop in the cold path, or a hand-rolled `while (i < text.length())` with
  `String.codePointAt(i)` + `Character.charCount(cp)` increment in the hot
  path (which is what the JDK itself uses internally and what `codePoints()`
  ends up as after the C2 JIT inlines the lambda).
- `len(text)` lowers to `text.codePointCount(0, text.length())`, not
  `text.length()` (which is UTF-16 code units). The UTF-16 form is exposed
  as `code_unit_len` in the runtime for users who explicitly want it; the
  byte form (UTF-8) is `utf8_byte_len`.

This is the area where the JVM target diverges most from BEAM: on BEAM the
canonical form is the UTF-8 binary; on the JVM the canonical form is the
JDK `String` (whose internal storage is compact Latin-1 or UTF-16, JEP 254).
The Mochi-level semantic stays the same: code-point indexing.

### 1.5 Literals

Integer literals; floating literals (`3.14`); boolean; string with C-style
escapes; triple-quoted multi-line strings (`"""..."""`); list `[...]`;
map `{key: val, ...}`; set `{a, b, c}`; record constructor `T { field: val }`.

The set literal `{a, b, c}` is distinguished from the empty/map literal
`{}` by the absence of `:` after the first element. The grammar must keep
these unambiguous; the JVM lowering picks the right constructor accordingly.

Lowering forms:

| Mochi | Java |
|-------|------|
| `42` | `42L` (note the L suffix; bare `42` is 32-bit `int` in Java) |
| `3.14` | `3.14` (already a `double` literal in Java) |
| `true` / `false` | `true` / `false` |
| `"hello"` | `"hello"` |
| `[1, 2, 3]` | `List.of(1L, 2L, 3L)` (immutable; `JEP 269`) |
| `{"a": 1, "b": 2}` | `Map.of("a", 1L, "b", 2L)` for <=10 entries; `Map.ofEntries(Map.entry(...))` above, except where insertion order matters in which case the lowering emits `new LinkedHashMap<>()` filled imperatively (see §3) |
| `{1, 2, 3}` (set) | `new LinkedHashSet<>(List.of(1L, 2L, 3L))` (insertion-ordered, see §3) |
| `Book { title: "X", pages: 10 }` | `new Book("X", 10L)` (record constructor; see §4 on record codegen) |

`List.of` and `Map.of` are immutable and reject `null`, which matches
Mochi semantics (no null at the language level, see §1.7). For very large
literals the lowering emits a `List.copyOf(Arrays.asList(...))` to avoid the
overloaded-method explosion.

### 1.6 Identifier mangling

Java identifiers may begin with letter, `$`, or `_` and continue with
letter/digit/`$`/`_`. Mochi identifiers are stricter (letter then
letter/digit/`_`) so every Mochi identifier is a legal Java identifier.
The problem is reserved words: `class`, `if`, `while`, `final`, `record`,
`sealed`, `permits`, `synchronized`, `default`, etc. Mochi has no such
restriction.

Mangling rules (full table in note 06 §2 and §3):

- Mochi variables that collide with a Java reserved word ⇒ suffix `_`
  (`class` ⇒ `class_`, `default` ⇒ `default_`). The reservation list is
  the JLS §3.9 keyword list, augmented with Mochi-internal helpers
  (`$$mochi_*`).
- Mochi local function references and method names use the same suffix
  rule. Camel-case is preserved (`fooBar` stays `fooBar`).
- Mochi package paths `mathutils/extra` ⇒ Java package
  `dev.mochi.user.mathutils.extra` for user code (configurable via
  `--jvm-base-package`; default `dev.mochi.user`). The `user.` segment is
  there to make the runtime / user distinction visible in stack traces.
- Mochi record type names ⇒ Java class names in PascalCase, unchanged
  (`Book` ⇒ `Book`). On collision with `java.lang.*` (e.g. `String`,
  `Object`, `Number`) the backend renames `String` ⇒ `String_` and
  emits an import-shadowing comment.
- Mochi sum-type variant constructors ⇒ Java record classes nested in a
  sealed interface (`Leaf` ⇒ `Leaf`, `Node` ⇒ `Node`, both implementing
  `Tree`; see §4 ADT lowering).

The mangling is deterministic (note 05 §3) and reversible via
JVMS-defined `SourceFile` and `LineNumberTable` attributes (and the newer
`SourceDebugExtension` for source maps, JSR 45) so stack traces can point
back to Mochi source. See note 10 §15.

### 1.7 Nullability

Mochi has no `null` at the language level. Optional values are expressed
via the `Option<T>` sum type. The JVM lowering must enforce this at the
FFI boundary: any value coming in from Java code (which can be `null`)
crosses a `Objects.requireNonNull` checkpoint or, where the type is
`Option<T>`, is wrapped via `Optional.ofNullable(...).map(Foo::ok)`.

The JVM target does **not** emit `Optional<T>` for Mochi `Option<T>`.
`Optional` is documented as "not designed for general use" by the JDK
authors (in particular, not for fields or arguments per JEP 269 guidance).
The lowering uses Mochi's own sealed interface `Option<T>` with variants
`Some<T>(T value)` and `None`. See §4.

## 2. Function and method core

### 2.1 Top-level functions

```mochi
fun add(a: int, b: int): int { return a + b }
```

Lowers to a Java `static` method in the module class:

```java
public final class MyModule {
  public static long add(long a, long b) { return a + b; }
}
```

Every Mochi source file produces one Java class named after the file
(`example.mochi` ⇒ `Example.java` ⇒ `class Example`) with all top-level
functions as `public static` methods. Module-level `let` and `var` become
`public static final` (for `let`) and `public static` (for `var`) fields,
initialised in a `<clinit>` block in declaration order.

Mochi `return` is explicit (unlike Erlang's "last expression"). The Java
lowering preserves `return` directly: `return e;` becomes Java `return e;`.
Implicit returns at the end of a block (where Mochi allows the last
expression to be the value) get an explicit `return` synthesised. See
note 05 §7 on the early-return lowering.

The docs warn there is **no implicit tail-call optimisation** in Mochi.
The HotSpot JVM does not do general TCO either (the JIT can sometimes
turn self-recursive tail calls into loops, but it is not guaranteed). The
backend therefore does *not* rely on TCO and emits a trampoline for any
self-recursive function whose call graph exceeds an inferred depth
(threshold tunable via `--jvm-trampoline-depth`, default 1000). See
note 05 §15.

### 2.2 First-class function values

```mochi
let square = fun(x: int): int => x * x
fun apply(f: fun(int): int, value: int): int { return f(value) }
```

Lower to Java lambdas:

```java
final java.util.function.LongUnaryOperator square = x -> x * x;
static long apply(java.util.function.LongUnaryOperator f, long value) {
  return f.applyAsLong(value);
}
```

The JDK provides specialised primitive functional interfaces
(`LongUnaryOperator`, `LongBinaryOperator`, `DoubleFunction`, etc.) that
avoid boxing for `long` and `double`. The backend picks the most specific
JDK interface that matches the Mochi signature; for signatures the JDK does
not specialise (e.g. `(int, string) -> bool` or arity > 2), the backend
emits a synthetic `dev.mochi.runtime.func.Fn{N}<...>` interface and uses
that. See note 06 §6 and note 04 §4 on the runtime function-interface
zoo.

Lambdas in Java compile to `invokedynamic` LambdaMetafactory bootstraps
(JEP 8003895), which generate a hidden synthetic class at first use. The
JIT inlines them aggressively. Closures escape freely; captured variables
must be effectively final at the Java source level, so the backend lifts
mutable captures into a 1-element array (`final long[] counter = {0};`)
or a `dev.mochi.runtime.Cell<T>` mutable box. See note 05 §16.

### 2.3 Methods on type blocks

```mochi
type Circle {
  radius: float
  fun area(): float { return 3.14 * radius * radius }
}
```

A method receives an implicit `self`; field names inside the block are
unqualified. Lowering: the record is a Java record and the method is an
instance method on that record:

```java
public record Circle(double radius) {
  public double area() { return 3.14 * radius * radius; }
}
```

This is the cleanest available lowering on JDK 21: records own their
fields, implement `equals`/`hashCode`/`toString` for free, and support
JEP 440 pattern matching. Field access inside a method is the record's
implicit accessor (`radius()` for `double radius`), which the Mochi
emitter writes as a direct field reference (`this.radius` is legal inside
a record's instance method body).

If the type has too many fields for a record (Java records have no hard
limit but readability degrades past ~10), or if the type is mutable
(`var` fields), the backend falls back to a plain `final class` with
private fields and explicit `equals`/`hashCode`. The threshold is
configurable; default is "always use record unless mutable".

### 2.4 Built-in `print`

Variadic, prints with default formatting and inserts spaces (cheatsheet:
`print("name = ", name, ...)`); newline at end.

Lowers to a runtime call `dev.mochi.runtime.IO.print(Object... args)`
where the runtime walks the varargs, applies per-type formatting (since
Java's `Object.toString` does the right thing for primitives via
auto-boxing but the wrong thing for arrays and the wrong thing for many
JDK collection types whose `toString` is `[1, 2, 3]` Java-style rather
than Mochi-style), inserts single-space separators, and writes the
trailing newline via `System.out`. The runtime caches a `PrintStream`
locally and flushes on every newline (matching Mochi's "line-buffered
stdout" guarantee). See note 04 §3 for the `dev.mochi.runtime.IO` class.

## 3. Collection core

Three primitive containers, all with structural typing:

- `list<T>`, ordered, growable. ⇒ JDK `java.util.List<T>`, immutable view
  emitted from `List.of` / `List.copyOf` / a private `ArrayList` wrapped.
- `map<K, V>`, keyed lookup. ⇒ `java.util.LinkedHashMap<K, V>`
  (insertion-ordered; see below).
- `set<T>`, unordered, unique members. ⇒ `java.util.LinkedHashSet<T>`.
- `string`, read-only `list<char>`-ish (see §1.4). ⇒ `java.lang.String`.

Lowering obligations (full per-type details in note 06 §1):

- `list<T>` is the workhorse. The default representation is
  `java.util.List<T>` with random access O(1) (backed by `ArrayList`).
  This is one of the *easy* wins on the JVM relative to BEAM (where lists
  are cons cells, O(n) random access). The cost is element boxing: a
  Mochi `list<int>` becomes `List<Long>`, with each `Long` a heap object.
  The runtime exposes a primitive-specialised `LongList`, `DoubleList`
  (backed by `long[]` / `double[]`) for hot paths; the compiler picks
  it automatically when the element type is monomorphic and the list is
  not exposed to Java FFI as a generic `List<Object>`. See note 06 §10.

- `map<K, V>` defaults to `LinkedHashMap` (not `HashMap`) because Mochi's
  iteration order is insertion order, documented in `docs/features/map.md`.
  This is one of the few places we deliberately *do not* pick the
  fastest JDK option (HashMap) because semantics force the choice. The
  `HashMap` variant is exposed as `hashmap<K, V>` (preview) for users who
  do not care about iteration order.

- `set<T>` is `LinkedHashSet` for the same reason. The query layer (§5)
  needs the *insertion-ordered* semantics for `union`/`except` to be
  deterministic.

- All collections are **value-semantically copied** at language level.
  JDK collections are mutable by default; the lowering enforces
  immutability by wrapping every collection that leaves a function via
  `List.copyOf` / `Map.copyOf` / `Set.copyOf` (which return unmodifiable
  views with structural sharing where possible). The VM enhancement spec
  0951 §1 ("each function call must allocate a fresh copy of any
  list/map literal") is satisfied by emitting fresh `List.of` /
  `Map.of` constructions at each call site (these are cheap, but not
  zero-cost; note 18 measures the impact).

Mutation operations (`xs.add(x)`, `m[k] = v`) lower to copy-on-write
helpers in the runtime that allocate a new collection with the change
applied. The runtime ships immutable persistent collection types
(`PersistentList`, `PersistentMap`, `PersistentSet`) based on
hash-array-mapped-trie / RRB-tree implementations (we ship our own,
small ones, rather than pulling in a 3rd-party library like
Vavr or Capsule; see note 06 §11). The compiler picks persistent vs
JDK based on the inferred mutation rate (cheap heuristic: any function
that calls `.add` / `.put` more than twice on the same value uses the
persistent form; everything else uses the JDK `List.of`-style immutable
view).

`for x in xs` lowers to an enhanced-for loop:

```java
for (var x : xs) { ... }
```

For maps, `for (k, v) in m` lowers to:

```java
for (var entry : m.entrySet()) { var k = entry.getKey(); var v = entry.getValue(); ... }
```

The JVM's enhanced-for compiles to an `Iterator` + `hasNext` + `next`
loop, which the C2 JIT inlines for the standard JDK collection types.

## 4. Algebraic data type core

Mochi's sum-of-products data types (`type Tree = Leaf | Node { ... }`) are the
cleanest fit for sealed interfaces + records, a combination that stabilised in
JDK 17 (sealed: JEP 409; records: JEP 395) and gained pattern-matching support
in JDK 21 (record patterns: JEP 440; switch patterns: JEP 441).

```mochi
type Tree =
  | Leaf
  | Node { value: int, left: Tree, right: Tree }
```

Lowers to:

```java
public sealed interface Tree permits Tree.Leaf, Tree.Node {
  record Leaf() implements Tree {}
  record Node(long value, Tree left, Tree right) implements Tree {}
}
```

The sealed interface lives at the module level; the variant records are
nested inside it. The constructor `Leaf` is exposed as `new Tree.Leaf()`
(or a cached singleton `Tree.LEAF` for nullary variants; the backend
detects nullary variants and synthesises the singleton). `Node` becomes
`new Tree.Node(v, l, r)`.

Pattern matching:

```mochi
match t {
  Leaf => 0
  Node { value, left, right } => value + sum(left) + sum(right)
}
```

Lowers to a JDK 21 switch expression with record patterns:

```java
return switch (t) {
  case Tree.Leaf leaf -> 0L;
  case Tree.Node(long value, Tree left, Tree right) -> value + sum(left) + sum(right);
};
```

Exhaustiveness is checked at *both* layers: Mochi's type checker rejects
non-exhaustive matches at compile time, and the Java compiler also rejects
non-exhaustive switches on a sealed interface (JEP 441), so both lines of
defence agree. If the Mochi checker accepts a match the Java compiler
guarantees the switch is exhaustive (the backend never has to emit a
`default` arm). See note 05 §10.

Guarded patterns:

```mochi
match shape {
  Circle { radius } if radius > 10.0 => "big"
  Circle { radius }                  => "small"
  _                                  => "other"
}
```

Lower to JEP 441 guarded patterns:

```java
return switch (shape) {
  case Circle(double radius) when radius > 10.0 -> "big";
  case Circle(double radius)                    -> "small";
  default                                       -> "other";
};
```

The `when` keyword (JDK 21) replaces the older `&&` proposal from
preview. The backend always emits `when`, since JDK 21 is the floor (see
note 02).

Generic ADTs:

```mochi
type Option<T> = | Some { value: T } | None
type Result<T, E> = | Ok { value: T } | Err { error: E }
```

Lower with generic type parameters on the sealed interface and each
variant record:

```java
public sealed interface Option<T> permits Option.Some, Option.None {
  record Some<T>(T value) implements Option<T> {}
  record None<T>() implements Option<T> {
    private static final None<?> INSTANCE = new None<>();
    @SuppressWarnings("unchecked")
    public static <T> None<T> instance() { return (None<T>) INSTANCE; }
  }
}
```

The `None<T>` singleton trick is necessary because `None` is parametric
in `T` but has no `T`-typed field; we cache one heap object and cast it.
This is exactly what `Optional.empty()` does inside the JDK, so the cost
is well-understood.

## 5. Query DSL

Mochi's query DSL (`from x in xs select { ... }`, with `join`, `group by`,
`order by`, `limit`, `offset`, `where`) is the densest sub-language. Its
JVM lowering uses the JDK Stream API as the primary IR. Full lowering is
in note 08, this section names the surface forms only.

```mochi
let adults =
  from p in people
  where p.age >= 18
  order by p.name
  select { name: p.name, age: p.age }
```

Lowers to:

```java
final var adults = people.stream()
  .filter(p -> p.age() >= 18)
  .sorted(Comparator.comparing(Person::name))
  .map(p -> new $$AdultRow(p.name(), p.age()))
  .toList();
```

`group by` lowers to `Collectors.groupingBy`; `join` lowers to a hash-join
helper in `dev.mochi.runtime.Query` (since `Stream` has no built-in join);
`limit` / `offset` are direct Stream operations.

Important: the **Mochi `stream<T>` type and the Java `Stream<T>` type are
not the same thing**. Java's `Stream` is a lazy, single-shot,
finite-or-infinite pipeline over an in-memory collection. Mochi's
`stream<T>` is a *time-evolving* publisher (closer to `Flow.Publisher` /
Project Reactor `Flux`). The query DSL uses the Java Stream type as an
internal lowering vehicle for *finite* collection queries; the public
`stream<T>` type uses `Flow.Publisher`. See note 09 for the agent /
stream lowering.

## 6. Stream and agent core

```mochi
stream Tick = { time: time }

agent ticker {
  every 1s emit Tick { time: now() }
}
```

Streams lower to `java.util.concurrent.Flow.Publisher<T>` (JDK 9+, JSR
266). Agents lower to virtual threads (JEP 444, GA in JDK 21). Each agent
gets its own virtual thread bound to a Loom-friendly `Thread.ofVirtual()`
factory; the thread blocks on a `LinkedTransferQueue<Object>` for incoming
messages (`on Tick { ... }` clauses).

```java
public final class Ticker implements Agent {
  private final TransferQueue<Object> $$mailbox = new LinkedTransferQueue<>();
  private final Thread $$thread;
  public Ticker() {
    this.$$thread = Thread.ofVirtual()
      .name("mochi-agent-ticker")
      .start(this::$$run);
  }
  private void $$run() {
    while (!Thread.currentThread().isInterrupted()) {
      // every 1s emit Tick { ... } loop
    }
  }
}
```

The mailbox uses unbounded `LinkedTransferQueue` rather than
`ArrayBlockingQueue` because Loom virtual threads can park cheaply on
the queue and there's no benefit to a bounded queue for the default
agent (back-pressure is a separate concern, see note 09 §4).

Agents talk via typed message constructors:

```mochi
ticker ! Tick { time: now() }
```

Lowers to:

```java
ticker.$$send(new Tick(Instant.now()));
```

Loom virtual threads make this lowering essentially free: each agent is
a few-KB stack carrier'd to a small platform-thread pool, scheduling is
fair, and blocking on the mailbox doesn't pin a platform thread. The big
gotcha is `synchronized` blocks (which pin in JDK 21 but are fixed by
JEP 491 in JDK 24): the runtime must avoid `synchronized` and use
`ReentrantLock` instead. See note 09 §6.

## 7. Logic programming core

```mochi
fact parent(alice, bob).
fact parent(bob, charlie).

rule ancestor(X, Y) :- parent(X, Y).
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

query ancestors_of(X) := ancestor(alice, X).
```

The logic core targets a small embedded Datalog engine. The JVM lowering
emits a runtime call into `dev.mochi.runtime.datalog.Engine`, with facts
and rules registered at module init time. The engine implements
semi-naive bottom-up evaluation; magic-set transforms are a v2 concern.

Datalog terms are represented as `java.lang.Object` (which can hold any
boxed primitive, `String`, record, or sealed-interface variant). Predicates
are `Predicate<Object[]>` lambdas. Unification is a plain equality check
(no variables in facts, only in queries). See note 08 §4 for the engine
internals.

## 8. AI and FFI shells

```mochi
let summary = ai("summarise this", text)
let result = ffi("std/json/parse", raw)
```

The `ai(...)` builtin lowers to a runtime call into
`dev.mochi.runtime.ai.AI.call(String prompt, Object... args)`. Provider
selection (OpenAI, Anthropic, local) is driven by env vars at runtime, not
by codegen choices. See note 04 §10.

The `ffi(...)` builtin lowers to a `dev.mochi.runtime.ffi.FFI.call(String
path, Object... args)` that looks up the named function in a module
registry. JVM FFI naturally uses the JNI / Java reflection mechanism for
calling host Java code; for Mochi-to-Mochi FFI calls the registry just
dispatches to the right module class. See note 11 §3.

The Mochi-to-Java FFI (calling arbitrary JDK / Maven Central code) is one
of the load-bearing reasons we want a JVM target at all (see note 02 §4).
The transpiler exposes `import "java/util/UUID"` and
`import "java/com/fasterxml/jackson/databind/ObjectMapper"` as first-class
forms. Each lowers to a Java `import` and surfaces the type's public API
as Mochi methods. See note 11.

## 9. What this surface does *not* include

- **Untyped `any`**: Mochi rejects it at the type layer. The JVM has
  `Object` and the temptation to weaken Mochi's type system to allow
  `any` is real; we resist. The cost would be losing static-typed FFI
  guarantees.
- **Implicit conversions**: ruled out above. Required to keep BEAM, C,
  and JVM behaviours identical.
- **Null at the language level**: see §1.7.
- **Inheritance**: Mochi has no class inheritance (only sealed-interface
  ADTs). The JVM `extends` mechanism is unused for user code. Internal
  helpers and JDK FFI are the only places `extends` appears.
- **Operator overloading**: Mochi does not let users overload `+` etc.
  Library code can define methods, but the operator syntax is reserved
  for built-in numeric and string types.
- **Macros / compile-time reflection**: deferred to a future MEP. The JVM
  has `javax.lang.model` annotation processors but we are not exposing
  them at the language level in v0.10 or v0.11.

## 10. Surface-to-JVM cheat sheet (cross-reference)

| Mochi form | JVM lowering | Note |
|------------|-------------|------|
| `let x = ...` | `final var x = ...` or `final long x = ...` | §1.1, note 05 §4 |
| `var x = ...` | `var x = ...` | §1.1 |
| `int` | `long` | §1.2, note 06 §5 |
| `float` | `double` | §1.2 |
| `string` | `java.lang.String` | §1.4 |
| `list<T>` | `java.util.List<T>` | §3, note 06 §10 |
| `map<K,V>` | `LinkedHashMap<K,V>` | §3 |
| `set<T>` | `LinkedHashSet<T>` | §3 |
| `record T { ... }` | Java `record` | §2.3 |
| `type T = A \| B` (sum) | sealed interface + records | §4, JEP 409 + JEP 395 |
| `match` | switch expression with patterns | §4, JEP 440 + JEP 441 |
| `fun(...) => ...` | Java lambda | §2.2, JEP 8003895 (LambdaMetafactory) |
| `from ... select ...` | Stream + Collectors | §5, note 08 |
| `agent ...` | Loom virtual thread + mailbox | §6, JEP 444, note 09 |
| `stream<T>` | `Flow.Publisher<T>` | §6, JSR 266 |
| `fact / rule / query` | runtime Datalog engine | §7, note 08 §4 |
| `ai(...)` | runtime `AI.call` | §8, note 04 §10 |
| `ffi(...)` | runtime `FFI.call` or direct JNI | §8, note 11 |

## 11. Open questions for note 02 (design philosophy)

- **Codegen IR**: Java source vs ClassFile API vs ASM vs ByteBuddy. (Resolved
  in note 02 §3 and note 05 §1: hybrid Java source + ClassFile API direct
  emission for hot lowerings.)
- **Boxing strategy**: when to specialise `List<Long>` to `long[]`-backed
  primitive list. (Note 06 §10.)
- **Persistent collections**: own implementation vs Vavr/Eclipse. (Note 02
  §7 picks own; note 06 §11 details.)
- **JDK floor**: 21 LTS confirmed (note 02 §2). JDK 25 LTS is the
  preferred target for native-image (note 02 §11).

## 12. Cross-references

- [[02-design-philosophy]] (next note): why each of the choices above
  was made.
- [[05-codegen-design]]: the IR layer that turns this surface into
  emitted Java / bytecode.
- [[06-type-lowering]]: the per-type details glossed here.
- [[08-dataset-pipeline]]: the query DSL lowering in full.
- [[09-agent-streams]]: agent and stream lowering on Loom.
- [[../0046/01-language-surface]]: the BEAM-target analogue of this note.
- [[../0045/01-language-surface]]: the C-target analogue.
