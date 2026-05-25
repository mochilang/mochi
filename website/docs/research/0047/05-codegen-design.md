# MEP-47 research note 05, Codegen design: choosing an IR layer for Mochi → JVM

Author: research pass for MEP-47 (Mochi → JVM transpiler).
Date: 2026-05-23 01:00 (GMT+7).
Target runtime: OpenJDK 21 LTS minimum, OpenJDK 25 LTS supported,
with forward compatibility to JDK 29 LTS (September 2027).

## 1. Why the choice of IR matters

Unlike BEAM (see [[../0046/05-codegen-design]]) or C (see
[[../0045/05-codegen-design]]), the JVM does not expose a finite,
documented ladder of intermediate languages. It exposes a marketplace
of third-party libraries plus one official stdlib API that finalised
in JDK 24. The front doors look superficially similar (each produces
a `ClassFile` structure conforming to JVMS chapter 4) but they sit at
very different abstraction levels and ship with very different
stability, tooling, and AOT promises.

The choice is load-bearing for a transpiler in six ways.

1. **Optimisations preserved or lost.** Every pass below the chosen
   layer runs for free; every pass above it we reproduce. Java source
   gives us all of `javac`'s lowering: closures via
   `LambdaMetafactory`, string concat via `StringConcatFactory`,
   pattern-switch desugaring, lambda capture promotion, exception
   tables, stack maps. Raw bytecode hands us all of that as work.

2. **Tooling attachment points.** JDWP, JFR, JMC, async-profiler,
   IntelliJ, VisualVM, `jstack`, `jcmd` all attach at specific
   abstraction layers. Bytecode emission with no `SourceFile`,
   `LineNumberTable`, or `LocalVariableTable` leaves the debugger
   with nothing to bind to, even when the class is valid.

3. **AOT interaction.** GraalVM `native-image` and OpenJDK Leyden
   make a closed-world assumption that conflicts with reflection,
   dynamic class loading, and arbitrary `invokedynamic` bootstrap
   methods. How Mochi closures lower (to a `Function<T,R>` instance,
   or to an `invokedynamic` site via `LambdaMetafactory`) determines
   whether the output runs under `native-image` without
   reachability-metadata files.

4. **Reflection / proxy implications.** A bytecode-synthesised class
   with no `INNERCLASSES` attribute confuses `getDeclaringClass`; a
   source emit handles this for us. Dynamic proxies generated at run
   time confuse the closed-world analysis.

5. **Class-file major-version churn.** JVMS major versions advance
   one per JDK release (65 = JDK 21, 66 = 22, 67 = 23, 68 = 24, 69
   = 25, 70 = 26). Libraries that hard-code a max version (the ASM
   family's perennial pain) silently break each release.

6. **Build pipeline cost.** Emit-source plus `javac` startup is
   1-3 s wall-clock cold; emit-bytecode-directly skips that.
   Mochi's incremental loop ([[10-build-system]]) targets p50 under
   500 ms per affected module, so we keep a long-running compiler
   instance in-process.

This document surveys the full ten-candidate ladder and lands on a
recommendation.

## 2. The candidate IR layers

Ten plausible front doors, ordered highest abstraction first:

| # | Candidate | Layer | Public entry point |
|---|-----------|-------|--------------------|
| 1 | Java source text (`.java`) | source | `javac`, `JavaCompiler` (JSR 199) |
| 2 | Kotlin source text (`.kt`) | source | `kotlinc` |
| 3 | Scala source text (`.scala`) | source | `scalac` / Dotty |
| 4 | JavaPoet AST | source builder | Palantir JavaPoet 0.9.0 |
| 5 | ASM bytecode | bytecode | OW2 ASM 9.8 / 9.9 |
| 6 | ByteBuddy | bytecode (on ASM) | net.bytebuddy 1.18.x |
| 7 | Janino | in-memory javac | janino-compiler 3.1.x |
| 8 | ClassFile API | bytecode (stdlib) | `java.lang.classfile`, JEP 484 |
| 9 | Eclipse JDT compiler (ECJ) | source compile | `org.eclipse.jdt.core` |
|10 | Custom IR (aotir-style) → ClassFile | custom | Mochi-only |

## 3. Layer-by-layer analysis

### 3.1 Java source text

What Clojure's `gen-class`, Eclipse Xtend, and most annotation
processors do: pretty-print the program as Java source, then invoke
`javax.tools.JavaCompiler` (JSR 199, since Java 6) or shell out to
`javac`. Reference: JLS 25, JVMS 25.

**Stability.** The most stable choice on the platform. The 24-month
LTS cadence (JDK 21, 25, 29) plus the deprecation policy (a feature
stays in the language at least one full LTS cycle after deprecation)
means a transpiler targeting JDK 21 source today still compiles
against JDK 29 in 2027 without edits, given we stay on finalised
features.

**Feature coverage at JDK 21.**

- Pattern matching for `switch` (JEP 441, final in 21).
- Record patterns (JEP 440, final in 21).
- Sealed classes (JEP 409, final in 17).
- Records (JEP 395, final in 16).
- Virtual threads (JEP 444, final in 21).
- Unnamed variables (JEP 456, final in 22, avoided at the JDK 21
  floor; lowered to `_unused$N`).

**Optimisations preserved.** All of `javac`'s lowering plus the
HotSpot C2 / Graal JIT at runtime. Lambdas lower to
`LambdaMetafactory.metafactory` indy; string concat to
`StringConcatFactory.makeConcatWithConstants`; pattern switch to
`SwitchBootstraps.typeSwitch`. All four of these bootstraps are on
the GraalVM allow-list per the *Native Image Compatibility Guide*.

**Tooling.** Full. `SourceFile`, `LineNumberTable`,
`LocalVariableTable`, `StackMapTable` get written by `javac`. The
debugger, JFR, async-profiler, IntelliJ all attach normally. SMAP
(JSR-45, §7) maps the synthesised Java back to Mochi source.

**Downsides for Mochi.**

- The pretty-print → re-parse round-trip costs ~30-40 percent of a
  cold compile; we mitigate by holding a JSR-199 `JavaCompiler`
  instance in-process across incremental builds.
- Mochi identifiers (e.g. `for'`, `foo::bar`) require mangling to
  legal Java identifiers; see [[06-type-lowering]] §4.
- Synthesised Java line numbers; without SMAP, debugger breakpoints
  land on the wrong file.
- We can only emit indy via the four `javac`-supported bootstraps
  (lambda, string concat, pattern switch, enum switch). Custom
  bootstraps require dropping to bytecode.

**Precedent.** Kotlin's KAPT/KSP, Lombok, Google auto-value, Dagger
2, Micronaut, Quarkus's Arc, Spring AOT all emit Java source.

### 3.2 Kotlin source text

Emit Kotlin, run `kotlinc`. Kotlin's surface expresses every Mochi
feature (sealed classes, data classes, `when`-as-pattern-switch,
coroutines-as-async).

**Stability.** Lower than Java source. Each Kotlin major release
(1.9, 2.0, 2.1, 2.2, 2.3.20) shifts the IR or bytecode shape. K2
(default since 2.0) reworked generics erasure and inline-function
capture. The Kotlin 2.3.20 release delegates JS to SWC, a sign of
backend churn.

**Downsides.** `kotlinc` startup is 1-2x slower than `javac`. Kotlin
stdlib (~1.5 MB) becomes a runtime dependency. Mochi → Kotlin → JVM
debugging needs two SMAP hops, which IntelliJ does not reliably
follow. Coroutine machinery uses heavy `invokedynamic` and
`MethodHandle`, hostile to native-image without metadata.

**Verdict.** Not a candidate. Too many moving parts for benefits we
can replicate in Java 21 (sealed + pattern switch + virtual threads).

### 3.3 Scala source text

Briefly considered, quickly rejected:

- `scalac` / Dotty cold-startup 3-5 s.
- `given` / implicit conversions introduce silent behaviour with no
  Mochi analogue.
- Official backends are JVM and JS only.
- `scala-library` runtime footprint (~5 MB) exceeds Mochi's whole
  target runtime ([[04-runtime]]).

**Verdict.** Pass.

### 3.4 JavaPoet AST

Java library for building `.java` source via a fluent typed API.
Square open-sourced it in 2015 and archived the `com.squareup:javapoet`
repo on 2024-10-10. The active fork is Palantir's `com.palantir.javapoet`,
current 0.9.0 (2025-11-27), with record and sealed support. The
sibling KotlinPoet is JetBrains-friendly.

JavaPoet does not replace `javac`; it replaces the
"string-template a Java file" step. The win is structural
correctness: method signatures, generic bounds, modifiers, imports
are all constructed via builders, so the output is syntactically
guaranteed to compile.

**Stability.** Palantir actively tracks JDK releases (21, 22, 23,
24, 25 supported within months of GA). The `com.squareup` →
`com.palantir` namespace rename is a one-time migration.

**Feature coverage.** Records (0.5.0+), sealed interfaces with
`permits` (0.6.0+), generics with bounds, annotations, lambdas via
`CodeBlock`. Pattern switch and record patterns are emitted via raw
`CodeBlock` text since JavaPoet does not type-check method bodies.

**Optimisations preserved.** Identical to §3.1 (we still run javac
on the output).

**Tooling.** Identical to §3.1.

**AOT.** Identical to §3.1.

**Downsides.** Method bodies fall back to text (`CodeBlock` is a
templated string), so structural correctness lives in the class
shell but not the inner control flow. Same two-step build tax as
plain source.

**Sweet spot.** Mochi's class scaffolding (records, sealed
interfaces, method signatures) goes through JavaPoet builders;
method bodies are templated source.

### 3.5 ASM (OW2 ASM 9.8 / 9.9)

The workhorse bytecode library since 2002. Used by Jacoco, Spring
(early), Hibernate, Mockito, Lombok internals, and ByteBuddy
(which embeds ASM). Visitor-based API plus a tree API.

**Current versions.** ASM 9.7 (May 2024) supports up to JDK 23 (major
version 67). ASM 9.8 (March 2025) adds JDK 25 (major 69). ASM 9.9
(late 2025) adds JDK 26 (major 70). The ASM Gradle ecosystem hit
"Unsupported class file major version 69" repeatedly through 2025-26
on plugins still pinning 9.7.

**Optimisations preserved.** Nothing above bytecode. We own
closures, generics erasure, capture-as-synthetic-fields,
`LambdaMetafactory` indy bootstrap arguments, constant pool, stack
maps, exception tables.

**JDK 21+ feature support.** Sealed via `PermittedSubclasses`
attribute (9.5+), records via `Record` attribute (9.0+). Pattern
switch lowered by hand to `tableswitch`/`lookupswitch` + nested
type checks. Lambdas via `invokedynamic` with `Handle` and
`ConstantDynamic`.

**Tooling.** We own every debug attribute by hand (`visitLineNumber`,
`visitLocalVariable`).

**AOT.** Bytecode is bytecode, but if we emit `invokedynamic` with
custom bootstrap methods (other than the four `javac`-standard
ones), `native-image` requires reachability metadata.

**Downsides.** Per-JDK upgrade pressure (the library version is
locked to bytecode major-version awareness). `COMPUTE_FRAMES` is
slow and not always correct on unusual control flow. Verbose: a
`System.out.println("hi")` is ~12 ASM instructions.

### 3.6 ByteBuddy

Sits on top of ASM with a fluent higher-level API. Current 1.18.x;
1.17.0 was the first to support JDK 25 (1.15.x stops at JDK 24).
Indy and pattern-switch dispatch drop to raw `MethodVisitor`
callbacks underneath.

**Strengths over raw ASM.** Far more ergonomic and readable; better
test fixtures; less line count for the same class shape.

**Downsides.** Indirection over ASM (the per-JDK upgrade pressure
does not disappear, it is just absorbed by ByteBuddy's release
cadence). ~3.5 MB build-time dependency. API churn at minor-version
boundaries.

**AOT.** ByteBuddy's *runtime* class generation use case is hostile
to native-image. Build-time use is fine.

### 3.7 Janino

In-process Java compiler optimised for runtime code generation
(Spark Catalyst, Flink, Calcite, Drools, Logback). Janino 3.1.x as
of late 2025 supports Java language features up to about JDK 11
reliably, partial records and sealed. JDK 21+ features (pattern
switch, record patterns, virtual threads) are unsupported.

**Verdict.** Right tool, wrong job. Janino targets runtime; Mochi
compiles ahead of time; using Janino would mean abandoning every
JDK 21+ feature we want. Pass.

### 3.8 ClassFile API (`java.lang.classfile`, JEP 484)

The official OpenJDK stdlib bytecode API. Timeline:

- JDK 22 (March 2024): JEP 457, first preview.
- JDK 23 (September 2024): JEP 466, second preview; `CodeBuilder`
  refined, attribute mappers became static methods.
- JDK 24 (March 2025): JEP 484, finalised (no longer preview).
- JDK 25 (September 2025): standard; JDK internals migrated off ASM.

**Stability.** Maximum among bytecode options. Part of `java.base`,
versioned with the JDK, designed to handle per-release class-file
format churn: each JDK's `ClassFile.of()` knows every major version
up to its own and emits older majors when asked. No ASM-style
9.7-vs-9.8-vs-9.9 negotiation.

**API shape.** Sealed interfaces and pattern matching internally,
since this is a 2024-era API:

```java
byte[] bytes = ClassFile.of().build(thisClass, cb -> cb
    .withFlags(ClassFile.ACC_PUBLIC | ClassFile.ACC_FINAL)
    .withSuperclass(CD_Object)
    .withMethod("hello", desc, ACC_PUBLIC | ACC_STATIC, mb -> mb
        .withCode(code -> code
            .getstatic(SYSTEM_OUT)
            .ldc("hi")
            .invokevirtual(PRINTLN)
            .return_())));
```

Three tiers of factories: low (1:1 JVM instructions), mid
(`aconst_null`, `loadConstant`), high (`block`, `ifThenElse`).

**Optimisations preserved.** Same as ASM (nothing above bytecode).
Net advantage: less version management.

**JDK feature support.** Records and sealed types as first-class
attributes. Lambdas via `invokedynamic` + `Handle`. Pattern switch
and record patterns are user-built (we lower them).

**Tooling.** Same level as ASM, but with better stdlib helpers for
`LineNumberTableAttribute`, `LocalVariableTableAttribute`,
`SourceFileAttribute`, `SourceDebugExtensionAttribute` (JSR-45).

**AOT.** Same caveats as ASM around custom indy bootstraps.

**Caveat.** Build tool must run on JDK 22+ to call the API. Output
target bytecode can be any major version. Mochi pins its build tool
to JDK 22 minimum, recommended JDK 25; output targets JDK 21
(default) or JDK 25 (via flag).

### 3.9 Eclipse JDT compiler (ECJ)

Standalone Java compiler from the Eclipse Foundation, available as
`ecj.jar`. Tracks JDK 25 within months of GA. Implements the same
JLS as `javac` with edge-case differences in type inference.

**Why we might choose.** Incremental compilation; runs without a
full JDK (the jar carries everything for compiling against a
classpath). Useful as a fallback in contexts where users do not
have a full JDK.

**Downsides.** Not the reference compiler; subtle JLS differences;
same source-emit tax as `javac`; bigger jar to ship.

**Verdict.** Not primary. Useful as an embedded fallback if a
target environment lacks `javac`; we do not need it for MEP-47's
audience.

### 3.10 Custom IR (aotir-style) → ClassFile API

MEP-45 built a custom `aotir` IR for Mochi → C, with monomorphisation
and closure conversion. We could analogously build Mochi → aotir →
ClassFile API, sharing pass 1 with MEP-45.

**Strengths.** Shared lowering with the C target; no source-emit
tax; direct control over indy bootstraps.

**Weaknesses.** Same as §3.5/§3.8 (we own debug attributes, stack
maps, exception tables). We also lose `javac` as a second-opinion
typechecker; Mochi's own type checker becomes load-bearing for
bytecode well-formedness.

This is the same trade-off as bytecode vs source, but without
`javac` as a safety net.

## 4. Recommendation

**Mochi should adopt a hybrid two-tier strategy:**

- **Primary path: Java source via JavaPoet (Palantir 0.9.0+) plus
  in-process `javac` (JSR 199 `JavaCompiler`).** Most Mochi code
  goes through this path. We get `javac`'s lowering for free
  (lambdas, string concat, pattern switch, exception tables, debug
  attributes, stack maps, AOT-friendly indy). JavaPoet gives
  structural correctness on the class scaffolding.
- **Secondary path: ClassFile API (JEP 484, stdlib in JDK 24+) for
  hot-path emit.** Specific cases needing custom constant-pool
  entries, custom indy bootstraps, or hand-tuned typeswitch tables
  go through ClassFile API. These produce `.class` directly,
  side-by-side with the javac output.

### 4.1 Why hybrid

Pure Java-source hits a ceiling at exactly two places:

1. **Mutable closure capture.** Mochi closures can capture refs
   (mutable cells); Java lambdas capture effectively-final. We
   model the mutable case via a wrapper class, all in source. *No
   bytecode descent required.*
2. **Sum-type dispatch hot path.** Mochi `match` on a sealed
   interface compiles to Java 21 pattern switch in 95 percent of
   cases. The remaining 5 percent (very wide enums, deeply nested
   patterns) benefit from a hand-tuned `SwitchBootstraps.typeSwitch`
   site with custom label ordering. Java source expresses the
   common case; ClassFile API takes the rare hot-path case.

### 4.2 Why not pure ClassFile API

The "correct" answer in 2030 once the ecosystem has migrated. In
2026 it has two problems: a smaller tutorial corpus, and no `javac`
typechecker as a belt-and-braces against codegen bugs. We design
for ClassFile API to grow into the primary path as the ecosystem
matures; we ship the hybrid first.

### 4.3 Why not ASM / ByteBuddy

Both are mature, both lock to bytecode major-version awareness, and
both are being slowly migrated *away from* by Oracle (the JDK is
moving off ASM internally; that motivation drove JEP 457/466/484).
ASM 9.9 is excellent; we just see no reason to depend on it when
the stdlib ClassFile API does the same job better starting in JDK 24.

## 5. Pass pipeline

Five passes; passes 1-2 are shared with MEP-45 (C target).

### Pass 1: monomorphisation (reused from MEP-45 aotir)

Generic functions instantiate at every call site;
`list<int>::map` and `list<string>::map` become two methods. The
output is a typed aotir tree with no remaining type variables. See
[[06-type-lowering]] §3 for the mangling table.

JVM-specific tweak: where Java erasure suffices (`list<T>` of
reference type with homogeneous operations), keep a single erased
`ArrayList<Object>`-shaped method and let JIT inlining specialise
it. The pass emits `#[mono=erase]` vs `#[mono=specialise]`. Default
policy: specialise on primitive type parameters; erase on reference
type parameters.

### Pass 2: closure conversion

Capture analysis identifies free variables; each becomes a
synthetic field on a generated implementation class.

- **Source path (default):** emit a Java lambda; `javac` lowers it
  to `LambdaMetafactory.metafactory` indy. Capture-by-reference
  promotion is `javac`'s problem.
- **Bytecode path (rare):** for closures needing a specific
  captured-field layout (e.g. agent stream cells the scheduler
  reaches into via unsafe access), emit a synthetic class via
  ClassFile API with explicit field descriptors.

### Pass 3: name mangling

`foo::bar` → package `mochi.user.foo`, class `bar`, or with the
monomorphisation suffix `bar$__inst<hash6>` where `hash6` is the
first 6 hex digits of BLAKE3 over the canonical print of
instantiation arguments. JVM-specific rules:

- Identifiers containing JVMS-illegal characters in unqualified
  names (`. ; [ /`) escape via `__$dot__`, `__$semi__`, etc.
- Mochi package paths map to Java packages with `_` separators.
- Generated synthetic classes (closure bodies, sum-type impls) take
  a `$$` prefix following Java's own convention.
- Two emitted JVM identifiers never collide across packages or
  generic instantiations.

### Pass 4: emit

Two sub-passes:

- **4a:** Java source via JavaPoet. Most class bodies. Writes
  `.java` files to a build temp directory.
- **4b:** Hot-path bytecode via ClassFile API. Specific classes
  (sum-type dispatch shims, agent runtime entry points, FFI thunks)
  bypass source.

Rule: any class emitted via 4b must have a JavaPoet-emitted
signature file (`.javasig`) so dependent modules typecheck through
the same Java toolchain.

### Pass 4.5: in-process `javac`

A long-lived `JavaCompiler` instance (JSR 199, via
`ToolProvider.getSystemJavaCompiler()`) compiles all 4a outputs in
one round, default `-source 21 -target 21`, overridable to 25 via
build flag. The instance is held across incremental rebuilds so
startup cost is paid once per session.

### Pass 5: postprocess

After 4 and 4.5 we have a tree of `.class` files. Packaging the jar
is deferred to [[10-build-system]]. The one operation we do here is
SMAP injection: an ASM- or ClassFile-API-based pass walks every
class and writes a `SourceDebugExtension` attribute mapping Java
line numbers back to Mochi line numbers.

## 6. Lowering details that depend on IR choice

### 6.1 Closures

Source path:

```java
// Mochi: let add = fn(x: int) -> fn(int) -> int { ... }
Function<Integer, Function<Integer, Integer>> add =
    x -> y -> x + y;
```

`javac` lowers each lambda to an `invokedynamic` site with
`LambdaMetafactory.metafactory` as the bootstrap method. Captured
`x` becomes a constructor argument on the synthetic class. Standard,
well-tested path.

Bytecode path (rare, ClassFile API):

```
invokedynamic apply:(I)Ljava/util/function/Function;
  BootstrapMethod #0
    LambdaMetafactory.metafactory(
      MethodHandles$Lookup, String, MethodType,
      MethodType, MethodHandle, MethodType) CallSite
    static_args: ["apply",
                  (Object)Object,
                  Foo.lambda$0$(I)Ljava/lang/Object,
                  (Integer)Integer]
```

This is the bytecode shape `javac` would emit anyway. We descend to
bytecode only for closures that need a custom captured-state layout.

### 6.2 Sum types

Mochi `enum Shape { Circle(r: float), Square(side: float) }` → the
canonical JDK 21 idiom:

```java
sealed interface Shape permits Shape.Circle, Shape.Square {
    record Circle(float r) implements Shape {}
    record Square(float side) implements Shape {}
}
```

Sealed types final since JDK 17 (JEP 409), records final since JDK
16 (JEP 395). JavaPoet 0.6.0+ has `TypeSpec.sealedInterfaceBuilder`.

### 6.3 Match expressions

Mochi:

```
match shape {
    Circle(r) -> pi * r * r,
    Square(s) -> s * s,
}
```

→ JDK 21 pattern switch with record patterns (JEP 440 + JEP 441):

```java
return switch (shape) {
    case Circle(float r) -> PI * r * r;
    case Square(float s) -> s * s;
};
```

Because `Shape` is sealed and the arms exhaust it, `javac` omits
the synthetic default and verifies exhaustiveness. Below the source
layer, `javac` lowers this to `SwitchBootstraps.typeSwitch`, on the
GraalVM allow-list.

For the rare hot path (a match with many arms over a wide sealed
hierarchy), we drop to ClassFile API and emit a hand-tuned
typeswitch with custom label order.

### 6.4 Generic lists, maps, sets

- Mochi `list<T>` → erased `ArrayList<T>`. JVM generics are
  erased; this is compatible with Mochi after monomorphisation
  (pass 1 emits a separate method per primitive specialisation;
  for reference types, erase).
- Mochi `map<K,V>` → `LinkedHashMap<K,V>` (preserves insertion
  order, matches Mochi map semantics).
- Mochi `set<T>` → `LinkedHashSet<T>`.

### 6.5 Tail calls

JVM has no TCO. Three options:

- **Trampolining for self-recursive tails.** Detect a direct
  self-recursive tail call; rewrite the function body as
  `while (true) { ... continue; }` with arguments reassigned to
  local slots and the call replaced by `continue`. Same lowering
  Scala and Kotlin use. Turns O(n) stack frames into O(1).
- **Reject mutual TCO.** Mutual tail calls become ordinary calls.
  HotSpot's default 512 KB stack with ~64 B frames gives ~8000
  levels before overflow; sufficient for non-pathological code.
  Detecting and trampolining a mutually-recursive function group
  requires lifting all members into a single dispatch loop with a
  tag, which obfuscates stack traces and confuses debuggers.
- **Loom virtual-thread `Thread.yield`.** Not relevant to TCO;
  virtual threads reduce the cost of parking, not stack depth.

**Recommendation:** trampoline self-recursive only. Mochi docs
([[01-language-surface]] §7) call out that mutual recursion is not
stack-optimised on the JVM target; the C target [[../0045]]
optimises it under clang.

### 6.6 Result, Option, errors

**Recommended:** `Result<T, E>` as a sealed interface.

```java
sealed interface Result<T, E>
    permits Result.Ok, Result.Err {
    record Ok<T, E>(T value) implements Result<T, E> {}
    record Err<T, E>(E error) implements Result<T, E> {}
}
```

Pattern-matched in code; no exceptions for ordinary errors.
Matches Rust, Swift, modern Kotlin idiom. Java exceptions are
reserved for genuinely exceptional cases (`assertion failed`,
`division by zero` where Mochi semantics dictate panic).

`Option<T>` lowers to `Optional<T>` for source-level Java interop,
with an internal `Some`/`None` sealed interface used only when
Optional's `null`-bridge conflicts with Mochi semantics (see
[[06-type-lowering]] §6).

### 6.7 Async / await

JDK 21 finalised virtual threads (JEP 444). Recommendation:
**virtual-thread-blocking style**, not `CompletableFuture` chaining.

- `await(f: Future<T>): T` lowers to `f.get()`. The calling thread
  is a virtual thread (the Mochi runtime uses
  `Executors.newVirtualThreadPerTaskExecutor()`), so the JVM
  unmounts and remounts without blocking the carrier.
- `async fn foo() -> T` lowers to a method with a synchronous
  signature returning `T` that may block. Callers invoke on a
  virtual thread via the runtime. The Mochi type system tracks
  "this might suspend" but the JVM signature is plain.
- For interop with Java APIs returning `CompletableFuture`, a
  `Mochi.await(CompletableFuture<T>): T` helper calls `.get()`
  inside a virtual thread; JDK 21 made `CompletableFuture.get()`
  virtual-thread-friendly per *Embracing Virtual Threads*.

This trades the coloured-functions complexity of
`CompletableFuture` chains for the structured-concurrency simplicity
of synchronous-looking code on virtual threads.

## 7. Cross-version stability

| Layer | JDK 21 → 25 | JDK 25 → 29 (Sept 2027) |
|-------|-------------|-------------------------|
| Java source | very stable; additive | stable; additive |
| Kotlin source | breaks per minor (2.0 K2, 2.3.20 SWC) | unknown, JetBrains-driven |
| JavaPoet (Palantir) | tracks JDK within months | expected to track |
| ASM | bumps per JDK GA (9.7 → 9.8 → 9.9) | will bump 9.10+ for 29 |
| ByteBuddy | bumps per JDK GA | bumps per JDK GA |
| ClassFile API | preview in 22, final in 24, stable in 25 | stable; additive |
| Janino | lags JDK 5-10 versions | continues to lag |
| ECJ | tracks JDK within months | tracks JDK within months |

Java source is the most stable; ClassFile API is the most stable
bytecode layer. ASM and Janino carry the most cross-JDK drift risk.
The hybrid (source primary, ClassFile API secondary) puts the bulk
of the surface area on the two most stable layers.

## 8. Tooling and debugging

### 8.1 Debug attributes

JDWP/JDI consume these class-file attributes:

- `SourceFile` (one per class).
- `LineNumberTable` (per method): bytecode-offset → source-line.
- `LocalVariableTable` and `LocalVariableTypeTable` (per method,
  optional): bytecode-range + slot → name + descriptor.
- `SourceDebugExtension` (one per class, JSR-45).

Source path (Java → javac): all set automatically with `-g:all`
(default). Bytecode path (ClassFile API): set by hand via
`SourceFileAttribute`, `LineNumberTableAttribute`,
`LocalVariableTableAttribute`. ClassFile API's `CodeBuilder.lineNumber(N)`
threads the entries.

### 8.2 javac `--enable-preview`

JDK 21 has no preview-only features Mochi needs; everything we want
is final (pattern switch, record patterns, sealed, virtual threads).
JDK 25 has preview features (scoped values evolving, structured
concurrency still preview) Mochi does not depend on.

**Recommendation:** never compile with `--enable-preview`. Target
finalised JLS features only. Avoids the "preview class files cannot
run on a different JDK" restriction.

### 8.3 JSR-45 SMAP

JSR-45 (Jakarta Debugging Support for Other Languages 2.0) defines
the `SourceDebugExtension` class-file attribute carrying an SMAP
string, mapping generated-language lines (Java) back to source-
language lines (Mochi). Format (simplified):

```
SMAP
Foo.mochi
Mochi
*S Mochi
*F
+ 1 Foo.mochi
Foo.mochi
*L
1#1,3:1
*E
```

Read by IntelliJ, Eclipse, NetBeans, `jdb`, async-profiler. The
post-process pass §5 writes this attribute on every emitted class.
Cap at 64 KB per class; we partition long modules to stay under.

### 8.4 JFR, async-profiler, JMC

These tools see whatever method names we emit. Mangled names
(`mochi.user.foo$bar`) appear in JFR events and profiler flame
graphs. We ship an IntelliJ / VS Code plugin that demangles in the
UI; same situation as the C target's `c++filt`.

## 9. Reflection and native-image

GraalVM `native-image` does closed-world analysis. The rules:

- **Reflection on emitted classes:** `Class.forName(name)` is
  invisible to the static analysis; we emit a
  `reachability-metadata.json` listing every Mochi-emitted class
  the user might reflect on. Build-time generated, alongside the
  jar.
- **Dynamic proxies:** Mochi does not generate proxies in the
  user-facing path. FFI shims for Java interfaces ([[04-runtime]])
  do; the proxy spec goes into reachability metadata.
- **invokedynamic bootstraps:** `LambdaMetafactory.metafactory`,
  `StringConcatFactory.makeConcatWithConstants`,
  `SwitchBootstraps.typeSwitch`, and `SwitchBootstraps.enumSwitch`
  are all on the GraalVM allow-list (built-in support). Any other
  bootstrap requires metadata.
- **MethodHandle:** invocation supported but receivers must be
  visible to static analysis. Mochi-emitted `MethodHandle` use is
  restricted to bootstrap arguments for `invokedynamic`.

**Guidance.** The source-emit primary path (§4) is most
native-image-friendly because `javac` only emits the four
allow-listed indy bootstraps. The bytecode-emit secondary path must
avoid exotic custom bootstraps; if a Mochi feature genuinely
requires one, we document the metadata as part of that feature's
lowering rules.

**Leyden.** OpenJDK Leyden (the broader AOT effort, distinct from
GraalVM) is on the same trajectory: tolerate a static set of indy
bootstraps, require opt-in metadata for the rest. Same
recommendation.

## 10. Decision matrix

Each candidate scored 0-5 on six axes. Higher is better. Asterisks
mark the hybrid recommendation.

| # | Candidate          | Stability | Control | Maturity | Tooling | AOT | Velocity | Total |
|---|--------------------|-----------|---------|----------|---------|-----|----------|-------|
| 1 | Java source        |     5     |    3    |     5    |    5    |  5  |     5    |  28   |
| 2 | Kotlin source      |     3     |    3    |     4    |    3    |  3  |     4    |  20   |
| 3 | Scala source       |     3     |    3    |     4    |    3    |  2  |     2    |  17   |
| 4 | JavaPoet *         |     5     |    4    |     4    |    5    |  5  |     5    |  28*  |
| 5 | ASM                |     3     |    5    |     5    |    3    |  4  |     3    |  23   |
| 6 | ByteBuddy          |     3     |    5    |     5    |    3    |  3  |     4    |  23   |
| 7 | Janino             |     2     |    3    |     3    |    2    |  2  |     3    |  15   |
| 8 | ClassFile API *    |     5     |    5    |     4    |    4    |  4  |     4    |  26*  |
| 9 | ECJ                |     4     |    3    |     4    |    4    |  4  |     3    |  22   |
|10 | Custom IR → CF API |     5     |    5    |     2    |    3    |  4  |     2    |  21   |

The hybrid pair (JavaPoet 28 for the shell, ClassFile API 26 for
the hot path) tops single-tier rankings. Notes:

- **Stability.** Java source is most stable (10+ year guarantees);
  Kotlin and Scala less so; ASM lags JDK by 6 months per release.
- **Control.** Bytecode-level options get 5; JavaPoet 4 because
  method bodies are text; pure source 3.
- **Maturity.** ASM and javac most mature; Janino, custom IR,
  ClassFile API newer.
- **Tooling.** Java source via javac gets all attributes for free.
- **AOT.** Best with javac/JavaPoet/ClassFile API outputs using
  stdlib bootstraps only.
- **Velocity.** Source with a typed builder (JavaPoet) is fastest
  to iterate; raw bytecode slowest.

## 11. Final recommendation

**Mochi targets the JVM via a hybrid of Java source (built with
Palantir JavaPoet 0.9.0+ and compiled in-process by JSR 199 `javac`)
for the bulk of emit, with `java.lang.classfile` (JEP 484, GA in
JDK 24, stdlib in JDK 25) for the small minority of cases that need
custom constant-pool entries, unusual invokedynamic bootstraps, or
hand-tuned typeswitch tables. We do not depend on ASM, ByteBuddy,
Janino, Kotlin, Scala, or ECJ. We pin our build tool to JDK 22+
(recommended JDK 25) so the ClassFile API is available; we pin
output bytecode to JDK 21 (LTS floor) or JDK 25 (LTS ceiling) per
project flag. Closures lower to Java lambdas; sum types to sealed
records; matches to pattern switch; tail self-recursion to a
while-loop trampoline; errors to sealed `Result<T, E>`; async to
virtual-thread-blocking `Future.get()`. We post-process every
emitted class with a JSR-45 SMAP attribute so the debugger and
profiler land on Mochi source, not the intermediate Java. This
choice maximises stability across the JDK 21 → 25 → 29 LTS line,
preserves all of `javac`'s lowering and HotSpot's optimisation,
stays inside the GraalVM native-image happy path, and keeps the
codegen surface small enough that a single contributor can read
every emitter in a sitting.**

**Backstop.** If a future JDK release breaks the Java source path
in a way Mochi cannot route around, fall back to pure ClassFile API
emit. The pass pipeline §5 already partitions emit between source
and bytecode; pushing more classes to the bytecode side is a code
change in pass 4 only. Conversely, if ClassFile API evolves in a
direction we do not like, the secondary path collapses into more
Java source. The hybrid carries both fallback directions from day
one.

## Sources

- *Java Language Specification, JDK 25*:
  https://docs.oracle.com/javase/specs/jls/se25/html/index.html
- *Java Virtual Machine Specification, JDK 25*:
  https://docs.oracle.com/javase/specs/jvms/se25/html/index.html
- JEP 441, Pattern Matching for switch:
  https://openjdk.org/jeps/441
- JEP 440, Record Patterns:
  https://openjdk.org/jeps/440
- JEP 409, Sealed Classes:
  https://openjdk.org/jeps/409
- JEP 395, Records:
  https://openjdk.org/jeps/395
- JEP 444, Virtual Threads:
  https://openjdk.org/jeps/444
- JEP 457, Class-File API (Preview):
  https://openjdk.org/jeps/457
- JEP 466, Class-File API (Second Preview):
  https://openjdk.org/jeps/466
- JEP 484, Class-File API (Final):
  https://openjdk.org/jeps/484
- JEP 456, Unnamed Variables and Patterns:
  https://openjdk.org/jeps/456
- `java.lang.classfile` API documentation (JDK 25):
  https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/lang/classfile/package-summary.html
- `LambdaMetafactory` (JDK 21):
  https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/invoke/LambdaMetafactory.html
- `SwitchBootstraps` (JDK 21 java.lang.runtime):
  https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/runtime/SwitchBootstraps.html
- Oracle Java SE Support Roadmap:
  https://www.oracle.com/java/technologies/java-se-support-roadmap.html
- Consolidated JDK 25 Release Notes:
  https://www.oracle.com/java/technologies/javase/25all-relnotes.html
- *Always up to date: the Class-File API*, INNOQ, April 2025:
  https://www.innoq.com/en/articles/2025/04/java-class-file-api/
- ASM versions, OW2:
  https://asm.ow2.io/versions.html
- ASM Maven artifact:
  https://mvnrepository.com/artifact/org.ow2.asm/asm
- ByteBuddy release notes (raphw/byte-buddy):
  https://github.com/raphw/byte-buddy/blob/master/release-notes.md
- Palantir JavaPoet (active fork):
  https://github.com/palantir/javapoet
- Square JavaPoet (archived 2024-10-10):
  https://github.com/square/javapoet
- Janino:
  https://janino-compiler.github.io/janino/
- Eclipse JDT Core / ECJ:
  https://eclipse.dev/eclipse/news/4.27/jdt.html
- *Native Image Compatibility Guide*, GraalVM:
  https://www.graalvm.org/latest/reference-manual/native-image/metadata/Compatibility/
- *Reflection in Native Image*, GraalVM JDK 21:
  https://www.graalvm.org/jdk21/reference-manual/native-image/dynamic-features/Reflection/
- *invokedynamic in GraalVM native image*, N. Dziubenko, Medium:
  https://medium.com/@nataliiadziubenko/invokedynamic-in-graalvm-native-image-how-is-it-possible-dd2fb6e58f4e
- *Embracing Virtual Threads*, Spring blog, October 2022:
  https://spring.io/blog/2022/10/11/embracing-virtual-threads/
- *Beyond Loom: weaving new concurrency patterns*, Red Hat:
  https://developers.redhat.com/articles/2023/10/03/beyond-loom-weaving-new-concurrency-patterns
- Jakarta Debugging Support for Other Languages 2.0 (JSR-45 successor):
  https://jakarta.ee/specifications/debugging/2.0/jdsol-spec-2.0
- Kotlin command-line compiler reference:
  https://kotlinlang.org/docs/command-line.html
- *What's new in Kotlin 2.3.20*:
  https://kotlinlang.org/docs/whatsnew2320.html
- Scala 3 / Dotty compiler:
  https://github.com/scala/scala3
- JSR 199, Java Compiler API:
  https://jcp.org/en/jsr/detail?id=199
