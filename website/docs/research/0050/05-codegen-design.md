# MEP-50 research note 05, Codegen design: choosing an IR layer for Mochi to Kotlin

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 11:08 (GMT+7).
Target toolchain: Kotlin 2.1 floor (K2 compiler, multidollar string interpolation, smart casts for sealed `when`, KMP source-set defaults), forward compatible to Kotlin 2.1.x point releases. All KMP targets the official Kotlin toolchain supports as Stable or Beta: JVM 17+, Android API 24+, iOS arm64 + simulator + Rosetta x64, macOS arm64 + x64, linuxArm64 + linuxX64, mingwX64, watchOS arm64, tvOS arm64, Kotlin/JS browser + nodejs, Kotlin/Wasm browser + nodejs (Alpha).

## 1. IR layer decision: KotlinPoet over raw string concatenation

The first decision for Mochi to Kotlin code generation is the layer at which we serialise the program tree. Two practical options exist on the Kotlin platform in 2026: emit Kotlin source text by string concatenation, or build a typed tree via the Square-maintained `KotlinPoet` package and serialise that tree. We choose the KotlinPoet route, with a small caveat (§7) about how the Go-side codegen pass actually links against it.

KotlinPoet lives at `https://github.com/square/kotlinpoet`. Current release: **1.18.1** (October 2024), Apache-2.0 licensed, maintained by Square. It is the de-facto standard for code generation in the Kotlin ecosystem (used by Dagger 2, Anvil, Moshi, Wire, Room, and the AndroidX KSP processors).

Three things tip the balance toward KotlinPoet.

1. **Indentation correctness.** Kotlin is brace-delimited and not indentation-sensitive at the parser level, but the official style (JetBrains' "Kotlin Coding Conventions" plus the `ktlint` rules) is rigid: 4-space indent, brace on the same line as the declaration, parameter lists wrapped past 100 columns. A string-concatenation emitter has to track an indent counter by hand; this is the most common source of cosmetic drift between machines or between toolchain versions. KotlinPoet pretty-prints deterministically from the tree shape.

2. **Syntax validity guarantee.** Every node in the KotlinPoet API carries typed children: constructing a `FunSpec` requires a return type, optional parameters as `ParameterSpec`s, and a body as a `CodeBlock`. Reserved-word handling is automatic via `escapeIfNecessary`. The worst that happens at runtime is a panic in our codegen, not malformed text reaching `kotlinc`.

3. **Type-safe imports.** KotlinPoet's `FileSpec.builder(packageName, fileName).addImport(...)` model knows about Kotlin imports (no duplicates, no shadowing, no unused imports), and `ClassName("kotlinx.coroutines", "Flow")` references compile down to the right import line at file write. A string emitter must track its own import set, and the first time a generic produces `kotlin.collections.List` versus `kotlinx.collections.immutable.PersistentList` is the first time the bug appears.

The single trade-off is the dependency footprint. `kotlinpoet` compiles to roughly 2 MB of products (jar + dependencies, transitively the `kotlin-reflect` library). That cold compile happens once per CI worker per KotlinPoet revision. This is why §7 prescribes a Go-native shadow tree rather than JNI-binding the Kotlin library.

## 2. No direct JVM bytecode emit (in the Kotlin pipeline)

JVM bytecode is a public, stable IR with a published specification (JVMS). MEP-47 (Mochi to JVM bytecode) goes there directly. MEP-50 (Kotlin) does **not**, even though the JVM target shares the same bottom of the stack.

The reason: MEP-50's primary value-add is the **Kotlin source artefact**. Users who want JVM bytecode without the Kotlin language indirection use MEP-47. Users who want JVM bytecode that also feeds Android, iOS (K/Native), Linux/macOS/Windows (K/Native), JS, and Wasm use MEP-50. The artefact is the source, not the bytecode.

Contrast with the other transpiler3 targets:

- **MEP-47 (JVM):** ClassFile API (JEP 484, GA in JDK 24) is an officially stable, in-stdlib bytecode emitter. JVM bytecode has decades of stability.
- **MEP-48 (CLR):** Roslyn `SyntaxFactory` for source emission; `System.Reflection.Emit` for IL fallback. Both are public APIs.
- **MEP-49 (Swift):** No equivalent stable IR below source. SIL is internal-only.
- **MEP-50 (Kotlin):** No bytecode emit even though stable bytecode exists, because the Kotlin source is the load-bearing artefact for cross-target reuse.

The lesson: MEP-50's stable input contract is the **Kotlin source language** as defined by the latest Kotlin Language Specification. Everything below that contract (`.kotlin_module`, IR backend, bytecode, LLVM IR for K/Native, JS IR, Wasm GC bytecode) is the responsibility of `kotlinc`.

## 3. Kotlin compilation pipeline

For context, the Kotlin compiler's internal pipeline (Kotlin 2.1, K2 frontend):

1. **Lexer + parser**: Kotlin source -> PSI tree (PsiBuilder).
2. **K2 FIR (Frontend IR)**: PSI -> FIR with resolved types, smart-cast information, and inferred generic arguments. Single tree, not multi-phase like the legacy K1.
3. **Backend IR (IR)**: FIR -> Kotlin IR, the lower IR shared by all backends.
4. **Per-target lowering**:
   - **JVM backend**: IR -> JVM bytecode (via the ClassFile API since 2.1.20; previously ASM).
   - **JS backend**: IR -> JS IR -> JavaScript text + .map file.
   - **Native backend**: IR -> LLVM IR -> object file via LLVM toolchain.
   - **Wasm backend**: IR -> Wasm GC text + binary.
5. **Linker**: per-target. Klib archives for K/Native; .jar for JVM; .js for JS; .wasm for Wasm.

Mochi never touches any of this. We emit `.kt` source text and hand off to `kotlinc` (or `gradle kotlinCompile`).

## 4. Pipeline diagram

```
+---------------------+
|  Mochi source       |
|  *.mochi files      |
+----------+----------+
           |
           v
+---------------------+
|  parse, type check  |
|  (shared front end) |
+----------+----------+
           |
           v
+---------------------+
|  aotir IR           |
|  (target-agnostic)  |
+----------+----------+
           |
           v
+---------------------+    +-------------------------+
|  monomorphisation   |    |  shared with            |
|  pass (shared)      |--->|  MEP-45 / MEP-46 /      |
+----------+----------+    |  MEP-47 / MEP-48 / 49   |
           |               +-------------------------+
           v
+---------------------+
|  closure conversion |
|  pass (shared)      |
+----------+----------+
           |
           v
+---------------------+    [MEP-50 begins here]
|  Kotlin codegen     |
|  ~4200 LOC Go       |
+----------+----------+
           |
           v
+---------------------+
|  KotlinPoet shadow  |
|  tree (Go side)     |
+----------+----------+
           |
           v
+---------------------+
|  pretty-print       |
|  canonical .kt      |
+----------+----------+
           |
           v
+---------------------+
|  ktlint --format    |
|  (optional)         |
+----------+----------+
           |
           v
+---------------------+
|  kotlinc / gradle   |
|  build              |
+----------+----------+
           |
           v
+---------------------+
|  .jar / .aar /      |
|  .klib / .js /      |
|  .wasm / native exe |
+---------------------+
```

The boxes above the "MEP-50 begins here" line are shared with the other transpiler3 targets. The boxes below are Kotlin-specific. The total Kotlin-specific code budget is roughly 4200 lines of Go for the codegen pass plus 600 lines for the Gradle project writer and 350 lines for the ktlint integration shim.

## 5. aotir IR reuse

The aotir IR designed for MEP-45 (Mochi to C) is target-agnostic by construction. It is a typed, monomorphised, closure-converted representation of Mochi programs with explicit lifetimes for stack allocation. Three properties make it reusable for Kotlin:

- **No assumption of C calling conventions.** aotir uses an abstract `Call` opcode with named arguments; the target backend maps to `fun` invocation in Kotlin, `INVOKEVIRTUAL` on JVM, `callvirt` on CLR, or C ABI on the C target.
- **No assumption of manual memory management.** aotir carries a per-allocation `lifetime` annotation (`stack`, `arena`, `heap`). The Kotlin target reads all three and emits Kotlin bindings: `stack` and `arena` become `val` locals (the JVM GC handles deallocation), `heap` becomes a heap-allocated `data class` instance.
- **No assumption of nominal vs structural typing.** aotir tracks whether a type is nominal (Mochi `record Foo { ... }`) or structural (Mochi tuple `(int, string)`). Kotlin has both: nominal types become `data class`s, structural types become `Pair<Int, String>` / `Triple<...>` for arity 2-3 or a generated `data class` for arity 4+.

The MEP-50 Kotlin codegen pass is roughly 4200 LOC in Go:

- ~1300 LOC: KotlinPoet shadow tree (one Go type per node kind we emit, plus serialisation to .kt text).
- ~1200 LOC: aotir-to-Kotlin lowering rules (one function per aotir opcode family).
- ~500 LOC: name mangling and package layout.
- ~400 LOC: closure-to-Kotlin-lambda ABI selection (`crossinline`, `noinline`, `suspend`).
- ~350 LOC: actor/agent lowering (custom actor class with `Channel<Message>`).
- ~250 LOC: sum-type / sealed-interface lowering.
- ~200 LOC: deterministic ordering pass (§27).

This matches the budget MEP-47 reports for its Java codegen (~3800 LOC) and is within MEP-49's budget (~4000 LOC).

## 6. Why emit Kotlin source, not JVM bytecode

Three reasonable alternatives to "emit Kotlin source" exist for a Kotlin-targeting transpiler:

- Emit JVM bytecode directly (skip `kotlinc`).
- Emit Kotlin IR (the K2 backend IR), pipe through the rest of the toolchain.
- Emit `.kotlin_metadata` plus JVM bytecode with the right annotations.

All three are rejected. Six reasons to stay at the Kotlin source layer:

1. **Cross-target reuse.** The same `.kt` file is compiled by `kotlinc` to JVM bytecode, by the K/Native compiler to LLVM IR + native, by the K/JS compiler to JS, and by the K/Wasm compiler to Wasm GC. Emitting bytecode would force us to also write a K/Native emitter, a K/JS emitter, and a K/Wasm emitter (four backends). The source-text path costs us one emitter.

2. **Debuggability.** A user staring at "what did Mochi produce from this `union` declaration" can open the generated `.kt` file in IntelliJ IDEA, set a breakpoint, and step through. With JVM bytecode the user needs `javap -c` and a tolerance for stack-machine reading; with K/Native LLVM IR the cognitive load is multiple orders of magnitude higher.

3. **Reviewability.** Mochi's golden test corpus (see MEP-50 §11 of the umbrella) checks the emitted Kotlin into git. A reviewer can read `src/commonMain/kotlin/MochiUser/Foo.kt` and tell whether the output is sensible. Reviewing the JVM bytecode equivalent is not realistic.

4. **Kotlin language features that have no IR equivalent.** Kotlin language features like `data class` (synthesised `equals` / `hashCode` / `toString` / `copy` / `componentN`), `inline class` (Kotlin 1.5+ value classes), `suspend fun` (state-machine transformation), and `sealed interface` (exhaustive `when`) are surfaced by the compiler's source-to-IR pass. Bypassing the source layer means re-implementing all of those.

5. **IntelliJ integration.** Source-level Kotlin drops straight into an IntelliJ project. Generated `.kt` files appear in the Project view, get indexed by the K2 IDE plugin, support Quick Help, support code-completion-on-Mochi-generated-API, and benefit from IntelliJ's incremental rebuild dependency graph.

6. **K2 strict null safety + Sendable inference.** The Kotlin compiler runs nullability checking (every `T?` vs `T` distinction) and KMP `actual`/`expect` cross-checking at the source-to-IR boundary. Generating IR by hand skips those checks. Mochi's whole point is to give users a safer source language than what they would write; if our generated Kotlin type-checks clean, we know we have not introduced null-deref bugs.

Kotlin source is the contract. Everything below is `kotlinc`'s job.

## 7. Codegen pass implementation language

Go, consistent with the other transpiler3 targets. Three options were considered for how Go talks to the Kotlin source tree:

- **Option A: JNI binding to KotlinPoet.** Spin up a JVM in the Go process, load KotlinPoet, call its builder API. Rejected: a JVM dependency just to invoke a code generator is unacceptable for the Mochi CLI, which targets single-static-binary distribution.

- **Option B: Sidecar Kotlin process.** Generate a Go data structure mirroring KotlinPoet's tree shape, serialise to JSON, ship to a sidecar JVM process that deserialises into KotlinPoet and pretty-prints. Rejected: same single-binary problem, plus IPC latency adds 50-100 ms per invocation.

- **Option C: Go-native shadow tree.** Build a Go data structure that mirrors KotlinPoet's tree shape, with one Go type per node kind. Render to canonical Kotlin source text directly from Go, with no JVM process in the loop at build time.

Option C wins for Mochi. The reasons:

- Mochi's pre-built binary distribution must be a single static Go executable; we do not want a JVM dependency just to compile Mochi itself.
- The set of node kinds Mochi emits is a strict subset of KotlinPoet (roughly 50 node kinds out of ~120). The shadow tree is small.
- Canonical pretty-printing is a deterministic walk over the tree with fixed indent/brace rules. About 700 LOC.
- We still shell out to `ktlint --format` post-emit (§8) for belt-and-braces formatting compliance.

The Go package path is `github.com/mochilang/mochi/transpiler3/kotlin/ktree`. "ktree" stands for Kotlin tree. Each node looks roughly like:

```go
type FunSpec struct {
    Modifiers   []Modifier // public, internal, private, suspend, inline, tailrec
    Name        Identifier
    TypeParams  []TypeParameterSpec
    Receiver    *Type // nullable: receiver type for extension functions
    Params      []ParameterSpec
    ReturnType  *Type
    WhereClause *WhereClause
    Body        *CodeBlock // nullable: abstract fun has no body
    KDoc        string
}

type ParameterSpec struct {
    Modifiers []Modifier // vararg, crossinline, noinline
    Name      Identifier
    Type      *Type
    Default   *CodeBlock // nullable
}

type CodeBlock struct {
    Format string         // KotlinPoet-style format with %T %L %N %S placeholders
    Args   []interface{}  // type-erased; runtime checked
}
```

Serialisation is a `func (n *FunSpec) Render(w *Writer)` method that emits canonical Kotlin text. The whole tree implements a single `Node` interface with `Render` and `Kind` methods.

## 8. ktlint integration

After Mochi writes a `.kt` file, the codegen pipeline shells out to `ktlint --format` to enforce JetBrains' Kotlin Coding Conventions. ktlint is a community-maintained linter and formatter at `https://github.com/pinterest/ktlint`. Current release: **1.5.0** (December 2024), supports Kotlin 2.1. Available as a single fat jar or via Homebrew, apt, or `brew install ktlint`.

The invocation:

```
ktlint --format --editorconfig .editorconfig src/commonMain/kotlin/**/*.kt
```

The `.editorconfig` Mochi ships at the package root:

```ini
[*.{kt,kts}]
indent_style = space
indent_size = 4
end_of_line = lf
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true
max_line_length = 120
ktlint_code_style = ktlint_official
ktlint_standard = enabled
ktlint_standard_no-wildcard-imports = enabled
ktlint_standard_trailing-comma-on-call-site = enabled
ktlint_standard_trailing-comma-on-declaration-site = enabled
```

Three reasons we run `ktlint` even though our pretty-printer already produces canonical text:

- **Belt and braces.** If a future change in our Go pretty-printer introduces a regression (extra blank line, missing space after comma), `ktlint --format` catches and fixes it before the file is committed to the golden corpus.
- **Community alignment.** `ktlint`'s rules align with the JetBrains Kotlin Coding Conventions plus the de-facto Square/Pinterest community style. Reviewers reading Mochi-generated Kotlin get the look they expect from hand-written Kotlin.
- **Configurable.** Users with strong opinions can override the `.editorconfig` in their project; Mochi respects the user's config and re-runs the formatter.

We do not depend on `ktlint` for correctness, only for cosmetics. If `ktlint` is unavailable on the build host, the build emits a warning and proceeds with our pretty-printer's output. This is similar to how MEP-49 treats `swift-format`.

## 9. Name mangling

Mochi names map to Kotlin names by a deterministic rule.

- **Package prefix.** Every Mochi module `m.n.p` becomes the Kotlin package `mochi.user.m.n.p` (configurable via `--kotlin-package-prefix`, default `mochi.user`). Public symbols are referenced as `mochi.user.m.n.p.symbol`. The user can override the prefix entirely via a `kotlin_package = "com.example"` package directive in the Mochi build manifest.

- **Reserved word handling.** Kotlin reserved words (`class`, `fun`, `object`, `when`, `is`, `as`, `in`, `for`, `if`, `else`, `return`, `package`, `import`, `val`, `var`, `typealias`, `interface`, `sealed`, `enum`, `data`, `suspend`, `inline`, `crossinline`, `noinline`, `tailrec`, `external`, `actual`, `expect`, `companion`, `init`, `this`, `super`, `throw`, `try`, `catch`, `finally`, `do`, `while`, `break`, `continue`, `null`, `true`, `false`, `by`, `where`, `out`, `in` as variance) get wrapped in backticks when they appear as identifiers: a Mochi field named `class` becomes Kotlin `` `class` ``. Backtick-escaping any identifier is allowed in Kotlin since 1.0 and round-trips through `kotlinc`.

  Soft keywords (context-dependent) like `field`, `it`, `value`, `param` do not need backticks at most positions, but Mochi codegen is conservative and backticks them anyway to avoid edge cases.

- **Stdlib name collisions.** Some Mochi types collide with Kotlin stdlib types: `String`, `Int`, `Long`, `List`, `Map`, `Set`, `Pair`, `Triple`, `Result`. We never reuse these names directly for Mochi-generated types; instead we prefix with the module: a Mochi `record String { ... }` in module `text` becomes `public data class TextString(...)` (camelCase concatenation) or `mochi.user.text.MochiString` (qualified) per the user's preference, configurable via `--kotlin-mangling-style={prefix,qualified}`. Default: qualified (no prefix in the type name; qualified at use sites).

- **Operator characters.** Mochi identifiers permitting `?`, `!`, `'` (prime) are escaped: `foo?` becomes `fooOpt`, `foo!` becomes `fooBang`, `foo'` becomes `fooPrime`. The escape table is in the type-lowering note ([[06-type-lowering]] §3).

- **Monomorphisation suffix.** Specialised instances get a six-hex suffix derived from BLAKE3 over the instantiation arguments: `mapInst_a1b2c3`. This matches the convention MEP-47 / MEP-49 use.

- **Top-level vs nested.** Top-level Mochi declarations become Kotlin top-level declarations (Kotlin natively supports them, unlike Java). Nested Mochi declarations become nested Kotlin declarations: a Mochi function inside another function lowers to a local function `fun inner() { ... }` inside the outer body, or to a lambda when the outer function captures it.

Two emitted Kotlin identifiers never collide across modules or generic specialisations. The mangling table is reversible via a sidecar `.mangle.json` file shipped alongside the generated `build.gradle.kts`.

## 10. Source layout

Default layout: **one `.kt` file per Mochi source file**. A Mochi source `geom/shapes/circle.mochi` produces `src/commonMain/kotlin/mochi/user/geom/shapes/circle/Circle.kt`. The file name is the PascalCase of the Mochi file's base name.

Optional layout: **one `.kt` file per Mochi top-level declaration**, behind a `--kotlin-split-by-decl` flag. The per-declaration mode is IDE-friendly (IntelliJ likes small files, indexes them faster), at the cost of more file-system churn during incremental builds.

A `settings.gradle.kts` is emitted at the project root:

```kotlin
rootProject.name = "mochi-out"

dependencyResolutionManagement {
    repositories {
        mavenCentral()
        google()
    }
    versionCatalogs {
        create("libs") {
            from(files("gradle/libs.versions.toml"))
        }
    }
}
```

A `build.gradle.kts` is emitted at the project root:

```kotlin
plugins {
    alias(libs.plugins.kotlin.multiplatform)
    alias(libs.plugins.kotlin.serialization)
    alias(libs.plugins.android.library) apply false
}

kotlin {
    jvmToolchain(17)
    jvm()
    iosArm64()
    iosSimulatorArm64()
    macosArm64()
    linuxX64()
    js(IR) { nodejs() }

    sourceSets {
        commonMain.dependencies {
            implementation(libs.mochi.runtime.core)
            implementation(libs.kotlinx.coroutines.core)
            implementation(libs.kotlinx.serialization.json)
        }
    }
}
```

A `gradle/libs.versions.toml` is emitted with pinned versions:

```toml
[versions]
kotlin = "2.1.0"
coroutines = "1.10.1"
serialization = "1.7.3"
datetime = "0.6.1"
ktor = "3.0.3"
agp = "8.7.0"
mochi-runtime = "0.1.0"

[libraries]
kotlinx-coroutines-core = { module = "org.jetbrains.kotlinx:kotlinx-coroutines-core", version.ref = "coroutines" }
kotlinx-serialization-json = { module = "org.jetbrains.kotlinx:kotlinx-serialization-json", version.ref = "serialization" }
kotlinx-datetime = { module = "org.jetbrains.kotlinx:kotlinx-datetime", version.ref = "datetime" }
mochi-runtime-core = { module = "io.mochi-lang:mochi-runtime-core", version.ref = "mochi-runtime" }

[plugins]
kotlin-multiplatform = { id = "org.jetbrains.kotlin.multiplatform", version.ref = "kotlin" }
kotlin-serialization = { id = "org.jetbrains.kotlin.plugin.serialization", version.ref = "kotlin" }
android-library = { id = "com.android.library", version.ref = "agp" }
```

A `gradle/wrapper/gradle-wrapper.properties` pins Gradle 8.11.1:

```properties
distributionUrl=https\://services.gradle.org/distributions/gradle-8.11.1-bin.zip
```

The KMP source-set layout follows the standard convention. Tests go in `src/commonTest/kotlin/` plus per-target `src/jvmTest/`, `src/iosTest/`, etc.

## 11. Top-level let and var lowering

Mochi `let x = 1` at module scope -> Kotlin `val x: Long = 1L` at file scope (Kotlin allows top-level `val` declarations, unlike Java). For Mochi `var x = 1`, lowering emits `var x: Long = 1L`.

Lazy initialisation when forward references exist:

```kotlin
val cache: Map<String, Long> by lazy { computeCache() }
```

For `var` with deferred initialisation:

```kotlin
private lateinit var config: Config
```

`lateinit` only works for non-null reference types; for nullable or primitive `var`s we use `var x: Long? = null` and check at use sites. The codegen pass picks based on the Mochi declaration's effective nullability.

For top-level expressions that need a side-effect block at startup, Mochi `init { ... }` lowers to a top-level `init` block inside a synthesised `object`:

```kotlin
private object MochiModuleInit {
    init {
        // user-provided init code
    }
}
```

The `MochiModuleInit` is loaded lazily; the runtime calls `MochiModuleInit::class.simpleName` from the main entry point to force class loading and run the `init` blocks in deterministic order.

## 12. Function lowering

Mochi `fun f(...)` at top-level -> Kotlin `fun f(...)` at file scope.

```kotlin
fun greet(name: String): String = "Hello, $name!"
```

Mochi `fun` with an explicit return:

```kotlin
fun greet(name: String): String {
    return "Hello, $name!"
}
```

The codegen pass emits **expression-body** form (`= expr`) when the function body is a single expression, and **block-body** form (`{ return expr }`) when the body is a block. This matches the Kotlin convention.

Nested Mochi functions lower to local functions inside the outer body:

```kotlin
fun outer(): Int {
    fun helper(x: Int): Int = x + 1
    return helper(42)
}
```

Closures (Mochi anonymous functions / lambdas) lower to Kotlin lambdas:

```kotlin
val add: (Long, Long) -> Long = { a, b -> a + b }
```

Mochi suspend functions (async) lower to `suspend fun`:

```kotlin
suspend fun fetch(url: String): String {
    return mochiHttp.get(url).body
}
```

Mochi `fun` with default arguments:

```kotlin
fun ping(timeout: Long = 1000L): Boolean = ...
```

Mochi varargs:

```kotlin
fun sum(vararg items: Long): Long = items.sum()
```

Inline functions for hot closures (when the Mochi static analysis flags a closure as inline-able):

```kotlin
inline fun <T> withTime(body: () -> T): T {
    val t0 = kotlin.time.Clock.System.now()
    val result = body()
    val dt = kotlin.time.Clock.System.now() - t0
    println("elapsed=$dt")
    return result
}
```

Crossinline and noinline modifiers are emitted when needed: `crossinline` when the lambda escapes the inlining context (passed to another non-inline function), `noinline` when the user passes the lambda to a non-inline target.

## 13. Block lowering

Kotlin has expression-valued `if`, `when`, `try`, and `do-while` (most blocks are expressions). Mochi `if-then-else` lowers to Kotlin `if`:

```kotlin
val y: Long = if (x > 0) 1L else -1L
```

Multi-arm Mochi match (over a non-sum type) lowers to `when`:

```kotlin
val name: String = when (n) {
    1 -> "one"
    2 -> "two"
    else -> "many"
}
```

For multi-statement blocks that produce a value, Kotlin uses `run { }` or implicit last-expression rules:

```kotlin
val result: Long = run {
    val a = compute()
    val b = a * 2
    a + b
}
```

For pure side-effect blocks (no value), Mochi `{ ... }` lowers to an inline `{ ... }` block under `Unit`:

```kotlin
{
    println("hi")
    flush()
}()  // immediately-invoked
```

In practice we never emit an IIFE pattern; we either inline the statements into the parent block or wrap them in `run { }` if a value is needed.

Mochi `try / catch / finally` lowers to Kotlin `try` (an expression):

```kotlin
val parsed: Int? = try {
    s.toInt()
} catch (e: NumberFormatException) {
    null
}
```

## 14. Return lowering

Kotlin's `return` is a statement; expression-body functions don't use it. The codegen pass uses these forms:

- **Single-expression body**: omit `return`. `fun f() = 42L`.
- **Block body with single return**: emit `return`. `fun f(): Long { return 42L }`.
- **Block body with control flow**: emit explicit `return` at each exit point.
- **Block body where last expression is the value**: emit `return` (Kotlin requires explicit return in block-body functions even when the last expression is the value, unless the function returns `Unit`).

For early returns from nested lambdas, Kotlin requires labelled returns:

```kotlin
fun firstPositive(xs: List<Long>): Long? {
    xs.forEach {
        if (it > 0L) return it  // returns from firstPositive, since forEach is inline
    }
    return null
}
```

For non-inline lambdas, the codegen pass emits `return@label`:

```kotlin
val result = transform { x ->
    if (x.isEmpty()) return@transform null
    x.uppercase()
}
```

## 15. Record lowering

Mochi `record T { f1: T1, f2: T2, ... }` lowers to Kotlin `data class T(val f1: T1, val f2: T2, ...)`.

```kotlin
@Serializable
data class Point(val x: Long, val y: Long)
```

Kotlin `data class` synthesises:

- `equals(other: Any?): Boolean` (structural equality)
- `hashCode(): Int` (combines field hashCodes)
- `toString(): String` (`"Point(x=1, y=2)"` form)
- `copy(x: Long = this.x, y: Long = this.y): Point` (functional update)
- `componentN(): T` for each field, enabling destructuring

Mochi `with`-expressions (functional update) leverage Kotlin's synthesised `copy`:

```kotlin
val p2 = p.copy(x = 10L)  // y unchanged
```

The `@Serializable` annotation (from kotlinx.serialization) generates a `KSerializer<Point>` at compile time. No runtime reflection.

For records with computed-property methods (Mochi `fun area(self)`), we emit member functions inside the data class:

```kotlin
@Serializable
data class Circle(val radius: Double) {
    fun area(): Double = kotlin.math.PI * radius * radius
}
```

For records that need to interop with Java (Mochi-generated APIs called from Java), we add `@JvmStatic` to companion-object methods and `@JvmField` to const properties.

## 16. Sum type lowering

Mochi `union T = A | B(...)` lowers to Kotlin `sealed interface T` with `data class` / `data object` variants:

```kotlin
sealed interface Tree {
    data object Empty : Tree
    data class Leaf(val value: Long) : Tree
    data class Node(val left: Tree, val right: Tree) : Tree
}
```

We prefer `sealed interface` (Kotlin 1.7+) over `sealed class` for two reasons:

1. **Multiple inheritance.** Variants of one sealed interface can also implement other interfaces (e.g., `Serializable`, `Comparable`).
2. **Cross-module sealing.** A `sealed interface` declared in commonMain can have actual `data class` members in jvmMain, iosMain, etc. (limited; the variants must be in the same module).

`data object` (Kotlin 1.9+) is used for variants without payload; it gives a singleton with synthesised `equals`/`hashCode`/`toString`. Before 1.9 we would use `object Empty : Tree` without the `data` keyword; the `data object` adds the proper `toString()`.

Mochi `match` lowers to Kotlin `when`:

```kotlin
fun depth(t: Tree): Long = when (t) {
    Tree.Empty -> 0L
    is Tree.Leaf -> 1L
    is Tree.Node -> 1L + maxOf(depth(t.left), depth(t.right))
}
```

The Kotlin compiler enforces exhaustiveness when the `when` value is used as an expression (Kotlin 1.6+; smart-cast-exhaustive in 2.1). Mochi's exhaustiveness check is the same check at the Mochi level; Kotlin re-checks at compile time and would reject a non-exhaustive `when` (which would never happen because Mochi rejected it first).

For variants with named payload fields, Mochi destructuring leverages Kotlin's destructuring declarations:

```kotlin
when (t) {
    is Tree.Node -> {
        val (left, right) = t  // uses componentN
        process(left, right)
    }
}
```

Or pattern-style with smart cast (no destructuring):

```kotlin
when (t) {
    is Tree.Node -> process(t.left, t.right)  // t smart-cast to Tree.Node
}
```

The codegen pass picks based on whether the source Mochi pattern uses named fields (`Node(left=l, right=r)` -> smart-cast + named access) or positional fields (`Node(l, r)` -> destructuring).

## 17. Match lowering (general)

Mochi `match` over a value lowers to Kotlin `when (value) { ... }`. The arms can be:

- **Constant patterns**: `1 -> ...`, `"hello" -> ...`. Emitted as literal arms.
- **Type patterns**: `is Tree.Node -> ...`. Kotlin smart-casts inside the arm.
- **Range patterns**: `in 1..10 -> ...`. Kotlin supports range arms.
- **Predicate patterns**: Mochi `when x if x > 0` -> Kotlin nested `when` or guard via `when (x) { is T -> if (x.size > 0) ... else ... }`.
- **Catch-all**: Mochi `_ ->` -> Kotlin `else ->`.

For guards (Mochi `match x { Foo if cond -> ... }`), Kotlin requires nesting because `when` arms don't support guards directly:

```kotlin
// Mochi: match shape { Circle(r) if r > 0 -> area(r); _ -> 0.0 }
when (shape) {
    is Shape.Circle -> if (shape.radius > 0) area(shape.radius) else 0.0
    else -> 0.0
}
```

This is more verbose than Swift's `case .circle(let r) where r > 0` form. We document the gap; an alternative is to lift the guard into a helper that lowers cleanly, but the inline form is what Kotlin developers write.

Kotlin 2.1 added smart-cast support for sealed `when` expressions across multi-arm conditions; this is mostly invisible to the codegen pass but improves the user-visible type narrowing.

## 18. Destructuring

Kotlin destructuring is based on `componentN()` operators. `data class` synthesises them automatically. Mochi destructuring lowers as:

- **Record field destructuring**: `val (x, y) = point` (uses `Point.component1()` / `component2()`).
- **List destructuring**: Kotlin doesn't have native list destructuring; we use indexed reads: `val first = list[0]; val second = list[1]`. For `(head, ...tail)` patterns, we emit `val head = list.first(); val tail = list.drop(1)`.
- **Map destructuring**: same as list; no native syntax. `val value = map["key"] ?: error("missing")`.
- **Pair / Triple**: `val (a, b) = pair` (built-in `componentN` on `Pair` / `Triple`).
- **Lambda parameter destructuring**: `list.forEach { (k, v) -> ... }` for `List<Pair<K, V>>` or map entries.

For nested destructuring (Mochi `let (a, (b, c)) = ...`), Kotlin requires manual unpacking:

```kotlin
val outer = expr
val a = outer.component1()
val inner = outer.component2()
val b = inner.component1()
val c = inner.component2()
```

This is more verbose than Mochi's source; the codegen pass adds a generated comment showing the original Mochi pattern.

## 19. Closure lowering

Mochi closures lower to Kotlin lambdas with explicit type annotations on the surrounding `val`.

A simple non-capturing closure:

```kotlin
val inc: (Long) -> Long = { x -> x + 1L }
```

A capturing closure where the capture is by value (Kotlin captures immutable `val`s by value automatically):

```kotlin
val base = 10L
val bump: (Long) -> Long = { x -> x + base }
```

A capturing closure over a mutable `var`. Kotlin captures `var`s by reference (the JVM lowering boxes the variable in a `Ref.LongRef` synthetic class). This is the same semantic as Java's "effectively final" + a wrapper class, except Kotlin makes the boxing automatic:

```kotlin
var counter = 0L
val bump: () -> Unit = { counter += 1L }
bump(); bump()
// counter is now 2L
```

For Mochi value-semantic captures (Mochi `[x] x = current_x` capture list, where the user wants a snapshot of `x` at lambda creation time), we emit a fresh local copy and capture that:

```kotlin
var current = 1L
val capturedCurrent = current  // snapshot at lambda creation
val snapshot: () -> Long = { capturedCurrent }
current = 2L
// snapshot() returns 1L (the snapshot), not 2L
```

This matches Mochi's "captures-by-copy" semantic for value-typed expressions. The transpiler emits the explicit copy when the static analysis flags a capture as value-typed.

Closures crossing actor boundaries (Mochi closures inside an `agent` method's reply to the caller) need to be safely shareable across coroutine contexts. Kotlin does not have Swift's `@Sendable` annotation; instead the compiler tracks closure capture via the `kotlinx.coroutines` strict mode (`kotlinx.coroutines.flow.flow {}` checks via `currentCoroutineContext`).

For closures that need a `@Suppress` annotation (when the static analysis flags a capture as crossing context bounds but the user has manually verified it):

```kotlin
@Suppress("ContextBoundsConflict")
val handler: (Event) -> Unit = { e -> shared.send(e) }
```

## 20. Async colouring

Mochi `async fun` lowers to Kotlin `suspend fun`. The Kotlin compiler transforms `suspend fun` into a state machine (continuation-passing style) at compile time; the user never sees the transformation.

```kotlin
suspend fun fetchUser(id: Long): User {
    val resp = mochiHttp.get("https://api.example.com/users/$id")
    return MochiJson.decodeFromString<User>(resp.body)
}
```

Async / await colouring:

```kotlin
// Mochi: let user = await fetchUser(42)
val user = fetchUser(42L)  // suspend fun call, implicitly awaited

// Mochi: spawn fetchUser(42)
val deferred = coroutineScope { async { fetchUser(42L) } }
val user = deferred.await()
```

For multi-task fan-out, Mochi `parallel for x in xs { body }` lowers to `coroutineScope { xs.forEach { x -> launch { body(x) } } }`:

```kotlin
coroutineScope {
    xs.forEach { x -> launch { body(x) } }
}
```

`coroutineScope { ... }` suspends until all child coroutines complete (structured concurrency). Cancellation of the outer scope cancels all children.

For `supervisorScope { ... }`, children failures don't cancel siblings (used by `MochiSupervisor`):

```kotlin
supervisorScope {
    workers.forEach { w -> launch { w.run() } }
}
```

The `withContext` switch:

```kotlin
val data = withContext(Dispatchers.IO) {
    readFile(path)
}
```

This is the idiomatic way to move CPU-bound or IO-bound work to the appropriate thread pool.

## 21. Datatype protocol conformance

Mochi records auto-implement protocols based on field types:

- **Serializable**: `@Serializable` annotation; kotlinx.serialization generates the `KSerializer<T>`.
- **Equatable / Hashable**: synthesised by `data class`.
- **Comparable**: emitted manually if the Mochi record explicitly conforms (no auto-synthesis).
- **Printable / toString**: synthesised by `data class`.

For sealed-interface conformance (Mochi `union T : Show`), the variants inherit the protocol declarations:

```kotlin
sealed interface Tree : Show {
    data object Empty : Tree {
        override fun show(): String = "Empty"
    }
    data class Leaf(val value: Long) : Tree {
        override fun show(): String = "Leaf($value)"
    }
}
```

Default-method implementations on a Mochi `trait` lower to default `interface` methods (Kotlin allows interface default implementations since 1.0):

```kotlin
interface Show {
    fun show(): String
    fun showDefault(): String = "<$this>"
}
```

## 22. Tail-call optimisation

Kotlin has a `tailrec` modifier on functions; the compiler verifies that the function's last call is a self-recursive call in tail position, and rewrites the call as a loop. Mochi tail-call analysis identifies functions whose call graph is a single self-recursive tail call, and emits `tailrec`:

```kotlin
tailrec fun gcd(a: Long, b: Long): Long =
    if (b == 0L) a else gcd(b, a % b)
```

The compile error you get for a non-tail call inside a `tailrec` function:

```
A function is marked as tail-recursive but no tail calls are found
```

is a hard guarantee; the transpiler only emits `tailrec` when its own static analysis verifies the tail position. If unsure, omit the modifier and accept the stack frame.

Kotlin does **not** support mutual tail-call optimisation (only self-recursive). For mutual recursion that Mochi flagged as tail-call-safe, the codegen falls back to a manual trampoline:

```kotlin
sealed interface Step<out T> {
    data class More<T>(val next: () -> Step<T>) : Step<T>
    data class Done<T>(val value: T) : Step<T>
}

tailrec fun <T> runTramp(s: Step<T>): T = when (s) {
    is Step.Done -> s.value
    is Step.More -> runTramp(s.next())
}
```

This pattern is a manual translation; the Mochi front end emits a warning when a mutual-tail-call structure is detected, suggesting the user refactor into a single self-recursive helper.

## 23. Source maps

Mochi-to-Kotlin line maps are emitted as a sidecar `.mochi.map` file per generated `.kt`. The format mirrors the Source Map v3 specification (originally a Chrome/Firefox JS source-map format, since adopted by TypeScript, Dart, Kotlin/JS, and others). Kotlin/JS itself emits `.js.map` files in this format, so the Mochi map is a "second hop" that runs on top.

```json
{
  "version": 3,
  "file": "MochiGeomShapes.kt",
  "sources": ["geom/shapes.mochi"],
  "names": ["Circle", "area", "radius"],
  "mappings": "AAAA,SAAS;EACP,..."
}
```

The map lets debugger UIs (IntelliJ IDEA's Kotlin debugger, VS Code's Kotlin extension via JDWP, Android Studio for Android targets) attribute Kotlin line numbers back to Mochi line numbers when stepping. The map is loaded by Mochi's own debugger adapter (a DAP server living in `transpiler3/kotlin/dap`) which translates breakpoint requests from `.mochi` coordinates into the `.kt` coordinates the underlying Kotlin debugger understands.

Caveats:

- The JVM debugger (JDI) maps `.kt` -> bytecode -> JVM. The Mochi map gives us the third hop. The three hops are fused by the DAP adaptor.
- For Kotlin/JS, the toolchain's `.js.map` is the second hop and the Mochi map is the third. The DAP adapter chains them.
- DWARF support for Mochi source files (via a custom DWARF producer on K/Native) is a deferral; the sidecar JSON map is simpler for v1.

## 24. KotlinPoet usage (illustrative)

For readers who want to see what the equivalent KotlinPoet API looks like (we use a Go-native shadow tree, but the shape is the same), here is a representative generation snippet:

```kotlin
// Generating a data class via KotlinPoet:
val pointClass = TypeSpec.classBuilder("Point")
    .addModifiers(KModifier.DATA)
    .primaryConstructor(
        FunSpec.constructorBuilder()
            .addParameter("x", LONG)
            .addParameter("y", LONG)
            .build()
    )
    .addProperty(PropertySpec.builder("x", LONG).initializer("x").build())
    .addProperty(PropertySpec.builder("y", LONG).initializer("y").build())
    .build()

// Generating a top-level fun:
val areaFun = FunSpec.builder("area")
    .receiver(ClassName("mochi.user.geom", "Circle"))
    .returns(DOUBLE)
    .addCode("return %T.PI * radius * radius\n", KMATH)
    .build()

// Building the file:
val file = FileSpec.builder("mochi.user.geom", "Geom")
    .addType(pointClass)
    .addFunction(areaFun)
    .build()

file.writeTo(outputDir)
```

KotlinPoet primitives:

- `FileSpec`: a single .kt file. Top-level container.
- `TypeSpec`: a class / interface / object / enum / annotation. Supports `data`, `sealed`, `inline`, etc., modifiers.
- `FunSpec`: a function (top-level or member). Supports `suspend`, `inline`, `tailrec`, `crossinline`, `noinline`.
- `PropertySpec`: a property (`val` / `var`). Supports getter / setter / delegate.
- `ParameterSpec`: a function parameter. Supports `vararg`, `crossinline`, `noinline`, default values.
- `TypeName`, `ClassName`, `TypeVariableName`, `LambdaTypeName`: type references.
- `CodeBlock`: an arbitrary code fragment. Uses %T (type), %L (literal), %N (name), %S (string), %M (member) placeholders.
- `AnnotationSpec`: an annotation invocation with arguments.
- `KModifier`: enum of all Kotlin modifiers (`PUBLIC`, `PRIVATE`, `INTERNAL`, `PROTECTED`, `DATA`, `SEALED`, `ABSTRACT`, `OPEN`, `OVERRIDE`, `FINAL`, `CONST`, `LATEINIT`, `INLINE`, `NOINLINE`, `CROSSINLINE`, `SUSPEND`, `TAILREC`, `EXTERNAL`, `OPERATOR`, `INFIX`, `EXPECT`, `ACTUAL`, ...).

Our Go shadow tree mirrors these node kinds one-for-one. The Go names are PascalCase: `FileSpec`, `TypeSpec`, `FunSpec`, etc.

## 25. Bytecode differential with MEP-47

MEP-47 emits JVM bytecode directly from Mochi IR; MEP-50 emits .kt source and hands off to `kotlinc`. On the JVM target the two flows produce **different** bytecode:

- **MEP-47**: Direct bytecode from Mochi IR. Faster builds (no kotlinc), no Kotlin language semantics in the way. Uses ClassFile API (JEP 484, JDK 24+).
- **MEP-50 JVM**: .kt source -> kotlinc -> bytecode. Slower build, but the same .kt source feeds K/Native, K/JS, K/Wasm.

Bytecode differences on the JVM target:

| Feature | MEP-47 emit | MEP-50 emit (via kotlinc) |
| --- | --- | --- |
| Number boxing | manual `Long.valueOf` calls | kotlinc-inserted boxing where nullable |
| Lambda capture | direct method handle | kotlinc-synthesised inner class on JVM < 9, or LambdaMetafactory on JVM 9+ |
| Sum types | manual switch table | sealed class with `when` -> tableswitch or lookupswitch |
| data class methods | manual equals/hashCode/toString | kotlinc-synthesised |
| suspend funs | manual continuation passing | kotlinc-synthesised state machine |
| inline class | not used | unboxed at most call sites |

The cross-target differential gate (`TestCrossTargetDifferential`) verifies both produce byte-equal stdout on every shared fixture. The bytecode itself differs; the runtime behaviour does not.

Users can pick either: MEP-47 for fastest JVM-only builds, MEP-50 for one-source-fits-all-targets. Mochi's build manifest exposes both:

```toml
[build.jvm]
target = "mep47"  # or "mep50"
```

Default: `mep50` because the polyglot user benefits more often than the JVM-purist user.

## 26. Generated code style

The JetBrains-shepherded Kotlin style applies:

- **Indent:** 4 spaces, never tabs. Matches the `ktlint` default. K&R brace style; Allman is not used in Kotlin idiom.
- **Trailing commas:** Kotlin 1.4+ allows trailing commas; we emit them for multi-line literals to minimise diff noise on append.
- **Modifiers:** `public` is the Kotlin default and we omit it. `internal` for module-scoped visibility. `private` for file-scoped. `protected` only on class members.
- **Type annotations:** explicit on every public declaration's signature; omitted on locals where inference is unambiguous.
- **`val` over `var`:** every binding that does not need mutation is emitted as `val`. The aotir lifetime annotation drives this.
- **Single expression bodies:** `fun f() = expr` when the body is one expression; `fun f(): T { return expr }` for block-body forms.
- **String templates over concatenation:** `"$name is $age"` over `name + " is " + age` for clarity.
- **`when` over chained `if-else`:** when there are 3+ arms or any non-trivial pattern.
- **Trailing lambda syntax:** `xs.map { it * 2 }` over `xs.map({ it * 2 })`.
- **`it` for single-parameter lambdas:** unless shadowing requires a named parameter.

A representative emitted file:

```kotlin
// Auto-generated by Mochi 0.x from geom/shapes.mochi
// Do not edit; re-run `mochi build --target kotlin` to regenerate.

package mochi.user.geom.shapes

import kotlinx.serialization.Serializable
import kotlin.math.PI

@Serializable
data class Circle(val radius: Double) {
    fun area(): Double = PI * radius * radius
}

@Serializable
sealed interface Shape {
    @Serializable
    data class CircleShape(val circle: Circle) : Shape
    @Serializable
    data class RectShape(val w: Double, val h: Double) : Shape
}

fun area(s: Shape): Double = when (s) {
    is Shape.CircleShape -> s.circle.area()
    is Shape.RectShape -> s.w * s.h
}
```

## 27. Deterministic output

Byte-identical Kotlin source across machines, across runs, across operating systems. Hard requirement for golden tests and for the issue-per-PR review workflow. The deterministic-ordering pass enforces:

- **Imports sorted alphabetically.** `kotlin.math.PI` before `kotlinx.coroutines.Flow` before `kotlinx.serialization.Serializable`. `ktlint`'s `no-wildcard-imports` and `import-ordering` rules re-check.
- **Top-level declarations ordered by source position in the Mochi file.** A Mochi declaration's line/column in the input controls its position in the output.
- **Map literal entries sorted by key** (for `mapOf(...)` where the Mochi source did not specify an order). Where order matters (Mochi `let m = ordered_map { ... }`), the order is preserved verbatim.
- **Set literal entries sorted by canonical hash** (same rule as maps).
- **Stable closure naming.** Anonymous closures get a name derived from BLAKE3 over their captured-variable list and body fingerprint, so two structurally identical closures in the same file always produce the same Kotlin synthetic class name (the JVM target observes this; on K/Native and K/JS the naming is purely cosmetic).
- **No timestamps in the output.** The `Auto-generated by Mochi` comment carries the Mochi version, not the build time. A reproducible build produces a byte-identical artifact regardless of when it ran.
- **Stable annotation order.** Annotations on a declaration sort by `package.Name` ascending: `@JvmStatic` before `@Serializable` because `kotlin` < `kotlinx`.

This determinism contract is verified by the MEP-50 gate test `TestKotlinDeterminism`, which compiles the same Mochi corpus twice and `cmp`s the outputs.

## 28. v1 vs v2 scope

**v1 ships:**

- Pure Kotlin source emission via the Go shadow tree (§7).
- `ktlint --format` post-processing (§8).
- `kotlinc` / `gradle build` driving the compile (§4).
- Source maps as sidecar `.mochi.map` files (§23).
- `build.gradle.kts` + `settings.gradle.kts` + `libs.versions.toml` at the package root (§10).
- All KMP targets via per-source-set configuration.
- MEP-50 JVM target produces .kt -> .jar; users can opt into MEP-47 instead for direct bytecode.

**v1 does not ship:**

- KotlinPoet round-trip parsing (the Go shadow tree is write-only; we never re-parse our own output).
- Kotlin Symbol Processing (KSP) integration. KSP is a Kotlin-side compile-time processor for annotation-based generation; Mochi's lowering happens before kotlinc runs, so KSP is not in the pipeline.
- Compose Multiplatform UI lowering (deferral, [[04-runtime]] §10).
- Library Evolution-style ABI stability annotations (Kotlin's `@Stable`, `@RestrictsSuspension` are not auto-emitted; the user adds them manually if needed).
- Direct .aar packaging (the user runs `gradle assembleRelease` themselves).

**v2 ships, opt-in:**

- KotlinPoet integration as an alternative codegen mode. Users who have a JVM installed can flip `--kotlin-codegen=kotlinpoet` and get the same output via the authoritative KotlinPoet tree.
- KSP integration for Mochi `@derive` annotations.
- Compose Multiplatform UI lowering for Mochi `view` declarations.
- `.klib` direct emission (skipping `kotlinc` for K/Native targets when feasible).
- Direct .aab / .apk packaging via the AGP Gradle tasks (driven by Mochi's build CLI).

The split keeps v1 small (no Kotlin-toolchain dependency at Mochi build time) and keeps v2 ambitious without making v1 impossible.

## 29. Honest pain points

These are real, not glossed over:

- **UTF-16 string semantics.** Kotlin `String` is UTF-16 on every target. Mochi `string.len(s)` returns the number of UTF-16 code units, not the number of grapheme clusters (Swift returns clusters via `Character`; Mochi semantics need to choose). Default: code unit count, with a warning to users who care about graphemes (use `BreakIterator`).
- **`Long` boxing on JVM.** Kotlin `Long?` (nullable) boxes to `java.lang.Long` on JVM. This is a real cost for Mochi `Option<int>` values; the codegen pass can sometimes unbox via the JVM `LongValue` synthesis (Kotlin 2.0+), but only inside `data class`. For `Long?` parameters on top-level fun, boxing is unavoidable. See [[06-type-lowering]] §11 for the full discussion.
- **K/Wasm Alpha.** Kotlin/Wasm is Alpha as of 2.1 (Beta in 2.1.20 for the Wasm GC target). Binary size is not yet competitive with K/JS for small programs; ABI stability not guaranteed across point releases. We ship K/Wasm as a target but document the caveat in every gate.
- **No mutual-tail-call optimisation.** `tailrec` only works for self-recursion. Mochi mutually-recursive tail calls fall back to manual trampolining (§22).
- **No exhaustive `when` guards.** Kotlin `when` arms don't support `if cond` guards; we emit nested `if` inside the arm body (§17). Verbose vs Swift's `case .x where cond`.
- **JNI is verbose.** Mochi FFI on Android / JVM emits `external fun` declarations plus a `.so` build. The user provides the C side. Cinterop (K/Native) is more ergonomic; the divergence is real.
- **Gradle is heavy.** `gradle build` cold-start is 8-15 seconds even on hot disks. Users who want fast feedback opt into the MEP-47 direct-bytecode path for the JVM.
- **kotlinx.collections.immutable pre-1.0.** Persistent collections work and are ABI-stable per JetBrains' commitment, but the version-0.3.8 number can read as risky to consumers.

## 30. Cross-references

- Runtime building blocks: [[04-runtime]].
- Type-by-type lowering details: [[06-type-lowering]].
- Per-target portability matrix: [[07-kotlin-target-portability]].
- Query DSL lowering details: [[08-dataset-pipeline]].
- Agent + stream lowering details: [[09-agent-streams]].
- Build system (Gradle, AGP, libs.versions.toml): [[10-build-system]].
- Testing strategy: [[11-testing-gates]].
- Risk register: [[12-risks-and-alternatives]].
- Shared decisions anchor: the shared-decisions anchor.
- MEP-49 sibling codegen note for comparison: [[../0049/05-codegen-design]].
- MEP-47 sibling JVM-bytecode codegen note: [[../0047/05-codegen-design]].

## Sources

1. *Kotlin 2.1.0 release notes*, kotlinlang.org/docs/whatsnew21.html (November 27 2024).
2. *Kotlin 2.1.20 release notes*, kotlinlang.org/docs/whatsnew2120.html (March 2025).
3. *Kotlin Language Specification*, kotlinlang.org/spec/.
4. *Kotlin Coding Conventions*, kotlinlang.org/docs/coding-conventions.html.
5. KotlinPoet 1.18.1, github.com/square/kotlinpoet.
6. ktlint 1.5.0, github.com/pinterest/ktlint.
7. JEP 484 ClassFile API, openjdk.org/jeps/484.
8. *KMP source set defaults*, kotlinlang.org/docs/multiplatform-hierarchy.html.
9. *Kotlin K2 compiler announcement*, kotlinlang.org/docs/k2-compiler-migration-guide.html.
10. *Kotlin sealed interfaces*, kotlinlang.org/docs/sealed-classes.html.
11. *Kotlin data objects*, kotlinlang.org/docs/object-declarations.html.
12. *Kotlin Multiplatform stability announcement*, kotlinlang.org/docs/multiplatform.html.
13. *Kotlin Native memory model*, kotlinlang.org/docs/native-memory-manager.html.
14. *Kotlin/Wasm overview*, kotlinlang.org/docs/wasm-overview.html.
15. *kotlinx.coroutines structured concurrency*, kotlinlang.org/docs/coroutines-basics.html.
16. *kotlinx.serialization documentation*, kotlinlang.org/docs/serialization.html.
17. *Gradle Kotlin DSL primer*, docs.gradle.org/current/userguide/kotlin_dsl.html.
18. *AGP 8.7 release notes*, developer.android.com/build/releases/gradle-plugin.
19. Source Map Revision 3 Proposal, sourcemaps.info/spec.html.
20. *Kotlin Symbol Processing (KSP)*, kotlinlang.org/docs/ksp-overview.html.
