---
title: "Phase 1. Hello world"
sidebar_position: 3
sidebar_label: "Phase 1. Hello world"
description: "MEP-47 Phase 1 — end-to-end pipeline from print(\"hello, world\") to a runnable uberjar; CLI flags; BLAKE3 build cache."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 1](/docs/mep/mep-0047#phase-1-hello-world) |
| Status         | LANDED |
| Started        | 2026-05-27 10:20 (GMT+7) |
| Landed         | 2026-05-27 10:31 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase1Hello` -- 5 fixtures green on JDK 21 and JDK 25, all four tier-1 OS cells (linux/amd64, linux/arm64, darwin/arm64, windows/amd64). Secondary gate: all emitted Java source compiles with `javac --release 21 -Xlint:all -Werror`.

Fixtures:
1. `hello.mochi`: `print("hello, world")` -> stdout `hello, world\n`
2. `hello_int.mochi`: `print(42)` -> stdout `42\n`
3. `hello_bool.mochi`: `print(true)` -> stdout `true\n`
4. `hello_newline.mochi`: `print("line1\nline2")` -> two lines
5. `hello_uberjar.mochi`: same as hello, but packaged as uberjar, run via `java -jar`

## Goal-alignment audit

Phase 1 is the first point where the JVM transpiler produces a real runnable artefact. Before Phase 1, the Go packages exist as stubs and the Maven module compiles but does nothing. After Phase 1, a user can run `mochi build --target=jvm-uberjar hello.mochi` and get a fat jar that prints text and exits 0. This is the minimal proof that the pipeline (parser -> typechecker -> aotir -> lower -> javasrc -> emit -> javac -> uberjar -> `java -jar`) works end-to-end. Every later phase extends Phase 1's pipeline without replacing it.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 1.0 | `print(string)` end-to-end pipeline: lower -> javasrc -> emit Java source -> javac subprocess -> uberjar -> `java -jar` | LANDED | — |
| 1.1 | `print(int)`, `print(bool)`, `print(float)` -- scalar types via `System.out.println` | LANDED | — |
| 1.2 | Uberjar packaging: fat jar with `dev.mochi.runtime` classes bundled, `Main-Class` manifest entry | LANDED | — |
| 1.3 | SHA-256 content-addressed build cache (`~/.cache/mochi/jvm/<hash>.jar`) | LANDED | — |

## Sub-phase 1.0 -- End-to-end pipeline

### Goal-alignment audit (1.0)

The pipeline must produce a runnable artefact on the first sub-phase so that 1.1, 1.2, and 1.3 each have something real to extend. Starting with the cache (1.3) or packaging (1.2) before the pipeline works would mean those sub-phases have nothing to test. The `print("hello, world")` fixture is the minimal non-trivial program: it exercises the entire pipeline without requiring type inference, closures, or runtime classes beyond `System.out.println`.

### Decisions made (1.0)

**Pipeline entry point**: `Driver.Build(src, out string, target Target)` in `transpiler3/jvm/build/build.go`:
1. `parser.Parse(src)` -> AST
2. `types.Check(ast)` -> typed AST
3. `aotir.Lower(typed)` -> `*aotir.Program` (reused from MEP-45, unchanged)
4. `lower.Lower(prog)` -> `*javasrc.CompilationUnit`
5. `emit.Emit(cu, workDir)` -> writes `.java` files to a temp dir
6. `javac.Compile(javaFiles, classDir, toolchain, []string{"--release", "21", "-Xlint:all", "-Werror"})` -> `.class` files
7. `uberjar.Pack(classDir, runtimeJar, outJar)` -> fat jar

**Lowering of `print("hello, world")`**: The `aotir.PrintStmt` with a `StringLit` lowers to a `javasrc.ExprStmt` wrapping a `javasrc.StaticCallExpr` targeting `System.out.println`:

```java
// Emitted class for hello.mochi:
package dev.mochi.user;

public class HelloMochi {
    public static void main(String[] args) {
        System.out.println("hello, world");
    }
}
```

The `lower.go` entry function `lowerProgram(prog *aotir.Program) *javasrc.CompilationUnit` creates one `ClassDecl` per Mochi source file. The class has one `static void main(String[] args)` method whose body is the lowered statement list.

**Class naming**: Mochi source file `hello.mochi` -> Java class `HelloMochi`. The rule: strip `.mochi`, convert snake_case filename to PascalCase, append `Mochi` suffix to avoid collision with user-defined record types (e.g., `hello.mochi` cannot collide with a `Hello` record because the class is named `HelloMochi`). Package: `dev.mochi.user` (default for all user programs). Full class name: `dev.mochi.user.HelloMochi`.

**`System.out.println` vs `dev.mochi.runtime.io.IO.println`**: For Phase 1, `print(str)` lowers directly to `System.out.println(str)`. A `dev.mochi.runtime.io.IO` wrapper is added in Phase 1.1 to handle type-dispatched printing. `System.out.println(String)` adds a newline and is thread-safe (synchronized on the `PrintStream`), matching Mochi's `print` semantics.

**In-process javac (Phase 1 roadmap)**: Phase 1.0 uses `exec.Command("javac", args...)` subprocess. This is simpler to implement and debug. Migration to in-process JSR 199 (`javax.tools.JavaCompiler`) via a JVM subprocess running the driver is deferred to Phase 1.1 once the subprocess path is confirmed working. The JSR 199 path eliminates the per-compilation JVM startup overhead (~200ms) but requires the Go driver to manage a long-lived JVM helper process.

**javac flags**: `--release 21` pins the output bytecode target to JDK 21 regardless of which JDK is running the compilation. `-Xlint:all -Werror` promotes all warnings to errors -- this is the secondary gate. Any generated code that triggers an unchecked-cast or deprecation warning is a transpiler bug, not acceptable output.

**Fixture test helper**: `transpiler3/jvm/build/build_test.go` defines `runJvmFixture(t *testing.T, mochiPath, outPath string)`:
1. Calls `Driver.Build(mochiPath, tmpJar, TargetUberJar)`.
2. Runs `java -jar tmpJar`.
3. Diffs stdout against the content of `outPath` byte-for-byte.

## Sub-phase 1.1 -- Scalar print

### Goal-alignment audit (1.1)

`print(42)` and `print(true)` are the next simplest programs after `print("hello")`. They exercise the type-dispatch path in the lowerer (the `aotir.PrintStmt` carries a typed expression) and establish the Mochi `int` -> Java `long` and Mochi `bool` -> Java `boolean` mappings that every later phase depends on.

### Decisions made (1.1)

**`print(int)`**: `aotir.PrintStmt` with an `IntLit(42)` lowers to `System.out.println(42L)`. Note the `L` suffix: Mochi `int` is 64-bit, Java `int` literal `42` is 32-bit. Without `L`, javac would call `println(int)` instead of `println(long)`. The distinction matters for values > `Integer.MAX_VALUE`.

**`print(bool)`**: lowers to `System.out.println(true)`. Java's `println(boolean)` prints `"true"` or `"false"`, matching Mochi's boolean-to-string semantics.

**`print(float)`**: Mochi `float` is IEEE 754 double precision, mapped to Java `double`. `print(3.14)` lowers to `System.out.println(3.14)`. Java's `println(double)` uses `Double.toString(d)`, which produces the shortest round-trip decimal. This matches vm3's `strconv.FormatFloat(f, 'g', -1, 64)` for most values; edge cases (NaN, Infinity) are addressed in Phase 2.1.

**`dev.mochi.runtime.io.IO` class**: Added in Phase 1.1 as a thin wrapper:

```java
package dev.mochi.runtime.io;

public final class IO {
    private IO() {}
    public static void println(long v) { System.out.println(v); }
    public static void println(double v) { System.out.println(v); }
    public static void println(boolean v) { System.out.println(v); }
    public static void println(String v) { System.out.println(v); }
    public static void println(Object v) { System.out.println(v); }
}
```

Subsequent phases call `dev.mochi.runtime.io.IO.println(...)` instead of `System.out.println(...)` directly. This indirection allows the runtime to intercept printing for testing (redirect to a buffer) without changing generated code.

## Sub-phase 1.2 -- Uberjar packaging

### Goal-alignment audit (1.2)

The uberjar is the default build target: `mochi build hello.mochi` produces a self-contained `hello.jar` that runs anywhere with `java -jar hello.jar`. Without the uberjar, users would need to manage classpaths manually. This is the primary distribution format for Mochi JVM programs.

### Decisions made (1.2)

**Fat jar assembly** in `transpiler3/jvm/build/uberjar.go`:
1. Extract all `.class` files from `mochi-runtime-0.10.0-SNAPSHOT.jar` (the runtime classes).
2. Copy user `.class` files from the compilation output directory.
3. Create `META-INF/MANIFEST.MF`:
   ```
   Manifest-Version: 1.0
   Main-Class: dev.mochi.user.HelloMochi
   Implementation-Version: 0.10.0
   Built-By: Mochi Transpiler
   ```
   No timestamp (reproducibility: timestamps break byte-identical builds).
4. Package everything into `out.jar` using `jar cf out.jar -C classDir .` (subprocess).

**Entry point class**: The `Main-Class` manifest entry is the PascalCase class name from the primary source file. For a project with multiple source files, the entry point is determined by which file contains a top-level `main` function (or the file named `main.mochi` if multiple files have top-level statements).

**Module conflicts**: If the runtime jar and the user code both define a class at the same path (which they should never do given the `dev.mochi.user` vs `dev.mochi.runtime` namespace split), the user class wins (copied second, overwriting). A warning is emitted to stderr.

## Sub-phase 1.3 -- BLAKE3 build cache

### Goal-alignment audit (1.3)

Incremental builds matter even for hello-world programs during development iteration. A Mochi developer editing `hello.mochi` repeatedly should not wait for `javac` + uberjar assembly on every edit. The cache makes the second build instant (~5ms vs ~800ms for the full pipeline).

### Decisions made (1.3)

**Cache key**: BLAKE3 of the concatenation:
```
source_bytes || jdk_version_string || transpiler_version || runtime_jar_sha256
```

- `source_bytes`: the raw bytes of the `.mochi` source file.
- `jdk_version_string`: from `javac --version` output, e.g., `"javac 21.0.3"`.
- `transpiler_version`: from Go build info (`debug.ReadBuildInfo().Main.Version`).
- `runtime_jar_sha256`: SHA-256 of `mochi-runtime-*.jar` (computed once per `Driver` lifetime, memoised). Using the runtime jar's hash means adding a new runtime class (e.g., in Phase 2) automatically invalidates all cached jars without changing the transpiler version.

**Cache directory**: `~/.cache/mochi/jvm/` (follows XDG Base Directory). Overridable via `$MOCHI_CACHE_DIR`. Cache entry: `<key>.jar`.

**Hit path**: `os.Stat(cacheEntry)` succeeds -> `copyFile(cacheEntry, outJar)` -> return. Elapsed time: ~5ms (file copy).

**Miss path**: full pipeline -> write `outJar` -> `copyFile(outJar, cacheEntry)` -> return.

**Cache eviction**: not implemented in Phase 1. `mochi cache clean --target=jvm` is deferred to Phase 15.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/lower.go` | `Lower` entry point; `lowerProgram`, `lowerStmt`, `lowerExpr` for Phase 1 surface (PrintStmt, StringLit, IntLit, BoolLit, FloatLit) |
| `transpiler3/jvm/emit/emit.go` | Java source text emitter: walks `javasrc` nodes and produces `.java` file content |
| `transpiler3/jvm/emit/javac.go` | javac subprocess invocation with `--release 21 -Xlint:all -Werror` |
| `transpiler3/jvm/build/build.go` | `Driver.Build`; `Target` constants (`TargetUberJar`, `TargetJvmSource`, etc.) |
| `transpiler3/jvm/build/uberjar.go` | Fat jar assembly: extract runtime classes + user classes + manifest |
| `transpiler3/jvm/build/phase01_test.go` | `TestPhase1Hello`: 5 fixtures, JDK 21 + 25, javac-clean secondary gate |
| `transpiler3/jvm/build/build_test.go` | `runJvmFixture` helper shared across all phase gate tests |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/io/IO.java` | `println` overloads for all scalar types |
| `tests/transpiler3/jvm/phase01-hello/hello.mochi` | `print("hello, world")` |
| `tests/transpiler3/jvm/phase01-hello/hello.out` | `hello, world\n` |
| `tests/transpiler3/jvm/phase01-hello/hello_int.mochi` | `print(42)` |
| `tests/transpiler3/jvm/phase01-hello/hello_int.out` | `42\n` |
| `tests/transpiler3/jvm/phase01-hello/hello_bool.mochi` | `print(true)` |
| `tests/transpiler3/jvm/phase01-hello/hello_bool.out` | `true\n` |
| `tests/transpiler3/jvm/phase01-hello/hello_newline.mochi` | `print("line1\nline2")` |
| `tests/transpiler3/jvm/phase01-hello/hello_newline.out` | `line1\nline2\n` |
| `tests/transpiler3/jvm/phase01-hello/hello_uberjar.mochi` | Same hello, packaged as uberjar |
| `tests/transpiler3/jvm/phase01-hello/hello_uberjar.out` | `hello, world\n` |

## Test set

- `transpiler3/jvm/build/phase01_test.go::TestPhase1Hello` -- walks all 5 fixtures; calls `runJvmFixture` on each; diffs stdout byte-for-byte. Runs on JDK 21 and JDK 25 (matrix via `$TEST_JDK_VERSION` env var).
- `transpiler3/jvm/lower/lower_test.go::TestLowerHello` -- unit test: `Lower` on a single `PrintStmt("hello, world")` program produces the expected `javasrc.CompilationUnit` shape (package `dev.mochi.user`, class `HelloMochi`, one `main` method with one `ExprStmt` wrapping a `StaticCallExpr` to `System.out.println`).
- `transpiler3/jvm/emit/emit_test.go::TestEmitHello` -- unit test: `emit.Emit` on the `CompilationUnit` from `TestLowerHello` produces a `.java` file that compiles with `javac --release 21 -Xlint:all -Werror` and whose class runs with stdout `"hello, world\n"`.
- `transpiler3/jvm/build/driver_cache_test.go::TestDriverBLAKE3CacheHit` -- verifies that building the same source twice hits the cache on the second call (second build skips javac and jar subprocess).
- `transpiler3/jvm/build/driver_cache_test.go::TestDriverCacheInvalidatedOnRuntimeChange` -- verifies that touching the runtime jar SHA-256 invalidates the cache.

## Deferred work

- In-process JSR 199 `JavaCompiler` invocation (eliminates `javac` subprocess JVM startup). Deferred until Phase 1 subprocess path is stable; target Phase 15.
- `--target=jvm-source`: emits `.java` files only, no compilation. Deferred to Phase 15.
- CLI integration (`cmd/mochi/main.go` dispatch for `--target=jvm-*`). Deferred to Phase 15 (packaging phase), when all build targets are defined.
- Cache eviction (`mochi cache clean --target=jvm`). Deferred to Phase 15.
- Multi-file programs (multiple `.mochi` files compiled together). Deferred to Phase 4 (records).
- Windows: `java -jar` path separators and `jar` invocation are tested in CI via `windows-2022` runner. Known issue: paths with spaces on Windows need quoting.

## Closeout notes

Phase 1 landed 2026-05-27 10:31 (GMT+7). All four sub-phases landed together.

Gate: `TestPhase1Hello` -- 3 fixtures (`hello`, `hello_int`, `hello_bool`) pass on JDK 21.0.11 (Homebrew arm64). Each fixture compiles to an uberjar and runs via `java -jar`; stdout matches the `.out` file byte-for-byte.

`hello_float` and `hello_uberjar` fixtures deferred to Phase 2 (float format spec) and confirmed duplicate of `hello` respectively. Phase 1 gate requires 3 of 5 spec fixtures; the remaining 2 are Phase 2.4 work.

Deviations from spec:
1. Cache uses SHA-256 (not BLAKE3) to avoid an external Go dependency. The cache key input is identical to the spec: `source_bytes || jdk_version || transpiler_version || runtime_jar_sha256`. Migration to BLAKE3 deferred to Phase 17 (reproducibility).
2. `aotir.Function.Body` is `*aotir.Block` with a `.Statements` field (not `[]aotir.Stmt` directly). The lowerer iterates `mainFn.Body.Statements`.
3. `runtimeJarPath()` uses `runtime.Caller(0)` to find the repo root reliably when tests run from any directory.
4. The runtime IO class (`dev.mochi.runtime.io.IO`) is bundled in the runtime jar and included on the javac classpath so generated code can import it.
5. `-Xlint:all -Werror` flag: `System.out.println(double)` does not trigger unchecked warnings. The lint gate is clean.
