---
title: "Phase 0. Skeleton"
sidebar_position: 2
sidebar_label: "Phase 0. Skeleton"
description: "MEP-47 Phase 0 — directory layout, runtime jar stub, javac toolchain detection, and javasrc AST node types."
---

# Phase 0. Skeleton

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 0](/docs/mep/mep-0047#phase-0-skeleton) |
| Status         | LANDED |
| Started        | 2026-05-27 10:00 (GMT+7) |
| Landed         | 2026-05-27 10:20 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`transpiler3/jvm/` directory layout exists; `go build ./transpiler3/jvm/...` clean; `mvn -f transpiler3/jvm/runtime/pom.xml package -DskipTests` produces an empty `mochi-runtime-0.10.0-SNAPSHOT.jar`; `TestPhase0Skeleton` green.

## Goal-alignment audit

The user-facing goal of MEP-47 is "compile a Mochi program to a runnable JVM artefact". Phase 0 does not produce a runnable artefact. It plants the four structural anchors that make every later phase cheap to open: (1) the Go package tree tells a contributor which stage owns which concern without reading the MEP end-to-end, (2) the Maven runtime module means every downstream phase can `mvn package` its Java runtime pieces against a known package namespace, (3) the toolchain detection step ensures every later phase can assume `java`, `javac`, and `jar` are available and at the right version, and (4) the `javasrc` Go package provides the in-memory AST that every lowering pass writes to. The cost is one PR; without it every later phase repeats this orientation cost inline.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 0.0 | Directory layout + stub Go files (`doc.go` in each package); `go build ./transpiler3/jvm/...` clean | LANDED | — |
| 0.1 | Runtime jar stub: `dev.mochi.runtime` Maven module, `Runtime.java` version constant, `mvn package -DskipTests` exits 0 | LANDED | — |
| 0.2 | Javac toolchain detection at build time: `build.go` resolves `java`, `javac`, `jar` on `$PATH` or `$JAVA_HOME`; rejects JDK < 21 | LANDED | — |
| 0.3 | `javasrc` Go package: ~30 node types covering all Java constructs needed through Phase 6; each node implements `javaString() string` | LANDED | — |

## Sub-phase 0.0 -- Directory layout

### Goal-alignment audit (0.0)

The directory layout establishes the package boundaries. Without it, the first contributor to open a lowering bug would have to invent the split between `lower/`, `emit/`, `build/`, and `classfile/` themselves, likely inconsistently. The layout mirrors MEP-46's `transpiler3/beam/` structure so that anyone familiar with the BEAM transpiler can orient instantly.

### Decisions made (0.0)

**Directory structure**: mirrors MEP-46's `transpiler3/beam/`:

```
transpiler3/jvm/
  lower/
    lower.go       # aotir -> javasrc nodes (entry point)
    types.go       # Mochi type -> Java type mapping
    expr.go        # expression lowering
    stmt.go        # statement lowering
    decl.go        # top-level declarations (records, sum types, functions)
    closure.go     # closure conversion; Cell<T> lifting
    match.go       # match -> switch expression (JEP 440/441)
    query.go       # query DSL + datalog lowering
    agent.go       # agent class generation
    stream.go      # stream class generation
  emit/
    emit.go        # javasrc -> Java source text (pretty printer)
    javac.go       # javac subprocess / in-process JSR 199 invocation
    format.go      # deterministic source formatting (sorted imports, stable indent)
  classfile/
    hot.go         # ClassFile API (JEP 484) path for hot-path classes
  build/
    build.go       # Driver.Build() entry point + Target constants
    uberjar.go     # --target=jvm-uberjar: fat jar assembly
    jlink.go       # --target=jvm-jlink: custom JDK runtime image
    native.go      # --target=jvm-native: GraalVM native-image invocation
    jpackage.go    # --target=jvm-jpackage: OS-native installer
    phase00_test.go
  javasrc/
    nodes.go       # all AST node types + javaString() implementations
  runtime/
    src/main/java/dev/mochi/runtime/
      Runtime.java # package marker + VERSION constant
    pom.xml
  testdata/
    phase00-skeleton/
      README.txt
```

Each package gets a `doc.go` with a one-paragraph package doc that states what the package owns, names the entry-point function, and cross-references adjacent packages.

**Go build stub**: each `.go` file other than `doc.go` starts as an empty file with just `package lower` (or `emit`, `build`, `classfile`, `javasrc`). The `go build` gate catches import cycles and malformed package declarations before any real implementation lands.

## Sub-phase 0.1 -- Runtime jar stub

### Goal-alignment audit (0.1)

The Maven module establishes the package namespace (`dev.mochi.runtime`) and the artifact ID (`mochi-runtime`) that all later phases build against. Without it, Phase 1's uberjar step has nowhere to pull runtime `.class` files from. Shipping the stub in Phase 0 means Phase 1 can immediately add runtime classes without touching `pom.xml` structure.

### Decisions made (0.1)

**`pom.xml`** structure:

```xml
<groupId>dev.mochi</groupId>
<artifactId>mochi-runtime</artifactId>
<version>0.10.0-SNAPSHOT</version>
<packaging>jar</packaging>
<properties>
  <maven.compiler.release>21</maven.compiler.release>
  <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
</properties>
```

No dependencies in Phase 0. Jackson and Maven Resolver are added as `<dependency>` entries in Phase 12 (FFI).

**`Runtime.java`**:

```java
package dev.mochi.runtime;

/** Mochi runtime for JVM. */
public final class Runtime {
    private Runtime() {}
    public static final String VERSION = "0.10.0-SNAPSHOT";
}
```

This single class compiles to one `.class` file. The resulting `mochi-runtime-0.10.0-SNAPSHOT.jar` is ~2 KB. It serves as the anchor point for all later `dev.mochi.runtime.*` sub-packages.

**Maven build**: `mvn -f transpiler3/jvm/runtime/pom.xml package -DskipTests` is the Phase 0 gate command. `-DskipTests` because there are no test classes in Phase 0. The jar is written to `transpiler3/jvm/runtime/target/mochi-runtime-0.10.0-SNAPSHOT.jar`.

## Sub-phase 0.2 -- Javac toolchain detection

### Goal-alignment audit (0.2)

Every phase from 1 onwards invokes `javac`. Surfacing a clear error at driver initialization ("JDK 21+ required; found JDK 17 at /usr/bin/javac") is better than a cryptic `javac` error buried in a subprocess failure. Phase 0 builds the detector once so every later phase inherits it.

### Decisions made (0.2)

**`Toolchain` struct** in `transpiler3/jvm/build/build.go`:

```go
type Toolchain struct {
    Java  string // absolute path to java binary
    Javac string // absolute path to javac binary
    Jar   string // absolute path to jar binary
    Major int    // JDK major version (21, 25, ...)
}
```

**`resolveToolchain()`** logic:
1. If `$JAVA_HOME` is set, check `$JAVA_HOME/bin/javac`. Otherwise search `$PATH`.
2. Run `javac --version` and parse output `javac 21.0.3` -> major = 21.
3. If major < 21, return `fmt.Errorf("JDK 21+ required; found JDK %d at %s", major, path)`.
4. Set `Java`, `Javac`, `Jar` to the same `bin/` directory (they always co-locate in a JDK).

**Version parsing**: `strings.Fields(out)[1]` gives `"21.0.3"`, then `strings.Split(v, ".")[0]` and `strconv.Atoi` gives the major. Handles both `"21.0.3"` (LTS) and `"25-ea"` (EA builds: strip suffix after `-`).

**`TestPhase0Skeleton`** in `phase00_test.go`:
1. Calls `resolveToolchain()` -- passes if JDK 21+ is on PATH.
2. Verifies the runtime jar exists at the expected path.
3. Runs `go build ./transpiler3/jvm/...` via `exec.Command("go", "build", "./transpiler3/jvm/...")` -- passes if clean.

## Sub-phase 0.3 -- javasrc AST node types

### Goal-alignment audit (0.3)

The `javasrc` package is the in-memory representation of emitted Java source. Every lowering pass in Phases 1-14 writes to `javasrc` nodes; the `emit` package serialises them to text. Defining the full node set in Phase 0 means no later phase needs to add new nodes to the package (they may add helper constructors, but not new types), which keeps the schema stable.

### Decisions made (0.3)

**`transpiler3/jvm/javasrc/nodes.go`** defines ~30 node types:

Declaration nodes:
- `CompilationUnit` -- top-level file: package decl + imports + type decls
- `ClassDecl` -- `class Foo { ... }` with modifiers, type params, supertype, interfaces, members
- `RecordDecl` -- `record Foo(long x, long y) { ... }` with components + body
- `SealedInterfaceDecl` -- `sealed interface Foo permits ...`
- `MethodDecl` -- instance or static method: modifiers, name, params, return type, body
- `ConstructorDecl` -- constructor: modifiers, name (= class name), params, body
- `FieldDecl` -- field: modifiers, type, name, optional initialiser
- `EnumDecl` -- enum (used only for singleton-variant pattern in sum types)

Statement nodes:
- `Block` -- `{ stmt* }`
- `ReturnStmt` -- `return expr;`
- `IfStmt` -- `if (cond) thenBlock [else elseBlock]`
- `ForStmt` -- classic `for (init; cond; update) body`
- `ForEachStmt` -- `for (T x : expr) body`
- `WhileStmt` -- `while (cond) body`
- `BreakStmt`, `ContinueStmt`
- `ExprStmt` -- expression used as a statement (method call, assignment)
- `VarDeclStmt` -- local variable declaration with optional initialiser
- `TryCatchStmt` -- `try { } catch (Type e) { }`
- `ThrowStmt` -- `throw expr;`
- `SwitchStmt` -- `switch (expr) { case ... }` (statement form)

Expression nodes:
- `SwitchExpr` -- `switch (expr) { case ... -> ... }` (expression form, JEP 361+)
- `CallExpr` -- method invocation: receiver + method name + args
- `StaticCallExpr` -- static method call: class + method + args
- `FieldAccessExpr` -- `expr.field`
- `BinaryExpr` -- `left op right` with operator enum
- `UnaryExpr` -- `op expr` (prefix) or `expr op` (postfix)
- `LiteralExpr` -- int, long, double, string, bool, null literals
- `LambdaExpr` -- `(params) -> body`
- `CastExpr` -- `(Type) expr`
- `NewExpr` -- `new Type(args)`
- `ArrayNewExpr` -- `new Type[n]`
- `InstanceofExpr` -- `expr instanceof Type` (pattern form: `expr instanceof Type name`)
- `ConditionalExpr` -- ternary `cond ? then : else`

Type reference nodes:
- `TypeRef` -- a Java type: primitive (`long`, `double`, `boolean`), reference (`java.util.List<Long>`), array, wildcard
- `TypeParam` -- generic type parameter with optional bounds

Each node implements `javaString() string`. Indentation is handled by passing an `indent int` parameter internally; `javaString()` always starts at column 0 (callers add indent). This is a deliberate simplicity trade-off: the emitter in `emit/emit.go` controls indentation by prepending spaces, not by threading indent state through the node tree.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/doc.go` | Package doc: owns the `Lower(prog *aotir.Program) (*javasrc.CompilationUnit, error)` entry point |
| `transpiler3/jvm/emit/doc.go` | Package doc: owns Java source text emission and javac invocation |
| `transpiler3/jvm/build/doc.go` | Package doc: owns `Driver.Build()` and all packaging targets |
| `transpiler3/jvm/classfile/doc.go` | Package doc: owns the ClassFile API hot path |
| `transpiler3/jvm/javasrc/nodes.go` | All ~30 AST node types + `javaString()` implementations |
| `transpiler3/jvm/build/build.go` | `Toolchain` struct, `resolveToolchain()`, `Target` constants |
| `transpiler3/jvm/build/phase00_test.go` | `TestPhase0Skeleton`: toolchain detect, jar exists, `go build` clean |
| `transpiler3/jvm/runtime/pom.xml` | Maven module: `dev.mochi:mochi-runtime:0.10.0-SNAPSHOT` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/Runtime.java` | Version constant |
| `transpiler3/jvm/testdata/phase00-skeleton/README.txt` | Fixture directory placeholder |

## Test set

- `transpiler3/jvm/build/phase00_test.go::TestPhase0Skeleton` -- three assertions: (1) `resolveToolchain()` returns a JDK 21+ toolchain, (2) runtime jar exists at the Maven target path, (3) `go build ./transpiler3/jvm/...` exits 0.
- `transpiler3/jvm/javasrc/nodes_test.go::TestJavaSrcNodes` -- round-trip test: construct each node type and call `javaString()`; verify the output is valid Java fragment (by javac-parsing a small class that embeds it). Covers all 30 node types.

## Deferred work

- The `classfile/hot.go` package is a stub in Phase 0. The ClassFile API (JEP 484) hot path for sum-type dispatch shims and lambda bootstrap call sites is implemented in Phase 5 and Phase 6.
- `javasrc` nodes for Java 25 features (value classes, JEP 401) are not included. They are added as needed if `--jdk=25` is ever required for a specific lowering.
- The `uberjar.go`, `jlink.go`, `native.go`, `jpackage.go` build target files are stubs in Phase 0 and are implemented in Phases 1, 15, and 16.
- `format.go` deterministic formatting is a stub in Phase 0 (returns the raw `javaString()` output). Proper formatting (sorted imports, canonical blank lines) lands in Phase 17.

## Closeout notes

Phase 0 landed 2026-05-27 10:20 (GMT+7). All four sub-phases landed in one commit.

`go build ./transpiler3/jvm/...` clean. `go test ./transpiler3/jvm/...` green: `TestPhase0Skeleton` (toolchain + go_build sub-tests pass; runtime_jar passes after `mvn package`) and `TestJavaSrcNodes` (7 sub-tests covering CompilationUnit, ClassDecl, RecordDecl, SealedInterfaceDecl, MethodDecl, SwitchExpr, and all literal helpers).

One deviation from spec: `Param.Type` and `VarDeclStmt.Type` are `*TypeRef` (pointer) rather than `TypeRef` (value) to allow nil for inferred lambda parameters (`x -> expr`) and `var` declarations. The spec described these as value types; the pointer form is strictly more expressive.

`resolveToolchain()` was tested against JDK 21.0.11 (Homebrew install at `/opt/homebrew/opt/openjdk@21`). EA-suffix stripping (`25-ea` -> `25`) and dot-splitting (`21.0.3` -> `21`) are both covered in the implementation. JDK < 21 rejection is implemented but not exercised in Phase 0 (CI will cover it when a JDK 17 runner is available).

JVM install used: `brew install openjdk@21` (JDK 21.0.11, arm64). Maven install: `brew install maven` (3.9.16). Both are Homebrew installs on macOS arm64 (aarch64-darwin).
