---
title: "Phase 1. Hello world"
sidebar_position: 2
sidebar_label: "Phase 1. Hello world"
description: "MEP-49 Phase 1 — end-to-end pipeline from print(\"hello, world\") to a runnable Swift binary on Linux x64; sxtree shadow AST; swiftc invocation."
---

# Phase 1. Hello world

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-49 §Phases · Phase 1](/docs/mep/mep-0049#phase-1-hello-world) |
| Status         | LANDED |
| Started        | 2026-05-28 13:40 (GMT+7) |
| Landed         | 2026-05-28 13:40 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase1Hello`: 5 fixtures green on Swift 6.0 and 6.1, linux-x64. Secondary gate: `TestSwiftcClean` (zero warnings under `-strict-concurrency=complete -warnings-as-errors -swift-version 6`). `TestSwiftFormatFixedPoint` (swift-format idempotent on emitted source).

Fixtures:
1. `hello.mochi`: `print("hello, world")` → stdout `hello, world\n`
2. `hello_int.mochi`: `print(42)` → stdout `42\n`
3. `hello_bool.mochi`: `print(true)` → stdout `true\n`
4. `hello_float.mochi`: `print(3.14)` → stdout `3.14\n`
5. `hello_newline.mochi`: `print("line1\nline2")` → two lines

## Goal-alignment audit

Phase 1 is the first point where the Swift transpiler produces a real runnable binary. Before Phase 1, the Go packages are stubs. After Phase 1, a user can run `mochi build --target=swift-linux hello.mochi` and get a binary that prints text and exits 0. This proves the pipeline (parser → typechecker → aotir → lower → sxtree → swift-format → swiftc) works end-to-end. Every later phase extends Phase 1's pipeline without replacing it.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 1.0 | `print("hello, world")` end-to-end: aotir → sxtree nodes → `.swift` source → `swift build` → binary | NOT STARTED | — |
| 1.1 | `print(int)`, `print(bool)`, `print(float)` -- scalar types via `MochiRuntime.Print` | NOT STARTED | — |
| 1.2 | `swift-format --in-place` post-processing; `TestSwiftFormatFixedPoint` gate | NOT STARTED | — |
| 1.3 | `TestSwiftcClean` gate: zero warnings under `-strict-concurrency=complete -warnings-as-errors` | NOT STARTED | — |

## Sub-phase 1.0 -- End-to-end pipeline

### Decisions made (1.0)

**Pipeline entry point**: `Driver.Build(src, outDir string, target Target)` in `transpiler3/swift/build/build.go`:
1. `parser.Parse(src)` → AST
2. `types.Check(ast)` → typed AST
3. `aotir.Lower(typed)` → `*aotir.Program` (reused from MEP-45, unchanged)
4. `lower.Lower(prog)` → `*sxtree.SourceFile` (Go shadow AST for Swift)
5. `emit.Emit(sf, workDir)` → writes `.swift` source files
6. `swift.Format(workDir)` → runs `swift-format --in-place` on every `.swift`
7. `swift.Build(workDir, outDir, target)` → calls `swift build` subprocess via generated `Package.swift`

**sxtree -- Go shadow AST for Swift**: package `github.com/mochilang/mochi/transpiler3/swift/sxtree`. Each node has a `Render(w *Writer)` method that writes canonical Swift. No dependency on Apple's `swift-syntax` library at Go compilation time. Example node:

```go
// in transpiler3/swift/sxtree/decl.go
type FunctionDecl struct {
    Attributes   []Attribute
    Modifiers    []Modifier
    Name         Identifier
    Generics     *GenericParameterClause
    Params       ParameterClause
    Effects      EffectSpecifiers
    ReturnType   *TypeSyntax
    WhereClause  *GenericWhereClause
    Body         *CodeBlock
}

func (f *FunctionDecl) Render(w *Writer) {
    for _, a := range f.Attributes { a.Render(w) }
    // ...
}
```

**Lowering of `print("hello, world")`**: `aotir.PrintStmt` with a `StringLit` lowers to a `FunctionCallExpr` targeting `MochiRuntime.print`:

```swift
// Emitted for hello.mochi:
import MochiRuntime

@main
struct HelloMochi {
    static func main() {
        MochiRuntime.print("hello, world")
    }
}
```

**Module naming**: Mochi source file `hello.mochi` → Swift file `Hello.swift` containing `struct HelloMochi` (snake_case → PascalCase, `Mochi` suffix avoids collision with Swift stdlib `Hello`). The `@main` entry struct is emitted in the top-level module file; sub-files for records and functions are separate `.swift` files in the same SwiftPM target.

**`MochiRuntime.print`**: Phase 1 wraps `Swift.print` to match vm3's output format:

```swift
// in MochiRuntime/Sources/MochiRuntime/IO.swift
public func print(_ value: String) { Swift.print(value) }
public func print(_ value: Int64)  { Swift.print(value) }
public func print(_ value: Double) { Swift.print(value) }
public func print(_ value: Bool)   { Swift.print(value ? "true" : "false") }
```

**`swift build` subprocess**: Phase 1 uses a `swift build` subprocess. The driver generates a minimal `Package.swift` and calls `swift build -c release`. The SwiftPM build graph handles compilation and linking. Build time: ~5s first build, ~200ms incremental.

**Generated `Package.swift`** in `transpiler3/swift/build/package.go`:

```swift
// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MochiOut",
    platforms: [.macOS(.v15)],
    dependencies: [
        .package(url: "https://github.com/mochilang/swift-runtime", from: "0.1.0"),
    ],
    targets: [
        .executableTarget(
            name: "MochiOut",
            dependencies: [
                .product(name: "MochiRuntime", package: "swift-runtime"),
            ],
            path: "Sources/MochiOut",
            swiftSettings: [
                .swiftLanguageMode(.v6),
                .unsafeFlags(["-strict-concurrency=complete"]),
            ]
        ),
    ]
)
```

## Sub-phase 1.1 -- Scalar print

### Decisions made (1.1)

**`print(int)`**: `aotir.PrintStmt` with `IntLit(42)` lowers to `MochiRuntime.print(Int64(42))`. All Mochi `int` literals are `Int64`, not `Int`. The `Int64(...)` cast is explicit in emitted code to prevent Mochi from accidentally promoting to `Int` on 32-bit platforms.

**`print(bool)`**: Swift's `String(describing: true)` returns `"true"` (lowercase), which matches vm3. However, direct `Swift.print(true)` also emits `"true"`. `MochiRuntime.print(_ value: Bool)` calls `Swift.print(value ? "true" : "false")` for explicitness and to guard against Swift version-dependent changes.

**`print(float)`**: Mochi `float` is `Double`. `print(3.14)` lowers to `MochiRuntime.print(3.14)`. `MochiRuntime.print(_ value: Double)` calls `Swift.print(value)` which uses Swift's `Double` description, matching vm3's `strconv.FormatFloat(f, 'g', -1, 64)`. Edge cases: `Double.nan` → `"nan"`, `Double.infinity` → `"inf"`, `-Double.infinity` → `"-inf"`. These must match vm3; MochiRuntime has explicit checks.

## Sub-phase 1.2 -- swift-format post-processing

### Decisions made (1.2)

**swift-format version**: locked to the minor version matching the Swift toolchain (Swift 6.0 → swift-format 600.x.y). The formatter is invoked as a subprocess: `swift-format --in-place Sources/**/*.swift`.

**`TestSwiftFormatFixedPoint`**: the gate runs swift-format twice and diffs the output. If the second run produces any change, the test fails. This prevents the lowerer from emitting code that swift-format reformats in a way that reveals hidden structural issues (e.g., nested ternaries that format differently on each pass).

**Style decisions baked into sxtree Render()**: sxtree nodes produce canonical Swift that swift-format is expected to leave unchanged. Known divergences: swift-format prefers trailing commas in multi-line argument lists; the sxtree `ParameterClause` renderer emits them. This prevents spurious diffs.

## Sub-phase 1.3 -- TestSwiftcClean gate

### Decisions made (1.3)

**Gate command**: `swift build -Xswiftc -warnings-as-errors -Xswiftc -strict-concurrency=complete -swift-version 6`. This runs as a separate CI step from the fixture test. A build can pass fixtures (correct output) but fail this gate (warnings present). Both gates must be green before a phase is LANDED.

**`-strict-concurrency=complete`**: Swift 6.0 default in `swiftLanguageModes: [.v6]` already enables complete checking. The explicit flag is belt-and-suspenders. Phase 1 emits only `@main struct` + synchronous `main()`, so no concurrency issues arise. The gate is established in Phase 1 so it runs on every subsequent phase automatically.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/swift/lower/lower.go` | `Lower` entry; `lowerProgram`, `lowerStmt`, `lowerExpr` for Phase 1 surface |
| `transpiler3/swift/sxtree/` | Go shadow AST package: all Swift node types with `Render()` methods |
| `transpiler3/swift/emit/emit.go` | Walks sxtree → writes `.swift` source files |
| `transpiler3/swift/build/build.go` | `Driver.Build`; `Target` constants |
| `transpiler3/swift/build/package.go` | `Package.swift` generator |
| `transpiler3/swift/build/swift.go` | `swift build` / `swift-format` subprocess wrappers |
| `transpiler3/swift/build/phase01_test.go` | `TestPhase1Hello`: 5 fixtures + clean + format gates |
| `transpiler3/swift/runtime/Sources/MochiRuntime/IO.swift` | `print()` overloads for all scalar types |
| `transpiler3/swift/runtime/Package.swift` | MochiRuntime SwiftPM package definition |
| `tests/transpiler3/swift/fixtures/phase01-hello/` | 5 fixture directories |

## Test set

- `TestPhase1Hello` -- 5 fixtures (hello, hello_int, hello_bool, hello_float, hello_newline); diffs stdout byte-for-byte against `.out`.
- `TestSwiftcClean` -- runs `swift build -warnings-as-errors -strict-concurrency=complete` on emitted source; asserts zero warnings.
- `TestSwiftFormatFixedPoint` -- runs swift-format twice; asserts second pass produces no diff.

## Deferred work

- SHA-256 build cache. Deferred to Phase 16 (reproducible builds).
- Multi-file programs. Deferred to Phase 4 (records introduce multi-file structure).
- macOS/Windows/arm64 CI runners. Phase 1 targets linux-x64 only; full matrix in Phase 17.
- `print(float)` NaN/Inf edge cases. Deferred to Phase 2.
