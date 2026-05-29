---
title: "03. Prior-art Go bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "Survey of existing Go-to-other-language bridges (gopy, gomobile bind, gobind, swig, c-shared / c-archive build modes, purego, wazero, cgo-style FFI consumers like the JNA / JNI Go variants, and the proposed dotnet-go integration). What each gets right, what each requires the user to write, and what MEP-74 borrows."
---

# 03. Prior-art Go bridges

This note surveys the existing landscape of Go-to-other-language and other-language-to-Go bridges as of 2026-05. The bridge MEP-74 specifies draws design decisions from the strongest of these and explicitly rejects boilerplate patterns from the weakest. The survey covers ten bridges across two directions:

| Bridge | Direction | Boilerplate | ABI | Status |
|--------|-----------|-------------|------|--------|
| gopy | Go → Python | Low (annotation-free) | cgo + Python C-API | Active (last release 2024-Q4) |
| gomobile bind | Go → Java/Objective-C | None for the subset it supports; subset is restrictive | gomobile-native ABI | Active (Go team-maintained) |
| gobind | Go → mobile (gomobile's internal codegen) | None | Internal | Internal-only |
| swig (Go target) | C/C++ → Go | High (`.i` interface file per binding) | swig-managed C glue | Active but Go target is in maintenance |
| c-archive / c-shared | Go → C / any | Medium (the user writes `//export` directives) | C ABI | Stable since Go 1.5 (2015) |
| purego | Other → Go via dlopen without cgo | Medium (the user writes Go-side signatures) | dlopen | Active (first GA 2024) |
| wazero | Wasm host → wasm-compiled Go | None (the wasm module is self-describing) | wasm component model | Active, GA 2023 |
| JNI-Go | JNI ↔ Go | High (JNI requires per-method registration) | JVM ABI | Niche |
| dotnet-go | Go ↔ .NET | Medium (the user writes a `[GoExport]` annotation per method) | CLR P/Invoke | Experimental, Microsoft Research |
| cppgo | C++ → Go | High (a `.cppgo` interface file per binding) | cgo via swig | Niche |

The rest of this note walks each in turn, calls out what MEP-74 borrows, and what MEP-74 rejects.

## gopy (Go → Python)

[gopy](https://github.com/go-python/gopy), maintained since 2014, generates Python bindings for a Go package by parsing the package via `go/types` and emitting a cgo wrapper plus a CPython extension module. The user invokes `gopy pkg <module-path>` and gets a `.so` they can `import` from Python.

What gopy gets right:

- **`go/types` as the ingest source.** gopy was the bridge that established `go/types` as the right level of abstraction for Go-bridge ingest. MEP-74 borrows this entire decision.
- **Closed type-mapping table.** gopy has a fixed translation from Go scalars / strings / slices / maps to Python equivalents and refuses on out-of-table cases. MEP-74's table is structurally similar.
- **Wrapper package, not direct cgo emission.** gopy emits a sibling Go package (`pkg_go.go`) that calls into the source. MEP-74's `go_wrap/<module>/` directory is the same pattern.
- **No annotation requirement.** The user does not add `// gopy:export` markers; gopy reads the package's exported items as-is.

What gopy gets wrong (from MEP-74's perspective):

- **Python-specific.** The output is CPython-extension-only; the wrapper is not reusable from other consumers.
- **No version pinning.** gopy operates on whatever is in `GOPATH`. No semver constraint, no checksum, no capability declaration.
- **No async story.** Goroutine-spawning items are reflected to Python but the Python side has no idiomatic way to await them.

MEP-74 takes gopy's structural decisions (go/types ingest, closed table, sibling wrapper package, no annotations) and adds the missing pieces (version pin, checksum verification, capability declaration, publish path).

## gomobile bind (Go → Java/Objective-C)

[gomobile bind](https://pkg.go.dev/golang.org/x/mobile/cmd/gomobile), Go-team-maintained since 2015, generates `.aar` (Android) and `.framework` (iOS) bindings from a Go package. The user runs `gomobile bind -target=android <pkg>` and gets an Android library.

What gomobile bind gets right:

- **Self-describing output artifact.** The `.aar` / `.framework` carries the Go runtime inside, so consumers do not need a Go toolchain. MEP-74's `TargetGoLibrary` with `cgo-export = true` produces a similar self-contained artifact.
- **Closed subset of Go that the bridge supports.** gomobile bind explicitly documents what it can translate (a narrow subset of Go types) and refuses everything else. The refusal-is-information principle MEP-74 also adopts.

What gomobile bind gets wrong (from MEP-74's perspective):

- **The subset is too narrow.** No channels in exported positions. No goroutine callbacks. No interfaces with non-builtin types in method signatures. No generics. MEP-74's subset is materially wider because the Mochi-side ABI does not have to play nicely with Java's GC / iOS's ARC.
- **Build artefact is heavy.** The .aar / .framework bundles the Go runtime; the artifact is megabytes. The c-archive path is lighter for native-host consumers.

MEP-74 takes gomobile bind's "documented subset + refusal" decision and skips the heavyweight build-output side.

## gobind (gomobile's internal codegen)

[gobind](https://pkg.go.dev/golang.org/x/mobile/cmd/gobind) is the underlying codegen tool gomobile invokes. It reads a Go package and emits Java / Objective-C source plus Go cgo glue. The output of gobind is the input to gomobile's downstream native-toolchain invocation.

What gobind gets right:

- **`go/packages` ingest.** Same as gopy. Solid choice.
- **JSON intermediate format.** gobind serialises the resolved API surface as JSON before downstream codegen. MEP-74's `ApiSurface` JSON document is a similar pattern, except MEP-74's JSON is consumed by the Mochi-side bridge binary rather than by a downstream codegen step.

What gobind gets wrong:

- **Tightly coupled to gomobile.** gobind is not designed as a standalone tool; using it outside of gomobile requires hacky scripting.

MEP-74's `package3/go/cmd/go-ingest` is structurally what gobind is, but standalone and tailored to Mochi.

## swig (Go target)

[SWIG](https://www.swig.org), the venerable C/C++-to-many-languages bridge, has a Go target. The user authors a `.i` interface file describing which C/C++ items to expose and how, and swig emits Go cgo bindings.

What swig gets right (for its problem space):

- **Languages-agnostic.** swig targets ~20 languages from a single `.i` file.
- **Mature.** Two decades of production use.

What swig gets wrong (from MEP-74's perspective):

- **Required interface file.** swig requires the user to author `.i`. Boilerplate violation.
- **Direction is wrong.** swig goes C/C++ → Go, not Go → other. The MEP-74 problem is the other direction.
- **Go target is in maintenance.** As of 2026, swig's Go target hasn't seen a feature commit in 18 months.

MEP-74 does not borrow from swig.

## `c-archive` / `c-shared` (Go's native FFI build modes)

Since Go 1.5 (August 2015), the `go build` command has accepted `-buildmode=c-archive` (emit a `.a` static library plus a `.h` header) and `-buildmode=c-shared` (emit a `.so` / `.dylib` shared library plus a `.h`). The user adds `//export <Symbol>` directives above each Go function they want exposed; cgo handles the codegen.

What c-archive gets right:

- **First-class Go feature.** No external tool. `go build -buildmode=c-archive` is in the official Go toolchain.
- **Universal C ABI.** Any C-compatible consumer (Mochi, Rust via FFI, Python via ctypes, JNI, you name it) can link against the .a.
- **The Go runtime ships inside.** The c-archive includes the Go scheduler, GC, channel multiplexer. The consumer does not need a Go toolchain at consume time.
- **Stable for a decade.** Since 2015. Production-grade.

What c-archive requires the user to write:

- The `//export <Symbol>` directive per Go function. This is per-symbol boilerplate.
- The C-side header is generated, but the consumer still has to know the symbol naming convention.

MEP-74's wrapper package IS a c-archive with auto-generated `//export` directives. The bridge writes the directives so the user does not have to. The bridge picks the symbol naming convention (`mochi_go_<module>_<fn>`) so collisions are predictable. The bridge handles the consume-side linking via the MEP-54 build driver.

This is the single largest piece of prior art MEP-74 builds on. The entire bridge is a layer of auto-generated `//export`-laden Go source that sits on top of the c-archive primitive.

## purego (call into Go without cgo)

[purego](https://github.com/ebitengine/purego), a project from the Ebiten game engine team, lets non-Go code call into Go-written shared libraries (.so / .dylib) via dlopen, bypassing cgo entirely. The Go side must be compiled to `-buildmode=c-shared`; the non-Go side declares the Go-side function signatures in its own language and resolves them via dlsym.

What purego gets right:

- **Avoids cgo overhead.** Direct dlopen is ~50ns per call versus ~200ns for cgo.
- **Works on platforms where cgo is awkward.** iOS, certain WASM hosts.

What purego requires the user to write:

- The Go-side function signatures in the consumer's own language. Boilerplate.
- The consumer must dlopen the .so / .dylib at runtime. More than c-archive's static-link path.

MEP-74 evaluated purego as the consumer-side ABI and rejected it because:

- The consumer-side boilerplate violates the no-boilerplate promise.
- purego's GA is too recent (2024) to bet on for the v1 bridge.
- Most pkg.go.dev modules are pure Go (no `.so` / `.dylib` published); the consumer would have to compile them locally anyway, which is exactly what the c-archive path does.

A future MEP-74 v2 could add purego as an alternative consume path on platforms where cgo is unavailable (wasm-js). See [[12-risks-and-alternatives]] §A12.

## wazero (wasm host → Go wasm module)

[wazero](https://wazero.io), a pure-Go wasm runtime by the Tetrate team, lets a Go program load a wasm module and call its exports. Since Go 1.21 supports `GOOS=wasip1 GOARCH=wasm`, a Go module can be compiled to wasm and consumed via wazero.

What wazero gets right (for its problem space):

- **No cgo on the consume side.** Wazero is pure Go.
- **Sandboxed execution.** The wasm module cannot reach beyond the wasm-imports the host provides.
- **Cross-platform.** Works wherever Go runs.

What wazero gets wrong (from MEP-74's perspective):

- **WASI-only Go modules.** Not every Go module compiles to wasm-wasip1; modules using cgo or platform-specific syscalls don't.
- **Two-level translation.** Go → wasm → wasm-host → consumer. The Mochi-side ABI is wasm, not native, which adds cost.

MEP-74's wasm-wasip1 publish gate (phase 17) leans on wazero for the wasm-consume-side. The native-host consume path uses the c-archive route.

## JNI-Go variants

Several projects bridge Go to JVM via JNI: [jnigi](https://github.com/timob/jnigi), [Go-Java-Bridge](https://github.com/yourbasic/go-java-bridge), and others. These typically require the user to write JNI-style `RegisterNatives` boilerplate on the Java side and `import "C"` cgo on the Go side.

What JNI-Go bridges get right: JVM compatibility, mature platform.

What they get wrong: per-method registration boilerplate, JVM lifecycle management. Not relevant to MEP-74 except as a counterexample of what not to do.

## dotnet-go (Microsoft Research, experimental)

A 2025 Microsoft Research project that bridges Go to .NET via the CLR's P/Invoke surface. The user writes `[GoExport]` C# attributes on placeholder methods; the bridge resolves them at load time against a Go c-shared library.

What dotnet-go gets right: explicit annotations make the binding surface auditable on the C# side.

What MEP-74 takes: nothing directly. The bridge serves as a counterexample for the "explicit annotation" path that MEP-74 rejects.

## cppgo (C++ → Go via swig)

A swig-on-Go-target variant that adds a `.cppgo` interface file for C++ specifics. Niche.

What MEP-74 takes: nothing.

## What MEP-74 borrows by component

| Component | Borrows from | Decision |
|-----------|--------------|----------|
| Ingest source | gopy, gobind | `go/packages.Load` + `go/types` |
| Wrapper-vs-direct | gopy, c-archive | Synthesised sibling wrapper package |
| ABI primitive | c-archive (Go-stdlib) | `go build -buildmode=c-archive` |
| Symbol naming | none (MEP-74 invents) | `mochi_go_<module-path-hash>_<fn>` |
| Refusal-is-info | gomobile bind | Closed table + SkipReport |
| JSON intermediate | gobind | `ApiSurface` JSON document |
| Cross-platform wasm | wazero | Wasm-wasip1 consume path (phase 17) |

## What MEP-74 explicitly rejects

| Pattern | Source | Reason |
|---------|--------|--------|
| Required interface file | swig, cxx, uniffi | Boilerplate violation |
| Per-method registration | JNI | Boilerplate violation |
| `[GoExport]` attribute | dotnet-go | Boilerplate violation |
| Heavyweight artifact | gomobile bind | Cost too high for native-host consumers |
| dlopen-only consume | purego | Pre-GA; consumer boilerplate |
| Subset narrower than the closed type table | gomobile bind | Bridge promises wider coverage |

## Cross-references

- [[02-design-philosophy]] §3 for why wrapper-package wins over direct.
- [[04-go-doc-ast-ingest]] for the `go/packages` schema deep-dive.
- [[09-abi-stability]] for the c-archive ABI contract.
- [[11-tinygo-embedded-wasm]] for the wazero / wasm-wasip1 consume path.
- [MEP-74 §1](/docs/mep/mep-0074#1-pipeline-overview) for the pipeline this prior art informs.
