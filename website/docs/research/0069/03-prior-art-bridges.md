---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "swift-bridge, UniFFI Swift backend, PyO3 analogues, cxx, swift-bindgen, Swig-Swift, ObjectMapper, DynamicCodable. What each gets right, what each requires the user to write, and what MEP-69 borrows."
---

# 03. Prior-art bridges

Seven prior-art systems bridge the Swift/C-compatible boundary. Each has a design centre; MEP-69 borrows from the best of each while addressing the remaining gaps.

## swift-bridge (chinedufn/swift-bridge)

**What it is.** A Rust crate that generates Rust and Swift glue code from a hand-written `#[swift_bridge::bridge]` module annotation. The user writes a Rust module that declares the functions and types to expose to Swift (or vice versa), and swift-bridge generates the `@_cdecl` Swift wrappers and the Rust `extern "C"` declarations.

**What it gets right.** swift-bridge proves that `@_cdecl` is the correct C-ABI mechanism for Swift interop; it handles String, `Vec<u8>`, `Option<T>`, and `Result<T, E>` via the same opaque-handle and out-pointer conventions MEP-69 adopts. It shows that synthesising the Swift wrapper (not hand-writing it) is the right approach for zero-boilerplate usage.

**What it requires the user to write.** A `#[swift_bridge::bridge]` module for every Rust type and function the user wants to expose to Swift. This is 5-50 lines of annotation per item. For importing an existing Swift package into Rust, the user would also need to write the corresponding bridge annotations for Swift-side items. This is exactly the boilerplate MEP-69 eliminates.

**What MEP-69 borrows.** The `@_cdecl` export pattern, the opaque-handle strategy for value types (wrapping `struct S` in an `UnsafeMutableRawPointer` owned by a `MochiHandle` class), the String C-ABI convention (`UnsafePointer<CChar>` + length), and the `_free` symbol convention for owned memory.

## UniFFI (Mozilla, Swift backend)

**What it is.** A multi-language binding generator from Mozilla that produces Swift, Kotlin, and Python bindings from a UDL (WASM Interface Definition Language-derived) `.udl` interface description file. The Swift backend generates a `<module>FFI.swift` file with `@_cdecl`-compatible entry points and a higher-level Swift API layer.

**What it gets right.** UniFFI's Swift backend proves that a full SwiftPM package can be auto-generated from a description: it emits a complete `Package.swift`, a `Sources/` tree with the generated Swift glue code, and a `Checksums/` directory for XCFramework binary verification. The `Checksums/` pattern (storing SHA-256 of the binary XCFramework in the `Package.swift` `binaryTarget` declaration) is adopted by MEP-69's XCFramework publish path.

**What it requires the user to write.** A `.udl` file describing every type and function to expose. For importing an existing Swift package, the user would have to write a UDL describing the Swift package's API, which is as much boilerplate as writing `@objc` annotations. UniFFI is excellent for Rust → Swift direction but not for Swift → Mochi direction without a UDL auto-generator.

**What MEP-69 borrows.** The SwiftPM package layout for the synthesised wrapper, the `Checksums/` XCFramework verification pattern, the `MochiMarshal.swift`-style shared helper file convention, and the `DispatchGroup`-bounded async dispatch pattern for synchronous wrapper entry points.

## PyO3 / neon / napi-rs (analogue pattern)

**What they are.** FFI bridge generators for Python, Node.js, and Deno respectively. Each allows Rust code to expose functions to a scripting runtime with zero boilerplate on the Rust side (the annotations are on the Rust functions, not the Python/JS code).

**What they get right.** The "annotate the host language, not the guest language" principle: the Rust programmer writes `#[pyfunction]` (PyO3) or `#[napi]` (napi-rs) and the scripting side needs no FFI code at all. MEP-69 cannot adopt this exactly (the Swift package author does not write Mochi annotations), but the synthesised wrapper achieves the same effect from the Mochi user's perspective: the Mochi code needs no FFI annotations.

**What MEP-69 borrows.** The "zero annotations on the guest side" user promise; the pattern of generating both a compiled wrapper and a scripting-language shim file from a single description source; the refusal-with-SkipReport model (analogous to PyO3's `pyo3::PyErr` for untranslatable types).

## cxx (dtolnay/cxx)

**What it is.** A Rust crate that generates type-safe C++ ↔ Rust bindings from a `#[cxx::bridge]` module. It is the inspiration for swift-bridge and demonstrates that a hand-authored bridge module can be concise and type-checked.

**What MEP-69 explicitly rejects from cxx.** The hand-authored bridge module. MEP-69's promise is zero boilerplate; cxx requires the user to write a bridge definition for every item. See [[12-risks-and-alternatives]] §A3.

**What MEP-69 borrows.** The `Result<T, E>` → status-code-plus-out-pointer ABI pattern, the concept of a "safe" bridge that refuses unsafe items unless explicitly opted in, and the idiom of generating both a Rust side and a counterpart side from a single source.

## swift-bindgen (nickelc/swift-bindgen)

**What it is.** An experimental tool that reads a C header and generates Swift `@_cdecl`-compatible bindings, analogous to `bindgen` (Rust's C header reader). Not widely adopted as of 2026.

**What MEP-69 borrows.** The concept of driving wrapper generation from a machine-readable API description (C header for bindgen, `.swiftinterface` for MEP-69). The one-pass "read description → emit wrapper" architecture.

## SWIG-Swift

**What it is.** The Swift backend for SWIG (Simplified Wrapper and Interface Generator), which generates Swift wrappers for C and C++ libraries from SWIG `.i` interface files.

**What it requires.** A `.i` interface file describing the C/C++ API. MEP-69 faces the reverse problem (wrapping Swift for Mochi) so SWIG-Swift is not directly applicable, but its experience shows that interface-file-driven generators are maintenance-heavy when the underlying API changes.

**What MEP-69 borrows.** The lesson: avoid interface files. Use the compiler's own machine-readable output (`.swiftinterface`) as the authoritative description to avoid the drift between interface file and implementation.

## ObjectMapper / Codable (Swift reflection patterns)

**What they are.** Swift libraries that use `Mirror` (Swift's runtime reflection API) to map between Swift types and JSON. Not FFI bridges per se, but they demonstrate how Swift's type system can be introspected at runtime.

**What MEP-69 borrows.** The observation that Swift runtime reflection (`Mirror`) is limited to checking properties and their values, not full type signatures. `Mirror` cannot tell you that a function takes `async` parameters or returns `some P`; `.swiftinterface` can. This reinforces the `.swiftinterface` bind-source choice.

## Summary table

| System | Bind source | Boilerplate | Zero-annotation import? | Direction |
|--------|------------|-------------|------------------------|-----------|
| swift-bridge | Hand-written `#[swift_bridge::bridge]` | 5-50 lines/item | No | Bidirectional (Rust↔Swift) |
| UniFFI (Swift) | Hand-written `.udl` | 3-20 lines/item | No | Bidirectional (Rust↔Swift/Kotlin/Python) |
| SWIG-Swift | Hand-written `.i` file | 5-100 lines/item | No | C/C++ → Swift |
| swift-bindgen | C header | Auto-generated | Yes (C side only) | C → Swift |
| PyO3 analogue | `#[pyfunction]` annotations on Rust | 1 annotation/item | Yes (Python side) | Rust → Python |
| **MEP-69** | `.swiftinterface` (compiler-generated) | Zero | Yes | Bidirectional (Swift↔Mochi) |

MEP-69 is the first bridge in this space to use the compiler's own interface format as the bind source with zero annotations on either side.

## Cross-references

- [[02-design-philosophy]] §1 for why `.swiftinterface` was chosen over the alternatives each tool uses.
- [[04-swiftinterface-ingest]] for the parser that reads `.swiftinterface`.
- [[12-risks-and-alternatives]] for the rejected alternative bridges.
