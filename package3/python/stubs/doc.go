// Package stubs implements MEP-71 Phase 3: PEP 561 stub discovery, typeshed
// pinning, the stubgen subprocess wrapper, and a focused `.pyi` parser.
//
// PEP 561 §"Stub-Only Packages" defines four sources of stubs for a Python
// package, in priority order:
//
//  1. Inline stubs. The package itself ships a `py.typed` marker file at the
//     package root and bundles `.pyi` files (or types in `.py`).
//  2. Sibling stubs. A separate distribution `<name>-stubs` (PEP 561 §"Stub-
//     only packages") ships under `<name>-stubs/` and is consulted ahead of
//     typeshed.
//  3. Typeshed. The community-maintained centralised stub repository at
//     https://github.com/python/typeshed. The bridge pins to a specific
//     typeshed commit so type metadata is reproducible across builds.
//  4. Stubgen fallback. When no other source is available the bridge invokes
//     `python -m mypy.stubgen` in a sandboxed venv to synthesise stubs from
//     the .py source. The output is marked as `partial = true` so the type
//     mapper (phase 4) refuses to emit wrappers for symbols without explicit
//     annotations.
//
// The Discovery type runs the 4-tier search and returns a StubSource carrying
// the file paths and the tier that produced them. Downstream phases (4-6) read
// the .pyi contents via the package's `Reader`.
//
// The .pyi reader is intentionally not a complete Python parser. It extracts
// the surface phase 4 needs (top-level classes, functions, type aliases,
// constants, imports) and treats type expressions as opaque strings. Phase 4
// parses those strings against the closed type-mapping table.
package stubs
