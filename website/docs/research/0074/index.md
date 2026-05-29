---
title: "MEP-74 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-74: language surface, design philosophy, prior-art Go bridges, go/packages + go/types ingest, the closed type-mapping table, the git-tag publish flow, the sum.golang.org checksum-DB integration, the goroutine bridge, ABI stability under cgo, Go interfaces and method sets, the TinyGo embedded subset, plus the risks and rejected alternatives register."
---

# MEP-74 research bundle

This bundle is the informative companion to [MEP-74](/docs/mep/mep-0074). It documents the design space the Go bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import go "<module>@<semver>" as <alias>` import shape, the `mochi.toml` `[go-dependencies]` + `[go]` tables, the CLI surface (`mochi pkg add go`, `mochi pkg publish --to=go+git+...`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why `go/packages` over alternatives, why a synthesised cgo wrapper package over direct cgo invocation, why no async runtime singleton is needed for Go, why sum.golang.org cross-check is mandatory by default. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | gopy, gomobile bind, gobind, swig, c-shared / c-archive build modes, purego, wazero, JNA / JNI Go variants, dotnet-go. What each gets right, what each requires the user to write, and what MEP-74 borrows. |
| [04. go/packages + go/types ingest](04-go-doc-ast-ingest.md) | The `go/packages.Load` flow, the `types.Type` discriminator tree, the stability story, why no nightly toolchain is needed, the Go-side helper binary shape. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed translation table, the refusal cases, the generic monomorphisation rule, the `string` vs `[]byte` parameter handling, the `error` desugar, the `chan` and `func` mappings. |
| [06. Go module publish flow](06-go-module-publish-flow.md) | The git-tag upload protocol, the canonical-import-path requirement, the module proxy's caching behaviour, the per-module metadata requirements, the publish-side gate. |
| [07. Sigstore and Go checksum DB](07-sigstore-go-checksumdb.md) | The sum.golang.org transparency log, the Merkle-tree consistency proof, the gosum-cosign workflow draft, the optional `<tag>.sig` sibling tag, the verification path at consume time. |
| [08. Goroutine bridge](08-goroutine-bridge.md) | The cgo boundary cost, the goroutine scheduler inside the c-archive, the channel-as-handle pattern, the callback-as-handle pattern, the cgo.Handle lifetime story. |
| [09. ABI stability](09-abi-stability.md) | The cgo `//export` ABI, the C-side ownership contract, the `runtime.KeepAlive` invariant, the `string` and slice round-trip, the c-archive vs c-shared decision, the cross-platform header story. |
| [10. Interfaces and method sets](10-interface-and-method-set.md) | Go's structural interface satisfaction, the method-set rules (value vs pointer receiver), how the bridge translates `type I interface { M() }` into Mochi extern types, the trait-object analogue. |
| [11. TinyGo and embedded](11-tinygo-embedded-wasm.md) | The TinyGo subset of pkg.go.dev, the no-cgo embedded path, the wasm-js and wasm-wasip1 surface, what kind of Go modules Mochi can consume on bare metal. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (cgo cost, GC interaction, generic explosion, proxy compromise, sumdb single-key trust) and the rejected alternatives (parse source directly, pkg.go.dev HTML, gomobile bind, plugin build mode, protobuf bridge, scheduler reimplementation). |

## Cross-references

- [MEP-74 spec](/docs/mep/mep-0074) — the normative document.
- [MEP-54](/docs/mep/mep-0054) — the Go transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the sister Rust bridge whose spec template MEP-74 mirrors.
- [Implementation tracking](/docs/implementation/0074/) — the per-phase delivery status.
