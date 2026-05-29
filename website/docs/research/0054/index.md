---
title: MEP-54 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-54 (Mochi-to-Go transpiler). Twelve notes covering language surface, Go design philosophy, prior art on source-to-Go transpilers and ecosystem tooling, runtime module building blocks, codegen via the structural gotree AST, type lowering, Go portability matrix, dataset pipeline, agents and streams via native goroutines and channels, go build system, testing gates, and risks."
---

# MEP-54 research notes

These twelve notes are the deep research that fed MEP-54 (Mochi-to-Go transpiler). They are informative; the MEP body at [/docs/mep/mep-0054](/docs/mep/mep-0054) is normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

Author: research pass for MEP-54 (Mochi-to-Go transpiler).
Date: 2026-05-29 (GMT+7).

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0054/language-surface) | Mochi features mapped onto Go 1.21+ lowering obligations |
| 02 | [Design philosophy](/docs/research/0054/design-philosophy) | Why Go, why the 1.21 floor, why native goroutines, why `gotree` not `go/ast`, why the runtime module is published |
| 03 | [Prior-art transpilers](/docs/research/0054/prior-art-transpilers) | GopherJS, TinyGo, Joy, gccgo, gollvm, esc.go, JavaPoet analogues, the Kubernetes code generators, `golang.org/x/tools/cmd/stringer`, `kube-openapi`, sqlc, and the rejected idea of using `go/ast` |
| 04 | [Runtime building blocks](/docs/research/0054/runtime) | The `dev.mochilang/runtime/go` module: agent, collections, datalog, llm, option, query, result, stream, stringz, timez. Why no third-party deps in the default build |
| 05 | [Codegen design](/docs/research/0054/codegen-design) | aotir-to-Go lowering via the structural gotree AST, rendering through `go/format`, the closure-environment lifting pass, monomorphisation, stable item ordering |
| 06 | [Type-system lowering](/docs/research/0054/type-lowering) | Mochi types onto int64 / float64 / string / `[]T` / `map[K]V` / `map[T]struct{}` / struct / discriminated interface + variant structs / `func(...)` |
| 07 | [Go target and portability](/docs/research/0054/runtime-portability) | Go 1.21 / 1.26 matrix, the GOOS+GOARCH cross-compile story, the wasm/js vs wasip1 split, vendor-vs-proxy mode, why no TinyGo for the default path |
| 08 | [Dataset pipeline](/docs/research/0054/dataset-pipeline) | Query DSL lowering via straight-line for loops + `slices` stdlib helpers, hash/merge joins, group-by, top-K via `container/heap`, Datalog semi-naive evaluation at compile-time |
| 09 | [Agents and streams](/docs/research/0054/agent-streams) | Mochi agents as a goroutine wrapping a `chan Msg` receive loop, streams as a struct of `[]chan T` subscriber slots, async colouring as a typecheck pass with `go` statement boundaries |
| 10 | [Build system](/docs/research/0054/build-system) | `go build` driver, the `Driver.Build` cache + sandbox, cross-compile invocation, deterministic flags (`-trimpath`, `-buildvcs=false`, `-ldflags=-buildid=`, `SOURCE_DATE_EPOCH=0`), pkg.go.dev publication |
| 11 | [Testing gates](/docs/research/0054/testing-gates) | Per-phase Go test gates with vm3 oracle, byte-equal stdout diff, `go vet` secondary gate, reproducibility SHA-256 gate, publish dry-run via `go mod tidy` + `go install` |
| 12 | [Risks and alternatives](/docs/research/0054/risks-and-alternatives) | Risk register (macOS LC_UUID, wasm-js no fetch glue, wasip1 no cgo, cassette drift, generic-method gap, hermetic-build gap, vet false positives) + rejected alternatives (`go/ast`, `sync.Mutex` channels, TinyGo default, inlined runtime, no published module, green-thread library, raw `go/printer`) |

Each note's filename uses the `NN-slug.md` convention; the leading `NN-` is stripped by Docusaurus for the URL path, so cross-links inside the notes use the unprefixed slug (e.g. `[[language-surface]]`).

The companion MEP body lives at [/docs/mep/mep-0054](/docs/mep/mep-0054). Implementation tracking lives at [/docs/implementation/0054/](/docs/implementation/0054/).
