---
title: "MEP-54 research bundle: Mochi to Go transpiler"
description: "Twelve research notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, target portability, dataset pipeline, agents and streams, build system, testing gates, and risks for the Mochi-to-Go transpiler proposed in MEP-54."
sidebar_position: 54
sidebar_label: "MEP-54"
---

# MEP-54 research bundle: Mochi to Go transpiler

Author: research pass for MEP-54 (Mochi to Go transpiler).
Date: 2026-05-29 (GMT+7).

This bundle contains the twelve research notes that informed [MEP-54, the Mochi-to-Go transpiler](/docs/mep/mep-0054). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0054/language-surface) | Mochi features mapped onto Go 1.22+ lowering obligations |
| 02 | [Design philosophy](/docs/research/0054/design-philosophy) | Why Go, why goroutine agents, why no generics for sum types, why CGo |
| 03 | [Prior-art transpilers](/docs/research/0054/prior-art-transpilers) | GopherJS, Yaegi, Tengo, CUE, Pkl, HCL, transpilers targeting Go in the wild |
| 04 | [Runtime building blocks](/docs/research/0054/runtime) | Go stdlib, goroutines, channels, net/http, encoding/json, sync, context |
| 05 | [Codegen design](/docs/research/0054/codegen-design) | Go source via gotree IR (not go/ast), aotir reuse, gofmt-compatible indent |
| 06 | [Type-system lowering](/docs/research/0054/type-lowering) | Mochi types onto int64/float64/string/[]T/map[K]V/struct/tagged union/func |
| 07 | [Go target and portability](/docs/research/0054/go-target-portability) | GOOS/GOARCH matrix, CGo cross, GOOS=wasip1 wasm, static linking, -trimpath |
| 08 | [Dataset pipeline](/docs/research/0054/dataset-pipeline) | Query DSL via range pipeline, BTreeMap deterministic group-by, compile-time Datalog |
| 09 | [Agents and streams](/docs/research/0054/agent-streams) | Goroutine agents, buffered channel mailbox, fan-out broadcast, context cancellation |
| 10 | [Build system](/docs/research/0054/build-system) | go module, go.sum, SOURCE_DATE_EPOCH=0 + -trimpath, CGo cross-compile, wasmtime |
| 11 | [Testing gates](/docs/research/0054/testing-gates) | Per-phase Go test gates, vm3 oracle, byte-equal stdout, reproducibility SHA-256 |
| 12 | [Risks and alternatives](/docs/research/0054/risks-and-alternatives) | CGo cross-compile complexity, goroutine leak risk, wasm net/http stub, cassette drift |

The companion MEP body lives at [/docs/mep/mep-0054](/docs/mep/mep-0054). Implementation tracking lives at [/docs/implementation/0054/](/docs/implementation/0054/).
