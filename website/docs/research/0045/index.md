---
title: MEP-45 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-45 (Mochi-to-C transpiler). Twelve notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, portability, dataset pipeline, streams/agents, build system, testing gates, and risks."
---

# MEP-45 research notes

These twelve notes are the deep research that fed [MEP 45. Mochi-to-C transpiler](/docs/mep/mep-0045). They are informative; the MEP body is normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0045/language-surface) | Mochi language features the C target must lower |
| 02 | [Design philosophy](/docs/research/0045/design-philosophy) | Why C, why monomorphisation, why differential testing |
| 03 | [Prior-art transpilers](/docs/research/0045/prior-art-transpilers) | Nim, Crystal, Vala, OCaml, Roc, Koka, Lean 4, etc. |
| 04 | [Runtime building blocks](/docs/research/0045/runtime) | GC, allocator, coroutines, channels, strings, JSON, HTTP, time |
| 05 | [Codegen design](/docs/research/0045/codegen-design) | aotir IR, name mangling, decision trees, setjmp/longjmp |
| 06 | [Type-system lowering](/docs/research/0045/type-lowering) | Type-by-type mapping to C |
| 07 | [C target and portability](/docs/research/0045/c-target-portability) | Tier-1/2/3 triples, zig cc, ABI |
| 08 | [Dataset pipeline lowering](/docs/research/0045/dataset-pipeline) | Query DSL, joins, group_by, datalog |
| 09 | [Streams and agents](/docs/research/0045/agent-streams) | M:N scheduler, fibers, channels |
| 10 | [Build system](/docs/research/0045/build-system) | mochi build pipeline, cache layout |
| 11 | [Testing and CI gates](/docs/research/0045/testing-gates) | Differential testing vs vm3, sanitiser matrix |
| 12 | [Risks and alternatives](/docs/research/0045/risks-and-alternatives) | Risk register, alternatives rejected |

See [MEP 45](/docs/mep/mep-0045) for the normative specification and [implementation tracking](/docs/implementation/0045/) for per-phase progress.
