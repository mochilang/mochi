---
title: MEP-48 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-48 (Mochi-to-.NET/CLR transpiler). Twelve notes covering language surface, .NET design philosophy, prior art on .NET languages and AOT toolchains, BCL runtime, codegen IR (Roslyn + Reflection.Emit), type lowering, portability, dataset pipeline, agents/streams on Channels + async, build system, testing, and risks."
---

# MEP-48 research notes

These twelve notes are the deep research that fed MEP 48 (Mochi-to-.NET/CLR transpiler). They are informative; the MEP body, once landed, will be normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0048/language-surface) | Mochi features mapped onto .NET lowering obligations |
| 02 | [Design philosophy](/docs/research/0048/design-philosophy) | Why .NET, why .NET 8 LTS floor + .NET 10 LTS ceiling, why Roslyn as IR |
| 03 | [Prior-art transpilers](/docs/research/0048/prior-art-transpilers) | C#, F#, VB.NET, Roslyn, Reflection.Emit, ILGenerator, Mono, IL2CPP, NativeAOT, ReadyToRun, Bridge.NET, Blazor, MAUI, Unity, Godot |
| 04 | [Runtime building blocks](/docs/research/0048/runtime) | .NET 8 / 10 LTS, Channels, IAsyncEnumerable, BCL, System.Text.Json, ImmutableCollections, FrozenDictionary |
| 05 | [Codegen design](/docs/research/0048/codegen-design) | C# source via Roslyn SyntaxFactory + Reflection.Emit PersistedAssemblyBuilder fallback |
| 06 | [Type-system lowering](/docs/research/0048/type-lowering) | Type-by-type mapping to CLR reified generics, value types, sealed-record unions, Span/Memory |
| 07 | [.NET target and portability](/docs/research/0048/dotnet-target-portability) | TFM matrix, RID matrix, Mono/Unity/IL2CPP positions, MAUI/Blazor/Unity/Godot |
| 08 | [Dataset pipeline](/docs/research/0048/dataset-pipeline) | Query DSL lowering via LINQ + PLINQ, hash-join, value-type specialisation |
| 09 | [Agents and streams](/docs/research/0048/agent-streams) | Channels for mailboxes, async/await colouring, IAsyncEnumerable for streams |
| 10 | [Build system](/docs/research/0048/build-system) | dotnet CLI, NuGet, MSBuild, packages.lock.json, NativeAOT publish, single-file |
| 11 | [Testing gates](/docs/research/0048/testing-gates) | Per-phase Go test gates, TFM matrix (net8.0/net10.0), Roslyn-clean gate, NativeAOT gate |
| 12 | [Risks and alternatives](/docs/research/0048/risks-and-alternatives) | Risk register, F# / VB.NET / Bridge.NET / direct IL emit rejected and why |

Each note's filename uses the `NN-slug.md` convention; the leading `NN-` is stripped by Docusaurus for the URL path, so cross-links inside the notes use the unprefixed slug (e.g. `[[language-surface]]`).

The companion MEP body lives at [/docs/mep/mep-0048](/docs/mep/mep-0048).
