---
title: "MEP-68 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-68: language surface, design philosophy, prior-art .NET bridges, assembly metadata ingest, the closed type-mapping table, the NuGet publish flow, NuGet trusted publishing, the async bridge, ABI stability, generics and reification, NativeAOT and trimming, plus the risks and rejected alternatives register."
---

# MEP-68 research bundle

This bundle is the informative companion to [MEP-68](/docs/mep/mep-0068). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import dotnet "..."` import shape, the `mochi.toml` `[dotnet-dependencies]` + `[dotnet]` tables, the CLI surface (`mochi pkg add dotnet`, `mochi pkg publish --to=nuget.org`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why assembly metadata over C# source parsing, why a C# shim with `[UnmanagedCallersOnly]` over direct P/Invoke, why CLR hosting over NativeAOT as default, why NuGet trusted publishing is the only publish path, why the type-mapping table is closed not open. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | pythonnet, IKVM.NET, CsWin32, ClangSharp, uniffi, GraalVM polyglot, JNI-style manual bridging, SwiftUI + C headers, diplomat. What each gets right, what each requires the user to write, and what MEP-68 borrows or diverges from. |
| [04. Assembly metadata ingest](04-assembly-metadata-ingest.md) | The ECMA-335 metadata schema, `System.Reflection.Metadata.MetadataReader`, the `mochi-dotnet-meta` CLI tool design, the JSON output schema, the Go-side parser shape, and the per-package ingest fixtures. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed CLR-to-Mochi type translation table, the refusal cases, the generic monomorphisation rule, the nullable and `Task<T>` handling, and the `SkipReport` format. |
| [06. NuGet publish flow](06-nuget-publish-flow.md) | The nuget.org upload protocol, the `.nupkg` / `.nuspec` shape, the per-package metadata requirements, the NuGet v3 API, the `TargetDotNetLibrary` emit path, and the `mochi pkg publish` end-to-end flow. |
| [07. NuGet trusted publishing](07-nuget-trusted-publishing.md) | The NuGet trusted publishing OIDC flow (GA March 2024), the GitHub Actions `id-token: write` permission, the nuget.org publisher configuration, the verification path at install time, and the comparison to PyPI PEP 740 / Cargo RFC #3724. |
| [08. Async bridge](08-async-bridge.md) | The `Task<T>` synchronous dispatch via `.GetAwaiter().GetResult()`, CLR thread pool semantics, the deadlock prevention `ConfigureAwait(false)` pattern, the `async-mode = "task-parallel"` opt-in, and cancellation semantics. |
| [09. ABI stability](09-abi-stability.md) | `[UnmanagedCallersOnly]` guarantees, the CLR hosting function-pointer load path, the `MochiMarshal` type conventions, string and list round-trip encoding, opaque handle strategy for interface types, and the NativeAOT vs CLR hosting ABI difference. |
| [10. Generics and reification](10-generics-and-reification.md) | How .NET reified generics (`List<int>` vs `List<string>` as distinct CLR types) differ from Java's type-erased generics and Rust's monomorphised generics; the `[dotnet.monomorphise]` explicit instantiation model; and the combinatorial explosion risk. |
| [11. NativeAOT and trimming](11-nativeaot-and-trimming.md) | The NativeAOT compile path (`PublishAot=true`), the IL trimmer and AOT-compatibility metadata, the `IsAotCompatible` NuGet property, the packages in the 20-package corpus that pass and fail AOT trimming (April 2026 analysis), and the CLR startup cost trade-off. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (CLR hosting versioning, TFM mismatch, shim compile time, startup overhead, NuGet offline restore, NativeAOT compat, Windows-only packages, trusted publishing config, generic explosion, GetAwaiter deadlock, package signing, version conflicts) and the rejected alternatives (C# source parsing, XML documentation, NativeAOT default, direct P/Invoke, COM interop, GraalVM, IKVM, dotnet-embed, long-lived API keys, WIT, auto-monomorphise, TFM verbatim mirror). |

## Cross-references

- [MEP-68 spec](/docs/mep/mep-0068) — the normative document.
- [MEP-53](/docs/mep/mep-0053) — the Rust transpiler this bridge builds the emit pipeline analogue on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the Rust package bridge, whose architectural shape MEP-68 follows.
- [Implementation tracking](/docs/implementation/0068/) — the per-phase delivery status.
