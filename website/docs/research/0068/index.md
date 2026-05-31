---
title: "MEP-68 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-68: language surface, design philosophy, prior-art .NET bridges, ECMA-335 assembly metadata ingest, the closed type-mapping table, the NuGet publish flow, NuGet OIDC trusted publishing, the async bridge, ABI stability, GC and memory management, NativeAOT subset and fallback, plus the risks and rejected alternatives register."
---

# MEP-68 research bundle

This bundle is the informative companion to [MEP-68](/docs/mep/mep-0068). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import dotnet "..."` import shape, the `mochi.toml` `[dotnet-dependencies]` + `[dotnet]` + `[dotnet.publish]` + `[dotnet.capabilities]` tables, the CLI surface (`mochi pkg add dotnet`, `mochi pkg publish --to=nuget.org`), and the per-import alias resolution semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why ECMA-335 binary metadata over Roslyn or source, why NativeAOT over CoreCLR hosting, why `[UnmanagedCallersOnly]` over P/Invoke, why the async bridge sits on `ManualResetEventSlim` + `ThreadPool`, why OIDC trusted publishing is the only publish path. |
| [03. Prior-art bridges](03-prior-art-bridges.md) | pythonnet, CppSharp, IKVM.NET, grpc-dotnet, NativeAOT interop experiments, swift-bridge analogues, uniffi for .NET (mozWinRT), ClangSharp, and the Unity IL2CPP pipeline. What each gets right, what each requires the user to write, and what MEP-68 borrows. |
| [04. Assembly metadata ingest](04-assembly-metadata-ingest.md) | The PE format, the CLI metadata root, the #~ compressed metadata stream, the TypeDef/MethodDef/FieldDef/PropertyDef/ParamDef/TypeSpec tables, type signature encoding from the #Blob heap, the XML documentation comment format, the Go-side parser shape, and the `NullableAttribute` detection for reference-type nullability. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed translation table, the refusal cases, the generic monomorphization rule, the `string` UTF-8 copy strategy, the `Task<T>`/`ValueTask<T>` async desugar, the `Nullable<T>` and nullable reference type handling, the `enum` integer-backed desugar, the `record` structural desugar, and the discriminated-union sealed-class-hierarchy pattern. |
| [06. NuGet publish flow](06-nuget-publish-flow.md) | The NuGet V3 protocol (registration endpoint, flat container endpoint, search endpoint), the `.nupkg` archive format (.nuspec + compiled assemblies + XML docs + icon), the `dotnet pack` invocation, the per-package metadata requirements, the sparse/flat container download path, and the publish-side gate. |
| [07. OIDC and NuGet trusted publishing](07-oidc-nuget-trusted-publishing.md) | The GitHub Actions `id-token: write` OIDC token, the nuget.org trusted publisher endpoint (GA November 2024), the token exchange flow, the package-owner claim verification, the Sigstore Fulcio + Rekor transparency log integration, the GitLab CI and Azure Pipelines paths, the `--dry-run` mock server harness, and the `--allow-apikey-fallback` transition flag. |
| [08. Async bridge](08-async-bridge.md) | The .NET Task-based async model vs. Mochi's colouring-only `async`, the `SynchronizationContext` deadlock hazard, the `ThreadPool.QueueUserWorkItem` + `ManualResetEventSlim` pattern, the `IAsyncEnumerable<T>` deferred path, the `CancellationToken` bridge, and the cost comparison against MEP-73's `tokio::block_on`. |
| [09. ABI stability](09-abi-stability.md) | `[UnmanagedCallersOnly]` guarantees and limitations, `[StructLayout(LayoutKind.Sequential)]` blittable struct pass-by-value, `GCHandle.Alloc` opaque handle strategy, `Marshal.AllocHGlobal` string buffers, the `nint` handle type on 32-bit and 64-bit platforms, static link vs. shared lib vs. CoreCLR hosting ABI comparison, and cross-RID ABI compatibility. |
| [10. GC and memory management](10-gc-and-memory.md) | The .NET tracing GC and object pinning, `GCHandle.Alloc(Normal)` vs. `GCHandle.Alloc(Pinned)`, the GCHandleTable per-process singleton, the Mochi-side `defer _free(handle)` discipline, the `ManualResetEventSlim` lifetime across the GC boundary, and the memory model assumptions NativeAOT makes when interoperating with non-.NET callers. |
| [11. NativeAOT subset and CoreCLR fallback](11-nativeaot-subset.md) | The NativeAOT compatibility surface (what works, what does not), the `RequiresDynamicCodeAttribute` and `ILLink.Descriptors` pre-check, the compatibility rate across the 25-package fixture corpus, the CoreCLR hosting API (`coreclr_initialize`, `coreclr_create_delegate`), the per-process CoreCLR instance model, the version-skew risk, and the phase-13 delivery plan for the fallback path. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (NativeAOT compatibility rate, ECMA-335 parser complexity, AOT compile time, four-RID matrix, GC handle leaks, nuget.org trusted publishing coverage, generic explosion, Windows cross-compilation, CoreCLR version skew, symbol name collisions, decimal precision, nullable annotation coverage) and the rejected alternatives register (bundled inspector binary, Roslyn as bind source, P/Invoke direction reversal, CppSharp/IKVM, gRPC local socket, WIT/componentize-dotnet, reference assemblies, long-lived API keys, value-type translation, dotnet-new templates, eager AOT compile). |

## Cross-references

- [MEP-68 spec](/docs/mep/mep-0068) — the normative document.
- [MEP-53](/docs/mep/mep-0053) — the Rust transpiler this bridge builds on.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-73](/docs/mep/mep-0073) — the Rust bridge whose bidirectional architecture, wrapper-synthesizer pattern, and GC handle strategy (adapted for .NET) MEP-68 mirrors.
- [Implementation tracking](/docs/implementation/0068/) — the per-phase delivery status.
