---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why ECMA-335 binary metadata over Roslyn, why NativeAOT over CoreCLR hosting, why [UnmanagedCallersOnly] over P/Invoke, why the async bridge uses ManualResetEventSlim + ThreadPool, and why OIDC trusted publishing is the only publish path."
---

# 02. Design philosophy

MEP-68 makes five architectural commitments that are not obvious from first principles. This note justifies each one.

## §1. ECMA-335 binary metadata as the canonical API surface

The question of how to read a .NET package's public API from Go has three plausible answers:

**A. Run a bundled C# inspector tool.** A small NativeAOT-compiled C# binary that loads an assembly via `System.Reflection.MetadataLoadContext` (load-for-inspection mode, no code execution) and emits JSON. This is conceptually clean (the same language inspecting itself) but requires platform-specific binaries in the mochi distribution (one per host RID), complicates the release pipeline, and is not air-gap-compatible.

**B. Use Roslyn (`Microsoft.CodeAnalysis`).** Roslyn provides the richest semantic model: it resolves type aliases, partial classes, extension methods, nullable annotations from source-level attributes, and proc-macro-equivalent source generators. But Roslyn requires C# source. Binary NuGet packages (commercial, legacy, or NativeAOT-published) ship only DLLs. Approximately 40% of the top-100 nuget.org packages do not include source link or Roslyn analyzers; the API surface is available only in the binary. A Roslyn-first strategy fails for this 40%.

**C. Parse ECMA-335 CLI metadata in Go.** Every .NET assembly is a PE binary carrying a self-describing CLI metadata section (ECMA-335 §II.24). The metadata tables record every public type, method, property, event, and generic parameter in a compact, well-documented binary format. The Go-side parser (~2,000 LOC) reads the PE header, locates the CLI header, parses the `#~` compressed metadata stream, and walks the TypeDef/MethodDef/FieldDef/PropertyDef tables. XML documentation comments (`.xml` files bundled alongside `.dll` in NuGet packages) are parsed separately and correlated with metadata items by member reference strings.

MEP-68 chose C. The key properties:

- **Universally present.** Every .NET assembly, regardless of language (C#, F#, VB.NET), compilation mode (JIT, NativeAOT, R2R), or package vintage, carries ECMA-335 metadata. The format has been stable since .NET 1.0 (2002).
- **No runtime required.** The Go parser reads raw bytes; no CLR, no JIT, no garbage collector is involved at `mochi pkg lock` time. This makes lock deterministic and air-gap-compatible.
- **Sufficient for the closed type table.** The bridge uses a closed type-translation table (see [[05-type-mapping]]). Every type the table covers (primitives, generics of primitives, Task<T>, enum, struct, record, sealed class) is fully described in ECMA-335. Items outside the table are skipped; ECMA-335 metadata is more than sufficient to classify them.
- **Handles NullableAttribute.** C# 8+ nullable reference type annotations are stored as `NullableAttribute` custom attribute records in the metadata tables. The Go parser reads these and sets the `Nullable` flag on method parameters and return types, driving the `T|nil` translation in the type table.

The bundled inspector tool (option A) is noted as an acceptable future optimisation if the Go parser proves maintenance-intensive. See [[12-risks-and-alternatives]] §A1.

## §2. NativeAOT over CoreCLR hosting

Two strategies exist for calling .NET code from a native binary at runtime:

**NativeAOT** (`dotnet publish /p:PublishAot=true`): compiles the entire .NET dependency graph to native machine code ahead-of-time. The output is a static archive (`.a` / `.lib`) or shared library (`.so` / `.dylib` / `.dll`) with no CoreCLR dependency. Entry points are `[UnmanagedCallersOnly]` extern C functions callable directly from C/C++/Rust/Mochi code. Startup time is ~0ms (the binary is already native). Memory layout is fully controlled by the AOT compiler.

**CoreCLR hosting** (`coreclr_initialize` + `coreclr_create_delegate`): the Mochi binary dynamically loads `libcoreclr.so` / `coreclr.dll` at startup, initialises the .NET runtime in-process, and uses `coreclr_create_delegate` to obtain function pointers to managed methods. The .NET code runs on the JIT, with the full GC and JIT overhead. Startup cost is 100-300ms. The coreclr shared library must be present on the host machine.

MEP-68 chose NativeAOT as the primary strategy for five reasons:

1. **No JIT cold-start.** A Mochi binary with ten `import dotnet` packages would pay 100-300ms of CoreCLR initialisation per process invocation. NativeAOT pays nothing; the code is already native.
2. **No runtime dep on the target machine.** A NativeAOT static link produces a fully self-contained binary. The user does not need to install .NET on the deployment machine.
3. **Deterministic memory layout.** NativeAOT compiles managed types to predictable struct layouts that match `[StructLayout(LayoutKind.Sequential)]` without runtime reordering. This is essential for the blittable-struct pass-by-value path.
4. **Matches MEP-53's native-binary principle.** MEP-53 emits Rust source and links a single native binary. Introducing a JIT runtime as a subprocess or in-process host would contradict that principle.
5. **Production-ready since .NET 8 (November 2023).** NativeAOT was experimental in .NET 7 and became a supported production feature in .NET 8. The June 2026 .NET 9 LTS release further improved it. Using NativeAOT is a forward-looking choice; CoreCLR hosting is a compatibility story.

CoreCLR hosting is retained as the phase-13 fallback for NativeAOT-incompatible packages (packages that use `Assembly.Load`, `Emit`, or XAML runtime). See [[11-nativeaot-subset]].

## §3. [UnmanagedCallersOnly] over other interop primitives

.NET has several mechanisms for native-to-managed or managed-to-native calls. The bridge uses `[UnmanagedCallersOnly]` for all NativeAOT entry points.

**Rejected alternatives:**

- **`[DllImport]` / P/Invoke**: this is managed calling native, not native calling managed. Wrong direction.
- **`[ComVisible]` / COM interop**: Windows-only, requires type library registration, deeply tied to the Windows COM object model. Not applicable on Linux or macOS.
- **`Marshal.GetFunctionPointerForDelegate`**: delegates are GC-allocated managed objects; getting a function pointer to one requires pinning the delegate for the duration of the call, which is error-prone across the GC boundary.
- **`RuntimeHelpers.GetFunctionPointer`**: a low-level JIT API that only works in JIT mode, not NativeAOT.
- **`[UnmanagedFunctionPointer]` attribute**: used for reverse P/Invoke (managed callbacks to native); the signature is constrained by `CallingConvention` and cannot return managed types.

`[UnmanagedCallersOnly]` is the correct primitive:

- It is the only attribute that NativeAOT recognises for producing a stable C ABI entry point.
- It enforces at compile time that the parameter and return types are blittable (no GC references in the signature directly; GC references are passed as `nint` handles, which are blittable integers).
- The entry point name is specified via the `EntryPoint` property, giving the bridge full control over the `mochi_dotnet_<pkg>_<Type>_<Method>` naming scheme.
- It works identically in NativeAOT and in the CoreCLR hosting fallback (where it falls back to a different code path but the same attribute).

## §4. ManualResetEventSlim + ThreadPool for the async bridge

.NET async (`async Task<T>`) carries a `SynchronizationContext` that controls where continuations resume. In ASP.NET classic (Framework 4.x), the sync context is the request context; calling `.Result` or `.GetAwaiter().GetResult()` on the calling thread blocked it while waiting for a continuation that needed the same thread, causing a deadlock.

In NativeAOT with `[UnmanagedCallersOnly]`, the calling thread is a native OS thread managed by the Mochi/Rust runtime. There is no SynchronizationContext on that thread. `.GetAwaiter().GetResult()` called directly on that thread is safe from the classic ASP.NET deadlock, but introduces a different risk: if the `async Task` method's implementation internally calls `Task.Run` or `ConfigureAwait(false)` and the continuation is scheduled on the ThreadPool, and if the ThreadPool is starved, the calling thread blocks indefinitely.

The bridge uses a conservative pattern:

```csharp
ThreadPool.QueueUserWorkItem(_ => {
    result = method_call().GetAwaiter().GetResult();
    mre.Set();
});
mre.Wait();
```

This pattern:
- Posts the entire async invocation to a fresh ThreadPool thread (no sync context).
- Blocks the calling native thread on a `ManualResetEventSlim` (a lightweight, non-allocating, non-CLR-managed synchronisation primitive).
- The `ManualResetEventSlim.Wait()` on the native thread does not interact with the CLR scheduler (it uses a raw OS event under the covers in NativeAOT mode).

Why `ManualResetEventSlim` over `SemaphoreSlim.WaitAsync`? The bridge's calling thread is a native C thread, not a CLR thread pool thread. `SemaphoreSlim.WaitAsync` returns a `Task` that must be awaited on a CLR-managed thread. `ManualResetEventSlim.Wait` blocks the OS thread directly, which is correct for a native caller.

Why not `Task.Run` + `.Result`? `.Result` on the calling native thread has the same no-sync-context safety, but `Task.Run` allocates a `Task` object. `ThreadPool.QueueUserWorkItem` + `ManualResetEventSlim` is allocation-free on the hot path (after the first call warms the pool).

## §5. OIDC trusted publishing as the only publish path

NuGet trusted publishing (GitHub Actions OIDC, GitLab CI, Azure Pipelines) went GA on nuget.org in November 2024. Any tooling released after that date that ships a long-lived `NUGET_API_KEY` path as the primary publish story is shipping a security regression. The pattern of supply-chain incidents via stolen API keys (npm event-stream 2018, PyPI March 2025 reflected-string flood, NuGet typosquatting wave 2023-2024) establishes the risk conclusively.

MEP-57's general principle is "long-lived tokens are deprecated". MEP-68 applies that principle to NuGet. The `--allow-apikey-fallback` flag is included for the transition period (the nuget.org trusted publisher setup requires a one-time UI configuration step) and will be removed once nuget.org's trusted-publishing GA is universally enforced.

The practical impact on users:
- Publishing from a CI environment (GitHub Actions, GitLab CI, Azure Pipelines, Buildkite) is zero-configuration after a one-time nuget.org UI setup.
- Publishing from a local developer machine requires the user to request an OIDC token from their identity provider (not supported by `mochi pkg publish --to=nuget.org` without `--allow-apikey-fallback`). Local publishing uses `--dry-run` for smoke-test purposes.

This matches the direction npm (Trusted Publishing GA April 2024), Maven Central (Sigstore GA October 2024), and PyPI (PEP 740 GA late 2025) have already taken.
