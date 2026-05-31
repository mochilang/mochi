---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks and alternatives"
description: "Complete risk register and rejected alternatives register for MEP-68."
---

# 12. Risks and alternatives

## Risk register

### R1. NativeAOT compatibility rate

**Risk**: A meaningful fraction of popular NuGet packages (estimated 15-25% of the top 100 based on the 25-package fixture analysis in [[11-nativeaot-subset]]) require the CoreCLR hosting fallback due to runtime code generation. If the fallback path is not ready when users import these packages, the bridge cannot cover them.

**Likelihood**: High (these packages are widely used: AutoMapper, MediatR, Humanizer, NHibernate, Castle Windsor).

**Mitigation**: Phase 13 (CoreCLR hosting fallback) is explicitly scoped as a delivery phase. The bridge generates a clear `SkipReport: package requires CoreCLR hosting (phase 13); use nativeaot = false` at lock time, so users get a clear diagnostic rather than a silent failure. The phase-13 gate covers the three incompatible packages in the fixture corpus (AutoMapper, MediatR, Humanizer).

---

### R2. ECMA-335 parser edge cases

**Risk**: The ECMA-335 CLI metadata format has 40+ table types, multiple coded-index flavors, and complex type signature encoding (generics, pointers, custom modifiers, forwarded types). A parser that covers the 25-package fixture corpus may miss edge cases in less-common NuGet packages (F# libraries, COM interop assemblies, signed assemblies with strong names, assemblies with nested types > 3 levels deep).

**Likelihood**: Medium.

**Mitigation**: the parser is fuzz-tested against a corpus of 500+ NuGet packages from the top 10,000 by download count. Any panic or incorrect parse is a phase-2 gate failure. The `metadata-sha256` in `mochi.lock` ensures that a parse mismatch (e.g., on a platform with a different byte order) is caught at `--check` time. The parser is open-source and accepts contributions for edge cases.

---

### R3. NativeAOT compile time

**Risk**: `dotnet publish /p:PublishAot=true` takes 30-120 seconds per package per RID. For a Mochi project with 5 .NET packages and 4 RIDs, the NativeAOT pass takes 10-40 minutes on first build. This blocks CI pipelines and developer inner loops.

**Likelihood**: High for large projects.

**Mitigation**: wrapper static libs are cached at `~/.cache/mochi/dotnet-deps/wrappers/<wrapper-sha256>-<rid>/`. Once built, a wrapper is never recompiled unless its `wrapper-sha256` changes (which only happens on a NuGet package update or a `mochi.toml` monomorphise change). In CI, the cache directory is stored as a build artefact between runs. The build orchestration (phase 7) parallelises the four RID compilations across available CPU cores.

---

### R4. Four-RID cross-compilation complexity

**Risk**: `dotnet publish -r linux-arm64` from a macOS host requires a cross-compilation toolchain. NativeAOT on Linux/macOS cross-compilation uses `clang` + LLVM for the native link step; the ARM64 target libraries must be present on the host. Windows cross-compilation (`-r win-x64` from Linux/macOS) is experimental in .NET 9.

**Likelihood**: Medium.

**Mitigation**: MEP-53 already requires `cargo-zigbuild` for cross-compilation of the Rust/Mochi binary; the bridge reuses `zig cc` as the cross-compilation C compiler for the NativeAOT link step (`<ZigCC>/zig cc --target=aarch64-linux-gnu` for linux-arm64). `win-x64` cross-compilation is marked `experimental` in phase 7 and documented as requiring either a Windows host or LLVM's `lld-link` with the Windows SDK sysroot.

---

### R5. GCHandle leak in collections

**Risk**: `extern type T` values stored in `list<T>` or `map<string, T>` (e.g., a pool of `NpgsqlConnection` objects) are not automatically freed when the collection goes out of scope. The compiler's `defer _free` insertion only covers simple local variable bindings.

**Likelihood**: Medium (common pattern for connection pools).

**Mitigation**: v1 emits a `WARNING: extern type T in list<T>; manual _free required` when a list of GC-handle-backed extern types is declared. A future MEP adds linear type tracking for `extern type` in collections. The `mochi-dotnet-runtime` package provides a `GCHandleList<T>` helper that frees all handles on dispose, accessible as `DotnetList.free_all(handles)`.

---

### R6. NuGet trusted publishing configuration friction

**Risk**: nuget.org trusted publishing requires a one-time per-package setup in the nuget.org UI. First-time publishers (or first-time publishers of a new package name) may not have completed this setup. `mochi pkg publish --to=nuget.org` returns HTTP 403 with a non-obvious error message.

**Likelihood**: High for first-time users.

**Mitigation**: on HTTP 403, the bridge prints: `"NuGet trusted publishing is not configured for package '<id>'. Visit https://www.nuget.org/packages/<id>/Manage to add a trusted publisher, then re-run mochi pkg publish."` and optionally opens the URL in the browser (`--open-browser`).

---

### R7. Generic monomorphization explosion

**Risk**: a user who declares 50 monomorphise entries for `System.Linq.Enumerable.Where<T>` (for 50 different record types) produces a NativeAOT wrapper with 50 `[UnmanagedCallersOnly]` exports and a correspondingly long AOT compile time.

**Likelihood**: Low (typical projects have 5-20 monomorphise entries).

**Mitigation**: the bridge enforces a soft limit of 100 monomorphise entries per package (configurable via `[dotnet] monomorphise-limit`). Above the limit, `mochi pkg lock` prints a warning and suggests using a Mochi adapter layer instead.

---

### R8. Windows cross-compilation linker

**Risk**: cross-compiling a `win-x64` NativeAOT static lib from Linux or macOS requires the MSVC linker or LLVM `lld-link` with the Windows SDK sysroot. Setting up the Windows SDK sysroot on a Linux CI agent is non-trivial (requires `xwin` or `winsdk-export`).

**Likelihood**: Medium for projects targeting Windows from non-Windows CI.

**Mitigation**: `win-x64` is marked `experimental` for cross-compilation in phase 7. The recommended path for `win-x64` is a Windows runner in CI (`runs-on: windows-latest`). The bridge documents the `xwin`-based setup for users who need Linux-to-Windows cross-compilation.

---

### R9. CoreCLR version skew

**Risk**: the CoreCLR version recorded in `mochi.lock` at lock time may differ from the version installed on the deployment machine at runtime. `coreclr_initialize` fails with a version mismatch.

**Likelihood**: Medium for long-lived deployments where .NET patch versions are regularly updated.

**Mitigation**: `--bundle-coreclr` bundles the exact `libcoreclr.so` version in the Mochi binary distribution. For deployments without bundling, `mochi pkg lock --check` must be run after any .NET SDK update; the CI gate enforces this. The check emits a clear error with the expected and found versions.

---

### R10. Symbol name collision across package major versions

**Risk**: if the user imports `Newtonsoft.Json` v12 and v13 in the same project (unlikely but possible in large transitive dep graphs), the symbol `mochi_dotnet_Newtonsoft_Json_v13_JsonConvert_SerializeObject` and `mochi_dotnet_Newtonsoft_Json_v12_JsonConvert_SerializeObject` would coexist. If the user imports the same package twice with different aliases, the linker sees duplicate `[UnmanagedCallersOnly]` entry points.

**Likelihood**: Low.

**Mitigation**: the bridge enforces that each package ID appears at most once in `[dotnet-dependencies]` (the version constraint must unify to a single resolved version). Transitive dep conflicts are resolved by "nearest wins" (the directly-declared version overrides transitive). If two incompatible versions are truly required, the user must use `dotnet-dependencies.alias` to rename one and the bridge generates separate wrapper symbol namespaces.

---

### R11. decimal precision and arithmetic cost

**Risk**: programs that use `decimal` arithmetic frequently (financial applications) pay ~5x the cost of `double` arithmetic because each operation crosses the C ABI boundary (two `MochiDecimal` structs in, one out).

**Likelihood**: Low for non-financial applications; Medium for financial applications.

**Mitigation**: v1 treats this as a known trade-off. A future Mochi `decimal` literal syntax and a Mochi `decimal`-native arithmetic pass (MEP-XX) will eliminate the per-operation ABI crossing for pure-Mochi decimal arithmetic. For the bridge path, the `MochiDecimal` struct is blittable and the overhead is ~10 ns per operation (not the ~50 ns of a GCHandle round-trip).

---

### R12. Nullable annotation absence in older packages

**Risk**: NuGet packages compiled before C# 8 (pre-2020) or packages that have not enabled `<Nullable>enable</Nullable>` have no `NullableAttribute` records. All their reference-type parameters are translated as non-null. A user who passes `nil` for a "logically nullable" parameter gets an `ArgumentNullException` from the .NET side at runtime.

**Likelihood**: High for older packages (Newtonsoft.Json pre-13.0, older EF Core, etc.).

**Mitigation**: the bridge emits a `SkipReport: reference type 'string' at parameter 'value' has no nullable annotation; passing nil may throw ArgumentNullException` for each affected parameter. Users can override with a hand-authored `extern fn ... from dotnet "..."` declaration that marks the parameter as nullable.

---

## Rejected alternatives register

### A1. Bundled `mochi-dotnet-inspect` binary for metadata extraction

A NativeAOT-compiled C# tool that reads an assembly and outputs JSON metadata. Simpler to implement than the Go-native ECMA-335 parser. Rejected: requires platform-specific binaries in the mochi distribution (one per host RID), complicates the release pipeline, is not air-gap-compatible. The Go-native parser is ~2,000 LOC and avoids these issues. See [[02-design-philosophy]] §1.

### A2. Roslyn as the bind source

Roslyn's `Microsoft.CodeAnalysis` NuGet provides the richest .NET semantic model. Rejected: requires C# source, which binary-only NuGet packages (commercial, legacy, NativeAOT-published) do not ship. ECMA-335 binary metadata is universally present. See [[02-design-philosophy]] §1.

### A3. P/Invoke as the wrapper mechanism

P/Invoke is .NET calling native C. The bridge direction is native (Mochi/Rust) calling .NET. P/Invoke cannot serve the bridge's direction. `[UnmanagedCallersOnly]` is the correct mechanism. See [[09-abi-stability]] §1.

### A4. CppSharp or IKVM.NET as the binding layer

CppSharp is C++→C#; wrong direction. IKVM is Java→.NET; not applicable. Neither provides a .NET→Mochi bidirectional bridge. See [[03-prior-art-bridges]].

### A5. gRPC / Unix socket for cross-runtime communication

~100µs per-call overhead, separate process lifecycle, defeats the no-boilerplate promise. Rejected for in-process library calls. Appropriate for distributed service communication, not for tight library integration. See [[02-design-philosophy]] (implicitly, performance principle).

### A6. WIT (Wasm Interface Types) / componentize-dotnet

The .NET `componentize-dotnet` tool (Bytecode Alliance) compiles .NET code to a Wasm Component Model component with a WIT-described interface. Pre-GA as of May 2026, covers a subset of .NET types. Deferred to a post-v1 extension (`[dotnet.publish] wit = true`). Not a rejection; deferred due to maturity.

### A7. Reference assemblies instead of implementation assemblies

.NET SDK ships reference assemblies (API-only DLLs without method bodies) that are smaller and faster to parse. But third-party NuGet packages for non-BCL libraries do not ship reference assemblies universally. Implementation assemblies are always present. See [[04-assembly-metadata-ingest]] §1.

### A8. Long-lived nuget.org API keys

`NUGET_API_KEY` long-lived tokens. Rejected: matches MEP-57's broader principle that long-lived tokens are deprecated. NuGet trusted publishing GA November 2024 makes this unambiguous for new tooling. See [[07-oidc-nuget-trusted-publishing]].

### A9. Value-type semantics in Mochi for .NET structs

Translating .NET `readonly struct` and `ref struct` into Mochi value-type annotations. Mochi does not have a ref/copy distinction; all `extern record` types are pass-by-copy at the type-system level (the ABI handles the details). `ref struct` (stack-only) is excluded entirely. See [[05-type-mapping]] §Struct types.

### A10. Using `dotnet new` templates for TargetDotnetLibrary emit

`dotnet new classlib` then modify. Rejected: indirect dependency on `dotnet new` template versions; the bridge directly owns the emitted project structure. See [[06-nuget-publish-flow]] §Direction 2.

### A11. Eager NativeAOT compile at `mochi pkg lock` time

Compile all wrappers at lock time (not build time). Rejected: 30-120 seconds per package per RID makes `mochi pkg lock` unacceptably slow for a routine developer action. The cache means the cost is amortised across builds, not paid at every lock. See Risk R3.

### A12. dlopen pre-built shared libraries from nuget.org

nuget.org does not host pre-built native binaries (only `.nupkg` source/managed archives). Cross-RID pre-built binaries would require the package author to publish a `<id>.linux-x64.<version>.nupkg` runtime package. Some packages do this (Grpc.Tools, various native codec packages) but it is not universal. The NativeAOT source-compile path is the only universally applicable approach.
