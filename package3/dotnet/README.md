# package3/dotnet

Bidirectional .NET NuGet bridge for Mochi (MEP-68).

## Directories

| Directory | Purpose |
|-----------|---------|
| `metadata/` | Go-native ECMA-335 CLI metadata reader (PE header, metadata tables, type signature decoder, XML doc parser) |
| `nuget/` | NuGet V3 protocol client (registration endpoint, flat-container download, version resolution, OIDC trusted publish) |
| `typemap/` | Closed .NET-to-Mochi type translation table and SkipReport emitter |
| `nativeaot/` | NativeAOT wrapper C# project synthesizer (generates `[UnmanagedCallersOnly]` wrapper projects) |
| `externemit/` | Mochi `extern fn` / `extern type` declaration emitter (produces `.mochi` shim files) |
| `build/` | Build orchestration (drives `dotnet publish /p:PublishAot=true`, manages NuGet.Config, links static libs) |
| `semver/` | .NET four-part version parsing and NuGet SemVer-2 range evaluation |
| `errors/` | Bridge error types and SkipReport structures |
| `wrapper/` | mochi-dotnet-runtime helper types (GCHandleTable, MochiString, MochiList, async bridge primitives) |

## Phase delivery

Phases 0-6 are LANDED (skeleton through grammar extension).
Phases 7-13 are NOT STARTED. See `/docs/implementation/0068/` for the tracking matrix.

## Design notes

See `/docs/research/0068/` for the full research bundle (12 notes covering language surface,
design philosophy, prior-art bridges, ECMA-335 ingest, type mapping, NuGet protocol,
OIDC publish, async bridge, ABI stability, GC/memory, NativeAOT subset, and risks).
