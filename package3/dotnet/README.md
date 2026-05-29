# package3/dotnet

Bidirectional .NET / NuGet package bridge for Mochi. Implements MEP-68.

## Layout

    errors/    SkipReason, BridgeError
    semver/    NuGet version / version-range
    nuget/     NuGet v3 API client + content-addressed .nupkg cache
    metacli/   mochi-dotnet-meta JSON schema + surface parser
    typemap/   closed CLR-to-Mochi type translation
    shimgen/   C# shim generator ([UnmanagedCallersOnly])
    emit/      Mochi extern emitter
    lockfile/  [[dotnet-package]] encoder/decoder/drift checker
    clrhosting/CLR hosting API bridge codegen
    publish/   NuGet package emit + trusted publishing
    build/     pipeline orchestration

## References

- MEP-68 spec: website/docs/mep/mep-0068.md
- Research: website/docs/research/0068/
