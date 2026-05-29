---
title: "04. Assembly metadata ingest"
sidebar_position: 5
sidebar_label: "04. Assembly metadata ingest"
description: "The ECMA-335 metadata format, `System.Reflection.Metadata.MetadataReader`, the `mochi-dotnet-meta` CLI tool design, the JSON output schema, the Go-side parser shape under package3/dotnet/metacli/, and the per-package ingest fixtures."
---

# 04. Assembly metadata ingest

This note documents how MEP-68's ingest pipeline turns a NuGet package into a machine-readable surface description. The pipeline runs at `mochi pkg lock` time via the `mochi-dotnet-meta` CLI tool; the Mochi Go binary parses the emitted JSON document.

## The ECMA-335 metadata format

.NET assemblies (`.dll` and `.exe` files) store type system information in ECMA-335 metadata tables embedded in the PE (Portable Executable) binary. The metadata is structured as a set of tables: TypeDef, MethodDef, FieldDef, Param, TypeRef, MemberRef, CustomAttribute, and others. Every public type and member in an assembly has a row in the appropriate table.

The `System.Reflection.Metadata` namespace (part of the .NET BCL since .NET Core 2.1, NuGet package `System.Reflection.Metadata@8.0` for downlevel use) provides a low-level, allocation-minimal reader over these tables:

```csharp
using System.Reflection.Metadata;
using System.Reflection.PortableExecutable;

using var stream = File.OpenRead("Newtonsoft.Json.dll");
using var peReader = new PEReader(stream);
var metadata = peReader.GetMetadataReader();

foreach (var typeHandle in metadata.TypeDefinitions) {
    var typeDef = metadata.GetTypeDefinition(typeHandle);
    var name = metadata.GetString(typeDef.Name);
    var ns = metadata.GetString(typeDef.Namespace);
    // ...
}
```

This approach reads the metadata directly from the PE binary without loading the assembly into the CLR. It is fast (reading a 2 MB assembly takes under 100 ms on modern hardware), allocation-minimal (the reader uses memory-mapped I/O), and deterministic (the same `.dll` always produces the same metadata).

## The `mochi-dotnet-meta` CLI tool

`mochi-dotnet-meta` is a single-file .NET executable (`mochi-dotnet-meta.exe` on Windows, a self-contained native binary on Linux/macOS via NativeAOT) that:

1. Accepts a `.dll` path (or a `.nupkg` path from which it extracts the correct TFM's DLL).
2. Opens the DLL with `PEReader` + `MetadataReader`.
3. Walks every public `TypeDefinition`, `MethodDefinition`, `FieldDefinition`, `PropertyDefinition`, and `EventDefinition`.
4. Resolves generic parameter instantiations and type reference chains.
5. Emits a JSON document to stdout.

The tool is invoked by the Go-side bridge at `mochi pkg lock` time:

```go
cmd := exec.Command("mochi-dotnet-meta", "--dll", dllPath, "--tfm", "net8.0")
out, err := cmd.Output()
// parse JSON from out
```

The tool is shipped as a content asset inside the `mochi` binary (as a `go:embed` resource). On first use, it is extracted to `~/.cache/mochi/tools/mochi-dotnet-meta` and executed from there. The tool is versioned alongside the bridge; a tool version mismatch at lock time is a hard error.

## JSON output schema

The `mochi-dotnet-meta` output is a single JSON object:

```json
{
  "assembly": "Newtonsoft.Json",
  "version": "13.0.3.0",
  "tool-version": "1.0.0",
  "types": [
    {
      "namespace": "Newtonsoft.Json",
      "name": "JsonConvert",
      "kind": "Class",
      "visibility": "Public",
      "generic-params": [],
      "methods": [
        {
          "name": "SerializeObject",
          "visibility": "Public",
          "is-static": true,
          "is-async": false,
          "return-type": { "kind": "String" },
          "params": [
            { "name": "value", "type": { "kind": "Object" } }
          ],
          "generic-params": []
        },
        {
          "name": "DeserializeObject",
          "visibility": "Public",
          "is-static": true,
          "is-async": false,
          "return-type": { "kind": "Nullable", "inner": { "kind": "Object" } },
          "params": [
            { "name": "value", "type": { "kind": "String" } }
          ],
          "generic-params": ["T"]
        }
      ],
      "properties": [ ... ],
      "fields": [ ... ]
    }
  ]
}
```

### Type kind discriminator

The `"kind"` field in a type node is one of:

- `"Int32"`, `"Int64"`, `"Single"`, `"Double"`, `"Boolean"`, `"Char"`, `"String"`, `"Void"`: primitive types.
- `"Object"`: `System.Object` (the CLR base type; maps to `any` in Mochi).
- `"Class"`, `"Struct"`, `"Interface"`, `"Enum"`, `"Delegate"`: user-defined types.
- `"Array"`: a CLR array type. Carries `"element"` with the element type node.
- `"GenericInst"`: a closed generic instantiation (e.g., `List<int>`). Carries `"definition"` (the open generic type name) and `"args"` (an array of type nodes).
- `"GenericParam"`: an open generic type parameter (e.g., `T` in `List<T>`). Carries `"name"` and `"index"`.
- `"Nullable"`: `T?` (a `Nullable<T>` wrapper). Carries `"inner"` with the inner type node.
- `"Task"`: `System.Threading.Tasks.Task`. Carries `"result"` with the result type node (null for `Task` without a result).
- `"Pointer"`: an unsafe pointer type (`T*`). Carried but always produces `SkipPointer` in the type-mapping pass.
- `"ByRef"`: a `ref T` parameter. Produces `SkipByRef` in the type-mapping pass.
- `"TypeRef"`: a cross-assembly type reference (resolved by name at lock time).
- `"FunctionPointer"`: a function pointer type (C# 9+ `delegate*`). Produces `SkipFunctionPointer`.

### Method visibility and attributes

The tool emits only methods with `visibility = "Public"` or `visibility = "Protected"`. Private, internal, and private-protected items are excluded from the JSON output entirely.

Computed attributes emitted per method:

- `"is-static"`: whether the method has the `static` modifier.
- `"is-async"`: whether the return type is `Task` or `Task<T>` (the method is declared `async` in C#; the tool infers this from the return type since `async` is not preserved in IL).
- `"is-virtual"`: whether the method is virtual or abstract (relevant for interface dispatch).
- `"is-extension"`: whether the method has the `[ExtensionAttribute]` custom attribute (a C# extension method).
- `"is-obsolete"`: whether the method has `[ObsoleteAttribute]`. Obsolete items are included with a warning flag.
- `"unsafe"`: whether the method has the `unsafe` keyword. Unsafe methods produce `SkipUnsafe` unless `[dotnet.capabilities] unsafe = true`.

## Go-side parser

The bridge's `package3/dotnet/metacli/` package implements a Go-side parser for the `mochi-dotnet-meta` JSON output:

```go
package metacli

type Assembly struct {
    Name        string   `json:"assembly"`
    Version     string   `json:"version"`
    ToolVersion string   `json:"tool-version"`
    Types       []TypeDef `json:"types"`
}

type TypeDef struct {
    Namespace   string    `json:"namespace"`
    Name        string    `json:"name"`
    Kind        TypeKind  `json:"kind"`
    Visibility  string    `json:"visibility"`
    GenericParams []string `json:"generic-params"`
    Methods     []MethodDef `json:"methods"`
    Properties  []PropertyDef `json:"properties"`
    Fields      []FieldDef `json:"fields"`
}

type MethodDef struct {
    Name        string    `json:"name"`
    Visibility  string    `json:"visibility"`
    IsStatic    bool      `json:"is-static"`
    IsAsync     bool      `json:"is-async"`
    IsVirtual   bool      `json:"is-virtual"`
    IsExtension bool      `json:"is-extension"`
    IsObsolete  bool      `json:"is-obsolete"`
    Unsafe      bool      `json:"unsafe"`
    ReturnType  TypeNode  `json:"return-type"`
    Params      []Param   `json:"params"`
    GenericParams []string `json:"generic-params"`
}
```

The parser constructs a normalised `ApiSurface` value:

```go
type ApiSurface struct {
    PackageName  string
    PackageVersion string
    ToolVersion  string
    Classes      []ClassSig
    Structs      []StructSig
    Interfaces   []InterfaceSig
    Enums        []EnumSig
    Skipped      []SkipReport
}
```

Walking the JSON is straightforward: for each type, walk its methods, check each method's parameter and return types against the closed translation table, and either include or skip.

## Schema stability

The `mochi-dotnet-meta` tool emits a `"tool-version"` field. The `mochi.lock` records `metadata-sha256` (SHA-256 of the JSON document). A bridge version that changes the tool produces different JSON; the SHA-256 drifts; `mochi pkg lock --check` fails. This is intentional: the user must re-lock when the tool version changes to regenerate with the new schema.

The tool version follows semantic versioning:

- A minor version bump adds new fields (backward-compatible).
- A major version bump changes existing fields (requires re-lock).

The bridge maintains a compatibility table analogous to MEP-73's rustdoc-types version table:

| Bridge version | Supported tool-version | Notes |
|----------------|----------------------|-------|
| 0.1.x | 1.0.x | Initial release. |
| 0.2.x | 1.0.x, 1.1.x | Adds `"is-extension"` field. |

## Ingest fixtures

The bridge's test corpus draws from the curated 20-package fixture set (April 2026 top-downloaded-on-nuget.org snapshot): Newtonsoft.Json, Serilog, Microsoft.Extensions.DependencyInjection, System.Text.Json, Dapper, NUnit, xUnit, FluentAssertions, AutoMapper, MediatR, FluentValidation, Polly, Bogus, Moq, RestSharp, StackExchange.Redis, Npgsql, EntityFramework Core, Microsoft.Extensions.Http, AWSSDK.Core.

For each package, the test:

1. Materialises the `.nupkg` at a known version from the content-addressed cache.
2. Extracts the correct TFM's `.dll` asset.
3. Runs `mochi-dotnet-meta --dll <path> --tfm net8.0` and captures the JSON output.
4. Parses the JSON via the Go-side `metacli` package.
5. Asserts that the parsed `ApiSurface` contains the expected number of public class, struct, interface, and enum entries (golden numbers checked into the test).
6. Asserts that the `Skipped` list contains the expected items (golden list).

The fixture set is regenerated annually to track package API changes; golden numbers are stored in `tests/package3/dotnet/metacli/<pkg>-<version>.golden.json`.

## Cross-references

- [[01-language-surface]] for the user-visible surface this ingest feeds.
- [[02-design-philosophy]] §2 for the rejection of Roslyn source parsing and XML documentation.
- [[05-type-mapping]] for what the bridge does with the parsed surface.
- [[09-abi-stability]] for the shim layer the surface drives.
- [MEP-68 §1](/docs/mep/mep-0068#1-pipeline-overview) for the normative pipeline.
