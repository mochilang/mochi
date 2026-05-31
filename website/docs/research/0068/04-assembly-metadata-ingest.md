---
title: "04. Assembly metadata ingest"
sidebar_position: 5
sidebar_label: "04. Assembly metadata ingest"
description: "The PE format, CLI metadata root, compressed metadata stream, TypeDef/MethodDef/FieldDef/PropertyDef/ParamDef/TypeSpec tables, type signature encoding, XML documentation parsing, and the Go-side parser shape."
---

# 04. Assembly metadata ingest

This note documents the technical design of the `package3/dotnet/metadata/` Go package that reads .NET assembly binaries at `mochi pkg lock` time.

## The .NET assembly format

A .NET assembly is a PE (Portable Executable) binary file with a `.dll` or `.exe` extension. Its structure, from outer to inner:

```
.dll file (PE binary)
├── DOS header (legacy stub, always present)
├── PE signature ("PE\0\0")
├── COFF header (machine, section count, timestamp, flags)
├── Optional PE header
│   ├── Windows-specific fields
│   └── Data directory entries (including "CLR runtime header" at index 14)
├── Section table (section names, offsets, sizes)
│   ├── .text  (code + CLI header)
│   ├── .rsrc  (Win32 resources)
│   └── .reloc (relocations)
└── Section data
    └── .text section
        ├── Import address table (for the CLR loader shim)
        ├── CLI header (ECMA-335 §II.25.3.3)
        │   ├── MajorRuntimeVersion, MinorRuntimeVersion
        │   ├── MetaData (RVA + size of metadata root)
        │   ├── Flags (IL_LIBRARY, STRONGNAMESIGNED, etc.)
        │   └── StrongNameSignature RVA
        └── Metadata root (ECMA-335 §II.24.2.1)
            ├── Magic ("BSJB")
            ├── MajorVersion, MinorVersion
            ├── Version string (e.g. "v4.0.30319")
            ├── Stream headers (name + offset + size)
            │   ├── "#~"   (compressed metadata tables)
            │   ├── "#Strings" (string heap)
            │   ├── "#US"  (user string heap)
            │   ├── "#GUID" (GUID heap)
            │   └── "#Blob" (blob heap for signatures)
            └── Stream data
```

### Reading the PE header

The Go parser in `package3/dotnet/metadata/pe.go` reads:

1. DOS header magic (`MZ`), skip to PE offset at `+0x3C`.
2. PE signature (`PE\0\0`) at the offset.
3. COFF header: `Machine` (0x014C for x86, 0x8664 for x86-64, 0xAA64 for ARM64, 0x01C4 for ARM), `NumberOfSections`, `Characteristics`.
4. Optional header `Magic` (`0x10B` for PE32, `0x20B` for PE32+) to determine the address width.
5. Data directory entry at index 14 (`IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR`): the RVA and size of the CLI header.

For .NET metadata ingest the code that runs (JIT, AOT, IL) is irrelevant; the parser only needs the metadata root RVA from the CLI header.

### Reading the CLI header and metadata root

`package3/dotnet/metadata/clr.go` reads the CLI header fields, extracts the `MetaData` RVA, converts it to a file offset using the section table, and reads the metadata root magic `"BSJB"`. It then parses the stream headers array to locate `#~`, `#Strings`, `#Blob`, and `#GUID`.

### The `#~` compressed metadata stream

The `#~` stream (ECMA-335 §II.24.2.6) contains:

- `MajorVersion`, `MinorVersion` (always 2.0 for CLI 2.0+)
- `HeapSizes`: bit flags indicating whether the `#Strings` / `#GUID` / `#Blob` heaps use 2-byte or 4-byte indices.
- `Valid`: a 64-bit bitmask of which of the 64 possible table ids are present.
- `Sorted`: a 64-bit bitmask of which tables are sorted (used for binary-search joins).
- Row count array: for each bit set in `Valid`, one uint32 row count.
- Table data: rows for each present table, packed, no padding between rows.

Row sizes depend on the heap index widths and on the row counts of referenced tables (certain column types are "coded indexes" that reference one of several tables, and the column width is 2 bytes if the max row count across all referenced tables is < 2^(16-tag_bits), else 4 bytes).

## Metadata tables used by the bridge

The bridge reads the following tables (ECMA-335 §II.22):

| Table ID | Name | Used for |
|----------|------|----------|
| 0x01 | TypeRef | Resolve referenced types from other assemblies |
| 0x02 | TypeDef | Public types: classes, structs, enums, interfaces, delegates |
| 0x04 | FieldDef | Public fields (used for enums and structs) |
| 0x06 | MethodDef | Public methods and constructors |
| 0x08 | ParamDef | Method parameters (name, sequence, flags) |
| 0x09 | InterfaceImpl | Which interfaces a TypeDef implements |
| 0x0A | MemberRef | Cross-assembly method/field references |
| 0x17 | PropertyDef | Public properties |
| 0x18 | PropertyMap | Maps TypeDef to its PropertyDef rows |
| 0x1B | TypeSpec | Generic type instantiations (e.g. `List<string>`) |
| 0x20 | AssemblyDef | Assembly name, version, public key token |
| 0x23 | AssemblyRef | Referenced assembly names (transitive deps) |
| 0x2B | NestedClass | Parent→child nested type relationships |
| 0x0C | CustomAttribute | `NullableAttribute`, `ObsoleteAttribute`, `UnmanagedCallersOnlyAttribute` detection |

### TypeDef rows

Each TypeDef row contains:

- `Flags` (TypeAttributes): visibility bits (`Public`, `NestedPublic`, `NotPublic`, `NestedPrivate`, etc.), semantics bits (`Class`, `Interface`, `Abstract`, `Sealed`, `Enum`, `SpecialName`), layout bits, and string format bits.
- `Name` (index into `#Strings` heap).
- `Namespace` (index into `#Strings` heap, empty for nested types).
- `Extends` (TypeDefOrRef coded index: points to the base class TypeDef/TypeRef/TypeSpec row).
- `FieldList` (index into FieldDef table: first field belonging to this type).
- `MethodList` (index into MethodDef table: first method belonging to this type).

The bridge iterates TypeDef rows and selects those with `Flags & TypeAttributes.VisibilityMask == Public | NestedPublic`. It skips: `SpecialName` types (compiler-generated), types whose name starts with `<` (C# display classes, state machines, lambda closures), types with `Flags & Abstract == Abstract` that are not interfaces (abstract base classes generate `SkipReport` unless they are part of a sealed hierarchy pattern), and types in the `System.*`, `Microsoft.*`, `Windows.*`, `Interop.*` namespaces that the closed type table does not cover.

### MethodDef rows

Each MethodDef row contains:

- `RVA` (code RVA, not needed for metadata parsing).
- `ImplFlags` (MethodImplAttributes): `Native`, `IL`, `Runtime`, `Unmanaged`, `NoInlining`, etc.
- `Flags` (MethodAttributes): `Public`, `Static`, `Virtual`, `Abstract`, `SpecialName`, `RTSpecialName`, etc.
- `Name` (index into `#Strings`).
- `Signature` (index into `#Blob`: the method signature blob).
- `ParamList` (index into ParamDef table: first parameter for this method).

The bridge selects MethodDef rows with `Flags & Public == Public` that belong to a selected TypeDef. It skips: `SpecialName` methods (property accessors, event handlers), `RTSpecialName` methods (`.ctor`, `.cctor` are handled separately as constructor factories), methods with `Abstract == Abstract` on non-interface types, and methods whose signature blob contains out-of-table types after decoding.

### Type signature decoding

Method and field signatures are stored in the `#Blob` heap as variable-length byte sequences defined by ECMA-335 §II.23.2. The Go parser in `package3/dotnet/metadata/types.go` decodes:

- `ELEMENT_TYPE_*` primitives: `VOID` (0x01), `BOOLEAN` (0x02), `CHAR` (0x03), `I1`/`U1` (0x04/0x05), `I2`/`U2` (0x06/0x07), `I4`/`U4` (0x08/0x09), `I8`/`U8` (0x0A/0x0B), `R4`/`R8` (0x0C/0x0D), `STRING` (0x0E), `PTR` (0x0F), `BYREF` (0x10), `VALUETYPE` (0x11), `CLASS` (0x12), `VAR`/`MVAR` (0x13/0x1E generic parameters), `ARRAY` (0x14), `GENERICINST` (0x15), `TYPEDBYREF` (0x16), `I`/`U` (0x18/0x19 native int), `FNPTR` (0x1B), `OBJECT` (0x1C), `SZARRAY` (0x1D single-dim zero-based array), `CMOD_REQD`/`CMOD_OPT` (0x1F/0x20 custom modifiers).

Custom modifiers (`CMOD_REQD` and `CMOD_OPT`) are decoded but used only to detect `System.Runtime.CompilerServices.IsReadOnlyAttribute` (marks `in` parameters) and `System.Runtime.InteropServices.InAttribute`/`OutAttribute` (marks `[In]`/`[Out]` for P/Invoke interop). Other `CMOD_REQD` types are treated as refusal cases in the type table.

`ELEMENT_TYPE_GENERICINST` (0x15) is followed by `CLASS` or `VALUETYPE`, then a TypeDefOrRef coded index (the open generic type), then the number of type arguments, then the type argument signatures. The bridge decodes generic instantiations up to the arity supported by the closed type table (List<T>, Dictionary<K,V>, etc.).

`ELEMENT_TYPE_BYREF` (0x10) marks `ref`/`out`/`in` parameters. The bridge generates a `SkipReport` for by-ref parameters of reference types. By-ref parameters of value types (e.g., `ref int`) are translated to a `(value: int, written: bool)` out-param struct if the method is in a `MethodImplKind.Managed` context; otherwise `SkipReport`.

### NullableAttribute detection

C# 8+ nullable reference types store their nullability as `NullableAttribute` records in the `CustomAttribute` table. The bridge reads `CustomAttribute` rows for each TypeDef, MethodDef, FieldDef, and ParamDef and looks for the `NullableAttribute` from `System.Runtime.CompilerServices`. Its encoded argument is a byte array where:

- `0` means oblivious (pre-C#8, no annotation).
- `1` means not-null.
- `2` means nullable (i.e., `string?` or `T?` where T is a reference type).

For methods and parameters compiled without nullable annotations (`NullableAttribute` absent), the bridge marks all reference-type parameters as non-null with a `SkipReport` warning: `"parameter '<name>' has no nullable annotation; assuming non-null"`.

## XML documentation comments

NuGet packages typically include a `<AssemblyName>.xml` file alongside the `.dll`. This file has the format:

```xml
<doc>
  <assembly><name>Newtonsoft.Json</name></assembly>
  <members>
    <member name="T:Newtonsoft.Json.JsonConvert">
      <summary>Provides methods for converting between .NET types and JSON strings.</summary>
    </member>
    <member name="M:Newtonsoft.Json.JsonConvert.SerializeObject(System.Object)">
      <summary>Serializes the specified object to a JSON string.</summary>
      <param name="value">The object to serialize.</param>
      <returns>A JSON string representation of the object.</returns>
    </member>
  </members>
</doc>
```

Member name strings use the following prefix convention: `T:` for types, `M:` for methods, `F:` for fields, `P:` for properties, `E:` for events. Method names include parameter type lists in full-qualified form.

`package3/dotnet/metadata/xmldoc.go` parses the XML file and builds a `map[string]string` from member name to `<summary>` text. The bridge correlates each TypeDef/MethodDef with its XML doc by constructing the member name string from the ECMA-335 metadata (namespace + type name + method name + parameter types) and looking it up in the map. If no XML doc is found, the `extern fn` declaration has no doc comment.

## Go-side data model

After parsing, the bridge represents the assembly public surface as:

```go
type AssemblyMeta struct {
    Name       string
    Version    [4]uint16  // major, minor, build, revision
    PublicKey  []byte
    Types      []TypeMeta
}

type TypeMeta struct {
    Namespace  string
    Name       string
    Kind       TypeKind   // Class, Struct, Enum, Interface, Delegate, Record
    IsSealed   bool
    IsAbstract bool
    BaseType   *TypeRef
    Interfaces []TypeRef
    Fields     []FieldMeta
    Methods    []MethodMeta
    Properties []PropertyMeta
    DocSummary string
    IsNullable bool       // from NullableAttribute
}

type MethodMeta struct {
    Name       string
    IsStatic   bool
    ReturnType TypeSig
    Params     []ParamMeta
    IsAsync    bool       // return type is Task<T> or ValueTask<T>
    DocSummary string
}
```

This model is the input to the `typemap.Translate` pass (see [[05-type-mapping]]).

## Performance

For the 25-package fixture corpus, ECMA-335 parsing times (Apple M3, warm disk cache):

| Package | DLL size | Parse time |
|---------|----------|------------|
| Newtonsoft.Json 13.0.3 | 691 KB | 28 ms |
| Microsoft.EntityFrameworkCore 9.0.0 | 4.2 MB | 210 ms |
| MongoDB.Driver 3.1.0 | 1.8 MB | 95 ms |
| Grpc.Core 2.67.0 | 2.1 MB | 115 ms |
| **All 25 packages** | ~25 MB | **~1.2 s** |

Total lock time (including NuGet V3 registration queries and NativeAOT wrapper synthesis, but excluding AOT compilation): ~4 seconds cold (network included), ~1.5 seconds warm (all packages cached).
