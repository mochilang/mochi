---
title: "04. Kotlin metadata ingest"
sidebar_position: 5
sidebar_label: "04. Kotlin metadata ingest"
description: "The @kotlin.Metadata annotation binary format, the kotlinx-metadata-jvm protobuf schema (versions 1-9), the Go-side ingest pipeline, the ClassReader approach for extracting metadata bytes without a JVM, and the stability story."
---

# 04. Kotlin metadata ingest

The bridge discovers the public Kotlin API surface of a JAR by reading `@kotlin.Metadata` class-file annotations without spawning a JVM. This note explains the annotation format, the `kotlinx-metadata-jvm` schema, the Go-side extraction pipeline, and the schema stability guarantees.

## The `@kotlin.Metadata` annotation

When `kotlinc` compiles a Kotlin source file to JVM bytecode, it writes a `@kotlin.Metadata` annotation onto every `.class` file. The annotation has five fields:

```java
@interface Metadata {
    int    k()     default 1;    // class kind: 1=class, 2=file, 3=synthetic, 4=multi-file-class-part, 5=multi-file-class-facade
    int[]  mv()    default {};   // metadata version: [major, minor, patch]
    String[] d1()  default {};   // binary metadata (protobuf payload, base64 encoded, split across strings)
    String[] d2()  default {};   // string table referenced by d1
    String xs()    default "";   // extra string (package name for file classes)
    String pn()    default "";   // package name for multi-file classes
    int    xi()    default 0;    // extra int flags
}
```

The `d1` array contains the serialised protobuf payload (the Kotlin metadata proto). `d2` is a string table; indices in `d1`'s proto reference strings in `d2`. Together they encode the full Kotlin type signature of everything declared in the class.

## Schema versions

The protobuf schema is versioned via the `mv` field:

| `mv` | Kotlin version | Notes |
|------|---------------|-------|
| `[1, 1, 0]` | 1.0 - 1.3 | Legacy schema; lacks nullable annotation, coroutines. |
| `[1, 4, 0]` | 1.4+ | Stable schema; adds `isSuspend` flag, value classes, explicit nullability. |
| `[1, 5, 1]` | 1.5+ | Adds `@JvmRecord` interop, inline classes as value types. |
| `[1, 8, 0]` | 1.8+ | Adds context receivers (experimental), multiplatform expect/actual. |
| `[1, 9, 0]` | 1.9+ | Stable; current as of Kotlin 1.9.23 (May 2026). |

The `kotlinx-metadata-jvm` library (published by JetBrains as `org.jetbrains.kotlinx:kotlinx-metadata-jvm`) provides a stable Kotlin API over this schema with backwards-compatible reading across all supported versions.

For the bridge's Go-side reader, the relevant schema is the protobuf format for schema version `[1, 9, 0]`, which covers all Kotlin 1.4+ artifacts. Schema versions `[1, 1, 0]` artifacts (pre-1.4) are rare on Maven Central (May 2026) and the bridge emits a warning and falls back to reflection-only type extraction for them.

## Extracting metadata bytes without a JVM

Class files are ZIP entries inside JARs. The bridge reads them using Go's `archive/zip` package. Each `.class` file is a stream of bytes in the JVM class file format (JVMS §4). The `@kotlin.Metadata` annotation sits in the `RuntimeVisibleAnnotations` attribute of the class file.

The extraction pipeline in `package3/kotlin/metadata/`:

```
JAR (zip)
  └── com/example/MyClass.class
        └── ClassFile.attributes[]
              └── RuntimeVisibleAnnotations
                    └── annotation: kotlin/Metadata
                          ├── k:  1               (int)
                          ├── mv: [1, 9, 0]       (int[])
                          ├── d1: ["..."]          (string[])
                          └── d2: ["MyClass", ...] (string[])
```

The Go-side class file parser (`metadata/classreader.go`) implements a minimal JVM class file reader (JVMS §4.1-§4.7) that:

1. Reads the class file magic (`0xCAFEBABE`), major/minor version.
2. Parses the constant pool (needed to resolve annotation type names and string values).
3. Skips fields and methods (not needed for annotation extraction).
4. Finds the `RuntimeVisibleAnnotations` attribute on the class descriptor.
5. Locates the annotation with the type descriptor `Lkotlin/Metadata;`.
6. Reads the `k`, `mv`, `d1`, `d2`, `xs`, and `xi` element-value pairs.

The `d1` string array is joined (the split is purely cosmetic; class-file constant pool string entries have a 65535-byte length limit) and base64-decoded to yield the raw protobuf bytes.

## Protobuf decoding (Go-side)

The raw protobuf bytes are decoded using `package3/kotlin/metadata/proto/` (auto-generated from JetBrains' `kotlin-metadata.proto` schema). The top-level message is `KotlinMetadata`, which contains a `ClassMetadata` or `PackageMetadata` depending on the `k` field.

Relevant proto fields for function extraction:

```protobuf
message Function {
    int32     flags              = 1;   // visibility, modality, isOperator, isSuspend, etc.
    int32     name               = 2;   // index into string table (d2)
    TypeRef   returnType         = 3;
    repeated TypeRef valueParameterType = 5;
    repeated TypeParameter typeParameters = 4;
    int32     receiverTypeId     = 6;   // extension receiver, if any
}

message TypeRef {
    int32     className          = 1;   // index into string table
    bool      nullable           = 2;
    repeated TypeProjection arguments = 3;
    int32     typeParameterName  = 7;   // if this is a type parameter reference
    int32     typeAliasName      = 9;   // if this is a typealias
}
```

The bridge walks `ClassMetadata.functions`, `ClassMetadata.properties`, and `ClassMetadata.constructors` for each class; accumulates `PackageMetadata.functions` for top-level (file-scoped) functions; and recursively processes `ClassMetadata.nestedClassNames` for inner classes and `ClassMetadata.sealedSubclassNames` for sealed class hierarchies.

## The API surface data model

After decoding, the bridge builds a language-neutral `APIObject` tree:

```go
type APIObject struct {
    ClassName   string
    Kind        ClassKind       // Class, Interface, Object, Enum, DataClass, SealedClass, CompanionObject
    Functions   []Function
    Properties  []Property
    Constructors []Constructor
    Nested      []*APIObject
    SealedSubs  []*APIObject
}

type Function struct {
    Name       string
    Receiver   *KotlinType    // nil for non-extension
    Params     []Param
    ReturnType KotlinType
    IsSuspend  bool
    Flags      FunctionFlags  // visibility, modality, isOperator, isInline, isExternal
}

type KotlinType struct {
    ClassName  string         // fully qualified, e.g. "kotlin.collections.List"
    Nullable   bool
    TypeArgs   []KotlinType   // generic type arguments
    IsTypeParam bool          // true if this is an unresolved type parameter
}
```

This `APIObject` tree is the input to the type-mapping and wrapper-synthesis stages.

## Handling of specific Kotlin constructs

**Companion objects:** A Kotlin `companion object` compiles to an inner class named `Companion`. Its static functions are accessible as `FooClass.INSTANCE` in Java. The bridge maps companion functions to artifact-level functions in the shim: `Foo.Companion.bar()` → `foo_bar()`.

**Data classes:** Kotlin data classes generate `component1()`, `component2()`, etc. accessor functions and a `copy(...)` function in the bytecode. The bridge emits `extern fn` declarations for the component accessors and `copy`, but names them after the property names recovered from the `Property` metadata entries.

**Sealed classes:** The `sealedSubclassNames` list in the metadata identifies all direct subclasses. The bridge emits a discriminant function `extern fn result_variant(r: Result): int` plus per-variant constructor/accessor functions.

**Value classes (inline classes):** Kotlin inline/value classes wrap a single underlying value. The bridge unwraps them: `@JvmInline value class UserId(val value: String)` becomes `string` in the type mapping.

**`object` singletons:** Mapped to a zero-argument `extern fn` that returns the singleton instance via `FooObject.INSTANCE`.

**Extension functions:** The receiver appears as the first parameter in the JVM bytecode. The bridge preserves this as the first `Param` in the `Function` model and names the shim function accordingly.

## Stability guarantees

JetBrains has maintained backwards-compatible `@kotlin.Metadata` schema reading in `kotlinx-metadata-jvm` since Kotlin 1.4 (September 2020). The guarantees are:

1. A reader built for schema version N can read schema versions 1.4 through N.
2. Unknown fields (future additions) are silently ignored by current readers.
3. The string table indices in `d1` reference `d2` by position; the bridge does not depend on the ordering of strings in `d2` being stable across Kotlin releases.
4. JetBrains commits to advance notice (via `kotlinx-metadata-jvm` release notes) before removing fields from the schema.

The bridge pins the `kotlinx-metadata-jvm` schema version in `mochi.lock` as `metadata-schema-version`. The `lock --check` gate detects if a JAR's metadata was re-generated with a different schema version (which can happen when a library vendor upgrades their Kotlin compiler version between bridge runs).

## Schema version history and migration

| Bridge action | Trigger |
|--------------|---------|
| No action | `metadata-schema-version` in lock matches the ingested JAR's `mv`. |
| Warning, re-ingest | `mv` is higher than the pinned version but known (newer Kotlin release). |
| Error | `mv` is lower than `[1, 4, 0]` (pre-stable schema). |
| Error | `k` is 0 (Java-only class file, no Kotlin metadata). |

For Java-only class files (no `@kotlin.Metadata`), the bridge falls back to Java reflection metadata via a minimal JVM class file parser that reads the Java bytecode `Signature` attribute (JSR-14 generics erasure) and produces opaque `extern type` declarations with no callable functions.

## Cross-references

- [[05-type-mapping]] for how the `APIObject` tree is translated to Mochi types.
- [[05-type-mapping]] §refusal-set for types that have no valid Mochi representation.
- [[10-generics-reification]] for the monomorphise table and reified inline generics.
- [MEP-70 §9](/docs/mep/mep-0070#9-architecture-component-map) for the `package3/kotlin/metadata/` package overview.
