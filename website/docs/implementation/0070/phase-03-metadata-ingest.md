---
title: "Phase 03: Kotlin metadata ingest"
sidebar_position: 5
sidebar_label: "Phase 03: Metadata ingest"
description: "Extract @kotlin.Metadata from .class files inside a JAR using a minimal JVM class file reader and protobuf decoder, without spawning a JVM."
---

# Phase 03: Kotlin metadata ingest

**Status:** Planned

## Deliverables

1. `package3/kotlin/metadata/classreader.go` — minimal JVM class file parser (JVMS §4.1-§4.7) that extracts `RuntimeVisibleAnnotations` → `kotlin/Metadata` fields (`k`, `mv`, `d1`, `d2`, `xs`, `xi`).
2. `package3/kotlin/metadata/proto/` — auto-generated protobuf Go bindings from JetBrains' `kotlin-metadata.proto` (schema version 9).
3. `package3/kotlin/metadata/ingest.go` — `IngestJAR(jarPath string) (*APIObject, error)` — open the JAR, iterate `.class` files, extract metadata, decode protobuf, build the `APIObject` tree.
4. `package3/kotlin/metadata/model.go` — `APIObject`, `Function`, `Property`, `Constructor`, `KotlinType`, `ClassKind` types (the language-neutral API surface model).
5. `package3/kotlin/metadata/schema.go` — schema version compatibility check; `ErrUnsupportedMetadataVersion` for pre-1.4 schema.

## Class file parsing scope

The class file parser implements only what is needed for annotation extraction:

- Magic number validation (`0xCAFEBABE`).
- Constant pool parsing (needed for annotation type name and string value resolution).
- Skip: fields, methods, code attributes.
- `RuntimeVisibleAnnotations` attribute on the class descriptor only.
- Annotation element value decoding for: `e` (enum), `s` (string), `B/C/D/F/I/J/S/Z` (primitives), `[` (array), `@` (nested annotation).

The parser does not implement a full JVMS §4 class file reader. Unknown attribute types are skipped. This keeps the parser at ~300 lines of Go.

## Gate

Extract API surface from `kotlinx-coroutines-core@1.7.3`:
1. At least 847 functions extracted (matches research note §04 figure).
2. 234 types with metadata.
3. `delay` function has `isSuspend = true`.
4. `CoroutineScope` interface detected.
5. `Job` sealed hierarchy detected with `SupervisorJob` and `CompletableJob` subclasses.
6. `@kotlin.Metadata` missing (Java-only class): produces opaque `APIObject` with `Kind = JavaClass`, no functions.
7. Schema version `[1, 1, 0]` (pre-1.4): produces `ErrUnsupportedMetadataVersion`.
