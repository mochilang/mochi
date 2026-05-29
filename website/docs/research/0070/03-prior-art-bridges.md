---
title: "03. Prior-art JVM bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "JNI, JNA, JNR-FFI, Project Panama, djinni, SWIG for Java, GraalVM Polyglot, Kotlin/Native C-interop, j2objc, Py4J, and Go-Java bridges. What each gets right, what each requires the user to write, and what MEP-70 borrows."
---

# 03. Prior-art JVM bridges

MEP-70 is not the first attempt to bridge native code and the JVM. This note surveys ten prior-art systems, cataloguing what each gets right, what boilerplate it requires, and what the Kotlin bridge borrows.

## JNI (Java Native Interface)

**What it is:** The official Java standard for calling C code from Java and vice versa. Every JVM ships with `jni.h` and supports `System.loadLibrary`. Functions are identified by mangled names (`Java_com_example_MyClass_myMethod`).

**What it gets right:** Zero dependencies beyond the JDK; works everywhere; battle-tested since Java 1.1.

**What it requires:** For each function you want to call, you write: (1) a `native` declaration in a Java class, (2) a C implementation with the mangled signature, (3) a `javah`/`javac -h` invocation to generate the header, (4) `System.loadLibrary` call in a static initialiser, (5) error handling for `UnsatisfiedLinkError`. For 100 functions, that is 500+ lines of glue spread across three languages.

**What MEP-70 borrows:** The JNI ABI is the wire protocol between GraalVM Native Image and the Mochi C layer. The bridge generates all JNI glue automatically; the user sees none of it.

## JNA (Java Native Access)

**What it is:** A library that uses Java reflection and libffi to call native shared libraries without writing C. The user declares a Java interface whose method names match the C function names.

**What it gets right:** Eliminates C code for the Java-calling-native direction. No compilation step for the Java side.

**What it requires:** Still requires the user to (1) know the C function signatures, (2) map C types to JNA types manually (`Pointer`, `NativeLong`, `Structure`), (3) handle lifecycle (library load, memory management). Not useful for the native-calling-JVM direction MEP-70 needs.

**What MEP-70 borrows:** The type-mapping annotation pattern (JNA's `@Structure.FieldOrder` and `Structure` subclassing for C structs) inspired the bridge's data-class accessor pattern.

## JNR-FFI

**What it is:** A higher-level native FFI library that underlies JRuby's native interop. Uses ASM bytecode generation to create efficient native call stubs at runtime.

**What it gets right:** More type-safe than JNA; generates actual bytecode rather than reflection-based dispatch; supports most C types including `off_t`, `size_t`, and calling conventions.

**What it requires:** Same declarative interface as JNA from the user's perspective. Not applicable to the native-calling-JVM direction.

**What MEP-70 borrows:** JNR's approach of treating the native interface as a first-class typed declaration (rather than raw `Pointer` handles) validates the bridge's strategy of emitting strongly-typed `extern type` declarations in the Mochi shim.

## Project Panama (java.lang.foreign, Java 22+)

**What it is:** The official Java standard replacement for JNI/JNA/JNR. Provides `MemorySegment`, `MemoryLayout`, `FunctionDescriptor`, and `MethodHandle`-based native call bindings. Includes `jextract` tool to auto-generate Java bindings from C headers.

**What it gets right:** Safe memory management (off-heap segments with explicit lifetime), typed function descriptors (no more raw `Pointer`), `jextract` eliminates manual binding for C headers.

**What it requires:** `jextract` consumes *C headers*, not Kotlin/JVM metadata. For the JVM-to-native direction, Panama provides `Linker.nativeLinker().downcallHandle(...)` which is cleaner than JNI but still requires per-function `FunctionDescriptor` declarations. For the native-to-JVM direction (MEP-70's case), Panama does not provide a solution; `jextract` only goes C→Java.

**What MEP-70 borrows:** `jextract`'s model of auto-generating Java bindings from a machine-readable interface description (C headers for Panama, `@kotlin.Metadata` for MEP-70) is the direct inspiration for the bridge's shim synthesis pipeline.

## djinni (Dropbox)

**What it is:** A cross-platform interface generator for sharing Kotlin/Java and Objective-C/Swift code. Given an IDL file, djinni generates JNI glue for Java/Kotlin and ObjC wrapper code for Apple platforms.

**What it gets right:** Eliminates hand-written JNI; generates type-safe wrappers for both platforms from a single IDL; handles lifecycle (object ownership, reference counting).

**What it requires:** The user writes a djinni IDL file for every interface they want to bridge. This is one-time boilerplate, but it is boilerplate nonetheless. There is no IDL for the entire Maven Central catalog.

**What MEP-70 borrows:** djinni's approach of synthesising both sides of the bridge (the Java/Kotlin side and the native side) from a single machine-readable description is the architectural template MEP-70 follows. MEP-70 replaces the hand-written IDL with auto-generated IDL derived from `@kotlin.Metadata`.

## SWIG (Simplified Wrapper and Interface Generator) for Java

**What it is:** A code generator that creates JNI wrappers from C/C++ header files, producing both C wrapper code and Java proxy classes.

**What it gets right:** Mature, widely used, handles complex C++ types.

**What it requires:** SWIG goes C++→Java, not Java→native. It is not applicable to MEP-70's direction. Mentioned for completeness because MEP-70 is the inverse: it takes the JVM API surface and emits a native wrapper.

## GraalVM Polyglot API

**What it is:** GraalVM's Truffle-based polyglot embedding: a host language (Java, Python, etc.) can create a `Context`, evaluate guest language code, and exchange values via the polyglot value API.

**What it gets right:** Zero-cost interop between Truffle languages running in the same JVM process; works for any Truffle-hosted language.

**What it requires:** The host *and* guest must both run inside a GraalVM JVM. For MEP-70, the Mochi runtime is a native binary compiled by the Go toolchain; it does not run inside a JVM. The Polyglot API is inapplicable.

**What MEP-70 borrows:** GraalVM's `native-image --shared` flag, which produces a `libwrap.so` with a JNI-compatible C ABI and the `graal_create_isolate` lifecycle, is the mechanism MEP-70's wrapper compiler uses. This is GraalVM technology but not the Polyglot API.

## Kotlin/Native C-interop

**What it is:** Kotlin/Native's mechanism for consuming C libraries. Given a `.def` file pointing to C headers, `cinterop` generates Kotlin wrappers that the Kotlin/Native compiler can use.

**What it gets right:** First-class Kotlin experience for C library consumption; handles memory management via `memScoped` and `cValuesOf`; supports structs, unions, and function pointers.

**What it requires:** The `.def` file is hand-written. The resulting Kotlin code only works in a Kotlin/Native context. For consuming JVM Kotlin libraries (the reverse direction), there is no official mechanism.

**What MEP-70 borrows:** The `.def` file model (a machine-readable description of a C library that generates typed wrappers) is analogous to MEP-70's `shim.mochi` (a machine-readable description of a Kotlin library that generates typed extern declarations). The key difference: MEP-70 synthesises the `.def`-equivalent from `@kotlin.Metadata` automatically.

## j2objc (Google)

**What it is:** A transpiler that converts Java source to Objective-C, used by Google to share Java library code with iOS apps.

**What it gets right:** Produces idiomatic Objective-C from idiomatic Java; handles the Java standard library by providing Objective-C equivalents.

**What it requires:** Source-to-source translation; requires the Java source code (not just the JAR); generates a large amount of Objective-C that must be compiled into the iOS app.

**What MEP-70 borrows:** j2objc demonstrates that the JVM class model can be faithfully represented in a non-JVM type system, validating the bridge's type-mapping approach. j2objc's handling of Java generics (type erasure at the Objective-C boundary) is directly analogous to MEP-70's monomorphise table.

## Py4J

**What it is:** A library that allows Python programs to call Java objects running in a JVM via a socket-based gateway.

**What it gets right:** Zero compilation step; any Java object is accessible from Python; bidirectional; works with arbitrary JVM versions.

**What it requires:** The JVM must be running as a server process; all calls go through a socket with JSON serialisation; latency is prohibitive for performance-sensitive code (microseconds → milliseconds per call).

**What MEP-70 borrows:** Py4J's model of dynamic proxy objects (Python objects that represent Java objects and dispatch method calls at runtime) is the highest-level abstraction in the space. MEP-70 rejects this model for performance reasons but its API surface inspection (Py4J's `gateway.entry_point.getClass().getMethods()`) illustrates the metadata-first approach.

## Go-Java bridges (gojava, gobind)

**What they are:** `gojava` is an early experiment in embedding a JVM inside a Go binary. `gobind` (part of the Go mobile toolchain) generates Go/Java/ObjC bindings for mobile apps.

**What they get right:** `gobind` demonstrates that Go can interop with Java on Android via JNI, which validates the Go-JNI connection MEP-70's wrapper uses.

**What they require:** `gobind` requires the user to annotate Go functions with `//go:generate gobind` directives and is tightly coupled to the Android/iOS mobile deployment model. The resulting Go API surface is limited to types gobind understands (primitives, strings, slices, and `error`).

**What MEP-70 borrows:** The fundamental feasibility validation: Go can call JNI functions, and JNI can reach into GraalVM Native Image. The bridge's Go-layer code (`package3/kotlin/wrapper/jni_caller.go`) uses the same technique gobind uses for Android, applied to GraalVM-produced shared libraries.

## Summary table

| System | Direction | Boilerplate required | MEP-70 borrows |
|--------|-----------|---------------------|----------------|
| JNI | both | C + Java per function | Wire ABI |
| JNA | native→JVM | Java interface per library | Type mapping model |
| JNR-FFI | native→JVM | Java interface per library | Typed declarations |
| Panama/jextract | native→JVM | auto from C headers | Auto-bind from metadata |
| djinni | both | IDL file per interface | Synthesise both sides |
| SWIG/Java | C++→Java | C++ headers (wrong direction) | N/A |
| GraalVM Polyglot | in-JVM only | None if in JVM | `--shared` flag |
| Kotlin/Native cinterop | C→Kotlin/Native | `.def` file per library | `.def`-equivalent auto-generation |
| j2objc | Java→ObjC | Java source required | Generic erasure strategy |
| Py4J | JVM↔Python | Socket gateway | Dynamic proxy concept |
| gobind | Go↔Java (Android) | `//go:generate` directives | Go-JNI feasibility |

No prior system provides automatic, no-boilerplate, bidirectional interop between a native compiled language and arbitrary Maven Central Kotlin artifacts from metadata alone. MEP-70 is new work.

## Cross-references

- [[02-design-philosophy]] for why GraalVM over Kotlin/Native and libjvm.so.
- [[04-kotlin-metadata-ingest]] for the metadata pipeline that replaces hand-written IDL.
- [[09-jvm-abi-stability]] for the GraalVM ABI versioning story.
