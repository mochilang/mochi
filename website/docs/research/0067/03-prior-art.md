---
title: "03. Prior-art bridges"
sidebar_position: 4
sidebar_label: "03. Prior-art bridges"
description: "GraalVM polyglot, Jython, JRuby, GoBridge, JNA/JNR-FFI, and what MEP-67 borrows from each."
---

# 03. Prior-art bridges

## GraalVM Polyglot

GraalVM's Truffle/Polyglot layer lets guest languages call Java objects by reference. The object model is first-class: no serialisation, no handle table. The cost is JVM lock-in: the binary must run on GraalVM. MEP-67 borrows the idea of treating opaque Java objects as handles but defers to JNI global refs instead of Truffle object references.

## Jython

Jython is a Python interpreter implemented in Java. It achieves bidirectional interop by running Python on the JVM. MEP-67 cannot use this approach: Mochi compiles to Go, not to JVM bytecode. Jython's type coercions (Python list to java.util.List) inform MEP-67's JSON-wire encoding for List/Map/Optional: both choose serialisation over a shared heap representation.

## JRuby

JRuby compiles Ruby to JVM bytecode and inherits Java's type system at runtime. Like Jython it achieves interop by living on the JVM. The JRuby team's work on boxing/unboxing primitive types and the `java_import` syntax directly inspired MEP-67's `import java` surface form.

## GoBridge (github.com/go-java)

GoBridge attempted to call Java from Go via JNI Invocation API and CGO. The project stalled in 2018. MEP-67 improves on GoBridge by adding: a content-addressed JAR cache, a reflection-based surface extractor, type mapping with a SkipReport, lock file integration, and a publish path. The CGO JNI embedding pattern is essentially the same.

## JNA / JNR-FFI

Java Native Access and Java Native Runtime call C/native code from Java. They operate in the reverse direction from MEP-67. MEP-67 reviewed JNA's `NativeLibrary` design and its `Pointer`-as-handle approach when designing `KindHandle`. The lesson: an opaque pointer handle is a workable substitute for a full type bridge when only a subset of an API needs marshalling.

## cglib / ByteBuddy

Java bytecode generation libraries used by mocking frameworks. MEP-67 does not generate bytecode: it generates Java source and uses `javac`. This keeps the toolchain simple (no ASM dependency) and produces human-readable wrapper source.
