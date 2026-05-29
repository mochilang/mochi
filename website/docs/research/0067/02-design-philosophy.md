---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why JNI over GraalVM polyglot, why Java reflection over annotation databases, and why KindHandle for unmappable types."
---

# 02. Design philosophy

## Why JNI and not GraalVM polyglot

GraalVM's Polyglot API can call Java from a guest language but requires running on GraalVM, not stock HotSpot or OpenJ9. MEP-67 targets any compliant JVM. JNI has been part of the JVM specification since Java 1.1 and is supported on every JVM Mochi is likely to encounter in production: HotSpot (OpenJDK), Eclipse OpenJ9, GraalVM (which also supports JNI alongside its polyglot API), and Android's ART.

## Why JNI and not JNA or JNR-FFI

JNA and JNR-FFI load native libraries from Java. MEP-67 goes the other direction: loading the JVM from a Go/Mochi binary. JNA/JNR-FFI do not provide a path to embed a JVM inside a Go process. The `libjvm` shared library plus JNI Invocation API is the only standard mechanism for that embedding.

## Why Java reflection over annotation databases

Third-party annotation databases (typedefs repositories, manually curated JSON schemas) go stale and have incomplete coverage. Java reflection (`java.lang.reflect`) operates directly on the compiled bytecode shipped in the JAR: the output is exactly as accurate as what the end user's code sees. There is no coverage gap and no schema drift.

## Why KindHandle for unmappable types

Rather than refusing the entire class when one method uses an unmappable return type, MEP-67 assigns the `KindHandle` kind (backed by a `long` JNI global reference) to the return value. The caller can pass the handle back to other bridged methods that accept it as a parameter. This preserves the usability of classes that mix mappable and unmappable APIs. The 15 `SkipReason` constants log which methods are excluded and why, giving users a clear picture of the bridge surface.

## Why the closed type table

An open type table (attempt to map every Java type) risks silent semantic mismatches. The closed table (15 base types, explicit refusal set) makes the boundary legible: if a Java type is not in the table, it either gets KindHandle or a SkipReport. Users can audit the SkipReport to decide whether to add a hand-written shim.
