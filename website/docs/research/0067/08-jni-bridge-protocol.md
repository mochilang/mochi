---
title: "08. JNI bridge protocol"
sidebar_position: 9
sidebar_label: "08. JNI bridge protocol"
description: "JNI calling convention, global object references, CGO embedding of the JVM, the java_jni build tag, and latency vs GraalVM polyglot."
---

# 08. JNI bridge protocol

## JNI calling convention

JNI native methods follow a predictable C symbol naming convention:

```
Java_{ClassName}_{methodName}
```

where `ClassName` uses `_` instead of `.` and `/`. The wrapper Java class declares static native methods; the Go side provides the implementations via CGO. The JNI method descriptor string encodes parameter and return types in the JVM's internal type notation.

## Global object references

The JVM garbage collector can move objects; a raw JNI `jobject` is only valid on the thread that received it and within the current JNI frame. To pass Java objects between calls, MEP-67 promotes them to JNI global references (`NewGlobalRef`) and stores them in a process-wide handle table keyed by `int64`. `KindHandle` return values carry the table key. The bridge releases global references (`DeleteGlobalRef`) when the handle is finalised.

## CGO JVM embedding

The `jni.Runtime.New` function calls `JNI_CreateJavaVM` from `libjvm` via CGO. The JVM is started with the bridge's wrapper JAR (and the upstream JAR) on the classpath. Only one JVM can be created per process (JNI Invocation API constraint); subsequent `New` calls return the existing `*Runtime`. The `java_jni` build tag guards all CGO code; the stub file (`jni_stub.go`) compiles without CGO and returns `ErrJNIUnavailable`.

## Latency

JNI method dispatch costs approximately 50-200 ns per call on HotSpot x86_64, compared to 5-20 ns for a C function call via CGO alone. For methods that return complex types via JSON serialisation the serialisation cost dominates. For methods returning primitives (int, long, boolean) the JNI overhead is the dominant cost. Applications with tight call loops should batch work on the Java side and return aggregated results.
