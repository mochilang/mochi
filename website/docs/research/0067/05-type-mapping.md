---
title: "05. Type mapping"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The closed Java-to-Mochi type translation table, the 15 SkipReason constants, boxed-primitive collapse, JSON wire encoding, and the KindHandle fallback."
---

# 05. Type mapping

## The closed table

| Java type | Mochi Kind | FFI repr |
|-----------|-----------|----------|
| `int` | KindInt | `int` |
| `long` | KindLong | `long` |
| `float` | KindFloat | `float` |
| `double` | KindDouble | `double` |
| `boolean` | KindBool | `int` |
| `void` | KindVoid | `void` |
| `java.lang.String` | KindString | `String` |
| `byte[]` | KindBytes | `byte[]` |
| `java.lang.Integer` | KindInt | `int` |
| `java.lang.Long` | KindLong | `long` |
| `java.util.List<T>` | KindList | `String` (JSON) |
| `java.util.Map<K,V>` | KindMap | `String` (JSON) |
| `java.util.Optional<T>` | KindOptional | `String` (JSON) |
| `java.util.concurrent.CompletableFuture<T>` | KindFuture | `String` (callback) |
| Unknown class | KindHandle | `long` (JNI ref) |

## Boxed-primitive collapse

`java.lang.Integer` and `java.lang.Long` map to the same `KindInt`/`KindLong` as their primitive counterparts. The JNI wrapper unboxes the value (`.intValue()`, `.longValue()`) before returning it across the boundary.

## JSON wire encoding

`List<T>`, `Map<K,V>`, and `Optional<T>` are marshalled to JSON strings using `com.google.gson.Gson` (or `Jackson` if present) in the wrapper class. The Go side unmarshals the JSON string into the corresponding Mochi type. This avoids the complexity of a shared-heap object protocol at the cost of a serialisation round-trip per call.

## The 15 SkipReasons

Methods are skipped (not bridged) when they involve: `SkipRawObject`, `SkipWildcard`, `SkipInnerClass`, `SkipVarArgs`, `SkipReflectiveType`, `SkipDeprecated`, `SkipSynchronized`, `SkipVolatile`, `SkipNative`, `SkipUncheckedGeneric`, `SkipFunctionalInterface`, `SkipAnnotationType`, `SkipEnumOrdinal`, `SkipThrowable`, `SkipConstructorOnly`. Each skip is recorded in a `SkipReport` with the method name, class name, and reason constant.

## KindHandle

When a method returns a class that is not in the closed table and not in the skip set, the return type is mapped to `KindHandle`: a `long` holding a JNI global reference. The handle can be passed as the first argument to other bridged instance methods. The JVM manages the referenced object's lifetime; the bridge runtime releases the global reference when the Mochi GC finalises the handle.
