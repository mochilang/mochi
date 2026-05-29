---
title: "11. Generics and type erasure"
sidebar_position: 12
sidebar_label: "11. Generics and type erasure"
description: "Java type erasure at the bytecode level, GenericSignature parser, EraseType/ErasedParams utilities, and the monomorphisation strategy."
---

# 11. Generics and type erasure

## Type erasure

Java compiles generic types to their erased form: `List<String>` becomes `List` in bytecode. The JVM has no runtime knowledge of the type parameter. Reflection on `Method.getGenericReturnType()` returns the pre-erasure generic type string if the class was compiled with debug information; otherwise it returns the erased type.

## GenericSignature

`typemap.GenericSignature` parses JVM generic type descriptor strings of the form `ClassName<Param1,Param2>`. It supports:

- Raw types (`List` with no params, `IsRaw() == true`)
- Single-param types (`List<String>`, `Optional<Integer>`)
- Two-param types (`Map<String,Integer>`)
- Nested generics (`Map<String,List<Integer>>`, handled by `splitTopLevelParams` which tracks bracket depth)
- Wildcard detection (`WildcardErased() == true` when any param contains `?`)

## EraseType / ErasedParams

`reflect.EraseType(name string) string` strips the `<...>` suffix from a Java type name, returning the raw class name. `ErasedParams` returns the top-level type parameter strings. `NormaliseTypeName` converts JVM internal notation (`$` inner-class separator and `[]` array suffix) to the form used in the reflection surface JSON.

## Monomorphisation strategy

For the type table, MEP-67 monomorphises only the supported generic containers:

- `List<T>`: T must be in the closed table or the method is skipped with `SkipUncheckedGeneric`.
- `Map<K,V>`: K and V must both be in the closed table.
- `Optional<T>`: T must be in the closed table.
- `CompletableFuture<T>`: T must be in the closed table.

This conservative strategy avoids the complexity of full parametric polymorphism while covering the most common generic APIs in Maven Central.
