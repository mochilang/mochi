---
title: "Phase 04: Type mapping"
sidebar_position: 6
sidebar_label: "Phase 04: Type mapping"
description: "Implement the closed Kotlin→Mochi type translation table, the refusal set, and the nullable-T? to Option<T> desugar."
---

# Phase 04: Type mapping

**Status:** Planned

## Deliverables

1. `package3/kotlin/typemap/map.go` — `Translate(kt KotlinType) (MochiType, bool)` — returns the Mochi type and `false` if the type is in the refusal set.
2. `package3/kotlin/typemap/table.go` — the closed translation table as a Go data structure (no reflection).
3. `package3/kotlin/typemap/refusal.go` — `IsRefused(kt KotlinType) RefusalReason` — identifies why a type is not bridgeable.
4. `package3/kotlin/typemap/nullable.go` — nullable type desugar: `T?` → `Option<T>`.
5. `package3/kotlin/typemap/collections.go` — collection type desugar: `List<T>` → `List<T>` (recursive translation of type arguments).

## Refusal reasons

```go
type RefusalReason int
const (
    NotRefused RefusalReason = iota
    RefusalUnresolvedTypeParam
    RefusalInlineReifiedNoMonomorphise
    RefusalDynamicType
    RefusalThrowableReturn
    RefusalRawContinuation
    RefusalRawLambda
    RefusalKClassReflection
    RefusalUnsignedIntJVM17
    RefusalJavaNonPrimitiveArray
)
```

## Gate

Type-map all public functions of all 20 corpus artifacts. Validate:

1. `kotlin.Int` → `int`.
2. `kotlin.String?` → `Option<string>`.
3. `List<kotlin.Long>` → `List<long>`.
4. `Map<kotlin.String, kotlin.Int>` → `Map<string, int>`.
5. `data class User(val id: Long, val name: String)` → `extern type User` with accessor fns.
6. `enum class Status { ACTIVE, INACTIVE }` → `extern type Status` with constructor fns.
7. `sealed class Result` with two subclasses → discriminant + sub-type accessors.
8. `suspend fun fetch(): String` → `RefusalRawContinuation` (without coroutines bridge config).
9. `fun <T> parse(s: String): T` → `RefusalUnresolvedTypeParam`.
10. Zero panics when processing all 20 corpus JARs.
