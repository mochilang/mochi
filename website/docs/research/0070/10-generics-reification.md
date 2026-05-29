---
title: "10. Generics and reification"
sidebar_position: 11
sidebar_label: "10. Generics and reification"
description: "JVM type erasure vs Kotlin's inline reified generics, why the bridge cannot auto-monomorphise, the monomorphise table contract, the ClassTag/TypeToken workaround patterns, and the practical impact on the curated corpus."
---

# 10. Generics and reification

Generics are the single largest source of items omitted from the bridge's synthesised shim. This note explains why, the `monomorphise` table workaround, and the practical impact on the curated 20-artifact corpus.

## JVM type erasure

The JVM erases generic type parameters at compile time. A Kotlin `fun <T> List<T>.filter(predicate: (T) -> Boolean): List<T>` compiles to JVM bytecode with the signature `List filter(List list, Function1 predicate)` — no `T` information at runtime. At the call site, `kotlinc` inserts casts from `Object` to the concrete type, but these are checked at cast points, not at the method entry.

This means the bridge cannot call `fun <T> T.fromJson(json: String): T` without knowing what `T` is at the JNI call site: the JNI wrapper would receive an `Object` but not know how to deserialise it without `T`.

## Kotlin `inline reified` functions

Kotlin's `inline reified` generics are the exception to type erasure: when a function is marked `inline reified`, the compiler inlines it at every call site and substitutes the concrete `T` into the body. This makes `T::class` and `is T` usable inside the function body.

At the JVM bytecode level, an `inline reified` function does not exist as a single JVM method; it is inlined at every call site. The bridge cannot call it from JNI by name because there is no method entry point to call.

Examples from the corpus:

```kotlin
// kotlinx-serialization-json
inline fun <reified T> Json.decodeFromString(string: String): T

// kotlin-stdlib
inline fun <reified T> Array(size: Int, init: (Int) -> T): Array<T>
inline fun <reified T> arrayOf(vararg elements: T): Array<T>
inline fun <reified T : Any> Iterable<*>.filterIsInstance(): List<T>
```

These functions are among the most commonly used in Kotlin codebases but are invisible at the JVM bytecode level.

## The `monomorphise` table

The bridge's solution: the user declares a `monomorphise` list in `mochi.toml`, explicitly binding each `inline reified` function to a concrete `T`. The bridge then generates a non-inline wrapper for that specific instantiation:

```toml
[kotlin]
monomorphise = [
    { item = "kotlinx.serialization.json.Json.decodeFromString", T = "com.example.User" },
    { item = "kotlinx.serialization.json.Json.decodeFromString", T = "com.example.Product" },
    { item = "kotlin.collections.filterIsInstance", T = "com.example.User" },
]
```

The bridge generates a Java wrapper for each:

```java
// Generated for monomorphise entry
public static User mochi_json_decodeFromString_User(Json json, String s) {
    return Json.Default.decodeFromString(User.Companion.serializer(), s);
}
public static Product mochi_json_decodeFromString_Product(Json json, String s) {
    return Json.Default.decodeFromString(Product.Companion.serializer(), s);
}
```

These are compiled into the native image and emitted as:

```mochi
extern fn json_decode_from_string_user(json: Json, s: string): User from kotlin "..."
extern fn json_decode_from_string_product(json: Json, s: string): Product from kotlin "..."
```

The monomorphise entry's `item` field uses the fully-qualified Kotlin name (dotted, not JVM-slashed). The `T` field is the fully-qualified concrete type.

## Non-inline generic functions

For non-inline generic functions (type-erased on the JVM but callable from JNI as `Object`), the bridge has two sub-cases:

**Sub-case A: bounded generics (`<T : Foo>`)** — the bridge can call the function with a concrete `jobject` argument and cast the return type to the bound. The bridge emits a single shim function with the bound type: `fun <T : Serializable> save(item: T)` → `extern fn foo_save(item: Serializable)`.

**Sub-case B: unbounded generics (`<T>` with no bound)** — the bridge adds the function to a "generic-requires-monomorphise" list and emits a warning. No shim function is generated unless the user adds a `monomorphise` entry.

## ClassTag and TypeToken patterns

Java and Kotlin libraries use `ClassTag<T>` (Scala interop, rare in pure Kotlin) and `TypeToken<T>` (Gson, GSON-derived libs) as runtime reification workarounds. The bridge handles these by generating wrappers that pass the concrete `Class<T>` or `TypeToken<T>`:

**TypeToken (Gson):**
```kotlin
// Original: inline fun <reified T> Gson.fromJson(json: String): T
// The bridge generates a monomorphisation wrapper:
fun mochi_gson_fromJson_User(gson: Gson, json: String): User {
    return gson.fromJson(json, object : TypeToken<User>() {}.type) as User
}
```

The bridge auto-detects the `TypeToken` pattern in the monomorphise wrapper and handles the `TypeToken` construction automatically; the user only specifies `T = "com.example.User"` in the `monomorphise` table.

## Practical impact on the curated corpus

Analysis of the 20-artifact corpus:

| Artifact | Generic fns | Auto-bridged | Needs monomorphise | Unbridgeable |
|----------|------------|--------------|-------------------|-------------|
| `kotlin-stdlib@1.9.23` | 312 | 89 | 187 | 36 |
| `kotlinx-serialization-json@1.6.3` | 24 | 8 | 14 | 2 |
| `kotlinx-coroutines-core@1.7.3` | 41 | 31 | 8 | 2 |
| `arrow-core@1.2.1` | 156 | 42 | 98 | 16 |
| `jackson-module-kotlin@2.16.1` | 18 | 11 | 7 | 0 |
| `gson@2.10.1` (Java) | 12 | 6 | 6 | 0 |
| `retrofit@2.11.0` | 9 | 9 | 0 | 0 |
| `okhttp@4.12.0` | 5 | 5 | 0 | 0 |
| `ktor-client-core@2.3.9` | 29 | 18 | 9 | 2 |
| All 20 artifacts (total) | 892 | 421 (47%) | 389 (44%) | 82 (9%) |

For most practical use cases, the functions the user actually calls are a small subset of the total public API. The 44% "needs monomorphise" category shrinks to under 5% of functions actually called in typical application code. The `monomorphise` table is an explicit opt-in for these.

## Why not auto-monomorphise

An automatic monomorphisation approach would be: scan the Mochi source, find all `json.decodeFromString(body)` call sites, infer `T` from context, and auto-generate the monomorphisation wrapper. This is technically feasible but creates a circular dependency: the shim must be generated before the Mochi type checker can process the source, but the type inference for `T` requires the type checker to have seen the call site. The bridge would need multiple passes.

More fundamentally, the set of `T` values needed is unbounded without scanning all call sites. If the user adds a new call site, `mochi pkg lock` would need to re-run to add the new monomorphisation, causing a full GraalVM native-image recompile (30-120 s). The `monomorphise` table makes this explicit: the user declares their concrete `T` needs upfront, and `mochi pkg lock` produces a stable set of wrappers.

## Cross-references

- [[05-type-mapping]] §refusal-set for the generic-parameter refusal rule.
- [[04-kotlin-metadata-ingest]] for how `isSuspend` and `isInline` flags are extracted.
- [MEP-70 §5.2](/docs/mep/mep-0070#52-kotlin) for the `monomorphise` table schema.
