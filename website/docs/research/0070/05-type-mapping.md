---
title: "05. Type mapping table"
sidebar_position: 6
sidebar_label: "05. Type mapping"
description: "The complete closed Kotlin→Mochi translation table, the refusal cases, the nullable-T? to Option<T> desugar, the generic monomorphisation rule, the sealed-class discriminant strategy, the data-class field accessor pattern, and the platform type handling."
---

# 05. Type mapping table

The bridge translates Kotlin types to Mochi types at lock time. The translation is closed: every Kotlin type is either in the table and maps to a Mochi type, or it is in the refusal set and is omitted from the synthesised shim with a diagnostic. This closed-world guarantee lets the bridge generate a shim that the Mochi type checker can accept without special-casing unknown types.

## Primitive and scalar types

| Kotlin type | Mochi type | JNI wire type | Notes |
|-------------|-----------|---------------|-------|
| `kotlin.Int` | `int` | `jint` (32-bit) | Mochi `int` is 64-bit; widening is safe. |
| `kotlin.Long` | `long` | `jlong` (64-bit) | Exact match. |
| `kotlin.Short` | `int` | `jshort` (16-bit) | Widened. |
| `kotlin.Byte` | `int` | `jbyte` (8-bit) | Widened. |
| `kotlin.Double` | `double` | `jdouble` (64-bit) | Exact match. |
| `kotlin.Float` | `float` | `jfloat` (32-bit) | Exact match (Mochi also has `float`). |
| `kotlin.Boolean` | `bool` | `jboolean` (uint8) | `JNI_TRUE`/`JNI_FALSE`. |
| `kotlin.Char` | `int` | `jchar` (UTF-16 code unit) | The bridge converts to a Mochi `int` (Unicode code point). |
| `kotlin.String` | `string` | `jstring` | UTF-16 JNI string → UTF-8 Mochi string at the wrapper boundary. |
| `kotlin.Unit` | (void) | `void` | `extern fn` with no return type. |
| `kotlin.Nothing` | (void) | `void` | Functions returning `Nothing` always throw; mapped to void with a thrown-exception error path. |
| `kotlin.Any` | `any` | `jobject` | Opaque reference. No further inspection. |

## Nullable types

Every Kotlin nullable type `T?` maps to `Option<T>` in Mochi:

| Kotlin | Mochi |
|--------|-------|
| `kotlin.String?` | `Option<string>` |
| `kotlin.Int?` | `Option<int>` |
| `MyClass?` | `Option<MyClass>` |
| `List<T>?` | `Option<List<T>>` |

At the JNI boundary, `null` maps to `Option.None` and a non-null value to `Option.Some(v)`. The wrapper generates a null-check before constructing the `Option.Some`.

## Collection types

| Kotlin type | Mochi type | Notes |
|-------------|-----------|-------|
| `kotlin.collections.List<T>` | `List<T>` | Copied across; not a live mutable reference. |
| `kotlin.collections.MutableList<T>` | `List<T>` | Mutable and immutable lists have the same Mochi type; mutations on the Kotlin side are not visible after the call returns. |
| `kotlin.collections.Set<T>` | `Set<T>` | Copied. |
| `kotlin.collections.MutableSet<T>` | `Set<T>` | Copied. |
| `kotlin.collections.Map<K,V>` | `Map<K,V>` | Copied. |
| `kotlin.collections.MutableMap<K,V>` | `Map<K,V>` | Copied. |
| `kotlin.Array<T>` | `List<T>` | JVM array; converted to Mochi list on copy. |
| `kotlin.IntArray` | `List<int>` | Primitive int array. |
| `kotlin.LongArray` | `List<long>` | Primitive long array. |
| `kotlin.ByteArray` | `bytes` | Mochi `bytes` type (if available) or `List<int>`. |

Collections are always copied, never shared as live references. This is a deliberate trade-off: live shared mutable collections across a JNI boundary create race conditions and memory-management complexity. The bridge copies on every call. For performance-sensitive code, the user should batch operations inside Kotlin.

## Structured types: data classes

A Kotlin `data class` is mapped to an `extern type` with named accessor functions for each property:

```kotlin
data class User(val id: Long, val name: String, val email: String?)
```

becomes:

```mochi
extern type User
extern fn user_new(id: long, name: string, email: Option<string>): User from kotlin "com.example.User"
extern fn user_id(u: User): long from kotlin "com.example.User.id"
extern fn user_name(u: User): string from kotlin "com.example.User.name"
extern fn user_email(u: User): Option<string> from kotlin "com.example.User.email"
extern fn user_copy(u: User, id: Option<long>, name: Option<string>, email: Option<Option<string>>): User from kotlin "com.example.User.copy"
```

The `copy` function uses `Option` for every parameter to match Kotlin's default-argument semantics: `Option.None` means "use the original value", `Option.Some(v)` means "override with v".

## Structured types: enum classes

```kotlin
enum class Status { ACTIVE, INACTIVE, PENDING }
```

becomes:

```mochi
extern type Status
extern fn status_active(): Status from kotlin "com.example.Status.ACTIVE"
extern fn status_inactive(): Status from kotlin "com.example.Status.INACTIVE"
extern fn status_pending(): Status from kotlin "com.example.Status.PENDING"
extern fn status_ordinal(s: Status): int from kotlin "com.example.Status.ordinal"
extern fn status_name(s: Status): string from kotlin "com.example.Status.name"
```

The enum values are exposed as zero-argument constructor functions. Kotlin's `values()` and `valueOf()` functions are also emitted.

## Structured types: sealed classes

```kotlin
sealed class Result {
    data class Success(val value: String) : Result()
    data class Error(val message: String, val code: Int) : Result()
}
```

becomes:

```mochi
extern type Result
extern fn result_variant(r: Result): int from kotlin "com.example.Result.__mochi_variant"
// 0 = Success, 1 = Error
extern fn result_is_success(r: Result): bool from kotlin "com.example.Result.__mochi_is_success"
extern fn result_is_error(r: Result): bool from kotlin "com.example.Result.__mochi_is_error"
extern fn result_as_success(r: Result): Option<ResultSuccess> from kotlin "com.example.Result.__mochi_as_success"
extern fn result_as_error(r: Result): Option<ResultError> from kotlin "com.example.Result.__mochi_as_error"

extern type ResultSuccess
extern fn result_success_new(value: string): ResultSuccess from kotlin "com.example.Result.Success"
extern fn result_success_value(s: ResultSuccess): string from kotlin "com.example.Result.Success.value"

extern type ResultError
extern fn result_error_new(message: string, code: int): ResultError from kotlin "com.example.Result.Error"
extern fn result_error_message(e: ResultError): string from kotlin "com.example.Result.Error.message"
extern fn result_error_code(e: ResultError): int from kotlin "com.example.Result.Error.code"
```

The `__mochi_variant`, `__mochi_is_*`, and `__mochi_as_*` functions are synthesised in the JNI wrapper (not present in the original Kotlin source) using `instanceof` checks in the generated Java shim code.

## Structured types: object singletons

```kotlin
object HttpClient {
    fun get(url: String): String { ... }
}
```

becomes:

```mochi
extern type HttpClient
extern fn http_client_instance(): HttpClient from kotlin "com.example.HttpClient.INSTANCE"
extern fn http_client_get(client: HttpClient, url: string): string from kotlin "com.example.HttpClient.get"
```

The singleton instance is obtained via `INSTANCE` (the JVM field that Kotlin compiles `object` to).

## Companion objects

```kotlin
class Foo {
    companion object {
        fun create(name: String): Foo { ... }
        val DEFAULT: Foo = Foo("default")
    }
}
```

becomes:

```mochi
extern type Foo
extern fn foo_create(name: string): Foo from kotlin "com.example.Foo.Companion.create"
extern fn foo_default(): Foo from kotlin "com.example.Foo.Companion.DEFAULT"
```

Companion object functions are promoted to artifact-level functions and prefixed with the class name.

## Kotlin Result<T>

```kotlin
Result<T>   →   Option<T>   (if the error branch is ignored)
              or a two-case shim (preferred when errors matter):
extern fn result_is_success(r: KotlinResult): bool
extern fn result_get_or_null(r: KotlinResult): Option<T>
extern fn result_exception_message(r: KotlinResult): Option<string>
```

Kotlin's built-in `kotlin.Result<T>` value class is a special case: it is an inline class over `Any?` that either holds a value or a `Throwable`. The bridge unwraps it and emits the three-function pattern above.

## Interface types

Kotlin interfaces that appear in function signatures are mapped to `extern type` (opaque handles). The bridge does not emit callable functions on interface types unless the interface is also defined in the same JAR (or a dependency JAR) and has concrete function bodies (Kotlin's `fun interface` or `interface with default implementations`). Pure abstract interfaces produce opaque `extern type` declarations only.

## Refusal set

The following types and constructs are **not emitted** into the shim. A `WARN` diagnostic is printed for each omitted item, naming the function and the problematic type.

| Kotlin construct | Reason |
|-----------------|--------|
| `fun <T> f(x: T)` (unresolved type parameter) | Cannot emit without concrete `T`. Add to `monomorphise`. |
| `inline reified fun <reified T> f()` without `monomorphise` entry | `T` is erased at JVM bytecode level; cannot call without knowing `T`. |
| `dynamic` type | Kotlin/JS only; cannot appear in JVM bytecode. |
| `java.lang.Throwable` and subclasses as return types | Exceptions are not values in Mochi; use `Option` or a sealed Result. |
| `kotlin.coroutines.Continuation<T>` | Raw continuation type; use `suspend fun` bridge instead. |
| `kotlin.jvm.functions.FunctionN<*>` (raw lambda type) | Cannot represent as a Mochi value without full closure support. |
| `kotlin.reflect.KClass<T>` | Runtime reflection type; no Mochi equivalent. |
| `kotlin.reflect.KFunction<T>` | Runtime reflection type; no Mochi equivalent. |
| Types requiring `@JvmField` with mutable field access | The bridge reads via accessor functions only. |
| Java array types `T[]` beyond the primitive arrays listed above | Prefer `List<T>` in the Kotlin API design; `T[]` for non-primitive T requires per-element JNI calls. |
| `kotlin.UInt`, `kotlin.ULong`, etc. (unsigned inline classes on JVM < 21) | JVM 17 and below have no native unsigned integer type. |

Functions that contain any refusal-set type in their parameter list or return type are omitted entirely from the shim for that type.

## Platform types

Java API types that Kotlin surfaces as "platform types" (neither definitively nullable nor non-nullable, written as `T!` in error messages) are treated as non-nullable by the bridge. This is consistent with Kotlin's own behaviour (caller must handle potential `null` if they know the API returns null). If a platform-type function returns null, the JNI wrapper will receive a `null` `jobject` and the bridge will return `Option.None` even though the declared Mochi type is `T` (non-optional). This is the one case where the Mochi type may not match the runtime behaviour; it is documented in the generated shim with a comment.

## Cross-references

- [[04-kotlin-metadata-ingest]] for how the `APIObject` tree is constructed.
- [[10-generics-reification]] for the `monomorphise` table and reified function handling.
- [[08-coroutines-bridge]] for `suspend fun` mapping.
- [MEP-70 §8](/docs/mep/mep-0070#8-type-mapping) for the normative table in the spec.
