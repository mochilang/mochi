---
title: "10. Generics and erasure"
sidebar_position: 11
sidebar_label: "10. Generics and erasure"
description: "Java generics type erasure, recovery via getGenericParameterTypes and ParameterizedType, what is recoverable vs lost, the [java.monomorphise] table, wildcard bounds, raw types, inner generic classes, and Kotlin reified generics when importing Kotlin JVM libraries."
---

# 10. Generics and erasure

This note covers the most complex part of the Java type bridge: Java generics and their interaction with type erasure.

## 1. What type erasure means

Java generics were introduced in Java 5 (2004) with a design constraint: backward compatibility with JVM bytecode that predates generics. The solution was type erasure: generic type parameters are removed at compile time, and the bytecode sees only the erased types.

```java
// Source code
public class Box<T> {
    private T value;
    public T get() { return value; }
    public void set(T value) { this.value = value; }
}

// Bytecode (after erasure) -- effectively compiled to:
public class Box {
    private Object value;
    public Object get() { return value; }
    public void set(Object value) { this.value = value; }
}
```

At the JNI level, `Box<String>` and `Box<Integer>` are the same class. `GetMethodID(env, boxClass, "get", "()Ljava/lang/Object;")` works for both.

## 2. What is recoverable via the Signature attribute

The Java compiler writes generic type information into the `Signature` bytecode attribute. This attribute is read by `java.lang.reflect` and exposed via:

- `Method.getGenericReturnType()` returns the generic return type (e.g., `T` for `Box.get()`, or `List<String>` for a method returning `List<String>`).
- `Method.getGenericParameterTypes()` returns the generic parameter types.
- `Field.getGenericType()` returns the generic type for fields.
- `Class.getTypeParameters()` returns the type variable declarations (`<T>`, `<K, V>`, etc.).

When the `Signature` attribute is present, the reflection API returns `ParameterizedType`, `TypeVariable`, `WildcardType`, or `GenericArrayType` instances (as described in [[04-reflection-api-ingest]]). When it is absent (JAR compiled with `-g:none`, or obfuscated), the reflection API returns plain `Class<?>` instances (the erased type).

What is always recoverable:

- Declared type parameters on a class: `class Box<T>` gives `TypeVariable[] { T }` from `getDeclaredTypeParameters()`.
- Declared type parameters on a method: `<T> List<T> emptyList()` gives `TypeVariable[] { T }`.
- Parameterized return types: `List<String>` returns a `ParameterizedType` with raw type `List` and actual type arguments `[String]`.
- Parameterized parameter types: same as return types.
- Wildcard bounds: `? extends Number` returns a `WildcardType` with upper bounds `[Number]`.

What is NOT recoverable (erased):

- Call-site instantiations: if someone calls `new Box<String>()`, the bytecode just calls `new Box()`. There is no bytecode record of the `<String>` argument.
- Runtime `instanceof` checks on generic types: `if (box instanceof Box<String>)` is a compile error in Java precisely because the type information is not available at runtime.

## 3. Bridge behaviour for generic types

### Type variables bound to known types (e.g., `List<String>`)

A method `public List<String> getNames()` has `getGenericReturnType()` returning a `ParameterizedType` with raw type `java.util.List` and actual type arguments `[java.lang.String]`. The bridge translates this as:

```
List<String> -> list<string>
```

The bridge can translate this concretely because the type argument is known and in the translation table.

### Type variables free (e.g., `T get()` on `Box<T>`)

A method `public T get()` on class `Box<T>` has `getGenericReturnType()` returning `TypeVariable { name = "T", bounds = [Object] }`. The bridge cannot translate this without knowing `T`. Options:

1. **Refuse (SkipReport)**: emit a SkipReport entry `"method Box.get() uses free type variable T; add a [java.monomorphise] entry to instantiate it"`. This is the default.
2. **Map to `any`**: treat `T` as `any` (an unsafe, dynamically-typed value). Not supported in v1 (Mochi does not have a universal `any` type).
3. **Monomorphise** (via `[java.monomorphise]`): the user specifies that `Box<String>` is the instantiation they want, and the bridge generates a method `box_String_get(): string`.

### [java.monomorphise] table

```toml
[java]
monomorphise = [
    { class = "com.example.Box", T = "java.lang.String" },
    { class = "com.example.Pair", K = "java.lang.String", V = "java.lang.Integer" },
    { method = "com.example.Utils.emptyList", T = "java.lang.String" },
]
```

For each entry, the bridge generates a specialised wrapper with the type variable substituted:

- `Box<String>` generates `extern type example_Box_String` and `extern fn example_Box_String_get(): string from java "..."`.
- `Pair<String, Integer>` generates `extern type example_Pair_String_Integer`, etc.

The monomorphise table is the only path to import a class or method with free type variables. The bridge does not auto-monomorphise (the combinatorial explosion of all possible instantiations would be unbounded; the user must declare which instantiations they need).

## 4. Wildcard bounds

Java wildcards appear in two forms:

| Java wildcard | Meaning | Bridge handling |
|--------------|---------|-----------------|
| `? extends T` | An unknown type that is a subtype of T | Map to `T_mochi` if T is in table (treat as `T`; the widening is safe). |
| `? super T` | An unknown type that is a supertype of T | SkipReport: "lower-bounded wildcard `? super T` not translatable; use `T` directly or add a monomorphise entry." |
| `?` (unbounded) | An unknown type | SkipReport: "unbounded wildcard not translatable; use Object-returning method or add a monomorphise entry." |

`? extends T` with T in the translation table is the most common case. For example, `List<? extends Number>` is mapped to `list<float>` (Number -> float, the widest numeric type). `List<? extends String>` is mapped to `list<string>` (String extends Object, and the wildcard upper bound is String).

`? super T` (contravariant wildcard) appears in producer positions (e.g., `void addAll(Collection<? super T> c)`). The bridge does not translate these because the consumer of the collection can add items of type T but cannot safely read them as T (they would be typed as Object). SkipReport is emitted.

## 5. Raw types

A raw type is a generic class used without type arguments (e.g., `List` instead of `List<T>`). Raw types appear in:

- Pre-Java-5 code that predates generics.
- Some bridging code that erases types intentionally.
- Reflection-heavy code that creates instances dynamically.

The bridge maps raw `java.util.List` to `list<any>` with a warning:

```
SkipReport [WARN] method Foo.getItems() returns raw List; mapped to list<any> with no element type safety.
```

`list<any>` is not a well-typed Mochi type in v1 (Mochi requires typed list elements). The bridge maps it to `list<string>` as a fallback (the most common case for raw `List` is that it actually contains strings or other string-convertible items), with a comment in the generated shim that the mapping is best-effort.

## 6. Inner generic classes

Generic inner classes (e.g., `ImmutableMap.Builder<K, V>`) are handled like outer generic classes: free type variables emit SkipReport entries, and the `[java.monomorphise]` table can specify instantiations.

```toml
[java]
monomorphise = [
    { class = "com.google.common.collect.ImmutableMap$Builder", K = "java.lang.String", V = "java.lang.Integer" },
]
```

The `$` in the class name is used for inner class lookup in the JNI bridge (`FindClass(env, "com/google/common/collect/ImmutableMap$Builder")`).

## 7. Kotlin reified generics

Kotlin (which runs on the JVM and whose libraries are published to Maven Central) has a feature called reified generics: when a function is declared `inline fun <reified T> ...`, the type argument T is available at runtime via `T::class` (which compiles to bytecode that calls `Class.forName`).

When MEP-67 imports a Kotlin library (e.g., `org.jetbrains.kotlinx:kotlinx-coroutines-core`), ReflectTool sees the Kotlin bytecode as standard Java bytecode. Kotlin's reified generics are not visible through `java.lang.reflect` (because the inline functions are inlined at the call site in Kotlin code; the function itself does not appear in the JAR). The bridge treats Kotlin libraries as Java libraries; Kotlin-specific features (reified generics, inline functions, extension functions, data classes, sealed classes) are handled as follows:

- **Extension functions**: appear as static methods in a companion class (e.g., `StringsKt.trimIndent(str)` for `String.trimIndent()`). The bridge exposes them as `extern fn`.
- **Data classes**: appear as regular classes with auto-generated `component1()`, `component2()`, `equals()`, `hashCode()`, `toString()`, `copy()` methods. The bridge exposes them as `extern type` with all methods.
- **Sealed classes**: appear as abstract classes with a set of permitted subclasses. The bridge exposes the abstract class as `extern type` and the subclasses as `extern type` (no Mochi sum type mapping in v1; the user receives an opaque handle and calls `instanceof` via a `_isInstance` extern fn).
- **Inline functions**: not visible in the JAR (they are inlined). SkipReport entries are not emitted (the functions do not appear in the JAR at all).
- **Reified generic functions**: not visible as generic functions in the JAR. SkipReport entries are not emitted.
- **Coroutine Continuation parameter**: Kotlin coroutines compile to state machines with a `Continuation<T>` parameter. The bridge skips functions with `Continuation` parameters (emitting SkipReport entries) because they cannot be called correctly from non-Kotlin code without a Kotlin coroutine runtime. The coroutine interop is handled separately by mapping `kotlinx.coroutines.Deferred<T>` to Mochi `async T` (same as `CompletableFuture`).
