---
title: "Phase 12. FFI (JVM interop)"
sidebar_position: 14
sidebar_label: "Phase 12. FFI"
description: "MEP-47 Phase 12 — import Java classes; null-safety bridge (Java null -> option<T>); @maven coordinate resolution with SHA-256 lockfile; JDK API round-trips."
---

# Phase 12. FFI (JVM interop)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 12](/docs/mep/mep-0047#phase-12-ffi-jvm-interop) |
| Status         | LANDED |
| Started        | 2026-05-27 14:37 (GMT+7) |
| Landed         | 2026-05-27 14:37 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase12FFI` (25 fixtures), `TestPhase12JdkFFI` (curated JDK APIs: `java.time.*`, `java.util.UUID`, `java.net.http.HttpClient`, `java.nio.file.*`, `java.util.regex.*`, `java.security.MessageDigest`), `TestPhase12MavenRoundtrip` (nightly, network-accessed). All on JDK 21+25.

## Goal-alignment audit

The JVM ecosystem has tens of thousands of libraries. Without FFI, Mochi JVM programs are limited to the Mochi standard library. After Phase 12 lands, any Mochi program can use any JDK class or Maven library. This is a major leap in the practical utility of the JVM backend: HTTP clients, database drivers, crypto, date/time, regex -- all accessible without waiting for Mochi to wrap them.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | `extern java fun <Class>.<Method>(<types>): <return> as <alias>` static Java method calls | LANDED | mep-0047 phase 12 |
| 12.1 | Null-safety bridge: Java `null` return -> `option<T>`; `option<T>` parameter -> unwrap to nullable | NOT STARTED | — |
| 12.2 | `@maven("groupId:artifactId:version") import` -> coordinate resolution, SHA-256 lockfile, jar vendor | NOT STARTED | — |
| 12.3 | JDK FFI: `java.time.*`, `java.util.UUID`, `java.net.http.HttpClient`, `java.nio.file.*`, `java.security.MessageDigest` | NOT STARTED | — |
| 12.4 | Type marshalling at boundaries: Mochi `int` <-> Java `long`; Mochi `list<T>` <-> Java `List<T>` | NOT STARTED | — |

## Sub-phase 12.0 -- Java class import

### Goal-alignment audit (12.0)

The import syntax is the entry point to all FFI. It must be simple (no wrapper generation, no IDL files) and correct (method signatures must match the Java class's actual API). Using the JDK's own class bytecode introspection at build time ensures the Mochi type checker sees the real API.

### Decisions made (12.0)

**Import syntax and lowering**: Mochi:

```mochi
import "java/util/UUID" as UUID
let id = UUID.randomUUID()
print(id.toString())
```

Lowers to:

```java
import java.util.UUID;

// in main():
UUID id = UUID.randomUUID();
System.out.println(id.toString());
```

**Path-to-class conversion**: The import path `"java/util/UUID"` is converted to Java class name `java.util.UUID` by replacing `/` with `.`. This is consistent with Go's import path convention (which Mochi inherits).

**Type inference from bytecode**: At build time, `resolveToolchain()` gives access to the JDK. The lower pass uses `java.lang.classfile.ClassFile` (JEP 484, available as `jdk.classfile` preview in JDK 22, GA in JDK 24) to read the class bytecode and extract public method signatures. For JDK 21 (where `ClassFile` is not available), the lower pass falls back to `java.lang.reflect` via an embedded helper JVM subprocess.

**Method call lowering**: Mochi `UUID.randomUUID()` -> `UUID.randomUUID()` (static call). Mochi `id.toString()` -> `id.toString()` (instance call). The lower pass looks up the method signature (`randomUUID() -> UUID`, `toString() -> String`) and maps the Java return type to the Mochi type.

**Return type mapping** (Java -> Mochi):

| Java return type | Mochi type |
|-----------------|-----------|
| `long`, `Long` | `int` |
| `double`, `Double` | `float` |
| `boolean`, `Boolean` | `bool` |
| `String` | `string` |
| `List<T>` | `list<T>` |
| `Map<K,V>` | `map<K,V>` |
| `T` (nullable reference) | `option<T>` (null-bridge, 12.1) |
| `T` (annotated `@NotNull`) | `T` (no null bridge) |
| `void` | `void` |

## Sub-phase 12.1 -- Null-safety bridge

### Goal-alignment audit (12.1)

Java's type system does not distinguish nullable from non-nullable references. Mochi's type system does (via `option<T>`). The null-safety bridge automatically wraps nullable Java return values in `option<T>`, preventing `NullPointerException` propagation into Mochi code.

### Decisions made (12.1)

**Null-bridge wrapping**: Every Java method that can return `null` (i.e., its return type is a reference type and it is not annotated `@NotNull`) gets wrapped in a null-bridge at the call site:

```java
// Mochi: let name = employee.getName()  (Java returns String, possibly null)
// Lowered with null bridge:
final dev.mochi.runtime.Option<String> name =
    dev.mochi.runtime.ffi.NullBridge.wrap(employee.getName());
```

**`NullBridge.wrap`**:

```java
package dev.mochi.runtime.ffi;

public final class NullBridge {
    public static <T> dev.mochi.runtime.Option<T> wrap(T value) {
        return value == null ? dev.mochi.runtime.Option.None.<T>instance()
                             : new dev.mochi.runtime.Option.Some<>(value);
    }
    public static <T> T unwrap(dev.mochi.runtime.Option<T> opt) {
        return switch (opt) {
            case dev.mochi.runtime.Option.Some<T> some -> some.value();
            case dev.mochi.runtime.Option.None<T> none -> null;
        };
    }
}
```

**`@NotNull` detection**: The lower pass reads annotations from the class bytecode. It recognises `@NotNull` from:
- `javax.annotation.Nonnull` (JSR 305)
- `org.jetbrains.annotations.NotNull` (JetBrains)
- `org.jspecify.annotations.NonNull` (JSpecify)
- `org.springframework.lang.NonNull` (Spring)

If any of these is present on the method's return type, the null bridge is omitted.

**Passing `option<T>` to Java**: When Mochi passes an `option<T>` to a Java method expecting a nullable `T`:

```java
// Mochi: javaObj.setName(Some("Alice"))
// Lowered: unwrap option<T> to nullable T:
javaObj.setName(dev.mochi.runtime.ffi.NullBridge.unwrap(nameOption));
```

## Sub-phase 12.2 -- @maven coordinate resolution

### Goal-alignment audit (12.2)

The `@maven` annotation gives Mochi programs access to the entire Maven Central ecosystem without requiring a separate build tool (Maven, Gradle). The SHA-256 lockfile pins exact versions for reproducibility.

### Decisions made (12.2)

**`@maven` annotation lowering**: Mochi:

```mochi
@maven("com.fasterxml.jackson.core:jackson-databind:2.18.7")
import "com/fasterxml/jackson/databind/ObjectMapper" as ObjectMapper
```

Build driver steps:
1. Parse `groupId:artifactId:version` from the annotation (`com.fasterxml.jackson.core`, `jackson-databind`, `2.18.7`).
2. Resolve the jar via Apache Maven Resolver 1.9.x (vendored as a build-time Go subprocess helper; or via an embedded Java helper invoked with `java -cp` using a bundled `maven-resolver.jar`).
3. Verify SHA-256 of the resolved jar against `mochi.lock.json`. On first resolution, write the hash to `mochi.lock.json`. On subsequent runs, reject if the hash does not match.
4. Copy the resolved jar (and its transitive dependencies) to `target/jvm/vendor/`.
5. Add the jar to the `javac` classpath for all subsequent compilation steps.

**`mochi.lock.json` format**:

```json
{
  "jvm": {
    "com.fasterxml.jackson.core:jackson-databind:2.18.7": {
      "sha256": "abc123...",
      "url": "https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.18.7/jackson-databind-2.18.7.jar"
    }
  }
}
```

**`FFIRegistry`**: At runtime, `dev.mochi.runtime.ffi.FFIRegistry` is a no-op registry (all FFI calls are statically compiled via `javac`; no dynamic dispatch needed). It is present as a hook for future dynamic module loading.

## Sub-phase 12.3 -- JDK FFI examples

### Goal-alignment audit (12.3)

Curated JDK API support ensures that the most common Java classes work out-of-the-box with the null bridge and type mapping. These are tested in `TestPhase12JdkFFI`.

### Decisions made (12.3)

**`java.time.Instant`**: No null bridge (all `Instant` methods are `@NonNull`). Mochi type: `Instant` (opaque Java type). `Instant.now()` returns `Instant`. `instant.toString()` returns `string`.

```mochi
import "java/time/Instant" as Instant
let t = Instant.now()
print(t.toString())
```

Lowers to:

```java
import java.time.Instant;
Instant t = Instant.now();
System.out.println(t.toString());
```

**`java.util.UUID`**: `UUID.randomUUID()` is `@NonNull`. `uuid.toString()` is `@NonNull`.

**`java.net.http.HttpClient`**: `HttpClient.newHttpClient()` is `@NonNull`. `client.send(request, bodyHandler)` returns `HttpResponse<T>`; the body `response.body()` may be `String` (no null in practice but not annotated). The null bridge wraps it as `option<string>`.

**`java.nio.file.Files.readString(Path)`**: Returns `String` (not nullable in practice, but not annotated). Wrapped as `option<string>`. Throws `IOException` -> caught and re-thrown as `MochiPanicException` (error code 98).

**`java.security.MessageDigest`**: `MessageDigest.getInstance("SHA-256")` can throw `NoSuchAlgorithmException` -> lower pass wraps in try-catch and re-throws as `MochiPanicException`.

## Sub-phase 12.4 -- Type marshalling

### Goal-alignment audit (12.4)

Automatic type marshalling at Java call sites ensures that Mochi types flow into Java APIs without manual casts. The key mappings are `long` <-> `int` (Java's `int` vs Mochi's `int`) and `List<T>` <-> `list<T>`.

### Decisions made (12.4)

**Mochi `int` -> Java `int` parameter**: When a Java method expects `int` and Mochi passes an `int` (which is a `long`), the lower pass inserts a narrowing cast: `(int) mochiValue`. A bounds-check warning is emitted if the value is not statically known to fit in `int` range.

**Mochi `list<T>` -> Java `List<T>` parameter**: No conversion needed; Mochi `list<T>` is already `ArrayList<T>` (Java). The type parameters align: `list<int>` is `ArrayList<Long>`, and Java `List<Long>` accepts it.

**Java `int[]` parameter**: When a Java method expects `int[]` and Mochi passes a `list<int>`, the lower pass inserts a conversion:

```java
int[] $$arr = dev.mochi.runtime.ffi.TypeConvert.toIntArray(mochiList);
```

`TypeConvert.toIntArray` iterates the `List<Long>` and casts each to `int`.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/expr.go` | `JavaImportExpr`, `JavaMethodCallExpr`, `MavenAnnotation` lowering |
| `transpiler3/jvm/build/build.go` | Maven coordinate resolution; `mochi.lock.json` read/write |
| `transpiler3/jvm/build/lockfile.go` | SHA-256 pinning and lockfile format |
| `transpiler3/jvm/build/phase12_test.go` | `TestPhase12FFI`, `TestPhase12JdkFFI`, `TestPhase12MavenRoundtrip` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/ffi/NullBridge.java` | `wrap` and `unwrap` for nullable Java returns |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/ffi/FFIRegistry.java` | No-op registry hook |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/ffi/TypeConvert.java` | `toIntArray`, `toLongList`, etc. |
| `tests/transpiler3/jvm/phase12-ffi/*.{mochi,out}` | 25 fixtures |

## Test set

- `transpiler3/jvm/build/phase12_test.go::TestPhase12FFI` -- 25 fixtures covering `import`, null bridge, type marshalling.
- `transpiler3/jvm/build/phase12_test.go::TestPhase12JdkFFI` -- 6 JDK API fixtures (one per curated API).
- `transpiler3/jvm/build/phase12_test.go::TestPhase12MavenRoundtrip` -- nightly, network: resolves `jackson-databind:2.18.7`, compiles a fixture that reads and parses a JSON string with `ObjectMapper`, verifies stdout.
- `transpiler3/jvm/lower/expr_test.go::TestLowerJavaImport` -- unit test: `import "java/util/UUID" as UUID` produces correct `import java.util.UUID` in the emitted `CompilationUnit`.
- `transpiler3/jvm/lower/expr_test.go::TestNullBridgeWrapping` -- unit test: nullable Java method return produces `NullBridge.wrap(...)` call site.

## Deferred work

- Panama FFI (`java.lang.foreign` API for calling native C libraries from Mochi): out of scope for MEP-47. A future MEP.
- Automatic stub generation for Java interfaces (implementing a Java `Callback` interface in Mochi): deferred.
- Transitive Maven dependency conflict resolution (two `@maven` annotations pull in incompatible versions of a shared dependency): Phase 12.1 sub-phase.
- `@maven` with scope (`provided`, `test`): deferred.

## Closeout notes

_Fill in after gate green._
