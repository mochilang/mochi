---
title: "Phase 05: Wrapper synthesiser"
sidebar_position: 7
sidebar_label: "Phase 05: Wrapper"
description: "Synthesise Java JNI wrapper source for each artifact's type-mapped API surface, then compile it with javac."
---

# Phase 05: Wrapper synthesiser

**Status:** Planned

## Deliverables

1. `package3/kotlin/wrapper/synth.go` — `Synthesize(surface *APIObject, outputDir string) error` — emit Java source files for the JNI wrapper.
2. `package3/kotlin/wrapper/jni_types.go` — Kotlin→JNI type conversion code generators: `jstring` (UTF-16↔UTF-8), `jobjectArray` (list copy), `jlongArray` (long[] copy), `jobject` (data class accessor).
3. `package3/kotlin/wrapper/handle_registry.go` — generate the `MochiHandleRegistry.java` class (for coroutines event-loop mode, Phase 13 prerequisite).
4. `package3/kotlin/wrapper/compile.go` — `CompileJava(srcDir, classpath, outputDir string) error` — invoke `javac` to compile the generated Java source to `.class` files, then `jar` to pack into a JAR.
5. `package3/kotlin/wrapper/reflect_config.go` — generate `reflect-config.json` and `resource-config.json` for GraalVM from the type-mapped surface.

## Generated wrapper pattern

For each public Kotlin function `fun Foo.bar(x: Int, y: String): List<Long>`, the synthesiser emits:

```java
// com/example/foo/MochiBridge.java (generated)
package com.example.foo;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import com.oracle.svm.core.c.CTypedef;

public class MochiBridge {
    @CEntryPoint(name = "com_example_foo_bar")
    public static long[] com_example_foo_bar(
            IsolateThread thread,
            long fooHandle,
            int x,
            @CTypedef(name = "jstring") long y_ptr,
            int y_len
    ) {
        Foo foo = (Foo) MochiHandleRegistry.get(fooHandle);
        String y = new String(MochiJNI.utf8Bytes(y_ptr, y_len), java.nio.charset.StandardCharsets.UTF_8);
        java.util.List<Long> result = foo.bar(x, y);
        return MochiJNI.longListToArray(result);
    }
}
```

The actual generated code uses `@CEntryPoint` (GraalVM annotation) for native image entry points, not traditional JNI `JNIEXPORT` signatures. This produces cleaner C headers.

## Gate

Synthesise wrappers for `okhttp@4.12.0` and `kotlinx-coroutines-core@1.7.3`. Validate:

1. Generated Java source compiles with `javac --release 21` without errors.
2. The wrapper JAR is produced under `kotlin_wrap/okhttp/wrapper.jar`.
3. `reflect-config.json` contains all data class types from the API surface.
4. `resource-config.json` is non-empty for `ktor-client-core` (which bundles resource files).
5. No `javac` warnings for unchecked casts (all casts are checked and annotated with `@SuppressWarnings("unchecked")`).
