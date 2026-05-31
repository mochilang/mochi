---
title: "11. Android and GraalVM"
sidebar_position: 12
sidebar_label: "11. Android and GraalVM"
description: "Android (ART/Dalvik) JNI without JNI_CreateJavaVM, why MEP-67 is out-of-scope for Android, GraalVM native-image as an alternative compilation path for publishing, --no-fallback limitations, GraalVM Polyglot API as a future path, Jakarta EE and OSGi, and JPMS impact on URLClassLoader reflection."
---

# 11. Android and GraalVM

This note covers two runtime environments that are adjacent to the JVM but require separate treatment: Android (which uses ART instead of HotSpot JVM) and GraalVM (which offers alternative compilation and polyglot paths).

## 1. Android (ART / Dalvik) and why it is out of scope

Android apps run on the Android Runtime (ART, introduced in Android 5.0 / API 21, 2014), which replaced Dalvik. ART compiles DEX bytecode (a format different from JVM bytecode) ahead-of-time using AOT compilation.

### ART has JNI but not JNI_CreateJavaVM

ART supports JNI: Android apps can write native code (C/C++) that calls Java via JNI, and Java code can call native functions via `System.loadLibrary`. The JNI API for calling Java from native code (`FindClass`, `GetMethodID`, `CallObjectMethod`, etc.) works on ART.

But ART does NOT implement `JNI_CreateJavaVM`. On Android, the JVM (ART) is created by the Android framework when the app process starts (`zygote` forks the app process, which already has a JVM running). There is no API for native code to create a new JVM; the app starts with a JVM already running.

MEP-67's JNI embedding approach assumes the native code creates the JVM via `JNI_CreateJavaVM`. This assumption is false on Android.

### MEP-67 is explicitly out of scope for Android

A Mochi binary running on Android cannot use MEP-67 as specified. Android support would require:

1. A separate MEP covering the ART embedding model (calling Java from native via `JNI_GetCreatedJavaVMs` to get the already-running JVM, rather than creating a new one).
2. DEX class loading instead of JAR class loading (Android uses `DexClassLoader`, not `URLClassLoader`).
3. Compatibility with the Android subset of the Java API (Android does not implement all of `java.util.concurrent`, `java.lang.ProcessBuilder`, `java.awt.*`, `javax.swing.*`, etc.).
4. The Maven artifact format is incompatible with Android: Android uses `.aar` (Android Archive) files which include DEX bytecode, resources, and native libraries, not JVM bytecode JARs.

The `n/a (ART)` entries in MEP-67's target matrix reflect this.

A future "MEP-67-Android" could specify the ART embedding path, but it is a substantially different design from MEP-67 and warrants a separate proposal.

### Android libraries on Maven Central

Some Maven Central artifacts work on Android (those that only use the Java API subset available on Android API 21+) and some do not (those using desktop-JVM-specific APIs). The `com.android.tools.build:gradle` plugin checks compatibility; MEP-67 does not perform this check.

## 2. GraalVM native-image as an alternative publish path

While GraalVM native-image is rejected as the consumer-side call mechanism (see [[02-design-philosophy]] §1), it has a role as an alternative publish path for MEP-67 Direction 2 (Mochi as Maven producer).

### The problem native-image solves

A Mochi library published to Maven Central via `TargetJavaLibrary` ships a JAR containing:

1. A `MochiWrap.class` that calls into `libmochi_<artifactId>.so` via JNI.
2. The native library itself (`native/<os>/<arch>/libmochi_<artifactId>.so`).

This JAR requires the consumer's JVM to be on a specific OS/architecture (the native library is platform-specific). A Moshi library published for Linux x86_64 cannot be used on macOS ARM64 without a separate artifact.

GraalVM native-image can compile the Mochi native library + the `MochiWrap.java` + the embedded JVM into a single self-contained shared library (`.so` or `.dylib`) that does not require a JVM at runtime. A consumer of this native-image-compiled library can `System.loadLibrary` it on any compatible platform without a JVM.

### How native-image publishing would work (future phase)

1. `mochi pkg publish --to=maven-central --mode=native-image`:
2. Build the Mochi native library as usual.
3. Compile `MochiWrap.java` and generate a `reflect-config.json` (the bridge knows all the classes and methods exposed; generating the config is straightforward since there is no third-party reflection).
4. Run `native-image --no-fallback --shared -o libmochi_<artifactId>` to produce a platform-specific shared library.
5. Package the shared library in a classifier JAR (`-linux-x86_64-native.jar`, `-darwin-aarch64-native.jar`).
6. Publish all classifier JARs alongside the standard JVM JAR.

This is a future phase, not in MEP-67's 16-phase plan. The complexity of multi-platform native-image builds in CI is significant; it would require a CI matrix with GraalVM installed on each target platform.

### --no-fallback limitations

GraalVM native-image's `--no-fallback` mode requires that all code paths are statically analyzable (no dynamic classloading, no arbitrary reflection, no JNI calls that are not declared in `jni-config.json`). Since `MochiWrap.java` is generated by the bridge (its contents are fully known at build time), the bridge can also generate the `jni-config.json` automatically. The Mochi native library itself does not use Java reflection, so `reflect-config.json` can be minimal.

## 3. GraalVM Polyglot API as a future path

The GraalVM Polyglot API (`org.graalvm.polyglot`) allows multiple language implementations (Java, JavaScript/Node.js, Python, Ruby, R, LLVM bitcode) to run within a single GraalVM JVM and share values directly.

If Mochi were implemented as a GraalVM Truffle language (a separate, large undertaking), it could use the Polyglot API for zero-overhead Java interop:

```java
try (Context context = Context.create()) {
    Value mochiModule = context.eval("mochi", "import java ...");
    Value result = mochiModule.invokeMember("myFunction", arg1, arg2);
}
```

This is a fundamentally different architecture than MEP-67 (Mochi is not a GraalVM Truffle language). A future "Mochi on GraalVM" could enable this path, but it would require implementing a Truffle interpreter for Mochi, which is a separate and much larger project.

## 4. Jakarta EE / OSGi considerations

### Jakarta EE

Jakarta EE (formerly Java EE) defines a set of enterprise APIs: Servlet, JPA, JAX-RS, CDI, etc. Many Maven Central artifacts implement or depend on Jakarta EE APIs (e.g., `jakarta.servlet:jakarta.servlet-api`, `jakarta.persistence:jakarta.persistence-api`).

MEP-67 treats Jakarta EE artifacts the same as any other Maven artifact. The bridge does not have special knowledge of Jakarta EE APIs. If a user imports `org.springframework.web.servlet:spring-webmvc`, the bridge will generate wrappers for the public API surface as ReflectTool sees it. Note that running a Spring MVC application from Mochi (with embedded Tomcat/Jetty container) would require the Mochi binary to host a JVM servlet container, which is architecturally unusual but technically possible with JNI embedding.

### OSGi

OSGi (Open Services Gateway initiative) defines a module system for Java that predates JPMS. OSGi bundles are JARs with special `MANIFEST.MF` headers (`Bundle-SymbolicName`, `Export-Package`, `Import-Package`). OSGi's classloading is hierarchical; each bundle has its own `ClassLoader`.

MEP-67's URLClassLoader approach loads all JARs into a single flat classloader hierarchy. OSGi's module isolation is not preserved. If a Maven Central artifact requires OSGi runtime (i.e., it only works when loaded by an OSGi bundle classloader), MEP-67 cannot bridge it correctly. In practice, most Maven Central JARs that publish OSGi manifests also work correctly in a flat classpath environment; the OSGi metadata is informational. The bridge emits a warning when a JAR's `MANIFEST.MF` contains `Bundle-ActivationPolicy: lazy` (indicating the JAR expects an OSGi environment for initialization) or when the JAR's code directly uses OSGi APIs (`org.osgi.*`).

## 5. JPMS impact on URLClassLoader reflection

Java 9 introduced the Java Platform Module System (JPMS). Key JPMS rules that affect MEP-67's URLClassLoader + reflection approach:

### Strong encapsulation

JPMS enforces strong encapsulation of module internals. A class in a module-internal package (a package not listed in `exports` in `module-info.class`) cannot be accessed by code outside the module, even via reflection. Specifically:

- `setAccessible(true)` on a field or method in an encapsulated package throws `java.lang.reflect.InaccessibleObjectException` (Java 9+).
- `loadClass("com.example.internal.Foo")` on a URLClassLoader with the module JAR on the classpath succeeds (the class is loaded), but calling `getDeclaredMethods()` and then `method.setAccessible(true)` to invoke them throws.

MEP-67's ReflectTool only accesses public APIs (`getDeclaredMethods()` filtered to `public` or `protected` modifiers, without calling `setAccessible(true)`). This is compatible with JPMS strong encapsulation: public members of exported packages are accessible without `setAccessible`.

### Automatic modules

A JAR on the module path without a `module-info.class` is treated as an automatic module. Automatic modules export all their packages and require all other modules. This means automatic modules can be reflected upon freely. Most Maven Central JARs are automatic modules in a JPMS context.

### Unnamed module

When a JAR is on the classpath (not the module path), it belongs to the "unnamed module." Code in the unnamed module can access all packages of all other JARs on the classpath, regardless of JPMS module declarations. MEP-67's URLClassLoader loads JARs as part of the unnamed module (classpath loading), which avoids most JPMS access control issues.

The `--add-opens` JVM flag can be used to forcibly open specific packages to reflection even from outside the module:

```toml
[java]
jvm-options = [
    "--add-opens=java.base/java.lang=ALL-UNNAMED",
    "--add-opens=java.base/java.util=ALL-UNNAMED",
]
```

MEP-67 does not add `--add-opens` flags by default (they are a crutch that works around JPMS encapsulation by effectively disabling it). The bridge generates a warning if `setAccessible` is needed for a reflected access and recommends adding the appropriate `--add-opens` flag or switching to a public API.
