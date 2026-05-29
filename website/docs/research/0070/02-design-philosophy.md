---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why kotlinx-metadata-jvm over Dokka JSON, why GraalVM Native Image over Kotlin/Native and libjvm.so, why a JNI-compatible C ABI over Panama FFI, why Sonatype Central Portal for publishing, and why the no-boilerplate constraint is the primary design forcing function."
---

# 02. Design philosophy

Five decisions shape MEP-70's architecture. Each has a clear rationale grounded in the "no boilerplate" promise, the stability of available machine-readable interfaces, and the coverage breadth across the Maven Central corpus.

## The no-boilerplate promise as a forcing function

MEP-70's primary UX goal is: the user writes one line and gets a working Kotlin library. Every other decision is subordinate to this. When two alternatives have equal correctness, the one requiring less user-written glue wins. This eliminates JNI-with-manual-headers, Kotlin/Native with `cinterop` blocks, Dokka with Gradle project setup, and Panama with `MethodHandle` declarations. The bridge must discover the API surface, generate all wrapper code, compile the native shim, and synthesise the Mochi extern declarations autonomously.

## Decision 1: kotlinx-metadata-jvm over Dokka JSON

**Dokka** (Kotlin's official documentation engine) can emit a machine-readable JSON description of a library's public API. However, using Dokka requires:

1. The Kotlin compiler binary on the machine.
2. A Gradle or Maven project structure describing the source roots.
3. A 10-60 second compilation pass per artifact.
4. Parsing an undocumented JSON format that changes between Dokka versions.

**kotlinx-metadata-jvm** reads the `@kotlin.Metadata` annotation that `kotlinc` writes into every `.class` file. It requires:

1. The JAR on disk (already fetched for use).
2. No toolchain binary.
3. A 5-50 millisecond parse pass per JAR.
4. A versioned, backwards-compatible protobuf schema (schema version numbers tracked in `KotlinClassMetadata`).

The `@kotlin.Metadata` annotation is the *source of truth* that all Kotlin tools consume. Dokka itself reads it internally. JetBrains guarantees schema backwards compatibility and documents the migration path for each version bump. Using it directly is three orders of magnitude faster and eliminates the toolchain dependency at lock time.

The only information Dokka provides that `kotlinx-metadata-jvm` does not is the KDoc comment text. The bridge recovers this from the `@kotlin.Suppress("unused")` pattern and source JARs when the `-sources.jar` is present, and falls back to empty KDoc otherwise. KDoc quality is a secondary concern; API surface accuracy is the primary one.

## Decision 2: GraalVM Native Image as the primary bridge runtime

Three paths exist for calling JVM bytecode from a native binary:

**Path A: libjvm.so embedding (JNI classic)**
- Embed the JVM as a shared library; start it via `JNI_CreateJavaVM`.
- Pros: works with 100% of Maven Central artifacts; no build-time compilation.
- Cons: requires a JRE on every end-user machine (not guaranteed in server or embedded contexts); 200-400 ms cold start on every process launch; full GC pauses visible across the FFI boundary; `libjvm.so` is not a stable ABI (changes with JVM version).

**Path B: GraalVM Native Image**
- AOT-compile the artifact + all dependencies + the JNI wrapper to a platform-native shared library (`libwrap.so`).
- Pros: no JVM on end-user machine; sub-5 ms initialization; stable `extern "C"` ABI via `graal_create_isolate`; GC runs inside the isolated image heap (pauses are invisible at the C boundary); the image is self-contained and versioned.
- Cons: 30-120 s build time per artifact at lock time; dynamic class loading (`Class.forName`, `URLClassLoader`) requires explicit `reflect-config.json`; some Kotlin frameworks (Quarkus, some Spring internals) require extensive reflection configuration.
- Coverage: approximately 85-90% of Maven Central Kotlin artifacts are native-image compatible without custom configuration; the bridge synthesises standard `reflect-config.json` and `resource-config.json` from the metadata scan, covering an additional 8-9%.

**Path C: Kotlin/Native compilation**
- Compile the artifact to a `.klib` / `.a` / `.so` using the Kotlin/Native compiler.
- Pros: true native binary, no JVM dependencies even at build time.
- Cons: only a fraction of Maven Central artifacts are Kotlin/Native-compatible (the Kotlin stdlib + multiplatform libraries); all Android Jetpack, all JVM-specific frameworks (Spring, Ktor-server, OkHttp, Retrofit, etc.) are JVM-only and simply do not compile with Kotlin/Native. Coverage is approximately 15-20% of Maven Central Kotlin artifacts.

MEP-70 chooses **Path B** as the primary path and **Path A** as the `runtime = "jvm-embed"` override for artifacts that defeat native-image (typically those with deep dynamic class loading). Path C is reserved for a future KMP-native bridge in a separate MEP.

## Decision 3: JNI-compatible C ABI over Project Panama FFI

GraalVM Native Image in shared-library mode exposes a JNI-compatible C ABI: the entry points are standard JNI function signatures (`jstring`, `jint`, `jlong`, `jobject`), prefixed by the `graal_create_isolate` / `graal_tear_down_isolate` lifecycle API. Mochi's existing C-ABI FFI machinery (from MEP-45 and MEP-53) already understands this shape.

Project Panama (`java.lang.foreign`, Java 22+) provides a safer, higher-level native interop model with explicit memory segments, layouts, and typed `MethodHandle` bindings. However:

1. Panama is a consumer-side API for calling *native code from Java*, not for exposing *Java code to native callers*. Using Panama to build a Kotlin-to-native bridge would require a separate Panama-aware callsite generator.
2. GraalVM Native Image does not expose a Panama-based ABI; it exposes JNI.
3. Mochi's existing FFI pipeline handles JNI-shaped C ABIs without modification.

Panama remains relevant when `runtime = "jvm-embed"` and the bridge is calling into an embedded JVM from the Mochi side; in that mode, the bridge could use JNI or Panama interchangeably. For the primary GraalVM path, JNI is the only available ABI.

## Decision 4: Sonatype Central Portal over legacy OSSRH

Maven Central publishing has two historical paths:

**Legacy OSSRH** (retired February 2024): required a free Sonatype JIRA account, manual namespace approval via ticket, and Nexus-based staging repository management. The API was not designed for automation.

**Central Portal** (GA February 2024): exposes a clean REST API (`https://central.sonatype.com/api/v1/publisher/upload`), uses standard HTTP multipart upload for deployment bundles, supports OIDC-based short-lived tokens for CI environments, and provides a status polling endpoint (`/api/v1/publisher/status`). New namespace registrations go through the portal. Existing OSSRH namespaces migrated automatically.

The bridge uses the Central Portal API exclusively. OSSRH is removed from scope because all new namespaces go through the portal, and the portal's REST API is far more automation-friendly. The bridge does not maintain an OSSRH compatibility layer.

GPG signing is still required by Maven Central as of May 2026. The bridge signs in-process using an ASCII-armored GPG private key loaded from the environment (`MOCHI_MAVEN_SIGNING_KEY` env var) or, when the OIDC flow is available, exchanges a short-lived CI token for a Central Portal session token that includes signing on the server side. Long-lived API tokens are not accepted as the only credential.

## Decision 5: No live Kotlin compiler at consumer lock time

Some alternative designs (notably the Groovy/Scala/Clojure bridges in other ecosystems) run the target language's compiler to generate a compatibility shim. This would mean running `kotlinc` at `mochi pkg lock` time to compile a generated Kotlin wrapper source. The problems:

1. `kotlinc` is a large download (50+ MB) and has a 2-5 second JVM startup cost per invocation.
2. The compilation output is JVM bytecode, which the bridge then needs to compile again with GraalVM.
3. The wrapper source can be generated from `@kotlin.Metadata` without ever running `kotlinc`; the bridge generates Java source (not Kotlin source) for the JNI layer, which `javac` compiles in under one second.

The wrapper synthesis pipeline is: `kotlinx-metadata-jvm` ingest → type mapping → **Java JNI wrapper source** generation → `javac` compilation → GraalVM native-image. The Kotlin compiler is not on the lock-time critical path.

## Cross-references

- [[01-language-surface]] for the resulting user-facing surface.
- [[03-prior-art-bridges]] for the alternatives survey.
- [[04-kotlin-metadata-ingest]] for the kotlinx-metadata-jvm pipeline.
- [[09-jvm-abi-stability]] for the GraalVM ABI versioning story.
- [[12-risks-and-alternatives]] for the full risk register.
