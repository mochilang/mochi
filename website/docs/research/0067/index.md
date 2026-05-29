---
title: "MEP-67 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-67: language surface, design philosophy, prior art, Maven Central ingest, type mapping, Maven publish flow, Sigstore attestation, JNI bridge protocol, mochi.lock integration, async CompletableFuture bridge, generics and type erasure, and risks."
---

# MEP-67 research bundle

This bundle is the informative companion to [MEP-67](/docs/mep/mep-0067). It documents the design space the bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. The bundle is meant to be read alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import java "groupId:artifactId@version" as alias` import shape, the `mochi.toml` `[java-dependencies]` table, the CLI surface (`mochi pkg add java`, `mochi pkg publish --to=maven-central`), and the per-import alias/sub-namespace resolution rule. |
| [02. Design philosophy](02-design-philosophy.md) | Why a bidirectional bridge, why JNI over GraalVM polyglot or Jython, why Java reflection over manual annotations, why the `KindHandle` opaque-reference strategy for unmappable types. |
| [03. Prior-art bridges](03-prior-art.md) | GraalVM Polyglot, Jython, JRuby, Kotlin/Native, GoBridge (github.com/go-java), JNA/JNR-FFI, cglib. What each gets right and what MEP-67 borrows. |
| [04. Maven Central ingest](04-maven-central-ingest.md) | The Maven Central HTTP API (search.maven.org v2), SHA-1 checksum verification, the content-addressed JAR cache layout under `~/.cache/mochi/java-deps/`, and the reflection tool embedded-JAR approach. |
| [05. Type mapping](05-type-mapping.md) | The closed translation table from Java types to Mochi kinds, the 15 SkipReason constants, the boxed-primitive collapse strategy, the JSON wire encoding for List/Map/Optional, and the KindHandle opaque-reference fallback. |
| [06. Maven Central publish flow](06-maven-publish-flow.md) | The Sonatype Central Portal API (GA March 2024), the deployment bundle ZIP structure (JAR, POM, SHA-1, MD5, GPG .asc), the upload + poll status cycle, and the dry-run validation path. |
| [07. Sigstore attestation](07-sigstore-attestation.md) | The in-toto v1.0 predicate schema, the Sigstore Bundle 0.3 format, canonical JSON for byte-stable attestation, OIDC token exchange with Fulcio, and the `EncodeBundleHeader` base64-RawURL encoding. |
| [08. JNI bridge protocol](08-jni-bridge-protocol.md) | The JNI calling convention (`Java_ClassName_methodName`), global object references and the handle table, CGO embedding of the JVM via `libjvm.so`, the `java_jni` build tag strategy, and latency vs GraalVM polyglot. |
| [09. mochi.lock integration](09-mochi-lock.md) | The `[[java-package]]` TOML table schema, the four-hash verification scheme (jar-sha256, jar-sha1, surface-sha256, wrapper-sha256), the `--check` mode drift detection gate, and the dependency chain encoding. |
| [10. Async CompletableFuture bridge](10-async-bridge.md) | The callback registry pattern in `jni.FutureRuntime`, the `AsyncPolicy` enum (AsyncBlocking vs AsyncGoroutine), the Java thenAccept→JNI-native dispatch chain, and the goroutine scheduling interaction. |
| [11. Generics and type erasure](11-generics-erasure.md) | Java type erasure at the bytecode level, the `GenericSignature` parser for JVM generic descriptor strings, the `EraseType`/`ErasedParams` utilities, and the monomorphisation strategy for List/Map/Optional. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (JVM version fragility, JNI global-ref leaks, reflection access module restrictions in Java 17+, Maven Central publish latency, GPG key management, CompletableFuture thread-pool interactions) and rejected alternatives (GraalVM polyglot default, JNA instead of JNI, Jython, annotation-database instead of reflection). |

## Cross-references

- [MEP-67 spec](/docs/mep/mep-0067) -- the normative document.
- [MEP-57](/docs/mep/mep-0057) -- the source-level package system whose manifest and lockfile the bridge extends.
- [Implementation tracking](/docs/implementation/0067/) -- the per-phase delivery status.
