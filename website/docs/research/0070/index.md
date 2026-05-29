---
title: "MEP-70 research bundle"
sidebar_position: 1
sidebar_label: "Overview"
description: "Twelve research notes covering the design space behind MEP-70: language surface, design philosophy, prior-art JVM bridges, Kotlin metadata ingest, the closed type-mapping table, the Maven Central publish flow, Sonatype Central Portal trusted publishing, the coroutines bridge, JVM ABI stability, generics and reification, the Kotlin Multiplatform and Android subset, plus the risks and rejected alternatives register."
---

# MEP-70 research bundle

This bundle is the informative companion to [MEP-70](/docs/mep/mep-0070). It documents the design space the Kotlin bridge sits in: prior art, the choices considered and rejected, the trade-offs accepted, and the open risks. Read it alongside the spec, not in place of it.

## Notes

| Note | Subject |
|------|---------|
| [01. Language surface](01-language-surface.md) | The `import kotlin "..."` import shape, the `mochi.toml` `[kotlin-dependencies]` + `[kotlin]` + `[kotlin.publish]` tables, the CLI surface (`mochi pkg add kotlin`, `mochi pkg lock`, `mochi pkg publish --to=maven-central`, `mochi pkg sync kotlin`), and the per-import alias semantics. |
| [02. Design philosophy](02-design-philosophy.md) | Why kotlinx-metadata-jvm over Dokka JSON, why GraalVM Native Image over Kotlin/Native and libjvm.so embedding, why a JNI-compatible C ABI over Panama FFI, why Sonatype Central Portal for publishing, why the no-boilerplate constraint shapes every decision. |
| [03. Prior-art JVM bridges](03-prior-art-bridges.md) | JNI, JNA, JNR-FFI, Project Panama, djinni, SWIG for Java, GraalVM Polyglot, Kotlin/Native C-interop, j2objc, Py4J, and Go-Java bridges (gojava, gobind). What each gets right, what each requires the user to write, and what MEP-70 borrows. |
| [04. Kotlin metadata ingest](04-kotlin-metadata-ingest.md) | The `@kotlin.Metadata` annotation binary format, the `kotlinx-metadata-jvm` protobuf schema (schema versions 1-9), the Go-side ingest pipeline, the ClassReader approach for extracting metadata bytes without a JVM, and the stability story. |
| [05. Type mapping table](05-type-mapping.md) | The complete closed Kotlin→Mochi translation table, the refusal cases, the nullable-T? to Option<T> desugar, the generic monomorphisation rule, the sealed-class discriminant strategy, the data-class field accessor pattern, and the platform type handling. |
| [06. Maven Central publish flow](06-maven-central-publish.md) | The Sonatype Central Portal REST API (POST `/api/v1/publisher/upload`), the deployment bundle format (JARs + POM + `.asc` signatures + checksums), the namespace verification requirements, the status polling protocol, and the legacy OSSRH migration. |
| [07. Sonatype trusted publishing + GPG signing](07-sonatype-trusted-publishing.md) | The OIDC short-lived token flow for Central Portal, GPG key management, the Maven Central signing requirement history, in-memory signing (no keyring on disk), Sigstore as a future alternative to GPG for JVM artifacts. |
| [08. Coroutines bridge](08-coroutines-bridge.md) | Kotlin coroutines architecture, the `suspend` keyword's CPS transformation, the `blocking` call adapter (`runBlocking`), the `event-loop` dispatch adapter, cancellation semantics across the C ABI boundary, the `kotlinx.coroutines.flow.Flow` consumer pattern. |
| [09. JVM ABI stability](09-jvm-abi-stability.md) | JVM bytecode binary compatibility rules, `@JvmName` and `@JvmOverloads` annotations, the Kotlin binary compatibility validator, the versioning policy for the synthesised wrapper, GraalVM Native Image versioning, and the `lock --check` reproducibility protocol. |
| [10. Generics and reification](10-generics-reification.md) | JVM type erasure vs Kotlin's `inline reified` generics, why the bridge cannot auto-monomorphise, the `monomorphise` table contract, the `ClassTag` / `TypeToken` workaround patterns, and the practical impact on the curated corpus. |
| [11. Kotlin Multiplatform and Android](11-kmp-android.md) | Kotlin Multiplatform (KMP) artifact structure (JVM + Native + JS targets in one artifact), `commonMain` vs JVM-specific API surface, Android AAR format (classes.jar + res/ + AndroidManifest.xml), the consumer path for AARs on non-Android hosts, and the Android-target bridge for mobile Mochi. |
| [12. Risks and alternatives](12-risks-and-alternatives.md) | The risk register (GraalVM build time, native-image limitations, coroutines overhead, kotlinx-metadata schema drift, Maven Central signing-key management, Central Portal OIDC GA timeline) and the rejected alternatives (Kotlin/Native primary, libjvm.so primary, Dokka JSON primary, Panama FFI, long-lived tokens, per-build JVM, ANTLR-based POM parser). |

## Cross-references

- [MEP-70 spec](/docs/mep/mep-0070) — the normative document.
- [MEP-57](/docs/mep/mep-0057) — the source-level package system whose manifest and lockfile the bridge extends.
- [MEP-53](/docs/mep/mep-0053) — the emit pipeline that gains `TargetKotlinLibrary`.
- [MEP-73](/docs/mep/mep-0073) — the Rust bridge, whose architecture MEP-70 mirrors.
- [Implementation tracking](/docs/implementation/0070/) — the per-phase delivery status.
