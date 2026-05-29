---
title: "MEP-70 implementation tracking"
sidebar_position: 1
sidebar_label: "Overview"
description: "Per-phase delivery status for MEP-70 (Mochi and Kotlin package bridge). 14 phases from skeleton through the full bidirectional bridge."
---

# MEP-70 implementation tracking

14-phase delivery plan for the Mochi↔Kotlin bridge. Each phase has a gating test against the curated 20-artifact fixture corpus.

## Status summary

| Phase | Title | Status |
|-------|-------|--------|
| 00 | Skeleton: Go module, error types, semver parser | Planned |
| 01 | Maven Central metadata client: POM fetch, version resolution | Planned |
| 02 | Blob cache: JAR/POM fetch + SHA-256/BLAKE3 verify | Planned |
| 03 | Kotlin metadata ingest: `@kotlin.Metadata` binary decode | Planned |
| 04 | Type-mapping table: Kotlin→Mochi closed table + refusal set | Planned |
| 05 | Wrapper synthesiser: Kotlin/Java JNI wrapper source generator | Planned |
| 06 | GraalVM native-image driver: compile wrapper to `libwrap.so` | Planned |
| 07 | Mochi extern emitter: synthesise `shim.mochi` | Planned |
| 08 | Grammar: add `kotlin` to `Lang` token; resolve `import kotlin` | Planned |
| 09 | MEP-53 build orchestration: `Driver.Build` triggers lock check + link | Planned |
| 10 | mochi.lock integration: `[[kotlin-package]]` R/W + `lock --check` | Planned |
| 11 | `TargetKotlinLibrary`: lower Mochi public API to JVM bytecode JAR | Planned |
| 12 | Maven Central publish: bundle builder + Sonatype Central Portal client | Planned |
| 13 | Coroutines bridge: `suspend fn` blocking + event-loop dispatchers | Planned |
| 14 | Generic monomorphisation + KMP JVM subset + Android AAR consumer | Planned |

## Phase detail pages

- [Phase 00: Skeleton](phase-00-skeleton.md)
- [Phase 01: Maven client](phase-01-maven-client.md)
- [Phase 02: Blob cache](phase-02-blob-cache.md)
- [Phase 03: Kotlin metadata ingest](phase-03-metadata-ingest.md)
- [Phase 04: Type mapping](phase-04-type-mapping.md)
- [Phase 05: Wrapper synthesiser](phase-05-wrapper.md)
- [Phase 06: GraalVM native image](phase-06-graalvm.md)
- [Phase 07: Extern emitter](phase-07-extern-emit.md)

## Corpus

All phases gate against the following 20-artifact fixture set:

```
kotlinx-coroutines-core@1.7.3
kotlinx-serialization-json@1.6.3
kotlinx-datetime@0.5.0
ktor-client-core@2.3.9
ktor-server-core@2.3.9
kotlin-stdlib@1.9.23
kotlin-reflect@1.9.23
arrow-core@1.2.1
exposed-core@0.44.1
koin-core@3.5.0
kotest-framework-engine@5.8.0
mockk@1.13.9
okhttp@4.12.0
retrofit@2.11.0
gson@2.10.1
jackson-module-kotlin@2.16.1
spring-context@6.1.3
micrometer-core@1.12.1
grpc-kotlin-stub@1.4.1
protobuf-kotlin@3.25.2
```

## Cross-references

- [MEP-70 spec](/docs/mep/mep-0070)
- [Research bundle](/docs/research/0070/)
