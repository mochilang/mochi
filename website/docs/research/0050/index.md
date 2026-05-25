---
title: "MEP-50 research bundle: Mochi to Kotlin transpiler"
description: "Twelve research notes covering language surface, design philosophy, prior art, runtime, codegen, type lowering, target portability, dataset pipeline, agent and stream lowering, build system, testing gates, and risks for the Mochi-to-Kotlin (Kotlin Multiplatform) transpiler proposed in MEP-50."
sidebar_position: 50
sidebar_label: "MEP-50"
---

# MEP-50 research bundle: Mochi to Kotlin transpiler

Author: research pass for MEP-50 (Mochi to Kotlin transpiler).
Date: 2026-05-23 (GMT+7).

This bundle contains the twelve research notes that informed [MEP-50, the Mochi-to-Kotlin transpiler](/docs/mep/mep-0050). The notes are informative; the normative spec is in the MEP body.

| # | Title | Topic |
|---|-------|-------|
| 01 | [Language surface](/docs/research/0050/language-surface) | The Mochi language surface and its Kotlin 2.1 lowering obligations |
| 02 | [Design philosophy](/docs/research/0050/design-philosophy) | Why Kotlin, why the 2.1 floor with K2, why `Channel` + `CoroutineScope` over the deprecated `actor { }` builder, why all KMP targets |
| 03 | [Prior art](/docs/research/0050/prior-art-transpilers) | Survey of source-to-Kotlin transpilers: J2K (IntelliJ Java-to-Kotlin), ts2kt, KotlinPoet, the Compose UI compiler, J2CL |
| 04 | [Runtime](/docs/research/0050/runtime) | The `MochiRuntime` KMP Gradle module: kotlinx.coroutines, kotlinx.serialization, kotlinx.collections.immutable, kotlinx.datetime, Datalog, agent supervisor, JSONValue |
| 05 | [Codegen design](/docs/research/0050/codegen-design) | IR-to-Kotlin lowering via KotlinPoet, ktfmt integration, `aotir` IR reuse |
| 06 | [Type lowering](/docs/research/0050/type-lowering) | Per-type details for every Mochi type (`Long`, `Double`, `String`, `List`, `LinkedHashMap`, `data class`, `sealed interface`, custom actor class, `Flow`) |
| 07 | [Target portability](/docs/research/0050/kotlin-target-portability) | KMP target matrix: JVM 17+, Android minSdk 24 / targetSdk 35, Kotlin/Native (iOS, macOS, Linux, Windows), Kotlin/JS (browser, Node.js), Kotlin/Wasm (Wasm GC) |
| 08 | [Dataset pipeline](/docs/research/0050/dataset-pipeline) | Query DSL lowering via Kotlin `Sequence` + `Flow` + kotlinx.collections.immutable; Datalog engine |
| 09 | [Agents and streams](/docs/research/0050/agent-streams) | Mochi agents as a custom actor class wrapping `Channel<Message>` + `CoroutineScope(SupervisorJob() + Dispatchers.Default)`; `Flow<T>` streams; structured concurrency |
| 10 | [Build system](/docs/research/0050/build-system) | Gradle Kotlin DSL, Kotlin Gradle Plugin, Android Gradle Plugin 8.7+, Maven Central publish, `gradle/libs.versions.toml` catalog, Gradle wrapper 8.11+ |
| 11 | [Testing gates](/docs/research/0050/testing-gates) | Per-phase Go test gates, Kotlin 2.1.0 / 2.1.20 version matrix, Play Console pre-launch validation, Kotlin/Native single-binary gate |
| 12 | [Risks and alternatives](/docs/research/0050/risks-and-alternatives) | Risk register, Java-source emission rejected, Scala interop rejected, Groovy DSL rejected, v2 deferrals (Compose Multiplatform, embedded Kotlin, distributed agents) |

## Cross-references

- [MEP-45 (C target)](/docs/mep/mep-0045)
- [MEP-46 (BEAM target)](/docs/mep/mep-0046)
- [MEP-47 (JVM target, direct bytecode)](/docs/mep/mep-0047)
- [MEP-48 (.NET target)](/docs/mep/mep-0048)
- [MEP-49 (Swift target)](/docs/mep/mep-0049)
