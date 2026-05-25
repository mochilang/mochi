---
title: MEP-47 research notes
sidebar_position: 1
sidebar_label: Overview
description: "Research notes feeding MEP-47 (Mochi-to-JVM transpiler). Twelve notes covering language surface, JVM design philosophy, prior art on JVM languages and AOT toolchains, JDK runtime, codegen IR layer, type lowering, portability, dataset pipeline, agents/streams on Loom, build system, testing, and risks."
---

# MEP-47 research notes

These twelve notes are the deep research that fed MEP 47 (Mochi-to-JVM transpiler). They are informative; the MEP body, once landed, will be normative. Each note is self-contained and can be read independently. Cross-references use `[[note-slug]]` markers.

| # | Title | Topic |
|---|---|---|
| 01 | [Language surface](/docs/research/0047/language-surface) | Mochi features mapped onto JVM lowering obligations |
| 02 | [Design philosophy](/docs/research/0047/design-philosophy) | Why JVM, why JDK 21 LTS, why Java source as IR |
| 03 | [Prior-art transpilers](/docs/research/0047/prior-art-transpilers) | Kotlin, Scala, Clojure, Groovy, JRuby, Eta, Ceylon, ASM, ByteBuddy, ClassFile API, GraalVM, Project Leyden, D8/R8/ART |
| 04 | [Runtime building blocks](/docs/research/0047/runtime) | JDK 21+ baselines, Loom, JFR, Jackson, snakeyaml-engine, java.time, java.net.http |
| 05 | [Codegen design](/docs/research/0047/codegen-design) | Java source via JavaPoet + javax.tools.JavaCompiler, ClassFile API hot path |
| 06 | [Type-system lowering](/docs/research/0047/type-lowering) | Type-by-type mapping to JVM primitives, JDK collections, records, sealed interfaces |
| 07 | [JVM target and portability](/docs/research/0047/jvm-target-portability) | JDK 21/25 LTS matrix, OS matrix, GraalVM, Android via D8/R8 |
| 08 | [Dataset pipeline](/docs/research/0047/dataset-pipeline) | Query DSL lowering via Stream API + collectors, hash-join, primitive specialisation |
| 09 | [Agents and streams](/docs/research/0047/agent-streams) | Loom virtual threads, `Flow.Publisher`, supervision in user-space |
| 10 | [Build system](/docs/research/0047/build-system) | Gradle/Maven plugins, Maven Central, jlink, jpackage, GraalVM native-image |
| 11 | [Testing gates](/docs/research/0047/testing-gates) | Per-phase Go test gates, JDK matrix, native-image gate, JFR pinning gate |
| 12 | [Risks and alternatives](/docs/research/0047/risks-and-alternatives) | Risk register, Kotlin/Scala/Akka rejected and why |

Each note's filename uses the `NN-slug.md` convention; the leading `NN-` is stripped by Docusaurus for the URL path, so cross-links inside the notes use the unprefixed slug (e.g. `[[language-surface]]`).

The companion MEP body lives at [/docs/mep/mep-0047](/docs/mep/mep-0047).
