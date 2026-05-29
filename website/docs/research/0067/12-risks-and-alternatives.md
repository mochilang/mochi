---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks and alternatives"
description: "Risk register and rejected alternatives for MEP-67."
---

# 12. Risks and alternatives

## Risk register

**JVM version fragility.** The JNI Invocation API is stable but `--add-opens` requirements vary across Java 9-21 for reflection on internal packages. MEP-67 targets the public reflection API only (`java.lang.reflect.*`) and does not access internal JDK packages. Risk: low for JARs that expose only public APIs; medium for JARs that use internal JDK classes internally (not exposed to reflection surface).

**JNI global-ref leaks.** Each `KindHandle` value holds a JNI global reference. If the Mochi GC does not finalise handles promptly (or if the user accumulates large numbers of handle values), the JVM heap may fill with unreachable Java objects. Mitigation: explicit `Close()` method on handle types; future MEP to integrate with Mochi's finaliser infrastructure.

**Java 17+ module access restrictions.** Java 9 introduced the module system; Java 17 enforces strong encapsulation by default. Reflection on non-exported packages requires `--add-opens`. MEP-67's reflection tool only reflects exported packages (those accessible to external callers), so the risk applies only to JARs that use reflection internally.

**Maven Central publish latency.** Central Portal validation can take 10-30 minutes for large bundles. `PollUntilPublished` defaults to a 60-minute timeout with 30-second poll intervals.

**GPG key management.** Optional GPG signing requires a pre-existing GPG key registered with Sonatype. The bridge documents the key setup but does not automate it. Sigstore keyless signing is the recommended path for new publishers.

**CompletableFuture thread-pool interactions.** Java's default `ForkJoinPool.commonPool()` sizes itself to `Runtime.availableProcessors() - 1`. In a process that also runs Go's runtime scheduler, this can lead to over-subscription. Mitigation: document that users should set `java.util.concurrent.ForkJoinPool.common.parallelism` to a small value.

## Rejected alternatives

**GraalVM polyglot as default.** Requires GraalVM; breaks on HotSpot, OpenJ9, and Android ART. Rejected in favour of JNI which works on all JVMs.

**JNA instead of JNI.** JNA calls native libraries from Java, not the reverse. Not applicable to embedding a JVM in a Go process.

**Annotation-database instead of reflection.** Annotation databases (e.g. manually curated JSON schema repos) go stale and have incomplete coverage. Rejected in favour of runtime reflection which is always authoritative.

**Jython/JRuby approach (run Mochi on the JVM).** Would require compiling Mochi to JVM bytecode. Incompatible with MEP-67's goal of keeping Mochi as a Go-compiled language and adding Java as a bridge target.

**Full generics support.** Tracking all type parameters through JVM signature descriptors, Kotlin metadata, and inner-class relationships is a multi-year project. Rejected for the initial 16 phases in favour of the conservative monomorphisation strategy.
