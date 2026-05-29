---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks and alternatives"
description: "The risk register (GraalVM build time, native-image limitations, coroutines overhead, kotlinx-metadata schema drift, Maven Central signing-key management, Central Portal OIDC GA timeline) and the rejected alternatives (Kotlin/Native primary, libjvm.so primary, Dokka JSON primary, Panama FFI, long-lived tokens, per-build JVM, ANTLR-based POM parser)."
---

# 12. Risks and alternatives

## Risk register

### R1: GraalVM native-image build time

**Risk:** Compiling a GraalVM native image for a complex artifact (Spring, Ktor, Arrow) can take 60-120 seconds per artifact. A project with 10 transitive Kotlin dependencies could have a `mochi pkg lock` runtime of 15-20 minutes, making the developer feedback loop painful.

**Likelihood:** High for projects with many Kotlin deps. Low for projects with 1-3 focused deps.

**Mitigations:**
- Images are cached by `native-image-sha256`; re-running `mochi pkg lock` with unchanged deps skips recompilation.
- Parallel compilation: the bridge compiles all artifact images concurrently (up to `GOMAXPROCS` workers).
- Pre-built image cache: the bridge checks a shared community cache (opt-in) before local compilation.
- Per-artifact granularity: each artifact compiles independently; adding one new dep only builds one new image.

**Acceptance threshold:** Total `mochi pkg lock` time under 5 minutes for a typical 5-artifact project. This is achievable with parallel compilation on modern hardware (4+ cores, SSD).

### R2: GraalVM native-image incompatibility

**Risk:** Some artifacts use dynamic class loading (`Class.forName`, `URLClassLoader`), proxy generation (`java.lang.reflect.Proxy`), or serialisation frameworks (Java serialisation, Kryo) that defeat GraalVM static analysis. The native image build fails or the image crashes at runtime.

**Likelihood:** Low-medium. Approximately 10-15% of Maven Central Kotlin artifacts require non-trivial GraalVM configuration. Spring Framework, some Hibernate internals, and some XML-parsing frameworks are in this category.

**Mitigations:**
- The bridge synthesises standard `reflect-config.json` and `resource-config.json` from the `@kotlin.Metadata` scan, covering the most common reflection patterns.
- `--initialize-at-run-time` flags for known problematic classes (the bridge ships a curated list matching GraalVM's own reachability metadata repository).
- `runtime = "jvm-embed"` fallback for artifacts that cannot be native-imaged.
- Detection: `mochi pkg lock` runs `native-image --dry-run` (static analysis only) before full compilation and reports which classes require explicit configuration.

### R3: kotlinx-metadata schema drift

**Risk:** JetBrains releases a new Kotlin version with a `metadata-schema-version` (mv field) not yet handled by the bridge's Go-side proto decoder.

**Likelihood:** Low. JetBrains has maintained backwards compatibility since Kotlin 1.4 (2020). New schema versions add fields (new flags, new annotation types) without removing existing ones. Unknown fields are ignored by proto decoders.

**Mitigations:**
- The bridge's proto decoder ignores unknown fields (standard protobuf behaviour).
- The bridge prints a `WARN: unknown metadata schema version [X, Y, Z]; some API items may be missing` and proceeds.
- The bridge pins `kotlinx-metadata-jvm@X.Y.Z` in its own `go.sum`; a bridge upgrade picks up schema improvements.

### R4: Coroutines bridge overhead

**Risk:** The `runBlocking` blocking adapter introduces latency (context switch to a coroutine thread, GraalVM isolate thread management) that makes the bridge unsuitable for high-frequency calls.

**Likelihood:** High for call-heavy loops. Benchmarks (GraalVM 21, M1 Mac) show `runBlocking` adds approximately 3-8 µs per call on top of the JNI crossing overhead (~1 µs). For calls made at 100 Hz or less, this is imperceptible. For tight loops at 100 kHz+, it is prohibitive.

**Mitigations:**
- Batch APIs: the user should call Kotlin functions that do bulk work, not individual fine-grained calls.
- Non-suspend Kotlin functions have near-zero overhead (JNI call + C function call, ~1 µs).
- The event-loop mode avoids per-call thread context switches for async-heavy code.
- Documentation clearly warns against using the Kotlin bridge for hot-path tight loops.

### R5: Maven Central signing-key management complexity

**Risk:** GPG key management is difficult for most developers: key generation, passphrase selection, keyserver upload, CI secret management, key rotation, and expiry handling are all error-prone. A lost or expired key blocks publishing.

**Likelihood:** Medium. Any organisation that has published to Maven Central before has navigated this; it is only new friction for first-time publishers.

**Mitigations:**
- The `mochi pkg publish --dry-run` flag validates the signing setup without uploading.
- Clear error messages when the key is missing, expired, or not registered on the keyserver.
- Mode 2 (Sonatype OIDC) eliminates key management entirely; it is the recommended path once GA.
- Documentation includes a step-by-step key generation and registration guide.

### R6: Sonatype Central Portal OIDC GA timeline

**Risk:** The Sonatype OIDC trusted-publishing flow (Mode 2) is in beta as of May 2026. If Sonatype changes the OIDC endpoint URL or token format before GA, the bridge's Mode 2 implementation breaks.

**Likelihood:** Medium (beta-era API changes are common).

**Mitigations:**
- Mode 2 is advertised as beta in all documentation and at runtime.
- Mode 1 (environment-variable key) is the stable fallback.
- The bridge uses a version-negotiation handshake with the Central Portal OIDC endpoint; if the endpoint returns a 404, it falls back to Mode 1 with a warning.

### R7: Capability detection accuracy

**Risk:** The bridge's capability detection (walking the JAR's bytecode for `java.net.Socket`, `java.io.File`, etc.) may produce false negatives (missing a capability) or false positives (flagging a capability that is never reached at runtime).

**Likelihood:** Medium. Bytecode-level capability detection is conservative (over-estimates capabilities). Under-detection is possible for dynamically loaded classes.

**Mitigations:**
- Conservative detection: the bridge flags a capability if any reachable code path *could* invoke it, not just if it *does*.
- The user declares `[kotlin.capabilities]` explicitly; the bridge only checks that the declaration is a superset of the detected set. False positives in detection cause the user to declare more capabilities than strictly needed, which is safe (conservative). False negatives in detection would be the dangerous case; the bridge errs toward over-detection.

## Rejected alternatives

### Alt 1: Kotlin/Native as the primary bridge runtime

**Proposal:** Compile Kotlin libraries to `.klib` / `.so` using the Kotlin/Native compiler, avoiding any JVM dependency.

**Reason rejected:** Coverage is too low. Approximately 80% of Maven Central Kotlin artifacts (all Android Jetpack, all Spring, OkHttp, Retrofit, etc.) are JVM-only and cannot be compiled by Kotlin/Native. The bridge would be useful only for Kotlin Multiplatform libraries, which are a valuable but small subset of the ecosystem. See [[02-design-philosophy]] §Decision-2.

### Alt 2: `libjvm.so` as the primary bridge runtime

**Proposal:** Embed the JVM as a shared library (`libjvm.so`), start it via `JNI_CreateJavaVM`, and call into it for every Kotlin invocation.

**Reason rejected:** (a) JRE required on end-user machine (not a safe assumption in container/embedded deployments), (b) 200-400 ms cold start on every Mochi process launch, (c) full JVM GC pauses are visible to the Mochi runtime, (d) `libjvm.so` ABI is not stable across JVM versions. Retained as a `runtime = "jvm-embed"` override for the ~10% of artifacts that defeat GraalVM.

### Alt 3: Dokka JSON as the primary API surface discovery

**Proposal:** Run Dokka (Kotlin's documentation engine) to produce a JSON file describing the public API, and parse that JSON instead of `@kotlin.Metadata`.

**Reason rejected:** (a) Requires `kotlinc` binary at lock time (50+ MB download), (b) 10-60 second compilation per artifact, (c) Dokka JSON format is undocumented and changes between versions, (d) `@kotlin.Metadata` is the canonical source that Dokka itself reads. See [[02-design-philosophy]] §Decision-1.

### Alt 4: Panama FFI as the JNI replacement

**Proposal:** Use Project Panama (`java.lang.foreign`, Java 22+) instead of JNI for calling Kotlin from the native Mochi side.

**Reason rejected:** Panama is a consumer-side API for calling *native code from Java*, not for exposing *Java code to native callers*. GraalVM Native Image exposes a JNI-compatible ABI; using Panama would require building an entirely different callsite generator that does not fit Mochi's existing FFI pipeline. See [[02-design-philosophy]] §Decision-3.

### Alt 5: Long-lived Sonatype tokens as the only publish credential

**Proposal:** Accept a long-lived Sonatype Central Portal user token (username + password) as the only publishing credential.

**Reason rejected:** Long-lived credentials stored in CI secrets are a supply-chain security risk. MEP-70 follows the same philosophy as MEP-73 (Rust bridge): prefer short-lived, OIDC-derived credentials that expire and leave a transparency log entry. Long-lived tokens are supported as Mode 1 for local publishing but are deprecated for CI.

### Alt 6: Run `kotlinc` to compile the JNI wrapper source

**Proposal:** Generate Kotlin source for the JNI wrapper (instead of Java source) and compile it with `kotlinc`.

**Reason rejected:** (a) `kotlinc` has a 2-5 second JVM startup cost per invocation, (b) the resulting `.class` files are JVM bytecode identical to what `javac` produces from Java source, (c) the JNI wrapper uses only Java APIs (`JNIEnv`, `jstring`, `jobject`) that Kotlin provides no special benefit for. `javac` is faster and sufficient.

### Alt 7: ANTLR-based POM parser

**Proposal:** Use an ANTLR-generated parser to parse Maven POM XML files.

**Reason rejected:** Go's standard library `encoding/xml` is sufficient for POM parsing. The POM schema has four relevant elements (`groupId`, `artifactId`, `version`, `dependencies`) that `xml.Unmarshal` handles correctly. An ANTLR parser would add a code-generation step with no benefit.

### Alt 8: Per-build GraalVM compilation (no caching)

**Proposal:** Recompile GraalVM native images on every `mochi build` rather than caching them in `mochi.lock`.

**Reason rejected:** 30-120 s per artifact per build is unacceptable. The lock file's `native-image-sha256` enables incremental builds: GraalVM compilation only runs when the JAR hash or the wrapper source changes.

## Cross-references

- [[02-design-philosophy]] for the primary rationale behind each design decision.
- [[09-jvm-abi-stability]] for the GraalVM versioning and reproducibility protocol.
- [[06-maven-central-publish]] for the publishing flow.
- [[07-sonatype-trusted-publishing]] for the OIDC and GPG signing paths.
