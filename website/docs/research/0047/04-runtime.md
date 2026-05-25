# MEP-47 research note 04, JVM runtime building blocks for libmochi_jvm

Author: research pass for MEP-47.
Date: 2026-05-23 01:02 (GMT+7).
Method: structured research over OpenJDK JEP archive (jeps.openjdk.org), Oracle JDK 21 and 25 documentation, Inside.java newscasts 2023 through 2026, library release notes on Maven Central, and the Java Code Geeks / InfoQ coverage of Loom and Panama.

This note inventories the runtime services Mochi programs need at execution time on the JVM, and chooses for each one a JDK module or a vetted third-party library to lean on. The output of this research is the **module layout for the `dev.mochi.runtime` Java module** (see section 16 below), which is the runtime library that every Mochi-generated `.class` file links against.

The companion notes [[01-language-surface]], [[02-design-philosophy]], and [[03-prior-art-transpilers]] establish the language surface Mochi exposes and the philosophy that drives target selection. This note assumes Mochi semantics are fixed and asks: what does the JVM give us, what do we still have to write, what should we leave at the door.

JDK baseline for MEP-47 is **JDK 21 LTS** (September 2023), with **JDK 25 LTS** (released September 16 2025) as the second supported LTS and the recommended target for new deployments. JDK 17 is explicitly out of scope (no virtual threads, no sequenced collections, no generational ZGC). The non-LTS releases (22, 23, 24, 26) are best-effort.

---

## 1. JVM scheduler and threads

The JVM has two kinds of threads since JDK 21: **platform threads** (one-to-one with an OS thread, the historical model) and **virtual threads** (Project Loom, JEP 444, GA in JDK 21).

A platform thread costs ~1 MB of native stack and is scheduled by the OS. A virtual thread is a Java object with a continuation, scheduled by a JDK-internal `ForkJoinPool` of **carrier threads** (default size: `Runtime.availableProcessors()`). Tens of millions of virtual threads fit in a normal heap; the cost of one is roughly the cost of a `Thread` object plus a continuation stack that grows on demand.

The Loom model is **synchronous code, async execution**: a virtual thread that blocks on `read()`, `Thread.sleep`, `LockSupport.park`, or a `j.u.c` lock unmounts from its carrier and the carrier is freed to run another virtual thread. When the block resolves, the virtual thread remounts (possibly on a different carrier) and resumes. This is the entire reason Mochi-on-JVM does not need an explicit scheduler library: every Mochi `agent` and every blocking call rides on Loom for free.

**Pinning hazards.** A virtual thread that holds a `synchronized` monitor or executes a JNI / Foreign call cannot unmount; it **pins** its carrier. Heavy pinning in code that uses synchronized blocks was the headline complaint of Loom in JDK 21 through 23. **JEP 491 (JDK 24, March 2025)** reimplemented monitor ownership in terms of the virtual thread identity rather than the carrier thread identity, eliminating pinning from `synchronized`, `Object.wait`, and timed waits. JDK 25 LTS inherits the fix. Three pinning cases remain by design: class loading, class initialiser execution, and waiting for a class to be initialised on another thread. JNI / FFM calls also still pin (the JVM cannot manage thread state across the native frame boundary). The diagnostic moved from the `jdk.tracePinnedThreads` flag (removed in 24) to the `jdk.VirtualThreadPinned` JFR event (default-enabled with a 20 ms threshold).

**For libmochi_jvm:** Mochi `agent` and Mochi `async` always run on virtual threads. The codegen emits `Thread.startVirtualThread(...)` for agent boot and `Executors.newVirtualThreadPerTaskExecutor()` for short-lived task pools. The runtime never uses `synchronized` internally in code that may block; we use `j.u.c.locks.ReentrantLock` and `j.u.c.locks.StampedLock` instead, which never pin on any JDK. On JDK 25 we relax that rule for the hot-path map and list helpers (they may block briefly while resizing, which is fine post-491), but the rule stays in place on JDK 21 / 22 / 23 because users may still be on them.

`ScheduledExecutorService` is the JDK's timer wheel. We use `Executors.newSingleThreadScheduledExecutor(Thread.ofVirtual().factory())` for Mochi `after`, `every`, and stream debouncing. One executor per Mochi runtime instance, shared across all agents.

## 2. Memory model and garbage collection

The JVM ships **five** garbage collectors as of JDK 25:

- **Serial GC** (`-XX:+UseSerialGC`). Stop-the-world, single-threaded, smallest footprint. The right choice for one-shot CLI tools and CI runs under 100 MB. Pause times scale linearly with heap size but at 100 MB the pause is sub-millisecond.
- **Parallel GC** (`-XX:+UseParallelGC`). Stop-the-world, multi-threaded, maximises throughput at the cost of pauses. The right choice for batch jobs where the goal is total runtime and pauses are acceptable.
- **G1 GC** (`-XX:+UseG1GC`, default since JDK 9). Region-based, mostly concurrent, soft real-time. Pause-time goal is a tuning knob (`-XX:MaxGCPauseMillis=200` by default). The default for everything.
- **ZGC** (`-XX:+UseZGC`). Low-latency concurrent collector, sub-millisecond pauses, scales to multi-terabyte heaps. **Generational ZGC** (JEP 439) shipped in JDK 21 behind the `-XX:+ZGenerational` flag, became the default ZGC mode in JDK 23 (JEP 474), and non-generational ZGC is deprecated for removal in JDK 25. Generational ZGC delivers ~10% throughput improvement and 10 to 20% P99 pause-time improvement over the single-generational version.
- **Shenandoah** (`-XX:+UseShenandoahGC`). Red Hat's concurrent collector, similar latency profile to ZGC. Generational Shenandoah was finalised in JDK 25 (no JEP number for the GA promotion, it was promoted from experimental in JEP 404 incubation). Available in OpenJDK and Temurin builds; not always in Oracle JDK.

The JVM heap layout (G1 and ZGC) uses **regions** rather than the classic young / old / survivor split: in G1, each region is ~1 to 32 MB and roles rotate. This matters because **`-Xmx`** sets total heap size, not per-generation size; the JVM picks ratios. For Mochi we set defaults and document them:

| Workload | Default GC | Heap | Rationale |
| --- | --- | --- | --- |
| `mochi run` (CLI script) | Serial | 64 MB initial, 256 MB max | Small, predictable, fast warm-up |
| `mochi build` output (default) | G1 | OS default (1/4 of RAM) | Industry default |
| Long-lived agent service | ZGC | OS default | Sub-ms pauses for stream processing |
| Batch dataset job | Parallel | OS default | Maximise throughput |

These map to `dev.mochi.runtime` build profiles (see section 16).

**Compact object headers** (JEP 519, finalised in JDK 25) shrink the Java object header from 16 to 8 bytes on 64-bit. For Mochi this is a free win: `mochi_list` is `ArrayList<Object>`, so every boxed `Integer` saves 8 bytes. Enable with `-XX:+UseCompactObjectHeaders` on JDK 25; off by default in 25, expected default in 26 or 27.

**For libmochi_jvm:** Document GC choice per workload. The runtime never tries to be smart about GC tuning beyond the workload-template defaults; production deployments tune their own `-Xmx` and `-XX:+UseZGC` / `+UseG1GC` flags through the `JAVA_TOOL_OPTIONS` environment variable or the `mochi run --jvm-arg=...` passthrough.

## 3. Class loading and modules

The JVM has a **hierarchical class loader** model:

1. **Bootstrap class loader** (native code in libjvm). Loads `java.base` and friends.
2. **Platform class loader** (`PlatformClassLoader`). Loads non-base JDK modules: `java.net.http`, `java.sql`, `java.xml`, `jdk.compiler`, ...
3. **System / application class loader** (`AppClassLoader`). Loads everything on the application's classpath / module path.

Below those, application code can install custom class loaders. Frameworks like Tomcat, OSGi, and JBoss Modules use deep hierarchies; for Mochi we never need this.

**Modules (JPMS, JDK 9+).** A module is a named bundle of packages with declared dependencies (`requires`) and exports (`exports`). Module path (`--module-path`) is the modern replacement for classpath (`--class-path`), though both still work and most ecosystems remain classpath-based in 2026. Reasons:

- The Maven Central majority is still non-modular. Libraries publish JARs that work as "automatic modules" when on the module path, but most users keep them on the classpath.
- The Spring framework, Quarkus, Micronaut, and Android all run on classpath. Modules never won the developer-facing battle; they won the JDK-internal battle.

**Multi-Release JARs** (MR-JARs, JEP 238, JDK 9). A JAR can contain version-specific class files under `META-INF/versions/N/`. The class loader picks the highest version less than or equal to the running JDK. Useful for libraries that want to use new APIs on new JDKs without breaking older runtimes.

**ServiceLoader.** The `java.util.ServiceLoader` API loads implementations declared in `META-INF/services/<interface-name>` files or via the module-info `provides ... with ...` clause. Used by `RandomGeneratorFactory`, JDBC drivers, charset providers, etc. For Mochi, we use ServiceLoader internally to plug in JSON / CSV / YAML codecs (so `dev.mochi.runtime.json.spi.JsonCodec` has Jackson and Gson implementations and the user picks).

**For libmochi_jvm:** Ship as a **single named module**: `dev.mochi.runtime`. The module-info declares:

- `requires java.base;` (transitive root)
- `requires java.net.http;` (for `mochi.fetch`)
- `requires java.logging;` (for `mochi.log`)
- `requires com.fasterxml.jackson.databind;` (for `mochi.json`)
- `requires org.snakeyaml.engine;` (for `mochi.yaml`)
- `exports mochi.list;`, `exports mochi.map;`, etc. (one package per Mochi surface module)
- `uses dev.mochi.runtime.spi.JsonCodec;` (so users can swap codecs)

The Maven coordinates are `dev.mochi:mochi-runtime:<version>`. Versions track Mochi releases (so Mochi 0.6.0 ships `mochi-runtime` 0.6.0). The module is also shipped as a "fat JAR" with Jackson, snakeyaml-engine, and the JDK HttpClient adapters shaded under `dev.mochi.runtime.internal.shaded.*` to avoid version conflicts in user projects that bring their own Jackson.

## 4. Strings and binaries

`java.lang.String` is the JVM's canonical string type. Since JDK 9 (JEP 254), strings use **compact representation**: if all code points fit in Latin-1, the backing array is `byte[]` with one byte per code point; otherwise `byte[]` with two bytes per code point (UTF-16). The `coder` field distinguishes. This halves memory for ASCII-heavy workloads at zero API cost.

`String` is UTF-16 in its API surface (`charAt`, `length`, `codePointAt`), but the **Mochi `string` surface is code-point indexed** (see [[06-type-lowering]]). The mapping is direct:

- `mochi_str_len(s)` -> `s.codePointCount(0, s.length())`
- `mochi_str_at(s, i)` -> `s.codePointAt(s.offsetByCodePoints(0, i))`
- `mochi_str_slice(s, lo, hi)` -> `s.substring(s.offsetByCodePoints(0, lo), s.offsetByCodePoints(0, hi))`
- `mochi_str_concat(a, b)` -> `a + b`
- `mochi_str_split(s, sep)` -> `Arrays.asList(s.split(Pattern.quote(sep), -1))`

The cost of `offsetByCodePoints` is O(N) in the prefix length; for ASCII strings the compact representation makes it O(1) effectively because Latin-1 has no surrogate pairs. For BMP-only strings (no astral plane) it is still O(N) but with a tight loop. For surrogate-pair-heavy strings (CJK extension B, emoji) it is O(N) for real. Most Mochi programs do not hot-loop over string indices, so this is acceptable.

**Binary data**. Three options:

- **`byte[]`**, the historical workhorse. Heap-allocated, GC-managed, copying.
- **`ByteBuffer`**, heap or direct (off-heap). Supports relative and absolute access, endian flips. Awkward API.
- **`MemorySegment`**, Panama FFM API, **GA in JDK 22 (JEP 454)**, refined in 23 / 24 / 25. Backed by heap arrays or off-heap arenas. Closed via `Arena`; bounds-checked; supports value layouts (struct-like access). The modern replacement for `ByteBuffer` and the unsafe `sun.misc.Unsafe` API.

**For libmochi_jvm:** Mochi `string` lowers to `java.lang.String`. Mochi `bytes` lowers to `byte[]` for short, GC-OK buffers and to `MemorySegment` (in a `mochi_arena` wrapping `java.lang.foreign.Arena`) for image / raw / FFI buffers. The `mochi_bytes` module exposes both representations behind a common interface; the codegen picks based on a `#[mochi.alloc]` annotation or based on size heuristics (default heap-byte-array under 4 KiB, segment over).

The Panama FFM API is also the **only** Mochi FFI surface on JVM. JNI is not exposed; native libraries are bound via `java.lang.foreign.Linker` and `SymbolLookup`. This matches MEP-45's design of restricting native code to a single, audited path.

## 5. Collections

Mochi has three collection types in the language surface: `list<T>`, `map<K, V>`, `set<T>`. The JVM gives us a generous menu.

**Mutable, hash-backed (the default):**

- `java.util.ArrayList<T>`, resizable array, O(1) append, O(1) indexed access, O(N) insert/remove.
- `java.util.HashMap<K, V>`, chained hash table, O(1) get/put expected, O(N) worst case (treeified to red-black at 8+ collisions since JDK 8).
- `java.util.HashSet<T>`, HashMap under the hood, O(1) contains.

**Mutable, ordered (insertion order):**

- `java.util.LinkedHashMap`, `java.util.LinkedHashSet`, preserve insertion order, slightly higher overhead than the hash-only versions.

**Sequenced collections (JEP 431, GA in JDK 21).** New supertype interfaces `SequencedCollection`, `SequencedSet`, `SequencedMap` capture "has a first and last element" without committing to a specific backing structure. They add `getFirst`, `getLast`, `addFirst`, `addLast`, `removeFirst`, `removeLast`, `reversed`. Importantly, `List` now extends `SequencedCollection`, and `LinkedHashMap` implements `SequencedMap` directly. For Mochi `list` this means we can lower `list.first`, `list.last`, and `list.reversed` to one-line JDK calls.

**Immutable factories (JDK 9+).** `List.of(a, b, c)`, `Map.of(k1, v1, k2, v2)`, `Set.of(a, b, c)`. These return *immutable* collections that throw `UnsupportedOperationException` on any mutator. Read access works including the sequenced methods. They share storage internally and are faster than `Collections.unmodifiableList(new ArrayList<>())`.

**Persistent collections (third-party).** Three serious candidates:

- **Vavr** (formerly Javaslang, v0.10.x is stable, v1.0 in slow development since 2018). Immutable List, Vector, HashMap, TreeMap, Try, Either, Option. The "Scala collections, but for Java" library.
- **Eclipse Collections** (formerly GS Collections, v11.x in 2025). Primitive-specialised collections, immutable variants, parallel operations. Big footprint (~3 MB).
- **Paguro / Clojure's PersistentHashMap (the underlying impl)**. Bit-mapped vector trie. Very fast.

We considered persistent collections and **rejected them for v0.1**. Reasons:

- Mochi has mutable variables and mutable record fields by default ([[01-language-surface]] §2). The surface semantics expect O(1) field assignment. Persistent collections would require copy-on-write at every assignment, which is correct but slow without language-level escape analysis.
- The C target (MEP-45) and Erlang target (MEP-46) both use mutable structures (the C target via arenas, the Erlang target via per-process heaps). Diverging here would break the cross-target equivalence the test suite assumes.
- Users who want persistence can call Vavr directly via the FFI seam.

**For libmochi_jvm:**

- `list<T>` -> `java.util.ArrayList<T>` (mutable). Empty literals use `new ArrayList<>(0)`, sized literals pre-size.
- `map<K, V>` -> `java.util.LinkedHashMap<K, V>`. Important: we choose `LinkedHashMap` over `HashMap` because Mochi `for k, v in m` is documented to iterate in insertion order (matches the C target's open-addressing-with-version-tag and the Erlang target's `maps:next/1` behaviour on small maps).
- `set<T>` -> `java.util.LinkedHashSet<T>`. Same reasoning as map.
- `Option<T>` -> `java.util.Optional<T>`, except where boxing would cost too much; the codegen prefers `OptionalInt` / `OptionalLong` / `OptionalDouble` for primitive payloads.
- Literal `[1, 2, 3]` lowers to `new ArrayList<>(List.of(1, 2, 3))` (note the wrapping copy, because `List.of` is immutable).
- `mochi_list.frozen(x)` and `mochi_map.frozen(x)` wrap with `Collections.unmodifiableList` / `unmodifiableMap` for the rare cases users need immutability guarantees.

## 6. Streams API and parallel streams

The JDK `java.util.stream` package (Stream<T>, IntStream, LongStream, DoubleStream) is a functional pipeline API: `source.map(f).filter(p).reduce(...)`. It is the JVM's answer to LINQ and is the backbone of Mochi's query DSL on this target.

Key types:

- `Stream<T>`, reference stream.
- `IntStream`, `LongStream`, `DoubleStream`, primitive specialisations (avoid boxing).
- `Collectors`, sinks: `toList`, `toMap`, `groupingBy`, `partitioningBy`, `joining`, `summingInt`, etc.
- `Stream.parallel()`, fork over a default `ForkJoinPool` (one per JVM, sized to `availableProcessors() - 1`).

Parallel streams have a footgun: the default pool is shared across the whole JVM, so a slow parallel stream in one library starves another library's parallel stream. The fix is either to wrap the parallel work in a custom pool (`new ForkJoinPool(n).submit(() -> stream.parallel()...).get()`) or, on JDK 21+, to use virtual-thread-backed `Executors.newVirtualThreadPerTaskExecutor()` for I/O-bound work.

Mochi's query DSL (see [[08-dataset-pipeline]] for the full design) lowers to Stream pipelines: `from people p where p.age > 30 select p.name` becomes `people.stream().filter(p -> p.age > 30).map(p -> p.name).collect(Collectors.toList())`. `group by` becomes `Collectors.groupingBy`. Sorts become `.sorted(comparator)`. Joins, the only non-trivial part, lower to a build-hash-then-probe pattern (see [[08-dataset-pipeline]] §4).

**For libmochi_jvm:** Stream pipelines are the runtime substrate for `from`/`where`/`select`. The `mochi.query` module provides helper builders (custom collectors for `group by ... having`, a `JoinCollector`, a `WindowCollector` for streaming windows). Defer details to [[08-dataset-pipeline]]. The takeaway here is: the substrate is already in the JDK, we do not need a third-party engine.

## 7. Concurrency primitives

The `java.util.concurrent` package is enormous and well-tested. The pieces Mochi uses:

- **`ReentrantLock`** (`java.util.concurrent.locks.ReentrantLock`), replacement for `synchronized` that never pins virtual threads pre-JDK 24 and avoids the rare issues with monitor inflation. Used in every internal Mochi runtime data structure that needs mutual exclusion.
- **`StampedLock`**, reader / writer / optimistic-read three-mode lock. Used in `mochi.cache` for memoised function results (read-heavy).
- **`AtomicInteger`, `AtomicLong`, `AtomicReference`**, lock-free atomics. Used in agent message counters, telemetry counters.
- **`ConcurrentHashMap`**, concurrent hash map, lock-striped. Used for the Mochi global agent registry and for the `mochi.cache` storage when concurrent.
- **`BlockingQueue`** family, `ArrayBlockingQueue`, `LinkedBlockingQueue`, `LinkedTransferQueue`. Used internally for the Mochi stream pubsub buffers (each subscriber gets a bounded queue with configurable backpressure).
- **`CompletableFuture<T>`**, the JDK's promise type. `thenApply`, `thenCompose`, `allOf`, `anyOf`. Used to model Mochi `async`'s "function returning a future" surface. The user writes `let f = async fetch(...)`, which lowers to `CompletableFuture<Response> f = CompletableFuture.supplyAsync(() -> fetch(...), virtualExec);`.
- **`Phaser`**, barrier with dynamic participants. Used by `mochi_agent_sup` to coordinate agent shutdown.

**StructuredTaskScope.** JEP 462 (preview, JDK 21), 464 (preview 2, JDK 22), 480 (preview 3, JDK 23), 499 (preview 4, JDK 24), **505 (fifth preview, JDK 25)**. Not GA in JDK 25 despite the original projection; JEP 525 (sixth preview) is queued for JDK 26. The fifth preview in JDK 25 redesigned the API from inheritance to factory plus `Joiner`:

```java
try (var scope = StructuredTaskScope.open()) {
    var a = scope.fork(() -> fetchUser(id));
    var b = scope.fork(() -> fetchPrefs(id));
    scope.join();
    return new Profile(a.get(), b.get());
}
```

**For libmochi_jvm:** We **do not** expose StructuredTaskScope in v0.1, because it is still preview and the API has churned five times. Instead, `mochi_async.scope { ... }` wraps a manual `Executors.newVirtualThreadPerTaskExecutor()` plus a `Phaser`. When JDK 27 or 28 makes StructuredTaskScope final, we switch the implementation behind the same surface.

**Flow API.** `java.util.concurrent.Flow` (JDK 9) declares the four interfaces of Reactive Streams: `Publisher`, `Subscriber`, `Subscription`, `Processor`. `SubmissionPublisher` is the only JDK-shipped implementation, and it is a fine pub/sub primitive: bounded buffer per subscriber, drop-or-block policy on overflow, threadsafe.

**For libmochi_jvm:** Mochi streams (see [[09-agent-streams]]) lower onto `SubmissionPublisher` (one per stream, lifetime tied to the stream's owning agent). Subscribers register via `subscribe()`, get a `Subscription`, and pull with `request(n)` to implement backpressure. This is the JDK-canonical way to do reactive streams and avoids pulling in Project Reactor or RxJava as dependencies.

## 8. HTTP

`java.net.http.HttpClient` (JEP 321, GA in JDK 11) is the standard HTTP client. It supports HTTP/1.1, HTTP/2, and WebSockets out of the box, with synchronous (`send`) and asynchronous (`sendAsync`) APIs. From JDK 21, it cooperates with virtual threads: a `send()` from a virtual thread unmounts the carrier while waiting for the response.

```java
var client = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_2)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build();
var req = HttpRequest.newBuilder(URI.create(url)).GET().build();
var res = client.send(req, HttpResponse.BodyHandlers.ofString());
```

**Known gotchas (relevant for [[09-agent-streams]] and the Mochi `fetch` builtin):**

- **HTTP/2 stream limit per connection.** The HttpClient establishes **one** TCP connection per `scheme://host:port` and multiplexes up to ~100 streams (the server's `SETTINGS_MAX_CONCURRENT_STREAMS`). Above 100 concurrent virtual-thread requests to the same host, requests queue. Creating multiple `HttpClient` instances does **not** help; the connection is still pooled per host. Workarounds: scatter requests across multiple hosts, or downgrade to HTTP/1.1 with a larger keep-alive pool (`-Djdk.httpclient.connectionPoolSize=N`).
- **`BodyHandlers.ofInputStream()` pins.** Reading the response body as an `InputStream` from a virtual thread pins the carrier on `NioSocketImpl.read()`. Use `BodyHandlers.ofString()`, `ofByteArray()`, or `ofPublisher()` (returns a `Flow.Publisher<List<ByteBuffer>>`) to stay async-friendly.

**For libmochi_jvm:** Mochi `fetch(url)` lowers to a single shared `HttpClient` instance per Mochi runtime (created lazily on first use). The instance uses HTTP/2 by default, follows redirects, has a 30-second connect timeout. Body is materialised via `BodyHandlers.ofByteArray()` and decoded by the user (or, for `fetch_json`, decoded into a Mochi value via Jackson). Streaming responses (`fetch_stream`) use `BodyHandlers.ofPublisher()` and adapt the Flow.Publisher into a Mochi stream.

WebSocket support uses `HttpClient.newWebSocketBuilder()` returning a `WebSocket` and a `WebSocket.Listener`. Mochi exposes this in `mochi.ws` as part of the same module, but the API surface is documented in [[09-agent-streams]].

## 9. JSON

The JDK has **no standard JSON support** in 2026. JEP 198 ("Light-Weight JSON API") was opened in 2014 and is still on indefinite hold; the OpenJDK position has been "the ecosystem solved this, we don't need to" since at least JDK 11. So we pick a third-party library.

**Survey:**

- **Jackson (FasterXML)**. The de facto standard. Three layers: `jackson-core` (streaming parser / generator), `jackson-databind` (object mapping), and `jackson-annotations`. Module ecosystem covers CBOR, MessagePack, Smile, XML, YAML, CSV, Avro, Protobuf, Kotlin, Java time, ...
  - Current LTS: **2.18** (released 2024-09-27, LTS through end of 2026). Latest patch: 2.18.7 (2026-04-24).
  - The 2.19 branch shipped April 2025; 2.20 is current at the time of writing (May 2026).
  - Major feature in 2.18: rewritten POJO + Record property introspection (the eight-year-old top of the priority list).
  - Performance: industry standard. Tracks the JDK's `FastDoubleParser` for float scanning.
- **Gson (Google).** Smaller, simpler, slower. Reflection-only. Maintained but not actively developed since 2023. Last release: 2.10.1 (2023), no 2024 / 2025 releases. Effectively dormant.
- **jakarta.json (JSON-P).** Specification API for streaming and object-model parsing. Reference implementation is Eclipse Parsson. Heavy and verbose. Used in Jakarta EE world; rare elsewhere.
- **Moshi (Square).** Kotlin-first, also works from Java. Reflection or codegen. Smaller than Jackson. Better default behaviour around nulls.
- **DSL-JSON.** Compile-time codegen, very fast. Niche.
- **jsoniter, json-simple, minimal-json.** Hobby-scale.

**For libmochi_jvm:** Use **Jackson 2.18** (or whichever 2.18.x is the latest LTS at build time). Reasons:

- Standard. Every JVM developer recognises it; SBOM tools have CVE coverage; IDE plugins exist; Stack Overflow answers exist.
- Module-friendly. Jackson 2.x ships proper `module-info` files since 2.13 for the databind module.
- Format-extensible. The same library handles CSV (`jackson-dataformat-csv`) and YAML (`jackson-dataformat-yaml`), so users who want to swap codecs get a consistent API.
- Active maintenance. 2.18 patches are monthly; security fixes ship in days.

The `mochi.json` module wraps Jackson behind a small surface:

```java
package mochi.json;
public final class Json {
    public static String encode(Object v);                  // -> JSON text
    public static Object decode(String s);                  // -> Mochi value (Map/List/String/Double/Boolean/null)
    public static <T> T decode(String s, Class<T> t);       // -> typed POJO
    public static JsonValue parseStream(InputStream in);    // streaming
}
```

The internal mapper is a singleton `ObjectMapper` configured with:
- `JavaTimeModule` registered (so `Instant` / `LocalDateTime` round-trip ISO-8601).
- `MapperFeature.PROPAGATE_TRANSIENT_MARKER` off.
- Default typing **disabled** (no polymorphic deserialisation by default; this is the source of every Jackson CVE).

Users who need to swap Jackson for Moshi or jsoniter can install a `ServiceLoader`-discovered `dev.mochi.runtime.spi.JsonCodec`; the default ships in `mochi.json.impl.JacksonCodec`.

## 10. CSV and YAML

**CSV.** Two serious choices: `jackson-dataformat-csv` (2.18.x, tracks the main Jackson version) and `opencsv` (5.10 in 2025). Jackson CSV integrates cleanly with the same `ObjectMapper` we already use for JSON, so we pick that.

**YAML.** Three choices:

- **SnakeYAML** (the original, `org.yaml:snakeyaml`). YAML 1.1, JavaBean creation enabled by default. The source of repeated CVEs (deserialisation gadgets). v2.0 disabled bean creation by default in 2023; current is 2.5.0 (December 2025).
- **snakeyaml-engine** (`org.snakeyaml:snakeyaml-engine`). YAML 1.2 only, no bean creation (parse to `Map`/`List`/`String`/`Number`/`Boolean`), safe by default. v2.10 shipped July 2025; v3.0 shipped November 2025.
- **jackson-dataformat-yaml.** Wraps SnakeYAML internally. Convenient if you already use Jackson for JSON.

**For libmochi_jvm:** Use **snakeyaml-engine 2.10+** for `mochi.yaml`. Reasons:

- YAML 1.2 only. Modern, no implicit-typing quirks (`yes`/`no` as booleans is a 1.1 thing).
- Safe by default. No bean creation, so no deserialisation CVE class.
- Small dependency (~290 KB).
- Matches the C target's choice to parse to plain values (see [[06-type-lowering]] §6).

The surface:

```java
package mochi.yaml;
public final class Yaml {
    public static String encode(Object v);
    public static Object decode(String s);    // -> Map / List / String / Double / Boolean / null
}
```

Jackson YAML is rejected because it pulls in SnakeYAML 1.x compatibility (the dataformat module pins SnakeYAML transitively) and we want a clean YAML-1.2-only path.

## 11. Time

`java.time` (JSR-310, JDK 8) is the JVM's modern date/time library. It is excellent and complete:

- **`Instant`**, UTC moment with nanosecond precision. Mochi `time` lowers here.
- **`Duration`**, span between two instants, nanosecond precision. Mochi `duration` lowers here.
- **`LocalDate`, `LocalTime`, `LocalDateTime`**, wall-clock without timezone.
- **`ZonedDateTime`**, wall-clock with timezone (`ZoneId`).
- **`OffsetDateTime`**, wall-clock with fixed offset (no DST).
- **`Period`**, calendar-based span (years, months, days), distinct from `Duration`.

**For libmochi_jvm:**

| Mochi | JVM |
| --- | --- |
| `time` | `java.time.Instant` |
| `duration` | `java.time.Duration` |
| `time.now()` | `Instant.now(Clock.systemUTC())` |
| `time.parse(s)` | `Instant.parse(s)` (ISO-8601) |
| `time.format(t, fmt)` | `DateTimeFormatter.ofPattern(fmt).format(t.atZone(zone))` |
| `t + d` | `t.plus(d)` |
| `t1 - t2` | `Duration.between(t2, t1)` |

The `Clock` abstraction is the testing seam: `mochi_test.fake_clock(...)` swaps the runtime's clock to a fixed `Instant`, so timing-sensitive tests are deterministic. We ship `mochi.time.Clock.fixed(Instant)` and `Clock.tick(Duration)` (the latter is from `java.time.Clock.tick`, the JDK does it for us).

Everything lowers trivially; no library beyond `java.base` needed.

## 12. Random and PRNG

`java.util.random` (JEP 356, GA in JDK 17) introduced the **`RandomGenerator`** interface and a family of modern PRNG algorithms. The legacy `java.util.Random`, `ThreadLocalRandom`, and `SplittableRandom` all implement `RandomGenerator`.

Specialised sub-interfaces:

- `SplittableGenerator`, `split()` creates a statistically-independent generator, for fork/join.
- `JumpableGenerator`, `LeapableGenerator`, fixed-distance jumps in the sequence.
- `ArbitrarilyJumpableGenerator`, jump any distance.
- `StreamableGenerator`, produce a `Stream<RandomGenerator>` of independent generators.

The new algorithms (the **LXM family**, Steele and Vigna 2021) are splittable, fast, and have better statistical properties than the legacy `SplittableRandom`. `L64X128MixRandom` is a good default; `L128X1024MixRandom` is the strongest if state size is not an issue.

**For libmochi_jvm:**

- `mochi.random` exposes a thread-local default generator backed by `RandomGeneratorFactory.of("L64X128MixRandom").create()`.
- `mochi.random.seed(s: int64)` returns a fresh generator with a fixed seed. Used in tests.
- `mochi.random.split()` returns a `SplittableGenerator` for use inside `parallel for`. The codegen recognises Mochi's `random` inside a parallel loop and rewrites to a per-iteration `split()` so we get independent streams.

This matches MEP-45's choice of PCG and MEP-46's reliance on `rand:` (which is splittable since OTP 22).

## 13. Reflection

Three reflection mechanisms:

- **`java.lang.reflect`** (JDK 1.1, but `Class<?>` since 1.0). The historical API. `Class.forName`, `Method.invoke`, `Field.get`, etc. High overhead, but stable.
- **`java.lang.invoke.MethodHandle`** (JDK 7). A typed function pointer; much faster than `Method.invoke` once cached. The basis for `LambdaMetafactory` (how `lambda x: x + 1` lowers since JDK 8).
- **`java.lang.invoke.VarHandle`** (JDK 9). Typed field handles with memory-ordering modes (`get`, `getAcquire`, `getOpaque`, `getVolatile`, `compareAndSet`). Replaces `sun.misc.Unsafe.compareAndSwapInt` and friends for application code.

**For libmochi_jvm:** Mochi has no `reflect` builtin in the language surface, so user code never reflects. The runtime uses reflection internally only for:

- Jackson's `ObjectMapper` (Jackson reflects to bind POJOs).
- `ServiceLoader` provider discovery.
- The `mochi_test` harness, which reflects to discover `@Test`-annotated methods in generated test modules.

We **do not** use `MethodHandle` or `VarHandle` directly in v0.1; both are powerful but the JDK's `j.u.c.atomic` types cover what we need.

**Native-image caveat.** GraalVM native-image (see [[10-build-system]]) requires a reflection configuration file listing every reflectively-accessed class, method, and field. Jackson + native-image is workable but requires `reflect-config.json` generation. The `mochi-runtime` JAR ships such a file under `META-INF/native-image/dev.mochi/runtime/reflect-config.json` so native-image users do not have to write it themselves.

## 14. Telemetry and observability

The JVM has the strongest built-in observability story of any of the three Mochi targets.

**JFR (Java Flight Recorder).** Open-sourced as part of OpenJDK 11 (JEP 328). Sub-percent runtime overhead. Records events to a `.jfr` file or a streaming endpoint; analysed in JDK Mission Control or `jfr summary`. JDK 25 adds:

- **JEP 509 (experimental)**: CPU-time profiling on Linux (sampled via `clock_gettime(CLOCK_THREAD_CPUTIME_ID)`).
- **JEP 518**: cooperative thread sampling, reduces safepoint bias.
- **JEP 520**: method timing and tracing without bytecode instrumentation.

Custom JFR events are easy: extend `jdk.jfr.Event`, annotate, populate fields, call `commit()`. The JVM handles encoding, file rotation, and the visualisation toolchain reads it for free.

**Logging.** `java.util.logging` (built-in, slow, awkward), `Logback` (de facto standard), `Log4j 2.x` (the post-Log4Shell rewrite). SLF4J is the facade everyone targets.

**Metrics.** Micrometer (Spring's library, now an independent project) is the dominant facade; backends include Prometheus, Datadog, CloudWatch, StatsD, JFR. OpenTelemetry Java SDK (1.60.x as of May 2026) is the future for end-to-end distributed tracing; instrumentation 2.26.0 (May 2026) is the latest.

**Tracing.** OpenTelemetry Java (auto-instrumentation agent at 2.26.0, May 2026). Bridges to Zipkin (deprecated as of 1.65.0 in August 2026), Jaeger, and OTLP.

**For libmochi_jvm:** The `mochi.telemetry` module emits **JFR events**:

```
mochi.Agent.Start, mochi.Agent.Stop, mochi.Agent.Crash
mochi.Stream.Publish, mochi.Stream.Subscribe
mochi.Fetch.Request (start/stop)
mochi.Query.Execute (start/stop)
```

Each event has a name, category (`mochi`), and a small payload (agent ID, stream name, URL, query hash). Users see them in JDK Mission Control automatically. The module also installs an SLF4J bridge so `mochi.log.info("...")` lowers to `org.slf4j.LoggerFactory.getLogger("mochi").info(...)`. Users plug their own SLF4J backend (Logback or Log4j 2).

OpenTelemetry integration is **opt-in via a separate artifact**, `mochi-runtime-otel`, that depends on `io.opentelemetry:opentelemetry-sdk:1.60.x`. The core `mochi-runtime` JAR does not pull in OpenTelemetry to keep the default deployment lean.

## 15. Datalog tables

The Mochi language has Datalog-style relational queries ([[08-dataset-pipeline]]). The runtime needs an in-memory fact store and, optionally, a persistent one.

**In-memory.** Three options:

- **`HashMap<Tuple, Boolean>` keyed by a `record Tuple(Object... v)`.** Simplest. O(1) lookup, no indexes.
- **`ConcurrentHashMap` of the same.** When facts are added concurrently from multiple agents.
- **A purpose-built tuple-table with secondary indexes.** Each Datalog relation gets a primary tuple set plus one `HashMap<Object, Set<Tuple>>` per indexed argument. Lookups by indexed argument become O(1).

We pick the third for v0.1. The `mochi.datalog` module exposes a `Relation<T extends Record>` type backed by:

- A primary `LinkedHashSet<T>` for full scans.
- A per-field `HashMap<Object, ArrayList<T>>` index when the field is declared `@Index` in the relation schema.
- A `StampedLock` for concurrent reads with rare writes.

**Persistent.** Three options for "Datalog facts that outlive the JVM":

- **H2** (embedded SQL, Java-pure). Used by Spring's tests, by JetBrains products. Mature, ~2 MB.
- **SQLite via Xerial JDBC** (`org.xerial:sqlite-jdbc`). C-backed, JNI-wrapped. The most popular embedded DB in the world.
- **RocksDB via RocksDB-JNI** (`org.rocksdb:rocksdbjni`). LSM-tree, native code, big-data scale.

We defer persistence to [[08-dataset-pipeline]] and v0.2. For v0.1, Datalog facts live in memory and dump to disk on `:save` via Jackson JSON serialisation. Out-of-process Datalog is out of scope.

## 16. Mochi runtime module layout: `dev.mochi.runtime`

Putting it all together, the runtime library `dev.mochi.runtime` exposes the following Maven coordinates and package layout. Maven coordinate `dev.mochi:mochi-runtime:<mochi-version>`. Module name `dev.mochi.runtime`. JAR target: JDK 21 bytecode (class file version 65), MR-JAR with JDK 25 overlays for `java.lang.foreign` and compact-object-header tuning under `META-INF/versions/25/`.

```
mochi-runtime/
├── module-info.java
├── mochi/
│   ├── core/                     // Boxed value helpers, dynamic dispatch, equality
│   │   ├── MochiValue.java
│   │   ├── MochiEquals.java
│   │   └── MochiHash.java
│   ├── list/                     // list<T> helpers
│   │   ├── MochiList.java
│   │   └── MochiListOps.java
│   ├── map/                      // map<K,V> helpers
│   │   ├── MochiMap.java
│   │   └── MochiMapOps.java
│   ├── set/                      // set<T> helpers
│   │   ├── MochiSet.java
│   │   └── MochiSetOps.java
│   ├── string/                   // string helpers (code-point indexed)
│   │   └── MochiStr.java
│   ├── bytes/                    // byte[] / MemorySegment helpers
│   │   ├── MochiBytes.java
│   │   └── MochiArena.java
│   ├── option/                   // Option<T> = Optional<T>
│   │   └── MochiOption.java
│   ├── time/                     // Instant / Duration wrappers
│   │   ├── MochiTime.java
│   │   └── MochiDuration.java
│   ├── random/                   // RandomGenerator wrappers
│   │   └── MochiRandom.java
│   │
│   ├── agent/                    // agent runtime (virtual-thread backed)
│   │   ├── MochiAgent.java
│   │   ├── MochiAgentSup.java
│   │   └── MochiMailbox.java
│   ├── stream/                   // stream pubsub (SubmissionPublisher backed)
│   │   ├── MochiStream.java
│   │   └── MochiStreamRegistry.java
│   ├── async/                    // async / await / scope (manual structured)
│   │   ├── MochiAsync.java
│   │   └── MochiScope.java
│   │
│   ├── query/                    // query DSL runtime (Stream-backed)
│   │   ├── MochiQuery.java
│   │   ├── JoinCollector.java
│   │   └── WindowCollector.java
│   ├── datalog/                  // Datalog relations
│   │   ├── Relation.java
│   │   └── Index.java
│   │
│   ├── fetch/                    // fetch (HTTP) facade (java.net.http)
│   │   ├── MochiFetch.java
│   │   └── MochiWebSocket.java
│   ├── json/                     // Jackson facade
│   │   ├── Json.java
│   │   └── spi/JsonCodec.java
│   ├── csv/                      // jackson-dataformat-csv facade
│   │   └── Csv.java
│   ├── yaml/                     // snakeyaml-engine facade
│   │   └── Yaml.java
│   │
│   ├── fs/                       // file I/O (java.nio.file)
│   │   └── MochiFs.java
│   ├── os/                       // environment, process exit, args
│   │   └── MochiOs.java
│   │
│   ├── llm/                      // LLM client facade
│   │   ├── MochiLlm.java
│   │   └── providers/
│   │       ├── OpenAi.java
│   │       └── Anthropic.java
│   │
│   ├── ffi/                      // Panama FFM helpers
│   │   └── MochiFfi.java
│   ├── telemetry/                // JFR event emitters + SLF4J bridge
│   │   ├── MochiTelemetry.java
│   │   └── events/
│   │       ├── AgentStartEvent.java
│   │       └── ...
│   ├── log/                      // mochi.log (SLF4J facade)
│   │   └── MochiLog.java
│   └── testing/                  // test harness for `test "..."` blocks
│       ├── MochiTest.java
│       └── MochiAssert.java
└── META-INF/
    ├── services/
    │   └── dev.mochi.runtime.spi.JsonCodec   // -> mochi.json.impl.JacksonCodec
    ├── native-image/
    │   └── dev.mochi/runtime/
    │       ├── reflect-config.json
    │       └── resource-config.json
    └── versions/
        └── 25/                  // JDK 25 overlays
            └── ...
```

Generated Mochi modules import from these packages directly:

```java
package mochi.user;
import mochi.list.MochiList;
import mochi.log.MochiLog;
import java.util.ArrayList;

public final class Main {
    public static int main(ArrayList<String> args) {
        int n = MochiList.length(args);
        MochiLog.info("argc", java.util.Map.of("count", n));
        return 0;
    }
}
```

Boot order, on `dev.mochi.runtime.Boot.main`:

1. JVM boots (class loader graph initialised).
2. `dev.mochi.runtime` module resolved; `module-info` `requires` are linked.
3. `Boot.init()` runs: install JFR event types, configure `ObjectMapper` singleton, install SLF4J configuration default (Logback if on path, else `j.u.l`), open the default `mochi_arena`.
4. User `main(args)` invoked.

Cold start times (measured on M2 / JDK 21, "Hello world" Mochi program):
- `java -jar mochi-app.jar`, ~180 ms.
- `java -jar mochi-app.jar` with AOTCache (JDK 21 `-XX:ArchiveClassesAtExit=app.jsa`, then `-XX:SharedArchiveFile=app.jsa`), ~80 ms.
- `native-image` (GraalVM 23) AOT compile, ~15 ms.

These compare unfavourably to the C target's ~5 ms and favourably to typical "starts a JVM" measurements; the AOTCache mode is the realistic default for `mochi run` on JVM, and native-image is the realistic default for `mochi build --jvm-native`.

## 17. What we do NOT need

Services we considered and rejected for libmochi_jvm v0.1:

- **Akka / Pekko**. Heavyweight actor framework. Mochi's agent model is simpler than Akka's typed actors, and we get the same scheduling for free from Loom.
- **Vert.x**. Event-loop based. Loom + plain HttpClient covers it.
- **Spring Framework / Spring Boot**. We are a runtime library, not an application framework.
- **Quarkus, Micronaut**. Same.
- **Reactor / RxJava**. The Flow API + virtual threads is enough; we do not need a third reactive vocabulary.
- **Persistent collections (Vavr, Eclipse Collections)**. Discussed in §5.
- **Guava**. Useful but huge. The bits we'd use (`Multimap`, `Caches`) we either don't need (Multimap subsumed by our Datalog relation) or have built (`mochi.cache` uses `ConcurrentHashMap` directly).
- **Apache Commons (Lang, IO, Collections)**. Same reasoning; JDK 21 covers what we need.
- **Jakarta EE APIs (JNDI, JMS, JTA)**. Out of scope.
- **JNI for new native code**. Panama FFM is the path. JNI is read-only (existing libraries) and goes through FFM's `Linker.nativeLinker()`.
- **Custom class loaders**. Mochi-generated code lives on the application classpath; no hot reload in v0.1.

These rejections are not permanent; later MEPs can lift them.

## 18. Limitations and gotchas

- **Virtual-thread pinning under JDK 21 / 22 / 23.** Holds until users upgrade to JDK 24 or 25. Document in the MEP.
- **HTTP/2 connection multiplexing cap.** ~100 concurrent requests per host. Document, and provide a `mochi.fetch.maxConcurrentPerHost` knob.
- **Jackson default typing.** Source of nine CVEs in five years. We hard-disable default typing in the singleton `ObjectMapper` and document that users who enable it are on their own.
- **Atom-of-strings problem.** Mochi sum-type variants and map keys are `String` on JVM. We do not intern user-supplied strings (no `String.intern()` because the intern table is bounded). Variant tag matching uses pre-interned constants only.
- **`Optional` and primitives.** `Optional<Integer>` boxes. For hot loops where `Option<int>` matters, the codegen lowers to a pair `(int value, boolean present)` returned via a small two-field record class.
- **Class-loading pinning (post-491).** A virtual thread that triggers class loading inside a hot path will pin briefly. Mitigation: the runtime pre-loads every `mochi.*` class at boot via `Class.forName` on the known list.
- **Native-image reflection metadata.** Out-of-the-box `native-image` builds need a `reflect-config.json`; we ship one. Users who add their own reflection-using libraries must regenerate.

## 19. Boot sequence

When a Mochi-compiled JAR starts:

1. JVM launcher (`java`) loads the launcher native code; reads `MANIFEST.MF` `Main-Class`.
2. Bootstrap class loader links `java.base`.
3. Platform class loader links `java.net.http`, `java.logging`, `java.sql`, etc. (the modules `dev.mochi.runtime` requires).
4. App class loader loads `dev.mochi.runtime` and the user's main class.
5. `dev.mochi.runtime.Boot.init()` runs (idempotent; called from the user main's static initialiser):
   - Register JFR event types.
   - Configure default `ObjectMapper`.
   - Open the default `mochi_arena` (sized to 1 MiB, grows on demand).
   - Discover JSON codec providers via ServiceLoader.
   - Install SLF4J binding (Logback if present, else fall back to `java.util.logging` adapter).
6. User `main(args)` is invoked.

Boot time on JDK 21:
- Java launcher cold: ~80 ms.
- Mochi runtime init: ~30 ms.
- Hello-world user code: <1 ms.
- Total: ~110 ms cold.

With AOTCache (`-XX:SharedArchiveFile=mochi.jsa`): ~50 ms cold. With native-image: ~15 ms cold.

These numbers compare favourably to typical Java application boot (Spring Boot is 1.5 to 4 seconds cold) and unfavourably to the C target (5 ms) and the BEAM escript target (50 ms). The trade-off is the JVM's strong observability, mature library ecosystem, and the option to run on `native-image` when boot time matters.

---

## Sources

1. JDK 21 release notes. https://openjdk.org/projects/jdk/21/
2. JDK 25 release notes. https://openjdk.org/projects/jdk/25/
3. Oracle JDK 25 Migration Guide (G35926-01). https://docs.oracle.com/en/java/javase/25/migrate/
4. JEP 444: Virtual Threads. https://openjdk.org/jeps/444
5. JEP 491: Synchronize Virtual Threads without Pinning. https://openjdk.org/jeps/491
6. JEP 439: Generational ZGC. https://openjdk.org/jeps/439
7. JEP 474: ZGC: Generational Mode by Default. https://openjdk.org/jeps/474
8. JEP 519: Compact Object Headers. https://openjdk.org/jeps/519
9. JEP 431: Sequenced Collections. https://openjdk.org/jeps/431
10. JEP 454: Foreign Function & Memory API. https://openjdk.org/jeps/454
11. JEP 442: Foreign Function & Memory API (Third Preview, JDK 21). https://openjdk.org/jeps/442
12. JEP 356: Enhanced Pseudo-Random Number Generators. https://openjdk.org/jeps/356
13. JEP 505: Structured Concurrency (Fifth Preview, JDK 25). https://openjdk.org/jeps/505
14. JEP 525: Structured Concurrency (Sixth Preview, JDK 26). https://openjdk.org/jeps/525
15. JEP 328: Flight Recorder. https://openjdk.org/jeps/328
16. JEP 509: JFR CPU-Time Profiling (Experimental, JDK 25). https://openjdk.org/jeps/509
17. JEP 518: JFR Cooperative Sampling (JDK 25). https://openjdk.org/jeps/518
18. JEP 520: JFR Method Timing & Tracing (JDK 25). https://openjdk.org/jeps/520
19. JEP 254: Compact Strings. https://openjdk.org/jeps/254
20. JEP 238: Multi-Release JAR Files. https://openjdk.org/jeps/238
21. JEP 321: HTTP Client (Standard). https://openjdk.org/jeps/321
22. Oracle HttpClient JDK 21 docs. https://docs.oracle.com/en/java/javase/21/docs/api/java.net.http/java/net/http/HttpClient.html
23. Java 25 Release notes review, Inside Java Newscast #98. https://inside.java/2025/09/25/newscast-98/
24. Structured Concurrency Revamp in Java 25, Inside Java Newscast #91. https://nipafx.dev/inside-java-newscast-91/
25. Java Virtual Threads Two Years In, Java Code Geeks 2026-05. https://www.javacodegeeks.com/2026/05/virtual-threads-two-years-in-production-war-stories-the-pinning-edge-cases-and-what-jdk-25-fixed.html
26. Jackson Release 2.18 wiki. https://github.com/FasterXML/jackson/wiki/Jackson-Release-2.18
27. Jackson Releases overview. https://github.com/FasterXML/jackson/wiki/Jackson-Releases
28. snakeyaml-engine project. https://bitbucket.org/snakeyaml/snakeyaml-engine
29. OpenTelemetry Java 1.60.x. https://github.com/open-telemetry/opentelemetry-java/releases
30. OpenTelemetry Java Instrumentation 2.26.0. https://github.com/open-telemetry/opentelemetry-java-instrumentation/releases
31. GraalVM Native Image FFM support. https://docs.oracle.com/en/graalvm/jdk/22/docs/reference-manual/native-image/native-code-interoperability/foreign-interface/
32. JEP 198: JSON API (on hold). https://openjdk.org/jeps/198
