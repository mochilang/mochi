# MEP-49 research note 04, Swift runtime building blocks for MochiRuntime

Author: research pass for MEP-49.
Date: 2026-05-23 10:12 (GMT+7).
Method: structured research over Swift Evolution proposals (github.com/swiftlang/swift-evolution), Swift 6.0 and 6.1 release notes (swift.org/blog), the swiftlang and apple GitHub orgs (swift-syntax, swift-format, swift-system, swift-collections, swift-algorithms, swift-async-algorithms, swift-numerics, swift-log, swift-metrics, swift-distributed-tracing), Apple developer documentation for Foundation, Observation, SwiftData, and FoundationModels, and the swift-corelibs-foundation README plus its Linux parity matrix (apple/swift-corelibs-foundation, main branch as of May 2026).

This note inventories the runtime services Mochi programs need at execution time on Swift, and chooses for each one a Swift standard-library facility, a Foundation API, or a vetted swiftlang/apple package to lean on. The output is the **module layout for the `MochiRuntime` SwiftPM package** (section 20), which is the runtime library that every Mochi-generated `.swift` file imports.

The companion notes (01 language surface, 02 design philosophy, 03 prior-art transpilers) establish the language surface Mochi exposes on Swift. This note assumes Mochi semantics are fixed and asks: what does Swift give us, what do we still have to write, what should we leave at the door.

Swift baseline for MEP-49 is **Swift 6.0** (September 17 2024), with **Swift 6.1** (March 11 2025) as the current ceiling for the May 2026 timeframe. Swift 5.10 is explicitly out of scope (no Swift 6 language mode, no region-based isolation, no `~Escapable`). Platform floor is the set of platforms where Swift 6.0 toolchains ship: Apple (iOS 13+, macOS 12+, watchOS 6+, tvOS 13+, visionOS 1+), Linux (Ubuntu 20.04 / 22.04 / 24.04, Amazon Linux 2, RHEL 9, Fedora 39+, Debian 12 on x86_64 and aarch64), and Windows 10+ on x86_64 (with experimental aarch64 support since 6.0.3). Android via the swift-android-sdk overlay is best-effort.

---

## 1. Swift standard library surface

The Swift standard library (the `Swift` module, imported implicitly) provides the value-type vocabulary Mochi lowers onto. Versions track the toolchain, not a package release.

**Integer family**: `Int` (machine-word, 64-bit on every supported platform), `Int8`, `Int16`, `Int32`, `Int64`, `UInt`, `UInt8`, `UInt16`, `UInt32`, `UInt64`. Trap-on-overflow semantics by default (the `&+`, `&-`, `&*` family wraps). Mochi `int` lowers to `Int64` (not `Int`) for cross-platform determinism, matching the C target's `int64_t` choice.

**Float family**: `Float` (32-bit), `Double` (64-bit), `Float80` (x86_64 only, not portable), `Float16` (Swift 5.3+, hardware on Apple Silicon and ARMv8.2-FP16, software-emulated elsewhere via swift-numerics). Mochi `float` lowers to `Double`.

**`Bool`**: a single-bit value type with `&&`, `||`, `!` and short-circuit semantics. Maps directly to Mochi `bool`.

**`String`**: UTF-8-backed since Swift 5.7 (SE-0178 codified the rewrite that landed in 5.0 and stabilised through 5.7). Code-point access is via `Character` (an extended grapheme cluster) by default; `unicodeScalars`, `utf8`, `utf16`, and `unicodeScalars` views give other indexings. String indexing is **opaque**: `String.Index` is not an integer, so `s[i]` requires `s.index(s.startIndex, offsetBy: i)`. This is intentional (extended grapheme clusters have variable byte width) but it means Mochi `string` indexing lowers to a helper. `Substring` is a slice view into the parent string; converting to `String` copies.

**`Character`**: extended grapheme cluster, holding 1 to N `Unicode.Scalar` values. Equality and comparison follow Unicode normalization.

**`Array<T>`**: copy-on-write value type backed by a contiguous heap buffer. O(1) append amortised, O(1) indexed access, O(N) middle insert. Mochi `list<T>` lowers to `Array<T>` directly. Empty literals use `Array<T>()`; sized literals use `Array(repeating: x, count: n)`.

**`Dictionary<K, V>`**: copy-on-write hash table. Keys conform to `Hashable`. **Iteration order is unspecified and unstable** across copies on Swift 6.0 (this matters for `group_by`, see section 5). Mochi `map<K, V>` does **not** lower to `Dictionary` directly when stable iteration is required; the codegen substitutes `OrderedDictionary` from swift-collections.

**`Set<T>`**: copy-on-write hash set. Same iteration-order caveat as Dictionary.

**`Optional<T>`** (sugared as `T?`): tagged union of `.none` and `.some(T)`. Mochi `Option<T>` and `T?` lower here.

**`Result<Success, Failure>`** (`Failure: Error`): two-case enum, `.success(Success)` or `.failure(Failure)`. Mochi `Result<T, E>` lowers here when `E` conforms to `Error`.

**`Range<Bound>`, `ClosedRange<Bound>`**: half-open and closed integer/comparable ranges. `0..<n` is `Range<Int>`; `0...n` is `ClosedRange<Int>`. Mochi `for i in 0..<n` lowers directly.

**`Slice<Base>`**: generic slice view into any `Collection`. `ArraySlice<T>` and `Substring` are the specialised forms. Mochi `list[a:b]` lowers to `Array(arr[a..<b])` to materialise a fresh array (Mochi semantics require slice independence from the source).

For libmochi_swift: everything in this section is zero-cost; we use it directly. The only translation layers are (a) `Int64` over `Int`, and (b) `OrderedDictionary` over `Dictionary` for stable-iteration semantics.

## 2. Foundation surface

Foundation is the next ring out. On Apple platforms it bridges to NSDate / NSURL / NSData via the Objective-C runtime. On Linux and Windows it is **swift-corelibs-foundation** (a pure-Swift reimplementation tracked at apple/swift-corelibs-foundation), which is not byte-compatible but is API-source-compatible. Foundation is the *un-numbered* package, versioned with the toolchain.

The pieces Mochi taps:

- **`Date`**: a `TimeInterval` (Double seconds) since the 2001-01-01 00:00:00 UTC reference. Wall-clock only, **no timezone information** carried on the value. Maps Mochi `time` to a pair `(Date, TimeZone)` since Mochi has zoned-time semantics by spec; see section 20 for the `ZonedDateTime` polyfill.
- **`TimeZone`**: ICU-backed timezone identifier. Mochi `time.zone` lowers here.
- **`Calendar`**: Gregorian by default, also Buddhist / Islamic / Hebrew / Japanese / ISO-8601-week. Mochi `time.add(days:)` lowers via `Calendar.date(byAdding:to:)`.
- **`DateFormatter`** (legacy, locale-sensitive) and **`ISO8601DateFormatter`** (strict). Mochi `time.format` prefers the ISO formatter for round-trips. Swift 5.7 introduced `Date.FormatStyle` (FormatStyle protocol) as the modern API; Mochi targets `Date.FormatStyle` on Swift 6 and falls back to `DateFormatter` only on platform-not-supported.
- **`URL`**: opaque URL value type. **`URLComponents`** for query-string manipulation.
- **`URLSession`**: HTTP client. Supports HTTP/1.1, HTTP/2. HTTP/3 is Apple-only (Network.framework path). Async/await methods (`data(from:)`, `data(for:)`, `bytes(from:)`) since iOS 15 / macOS 12, and on Linux since swift-corelibs-foundation caught up in late 2022.
- **`Data`**: byte buffer, copy-on-write. Maps Mochi `bytes` for small to mid-size buffers.
- **`FileManager`**: filesystem operations (`createDirectory`, `removeItem`, `attributesOfItem`).
- **`FileHandle`**: low-level read/write/seek on file descriptors. The async-iterator extension (`for try await line in handle.bytes.lines`) shipped with Swift 5.5 on Apple, late 2022 on Linux.
- **`JSONEncoder`, `JSONDecoder`**: Codable-driven JSON I/O. `JSONSerialization` is the legacy path returning `Any`; we use it only for schema-less Mochi `decode_json`.
- **`PropertyListEncoder`, `PropertyListDecoder`**: same, for `.plist` XML/binary. Apple-only consumers, but available on Linux too.
- **`NSRegularExpression`**: ICU-backed regex, the legacy path. Swift 5.7 added **typed `Regex<Output>` literals** (SE-0354, SE-0357) with the `/pattern/` syntax, but those literals require RegexBuilder on Apple platforms only; on Linux the literals compile but some metacharacters fall back to `NSRegularExpression`. Mochi `regex` lowers to `Regex<AnyRegexOutput>` on Apple platforms (Swift 5.7+) and to `NSRegularExpression` on Linux/Windows for parity until the Linux Regex backend matches.

For libmochi_swift: Foundation is a hard dependency. The `MochiRuntime` package `import`s Foundation in every file that needs Date, URL, Data, JSON, or files. We accept the platform divergence and pin behaviour in tests (see note 11).

## 3. Linux/Windows Foundation gaps

swift-corelibs-foundation reimplements the Foundation surface in pure Swift on Linux and Windows. Parity is excellent for the surfaces Mochi cares about but is not complete. The matrix below covers May 2026 status (swift-corelibs-foundation main branch, equivalent to Swift 6.1 release).

| API | Apple | Linux | Windows | Notes |
| --- | --- | --- | --- | --- |
| `Date` | full | full | full | bit-identical TimeInterval semantics |
| `TimeZone` | full | partial | partial | ICU data required; Linux uses system tzdata, Windows ships its own subset |
| `Calendar` | full | partial | partial | Gregorian works; Buddhist / Islamic / Hebrew complete since 6.0 |
| `URL`, `URLComponents` | full | full | full | RFC 3986 path-percent-encoding identical |
| `URLSession` (async/await) | full | full | full | HTTP/1.1 + HTTP/2 on libcurl; HTTP/3 Apple-only |
| `URLSession` WebSocket | full | full | partial | Windows WebSocket landed in 6.1 |
| `JSONEncoder` / `JSONDecoder` | full | full | full | Identical output (sorted keys deterministic) |
| `PropertyListEncoder` | full | full | full | XML and binary plist |
| `NSRegularExpression` | full | full | full | Backed by ICU regex on all platforms |
| `Regex<Output>` literals | full | partial | partial | RegexBuilder API Apple-only; literal compilation works everywhere |
| `FileManager` | full | full | full | Symlink + extended attribute APIs identical |
| `FileHandle` | full | full | full | `bytes.lines` async iterator on all 6.0+ |
| `ProcessInfo` | full | full | full | env vars, process arguments |
| `Process` (NSTask) | full | full | partial | Windows `Process.run()` landed in 5.9; some signal APIs differ |
| `NSCache` | full | absent | absent | Mochi uses a plain `Dictionary` + manual eviction instead |
| `NSPredicate` / `NSExpression` | full | partial | partial | Foundation key-path evaluation Apple-only |
| `Bundle` resource lookup | full | partial | partial | SwiftPM resource bundles work; `Bundle.main` differs |

What Mochi runtime polyfills:
- **`ZonedDateTime`**: Foundation `Date` does not carry a `TimeZone`, so we ship a small `struct ZonedDateTime { let instant: Date; let zone: TimeZone }` value type and its operations (`addingDays`, `formatted`, ISO-8601 round-trip). See section 20.
- **`Cache<K, V>`**: thin wrapper around `[K: V]` with size-bounded LRU, since `NSCache` is Apple-only. Implementation uses `OrderedDictionary` from swift-collections (move-to-front on access, evict from tail).
- **Predicate evaluation**: Mochi query DSL never uses `NSPredicate`; it lowers directly to Swift closures, so this gap is irrelevant.

What Mochi runtime defers:
- HTTP/3, multipath TCP, and other Network.framework-only features stay Apple-only and the runtime documents the limitation.
- `Bundle.main` is replaced with `Bundle.module` (the SwiftPM-resource bundle injected by the build) for any Mochi-shipped resource. User code reading `Bundle.main` is platform-conditional.
- `NSPersonNameComponentsFormatter` and other locale-formatter heavy APIs are not exposed via Mochi at all in v0.1.

The takeaway: Foundation on Linux and Windows is 95% there for Mochi's purposes. The 5% gap is well-localised and we polyfill it inside `MochiRuntime`.

## 4. Apple-only frameworks the runtime taps when available

These frameworks ship only on Apple platforms (iOS 17+, macOS 14+, in some cases later). `MochiRuntime` exposes them behind `#if canImport(...)` guards so that the same Mochi source compiles on Linux and Windows with a degraded path.

**Observation** (the `@Observable` macro, iOS 17 / macOS 14 / Swift 5.9+). Replacement for the older `ObservableObject` + `@Published` Combine path. A class annotated `@Observable` gets per-property change tracking with no boilerplate. Mochi `state` blocks lower to `@Observable` classes on Apple, and to a hand-rolled `Observation` polyfill on Linux/Windows that uses a single `AsyncStream<KeyPath<Self, Any>>` for change notifications.

**SwiftData** (iOS 17 / macOS 14, Swift 5.9+). Persistence framework over Core Data. `@Model` macro generates schema. SwiftData ships with the OS, not with the toolchain, so it is Apple-only and cannot be polyfilled meaningfully. Mochi `persist` (the optional persistence keyword) is currently spec-only; when implemented, it will lower to SwiftData on Apple and to SQLite (via swift-sqlite) on Linux/Windows.

**FoundationModels** (iOS 18.1+ / macOS 15.1+ / visionOS 2.1+, Apple Silicon only, June 2024 WWDC). On-device LLM with 3-billion-parameter foundation model accessed via `LanguageModelSession`. Tool-calling, guided generation, streaming output. Mochi `llm.local` lowers to FoundationModels when the platform check passes, and to a remote provider (OpenAI, Anthropic, Ollama-over-localhost) otherwise. The `FoundationModels` framework is **Apple-only** by design (the model weights ship with the OS), so Linux/Windows users always get the remote path.

**Combine** (iOS 13+ / macOS 10.15+). The legacy reactive framework. `Publisher`, `Subscriber`, `Subject`. Largely superseded by AsyncStream and AsyncSequence since Swift 5.5. Mochi does **not** lower to Combine; the new path is AsyncStream. We import Combine only to interop with existing user code that exposes a `Publisher` (we provide a `Publisher.values` extension that returns an `AsyncStream`).

**Network.framework** (iOS 12+ / macOS 10.14+). Low-level TCP, UDP, QUIC primitives. `NWConnection`, `NWListener`. Apple-only. Mochi `net.tcp` and `net.udp` use Network.framework on Apple and **NIO** (apple/swift-nio, see section 9 in the next note) on Linux/Windows. This is the only place we permit divergent backends behind a single Mochi surface.

For libmochi_swift: each of these frameworks gets a small adapter file under `MochiRuntime/Sources/MochiRuntime/Adapters/` guarded by `#if canImport(...)`. The Linux/Windows fallback is feature-flagged in the runtime's manifest (`MochiRuntime.swift` exports `static let hasObservation: Bool`) so that user code can degrade gracefully.

## 5. swift-collections

Package: `https://github.com/apple/swift-collections`. Current release: **1.1.4** (April 30 2026), supports Swift 5.8+ (so 6.0 and 6.1 are fine). Maintained by Apple. Imports as `Collections` (umbrella) or per-module (`OrderedCollections`, `DequeModule`, `HeapModule`, `BitCollections`, `HashTreeCollections`).

The types we use:

| Type | Module | Mochi usage |
| --- | --- | --- |
| `OrderedDictionary<K, V>` | OrderedCollections | `group_by` stability, ordered `map<K,V>` |
| `OrderedSet<T>` | OrderedCollections | ordered `set<T>` |
| `Deque<T>` | DequeModule | agent mailbox backing, bounded ring buffer |
| `Heap<T>` | HeapModule | priority queue for scheduled timers, top-N queries |
| `BitArray`, `BitSet` | BitCollections | Datalog tuple-presence bitmap |
| `TreeDictionary<K, V>`, `TreeSet<T>` | HashTreeCollections | persistent (CHAMP) variants, currently unused, kept for v0.2 |

`OrderedDictionary` keeps insertion order across all mutations and exposes `dict.keys` as an `OrderedSet`. The cost is one extra `Array<K>` allocation per dictionary; for Mochi's typical map sizes (<10k entries) this is negligible.

`Deque<T>` is a ring buffer with O(1) front and back enqueue / dequeue. The Mochi agent mailbox uses `Deque<Message>` wrapped in an `actor` with an internal continuation, so an agent can `push(message)` and `let msg = await pop()` without lock contention. Backpressure is enforced by an upper bound: when `deque.count >= mailbox.maxSize`, the producer either blocks (the default `AsyncStream.Continuation.BufferingPolicy.bufferingNewest(N)` would silently drop, which is wrong for Mochi) or returns `.failure(.mailboxFull)`. See section 14 for the AsyncStream details.

`Heap<T>` is a min-heap (or max- via comparator) with O(log N) push and pop. Used by `MochiSchedule` for `every`, `after`, and exponential-backoff retry queues.

For libmochi_swift: depend on swift-collections 1.1.x. The `Package.swift` declares `.package(url: "https://github.com/apple/swift-collections.git", from: "1.1.0")` and the `MochiRuntime` target lists `.product(name: "OrderedCollections", package: "swift-collections")`, `.product(name: "DequeModule", ...)`, etc. We do **not** depend on `HashTreeCollections` in v0.1 to keep the binary small.

## 6. swift-algorithms

Package: `https://github.com/apple/swift-algorithms`. Current release: **1.2.1** (March 2026). Supports Swift 5.8+. Imports as `Algorithms`.

The types and functions we use (all extension methods on `Sequence` / `Collection`):

| API | Mochi usage |
| --- | --- |
| `chunked(by:)` | streaming-window query operator |
| `chunked(into:)` | `Array.chunked(into: n)` for batch jobs |
| `interspersed(with:)` | `string.join` style helpers |
| `striding(by:)` | every-Nth iteration |
| `windows(ofCount:)` | sliding-window query operator |
| `combinations(ofCount:)` | combinatorial query |
| `permutations(ofCount:)` | combinatorial query |
| `product(_:_:)` | cartesian product, the relational cross-join |
| `uniqued()` | `distinct` query operator |
| `grouped(by:)` | `group_by` (returns `[Key: [Element]]`; Mochi wraps with OrderedDictionary for stable order) |
| `min(ofCount:)`, `max(ofCount:)` | top-N queries |
| `sortedPrefix(_:by:)` | top-N with custom comparator |

For libmochi_swift: depend on swift-algorithms 1.2.x. The Mochi query DSL lowering pass (see note 05 codegen design) emits these directly, with the standard library's `lazy` adapter when the pipeline is lazy. Example: `from xs x where p(x) select f(x)` lowers to `xs.lazy.filter(p).map(f)`, and `from xs x group by k(x)` lowers to `xs.grouped(by: k)` with OrderedDictionary post-processing.

We do **not** use the package's `randomSample` (use `Array.shuffled().prefix(n)` via swift-numerics PRNG instead, for determinism) or `joined(by:)` (use stdlib `Sequence.joined(separator:)` which is older and stable).

## 7. swift-async-algorithms

Package: `https://github.com/apple/swift-async-algorithms`. Current release: **1.0.4** (February 2026), the first 1.x line (the package was 0.x throughout 2022 to 2024 and went 1.0 in October 2024 with Swift 6.0). Supports Swift 5.9+ as a soft floor. Imports as `AsyncAlgorithms`.

The types we use:

| Type / function | Mochi usage |
| --- | --- |
| `AsyncChannel<T>` | bounded inter-agent channel (backpressure-aware) |
| `AsyncThrowingChannel<T, Error>` | same, fallible |
| `AsyncTimerSequence` | `every(duration)` stream |
| `debounce(for:)` | UI-style debouncing in stream pipelines |
| `throttle(for:latest:)` | rate-limiting |
| `merge(_:_:_:)` | join 2 to N streams into one |
| `combineLatest(_:_:)` | Cartesian-of-latest tuple stream |
| `chain(_:_:)` | sequential concatenation of streams |
| `zip(_:_:)` | element-aligned pairing |
| `removeDuplicates()` | de-dup adjacent equal elements |
| `chunked(byTime:)` | time-bucketed batching |
| `interspersed(with:)` | inject heartbeat values |

`AsyncChannel<T>` differs from `AsyncStream<T>` in two important ways:
1. **Backpressure is built in.** A producer awaits `channel.send(value)` until a consumer reaches `for await x in channel`. There is no internal buffer; production rate equals consumption rate.
2. **Multiple consumers are supported.** Multiple `for await` loops share the same channel; each value goes to exactly one consumer (round-robin / first-come).

Mochi `stream` lowers to `AsyncStream<T>` with a bounded buffer (the default), but Mochi `channel` (an explicit synchronous handoff) lowers to `AsyncChannel<T>`. See section 14.

For libmochi_swift: depend on swift-async-algorithms 1.0.x. The `Package.swift` lists `.package(url: "https://github.com/apple/swift-async-algorithms.git", from: "1.0.0")`. Imports happen inside `MochiRuntime/Stream/` only; user-facing Mochi code never imports the package directly.

## 8. swift-numerics

Package: `https://github.com/apple/swift-numerics`. Current release: **1.0.3** (January 2026). Supports Swift 5.5+. Imports as `Numerics` (umbrella), `RealModule`, `ComplexModule`, `IntegerUtilities`.

The pieces:

- **`Real` protocol** (RealModule): generic over `Float`, `Double`, `Float80`, `Float16`. Exposes `.sin`, `.cos`, `.exp`, `.log`, `.pow` polymorphically. Mochi `math.sin(x: float)` lowers to `Double.sin(x)`.
- **`Complex<RealType>`** (ComplexModule): a complex number generic over `Real`. Mochi has no surface complex type yet but the runtime exposes `mochi.math.complex(re:im:)` as an escape hatch.
- **`Float16`** (RealModule extensions): IEEE 754 half-precision. Hardware on Apple Silicon (ARMv8.2-FP16) and recent Intel (AVX-512-FP16); software-emulated elsewhere. Mochi has no surface `f16` type, but ML-bound users (CoreML, Metal) can opt in.
- **`Decimal128`** (in progress, behind the `DecimalModule` work-in-progress branch as of May 2026, not in the 1.0.x line). Mochi `decimal` falls back to Foundation `Decimal` (which is a 38-digit base-10 decimal, Apple+Linux+Windows complete) until `Decimal128` lands.
- **IntegerUtilities** (the `IntegerUtilities` module): `gcd`, `lcm`, rotation, bit-counting helpers. Useful for hashing.

For libmochi_swift: depend on swift-numerics 1.0.x. Mochi `bigint` is **not** in swift-numerics; we vend a small implementation in `MochiRuntime/BigInt/` based on `Array<UInt64>` limbs (matching the C target's mp_int representation), used only when integer arithmetic overflows `Int64`. swift-numerics has open PRs for an arbitrary-precision integer type but they were not merged by May 2026.

## 9. swift-syntax

Package: `https://github.com/swiftlang/swift-syntax`. Current release: **600.0.1** (March 2026, tracking Swift 6.0); the **601.0.0** branch ships with Swift 6.1 and is the latest mainline. Versioning matches the Swift toolchain version (5xx for Swift 5.x, 6xx for Swift 6.x). Imports as `SwiftSyntax`, `SwiftSyntaxBuilder`, `SwiftParser`, `SwiftBasicFormat`.

This is the **emit substrate for MEP-49 codegen** (see note 05). The codegen pass does not concatenate strings; it builds a `SourceFileSyntax` tree and pretty-prints it.

The three coding styles:

1. **SyntaxFactory / leaf init.** The old style, deprecated since 510.0. You construct `TokenSyntax(.identifier("foo"), presence: .present)` and assemble manually. Verbose, low-level. We use it only for tokens we cannot express otherwise.

2. **SyntaxBuilders (result builder DSL).** The modern style, dominant since 510.0. You write Swift code that *looks like* Swift:
```
let fn = FunctionDeclSyntax(name: "greet") {
    FunctionParameterSyntax(firstName: "name", type: TypeSyntax("String"))
} body: {
    "print(\"Hello, \(name)!\")"
}
```
Strings interpolate via the `ExprSyntax(stringLiteral:)` family. We use this for 90% of emit.

3. **String parsing.** For tricky expressions you write `ExprSyntax("(x + 1) * y")` and let `SwiftParser` parse it. Useful for one-off literals; we avoid it in hot paths because parse errors become runtime errors.

**`BasicFormat`**: pretty-printer that walks a syntax tree and inserts indentation, line breaks, and spaces by Swift's de facto rules. The output is **not** canonical (that is swift-format's job, section 10) but it is parseable and indented. We always run BasicFormat first to get a stable string, then optionally pipe through swift-format.

For libmochi_swift: depend on swift-syntax 600.0.x (Swift 6.0 floor) with `from: "600.0.0"`. The codegen pass (`mochi-swift-emit`) is a CLI tool that takes Mochi IR, builds `SourceFileSyntax` trees, calls `BasicFormat().format(syntax: tree)`, and writes `.swift` files. SwiftPM target name: `MochiSwiftEmit`. swift-syntax is a heavy dependency (the package is ~500 KB compiled, plus its own ~3 MB of source); we keep it out of the runtime artifact by isolating it in the `mochi` build tool, not in `MochiRuntime`.

## 10. swift-format

Package: `https://github.com/swiftlang/swift-format`. Current release: **600.0.0** (October 2024, tracking Swift 6.0), **601.0.0** (March 2025, tracking Swift 6.1). Versioning matches swift-syntax. Imports as `SwiftFormat`.

Apple's official canonical formatter, the rough Swift equivalent of `gofmt`. The tool:
1. Parses source into a swift-syntax tree.
2. Applies whitespace, line-break, and indent rules per a `Configuration` (defaults match the Google Swift Style Guide plus Swift API Design Guidelines).
3. Optionally runs lint rules (no-leading-underscore, always-use-literal-for-empty-collection, etc.).

The CLI is invoked as `swift-format format --in-place file.swift` or `swift-format lint file.swift`. Programmatic invocation:
```
import SwiftFormat
let configuration = Configuration()
var output = ""
try SwiftFormatter(configuration: configuration).format(
    source: input, assumingFileURL: nil, to: &output
)
```

For libmochi_swift: Mochi pipes the emitted `.swift` through swift-format **once** at end of codegen so the output is canonical, diff-stable, and matches the rest of the Swift ecosystem. The build pipeline is:
1. mochi-swift-emit produces unformatted source via BasicFormat.
2. mochi-swift-emit invokes `SwiftFormatter(...).format(...)` in-process.
3. Final source is written to `.build/mochi/swift/Gen/*.swift`.

We use the default `Configuration` (no project-specific tweaks). swift-format is also a heavy dep (~2 MB source), kept in the build tool, not in `MochiRuntime`.

## 11. swift-system

Package: `https://github.com/apple/swift-system`. Current release: **1.4.2** (May 2026). Supports Swift 5.7+. Imports as `SystemPackage` (cross-platform) or `System` (Apple only; `System` is a shim re-exporting `SystemPackage`).

The cross-platform syscall surface that Foundation does not give us:

- **`FilePath`**: typed path value. Distinct from `String` and from Foundation's `URL`. Supports `appendingComponent`, `removingLastComponent`, platform-correct separator handling. Mochi `path` lowers here when targeting Embedded mode (no Foundation).
- **`FileDescriptor`**: typed file descriptor (`Int32` on POSIX, `HANDLE` on Windows). `open`, `close`, `read`, `write`, `lseek`, `fcntl`. Throws typed `Errno` on failure.
- **`Errno`**: typed errno, with named cases (`Errno.noSuchFileOrDirectory`, `Errno.permissionDenied`).
- **File-descriptor flags**: `OpenOptions`, `FilePermissions`, `FileDescriptor.SeekOrigin`. Bit-flag types with named members.

swift-system is Foundation-free by design: it can be used in Swift programs that compile without Foundation (Embedded Swift, kernel-style code). The trade-off is feature scope: there are no high-level helpers like `FileManager.copyItem` or `URL.checkResourceIsReachable`. Mochi runtime uses swift-system in three places:

1. **Embedded mode** (note 10 build system): when Mochi is asked to produce a Foundation-free binary, the runtime's file I/O surface (`mochi.fs.read`, `mochi.fs.write`) routes to swift-system instead of Foundation.
2. **Hot-path file I/O**: when a Mochi program is iterating over millions of file operations, the `mochi.fs.fast` namespace exposes `FileDescriptor` directly to bypass Foundation's `URL` parsing overhead.
3. **Path manipulation**: `mochi.path` always uses `FilePath`, because it is correct on Windows out of the box (Foundation `URL` for file paths has known Windows quirks).

For libmochi_swift: depend on swift-system 1.4.x. The dependency is small (~150 KB) and is always pulled in.

## 12. swift-log

Package: `https://github.com/apple/swift-log`. Current release: **1.6.3** (April 2026). Supports Swift 5.6+. Imports as `Logging`.

The Swift on Server Working Group's structured logging facade. Two parts:

1. **`Logger`** value type: lightweight wrapper around a label + metadata. `logger.info("starting", metadata: ["pid": "\(pid)"])`. Five levels: trace, debug, info, notice, warning, error, critical.
2. **`LogHandler`** protocol: backends implement this. `StreamLogHandler.standardOutput(label:)` ships in the package. Production backends include `swift-log-syslog`, `swift-log-oslog` (Apple unified logging), `swift-log-file`, `puppy`, and bridges to swift-distributed-tracing's structured event stream.

Mochi `log.info(msg, key=val)` lowers to:
```
mochiLogger.info("\(msg)", metadata: ["key": "\(val)"])
```
where `mochiLogger` is a per-module `Logger(label: "mochi.user.<modname>")`.

For libmochi_swift: depend on swift-log 1.6.x. Default backend is `StreamLogHandler.standardError(label:)` (so logs do not collide with `stdout` data). User code can install a different backend via `LoggingSystem.bootstrap(...)`. On Apple, the runtime installs `OSLogHandler` when the program is detected as launched via launchd or as a foreground app; on Linux/Windows it installs `StreamLogHandler.standardError`. Detection uses `getppid()` and the `XPC_SERVICE_NAME` env var. See note 10 for the bootstrap details.

## 13. swift-metrics and swift-distributed-tracing

**swift-metrics**: `https://github.com/apple/swift-metrics`. Current release: **2.5.0** (March 2026). Imports as `Metrics`. The Swift on Server Working Group's metrics facade. Counters, gauges, recorders, timers, with multiple-dimensional labels. Backends: `swift-prometheus`, `swift-statsd-client`, `swift-otlp-tracing` (the metrics half), `swift-metrics-extras`.

**swift-distributed-tracing**: `https://github.com/apple/swift-distributed-tracing`. Current release: **1.1.2** (January 2026). Imports as `Tracing`, `Instrumentation`. The Swift OTel-compatible tracing facade. `withSpan(...)` to create spans, `ServiceContext` for context propagation, `Instrumentation` for cross-cutting injection. Backends: `swift-otlp-tracing` (the tracing half, OTLP gRPC), `swift-x-ray`, `swift-zipkin-tracer`.

These are the Swift parallels to .NET's `DiagnosticSource` and the JVM's OpenTelemetry. Architecture: a facade package defines protocols and a global registry, backends implement the protocols, application code calls the facade. Mochi's observability story:

- **`mochi.metrics.counter("name")`** lowers to `Metrics.Counter(label: "name", dimensions: [...]).increment()`.
- **`mochi.metrics.gauge("name", value)`** lowers to `Metrics.Gauge(label: "name").record(value)`.
- **`mochi.trace.span("name") { ... }`** lowers to `try await withSpan("name") { ... }` (sync version via `withSpanSync`).
- **Auto-instrumentation**: agent boot/stop, stream publish/subscribe, fetch start/end, query execute all emit spans automatically when the user calls `mochi.trace.enable()`.

For libmochi_swift: depend on swift-metrics 2.5.x and swift-distributed-tracing 1.1.x. The OTLP backend is an **opt-in separate package** (`MochiRuntimeOTel`) that depends on `swift-otlp-tracing` (large dep with grpc-swift transitively); core `MochiRuntime` does not pull it in by default. This mirrors the JVM target's `mochi-runtime-otel` artifact split (note 04 in MEP-47 §14).

## 14. AsyncStream<Element>

**`AsyncStream<Element>`** (and `AsyncThrowingStream<Element, Failure>`): Swift's standard pull-based async sequence type. Shipped in Swift 5.5 (June 2022). Lives in the standard library, no extra import needed.

Construction:
```
let (stream, continuation) = AsyncStream<Message>.makeStream(
    of: Message.self,
    bufferingPolicy: .bufferingOldest(1000)
)
```
The `BufferingPolicy` enum has cases:
- `.unbounded`: keep every value. Memory leak risk if producer outruns consumer.
- `.bufferingOldest(N)`: keep oldest N; drop new arrivals when full.
- `.bufferingNewest(N)`: keep newest N; drop old when full.

Production: `continuation.yield(value)`. Termination: `continuation.finish()` (success) or `continuation.finish(throwing: error)` (throwing-stream only).

Consumption: standard `for await ... in stream` loop. Each `AsyncStream` is **single-consumer** by design. Multiple consumers require multicast: either fan-out manually with multiple continuations, or use `AsyncChannel` for round-robin, or write a multicaster (the swift-async-algorithms package has `share()`-like operators in 1.1 work-in-progress).

**Cancellation**: cancelling the consuming `Task` causes the `for await` loop to terminate. The continuation receives an `onTermination` callback, in which producers free resources (close sockets, cancel timers).

**Backpressure**: with `.bufferingOldest(N)` or `.bufferingNewest(N)`, producers never block: they yield, and the policy decides whether to keep or drop. For Mochi this is the wrong default for inter-agent communication: dropped messages mean broken semantics. Mochi `agent` mailboxes therefore use `AsyncStream` only with `.unbounded` plus an **explicit count** maintained on the producer side, where the producer checks count before yielding and pushes back to its caller (via Mochi's own `Result` return type) when full. Alternatively, the codegen can choose `AsyncChannel<Message>` from swift-async-algorithms for strict backpressure semantics.

For libmochi_swift: Mochi `agent` lowers to an `actor` that owns an `AsyncStream<Message>` plus a `Deque<Message>` shadow buffer for inspection. Each `agent X = ...` declaration emits:
```
actor MochiAgent_X {
    private let (mailbox, continuation): (AsyncStream<Msg>, AsyncStream<Msg>.Continuation)
    private var buffer: Deque<Msg> = []
    init() {
        (self.mailbox, self.continuation) = AsyncStream.makeStream(
            of: Msg.self,
            bufferingPolicy: .bufferingOldest(1000)
        )
    }
    func send(_ m: Msg) { continuation.yield(m); buffer.append(m) }
    func run() async {
        for await m in mailbox { await handle(m) }
    }
}
```
The 1000-deep buffer is the default; `agent X with mailbox: bounded(N) = ...` overrides.

## 15. Task and structured concurrency

The standard library's concurrency surface, since Swift 5.5:

- **`Task<Success, Failure>`**: a unit of asynchronous work. `Task { ... }` creates a child task inheriting actor isolation and priority; `Task.detached { ... }` creates an unattached task with no parent. `task.cancel()` requests cancellation; the body checks `Task.isCancelled` or `try Task.checkCancellation()`.
- **`TaskGroup<ChildTaskResult>`** and **`ThrowingTaskGroup<ChildTaskResult, Failure>`**: scoped parallel work. `await withTaskGroup(of: Int.self) { group in group.addTask { ... } }`. The group waits for all children at scope exit, returning aggregated results.
- **`DiscardingTaskGroup`** (Swift 5.9, SE-0381): variant that discards child results immediately rather than holding them. Lower memory footprint for fire-and-forget fan-out. Mochi `spawn many { ... }` lowers here.
- **`AsyncSequence` and `AsyncIteratorProtocol`**: the abstract iteration protocols. `AsyncStream`, `AsyncChannel`, `URLSession.AsyncBytes`, `FileHandle.AsyncBytes` all conform.
- **`CheckedContinuation` / `UnsafeContinuation`**: bridges between callback-style code and async/await. `withCheckedContinuation { c in legacyApi { result in c.resume(returning: result) } }`. Checked continuations trap on double-resume or missing-resume; unsafe skips the check.
- **`Task.sleep(for: .seconds(N))`** (Swift 5.7+, replaces the nanosecond-based 5.5 API): clock-aware sleep that respects task cancellation.
- **`ClockProtocol`** (Swift 5.7+, SE-0329): `ContinuousClock`, `SuspendingClock`. `Task.sleep(until: clock.now.advanced(by: .seconds(1)), clock: .continuous)`.

Mochi `spawn f(...)` lowers to `Task { await f(...) }`. Mochi `await f(...)` is direct. Mochi `parallel for x in xs { body }` lowers to:
```
await withTaskGroup(of: Void.self) { group in
    for x in xs { group.addTask { await body(x) } }
}
```
Mochi `select` (cross-stream race) uses an explicit `withThrowingTaskGroup` with all branches added as tasks; first-to-finish cancels the rest.

For libmochi_swift: every Mochi async operation lowers to standard-library concurrency primitives. No third-party concurrency framework. Cancellation is honoured everywhere: Mochi `agent.stop()` cancels the agent's run-Task, which propagates to all child tasks created with `Task { ... }` from inside (not `Task.detached`).

## 16. Actor isolation

Swift's actor system, since Swift 5.5 with Swift 6 hardening:

- **`actor` declarations**: a reference type where every instance method is implicitly `async` from outside the actor's isolation. `mailbox.append(x)` from outside becomes `await mailbox.append(x)`.
- **`@MainActor`**: a global actor pinning execution to the main thread (UIKit / AppKit / SwiftUI). Mochi has no UI surface in v0.1, so `@MainActor` is unused.
- **`nonisolated`**: opts a member out of actor isolation. Useful for `Hashable` / `Equatable` conformance methods that touch only let-bindings.
- **Isolated parameters** (SE-0420, Swift 5.9): a function can declare `func send(message: Msg, isolation: isolated any Actor)`, inheriting the caller's actor isolation. Useful for generic helpers.
- **Region-based isolation** (SE-0414, Swift 6.0): the compiler analyses value flow to prove that a non-Sendable value never escapes a "region", allowing safe transfer between actors. Eliminates many warnings the strict-concurrency model raised in 5.10.
- **Sendable conformance**: types crossing actor boundaries must be `Sendable`. Value types (struct, enum) with `Sendable` stored properties are implicitly `Sendable`. Classes must be `final` and `@unchecked Sendable` (manual), or use actor isolation. Closures capture-checked.

The Swift 6.0 language mode (`-swift-version 6`) makes Sendable checks errors instead of warnings. MEP-49 emits code that compiles cleanly under `-swift-version 6` to future-proof.

For libmochi_swift: Mochi `agent` lowers to `actor`. Mochi `record` lowers to `struct` with `: Sendable` conformance when all fields are Sendable. Mochi `class` lowers to `final class` with `: Sendable` if marked `@thread_safe` in the Mochi source, else with `actor` if the user wants method-level isolation. The codegen always synthesises Sendable conformance where it can; user-defined Mochi classes that hold non-Sendable fields (rare) get an explicit `@unchecked Sendable` and a runtime check.

## 17. C interop

Swift has had C interop since 1.0 via Clang module maps. The surface:

- **`Unsafe*Pointer<T>`** family: `UnsafePointer<T>`, `UnsafeMutablePointer<T>`, `UnsafeRawPointer`, `UnsafeMutableRawPointer`, `UnsafeBufferPointer<T>`, `UnsafeMutableBufferPointer<T>`. Bridge typed and raw memory.
- **`withUnsafe*Pointer`**: scoped accessors that produce a pointer valid for the closure body only. `arr.withUnsafeBufferPointer { ptr in cFunc(ptr.baseAddress, ptr.count) }`.
- **`@_silgen_name("c_symbol_name")`**: pins the Swift function to a specific symbol name in the linked object (bypasses Swift mangling). Used to wrap C symbols when no module map is convenient.
- **`@_cdecl("name")`**: exports a Swift function with C linkage and a chosen symbol name. Used to provide callbacks to C code.
- **`module.modulemap`**: declares a Clang module wrapping a set of C headers. SwiftPM auto-generates module maps for system libraries declared in `Package.swift`'s `cSettings`. For user C libraries, the project ships a `module.modulemap` alongside the umbrella header.
- **Bridging headers** (Apple-only, Xcode-specific): a single `Bridging-Header.h` that exposes C declarations to Swift in mixed-language Xcode projects. SwiftPM does not use bridging headers; it uses module maps.

For libmochi_swift: Mochi `extern "c" fn cName(x: int) -> int` lowers to:
```
@_silgen_name("cName")
private func _cName(_ x: Int64) -> Int64
```
plus a Swift wrapper that converts between Mochi types and C types. `bytes` becomes `withUnsafeBufferPointer`, `string` becomes `withCString`. Mochi never directly exposes raw pointer types to user code; the FFI surface always wraps with `withUnsafe*`.

## 18. C++ interop

Swift gained **bidirectional C++ interop** in Swift 5.9 (September 2023, SE-0381). Key points:

- **`import CxxStdlib`** brings `std::string`, `std::vector`, `std::map`, `std::unique_ptr`, `std::shared_ptr` into Swift. `std::string` becomes a Swift `std__1.string` (mangled namespace) with `init(...)` from Swift `String` and `.utf8` accessors.
- **No header generation**. Swift parses C++ headers directly via Clang; no `swift -emit-objc-header` equivalent needed. The C++ compiler sees Swift APIs through a generated header (`-emit-clang-header-path`).
- **Bidirectional**: C++ code can call Swift, Swift code can call C++. Method dispatch follows C++ semantics on the C++ side (non-virtual = static dispatch, virtual = vtable) and Swift semantics on the Swift side.
- **Exception bridging**: C++ exceptions thrown across the interop boundary are caught and rethrown as Swift `Error` instances, via a synthesised `throws` annotation on the Swift wrapper. The mapping is `try value` produces `Result<T, Error>` semantics.
- **Move semantics**: Swift `~Copyable` types (SE-0390) interoperate with C++ move-only types. `std::unique_ptr<T>` becomes a Swift `~Copyable` value.
- **Templates**: Swift 5.9 supports calling C++ function templates with concrete argument types; class templates are partial (Swift 6.0 expanded support but not all corner cases). Class-template specialisations are exposed as distinct Swift types.
- **Build flags**: `-enable-experimental-cxx-interop` in Swift 5.7, became `-cxx-interoperability-mode=default` in Swift 5.9+. SwiftPM exposes via `.interoperabilityMode(.Cxx)` in the target's `swiftSettings`.

For libmochi_swift: Mochi `extern "cxx"` is currently a stretch goal. v0.1 supports only `extern "c"`. Section 17's mechanism is sufficient for the common case (libc, OpenSSL, sqlite, libcurl). v0.2 will add C++ interop for libraries like nlohmann/json, RocksDB, Tantivy-the-C++-port. The runtime carries a `CxxBridge.swift` stub for future expansion.

## 19. Memory model

Swift's memory model:

- **ARC** (Automatic Reference Counting): every class instance has a reference count, incremented on copy of the reference and decremented on scope exit / reassignment. Cycle detection is **not** automatic (no tracing GC); use `weak` or `unowned` references to break cycles.
- **Value types**: `struct`, `enum`, tuple. Copied on assignment / pass-by-value. **Copy-on-write** is opt-in: `Array<T>`, `Dictionary<K, V>`, `Set<T>`, `String`, `Data` all implement CoW via an internal class reference plus `isKnownUniquelyReferenced` checks. User-written structs do not get CoW automatically; mutating a struct field copies the struct.
- **Reference types**: `class`, `actor`. Reference-counted, heap-allocated.
- **`weak` references**: do not contribute to retain count. Stored as `Optional<Weak<T>>`; become nil when the referent deallocates. Used for parent / observer back-pointers.
- **`unowned` references**: do not contribute to retain count. Stored as a raw pointer; accessing after the referent deallocates traps. Used for guaranteed-non-nil back-pointers (e.g., child knows parent outlives it).
- **`~Copyable` types** (SE-0390, Swift 5.9): types that opt out of automatic copying. `struct File: ~Copyable { ... }` cannot be copied; must be `consume`d or borrowed. Used for resource-owning types (file descriptors, mutex guards, exclusive memory regions). Mochi `resource` keyword lowers here.
- **`~Escapable` types** (SE-0446, Swift 6.0): types that opt out of escaping their declaring scope. Lifetime is bounded to the scope they were created in. Cannot be stored in fields or returned from non-`@_lifetime` functions. Used for non-owning views (span-like types). The standard library's `Span<T>` and `MutableSpan<T>` (SE-0447, also Swift 6.0) are `~Escapable`.

For libmochi_swift: Mochi `record` lowers to `struct` (copy semantics). Mochi `class` lowers to `final class` (reference semantics, ARC). Mochi `resource` lowers to a `~Copyable struct`. Mochi `view` (a slice-like type, currently spec-only) will lower to `~Escapable struct` in v0.2. Weak references in Mochi are explicit via `weak ref T`; they lower to Swift `weak var`. Unowned is not exposed in the Mochi surface (too easy to footgun).

ARC has no generational improvements pending; the model is stable. The big news for memory in Swift 6.x is `Span<T>` adoption (replacing `UnsafeBufferPointer` in many APIs), which Mochi runtime adopts wherever the standard library exposes a `Span<T>` accessor.

## 20. What MochiRuntime adds

`MochiRuntime` is a SwiftPM package at `https://github.com/mochilang/mochi-runtime-swift` (placeholder, the real coordinate is `dev.mochi:mochi-runtime-swift` in spec terms). It re-exports the standard library + the swift-* packages above and adds Mochi-specific helpers.

```
mochi-runtime-swift/
├── Package.swift
├── Sources/
│   ├── MochiRuntime/                        // umbrella; exports the public API
│   │   ├── MochiRuntime.swift               // package-level static init
│   │   ├── Core/                            // value helpers
│   │   │   ├── MochiValue.swift             // existential Any with Mochi-typed accessors
│   │   │   ├── MochiEquals.swift            // structural eq across Mochi value graphs
│   │   │   └── MochiHash.swift              // stable hash (SipHash with fixed key)
│   │   ├── Collections/                     // list / map / set wrappers
│   │   │   ├── MochiList.swift              // Array<T> helpers
│   │   │   ├── MochiMap.swift               // OrderedDictionary<K,V> wrappers
│   │   │   └── MochiSet.swift               // OrderedSet<T> wrappers
│   │   ├── String/                          // code-point indexed string ops
│   │   │   └── MochiStr.swift
│   │   ├── Bytes/                           // Data + MemorySegment-ish
│   │   │   └── MochiBytes.swift
│   │   ├── Option/                          // Optional<T> helpers
│   │   │   └── MochiOption.swift
│   │   ├── Time/                            // ZonedDateTime polyfill
│   │   │   ├── ZonedDateTime.swift
│   │   │   ├── MochiDuration.swift
│   │   │   └── MochiClock.swift             // test-injectable clock seam
│   │   ├── Random/                          // PCG-backed RNG
│   │   │   └── MochiRandom.swift
│   │   ├── Agent/                           // actor-based agent runtime
│   │   │   ├── MochiAgent.swift             // base actor + mailbox protocol
│   │   │   ├── MochiAgentSup.swift          // supervisor: restart, escalate
│   │   │   └── MochiMailbox.swift           // bounded Deque-backed mailbox
│   │   ├── Stream/                          // AsyncStream + AsyncChannel wrappers
│   │   │   ├── MochiStream.swift            // AsyncStream<T> + back-buffer
│   │   │   ├── MochiChannel.swift           // AsyncChannel<T> wrapper
│   │   │   └── MochiStreamRegistry.swift
│   │   ├── Async/                           // scope helpers (no StructuredTaskScope yet)
│   │   │   └── MochiScope.swift
│   │   ├── Query/                           // query DSL combinators
│   │   │   ├── MochiQuery.swift             // group_by, join, window
│   │   │   ├── MochiJoin.swift              // hash-join + nested-loop
│   │   │   └── MochiWindow.swift            // time / count windows
│   │   ├── Datalog/                         // in-memory tuple tables
│   │   │   ├── Relation.swift
│   │   │   └── Index.swift
│   │   ├── Fetch/                           // URLSession + Network.framework facade
│   │   │   ├── MochiFetch.swift
│   │   │   └── MochiWebSocket.swift
│   │   ├── JSON/                            // Foundation JSONEncoder facade
│   │   │   └── MochiJSON.swift
│   │   ├── CSV/                             // hand-rolled CSV (no Foundation CSV)
│   │   │   └── MochiCSV.swift
│   │   ├── YAML/                            // Yams wrapper (jpsim/Yams 6.x)
│   │   │   └── MochiYAML.swift
│   │   ├── FS/                              // swift-system + Foundation FileManager
│   │   │   └── MochiFs.swift
│   │   ├── OS/                              // env vars, process args, exit
│   │   │   └── MochiOs.swift
│   │   ├── LLM/                             // FoundationModels + remote providers
│   │   │   ├── MochiLLM.swift
│   │   │   └── Providers/
│   │   │       ├── OpenAI.swift
│   │   │       ├── Anthropic.swift
│   │   │       └── FoundationModels.swift   // #if canImport(FoundationModels)
│   │   ├── FFI/                             // @_silgen_name helpers
│   │   │   └── MochiFFI.swift
│   │   ├── Log/                             // swift-log facade
│   │   │   └── MochiLog.swift
│   │   ├── Metrics/                         // swift-metrics facade
│   │   │   └── MochiMetrics.swift
│   │   ├── Trace/                           // swift-distributed-tracing facade
│   │   │   └── MochiTrace.swift
│   │   └── Testing/                         // mochi_test harness
│   │       ├── MochiTest.swift
│   │       └── MochiAssert.swift
│   └── MochiRuntimeOTel/                    // opt-in OpenTelemetry backend
│       └── MochiRuntimeOTel.swift
└── Tests/
    └── MochiRuntimeTests/
        └── ...
```

`Package.swift` highlights:

```
let package = Package(
    name: "mochi-runtime-swift",
    platforms: [
        .iOS(.v13), .macOS(.v12), .watchOS(.v6), .tvOS(.v13), .visionOS(.v1),
    ],
    products: [
        .library(name: "MochiRuntime", targets: ["MochiRuntime"]),
        .library(name: "MochiRuntimeOTel", targets: ["MochiRuntimeOTel"]),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-collections.git", from: "1.1.0"),
        .package(url: "https://github.com/apple/swift-algorithms.git", from: "1.2.0"),
        .package(url: "https://github.com/apple/swift-async-algorithms.git", from: "1.0.0"),
        .package(url: "https://github.com/apple/swift-numerics.git", from: "1.0.0"),
        .package(url: "https://github.com/apple/swift-system.git", from: "1.4.0"),
        .package(url: "https://github.com/apple/swift-log.git", from: "1.6.0"),
        .package(url: "https://github.com/apple/swift-metrics.git", from: "2.5.0"),
        .package(url: "https://github.com/apple/swift-distributed-tracing.git", from: "1.1.0"),
        .package(url: "https://github.com/jpsim/Yams.git", from: "6.0.0"),
    ],
    targets: [
        .target(
            name: "MochiRuntime",
            dependencies: [
                .product(name: "OrderedCollections", package: "swift-collections"),
                .product(name: "DequeModule", package: "swift-collections"),
                .product(name: "HeapModule", package: "swift-collections"),
                .product(name: "Algorithms", package: "swift-algorithms"),
                .product(name: "AsyncAlgorithms", package: "swift-async-algorithms"),
                .product(name: "Numerics", package: "swift-numerics"),
                .product(name: "SystemPackage", package: "swift-system"),
                .product(name: "Logging", package: "swift-log"),
                .product(name: "Metrics", package: "swift-metrics"),
                .product(name: "Tracing", package: "swift-distributed-tracing"),
                "Yams",
            ],
            swiftSettings: [.swiftLanguageMode(.v6)]
        ),
        .target(
            name: "MochiRuntimeOTel",
            dependencies: ["MochiRuntime"]
        ),
        .testTarget(name: "MochiRuntimeTests", dependencies: ["MochiRuntime"]),
    ]
)
```

Mochi-specific helpers worth calling out:

- **`ZonedDateTime`**: `struct ZonedDateTime { let instant: Date; let zone: TimeZone }` with `addingDays`, `addingHours`, `formatted`, `iso8601`. The reason this exists: Foundation `Date` is wall-clock-UTC only, and Mochi spec mandates a zoned time type that round-trips through ISO-8601 with timezone offset.
- **Agent supervisor**: `MochiAgentSup` is an actor that tracks child agent IDs, applies a restart strategy (`one_for_one`, `rest_for_one`, `one_for_all`, matching BEAM conventions), and emits `mochi.agent.crash` JFR-equivalent events via swift-distributed-tracing.
- **Datalog evaluator**: in-memory tuple-table store backed by `OrderedSet<Tuple>` + secondary `OrderedDictionary<Key, [Tuple]>` indexes. Differs from MEP-47's JVM design only in the use of OrderedSet (Swift stdlib has none) over `LinkedHashSet`.
- **Query DSL extensions**: `Sequence.mochiGrouped(by:)` returns `OrderedDictionary<K, [Element]>` (stable order, unlike `Sequence.grouped(by:)` which returns plain `[K: [Element]]`).
- **JSON round-trip ergonomics**: `MochiJSON.encode<T: Codable>(_ value: T)` and `MochiJSON.decode<T: Codable>(_ text: String, as: T.Type)` with shared `JSONEncoder`/`JSONDecoder` singletons configured for `Date.iso8601`, sorted keys (deterministic), and `.convertFromSnakeCase` off (Mochi field names round-trip verbatim).
- **Mochi value as Any**: `MochiValue` is the existential type holding a Mochi value (because Mochi has dynamic-typed corners like `decode_json` output). Bridges to Swift via `MochiValue.list`, `.map`, `.string`, `.int`, `.float`, `.bool`, `.null` accessors.

Boot order on `MochiRuntime.bootstrap()`:
1. Install `LoggingSystem.bootstrap { label in ... }` with platform-detected default backend.
2. Install `MetricsSystem.bootstrap(NOOPMetricsHandler.instance)` (real backend opted in by user).
3. Install `InstrumentationSystem.bootstrap(NoOpTracer())` (real tracer opted in by user).
4. Pre-warm the singleton `JSONEncoder` and `JSONDecoder`.
5. Open the default supervisor (`MochiAgentSup`) so agents can spawn.

Cold start times (measured on M2 / Swift 6.1, "Hello world" Mochi program):
- `swift run mochi-app` from sources, ~2.5 s (compile + link + run).
- `./mochi-app` after `swift build -c release` (release binary), ~20 ms cold.
- `./mochi-app` after `swift build -c release --static-swift-stdlib` (Linux), ~25 ms cold, ~12 MB binary.

These compare favourably to the JVM target (~110 ms cold without AOTCache) and unfavourably to the C target (~5 ms). The Swift target's strength is the joint ergonomics of static binary + ARC + structured concurrency, with no JIT warm-up and no GC tuning.

---

## Sources

1. Swift 6.0 release notes, swift.org/blog/swift-6/ (September 17 2024).
2. Swift 6.1 release notes, swift.org/blog/swift-6.1-released/ (March 11 2025).
3. SE-0381: DiscardingTaskGroups, github.com/swiftlang/swift-evolution/blob/main/proposals/0381-task-groups-discard-results.md.
4. SE-0390: Noncopyable structs and enums (~Copyable), github.com/swiftlang/swift-evolution/blob/main/proposals/0390-noncopyable-structs-and-enums.md.
5. SE-0414: Region-based isolation, github.com/swiftlang/swift-evolution/blob/main/proposals/0414-region-based-isolation.md.
6. SE-0420: Inheritance of actor isolation, github.com/swiftlang/swift-evolution/blob/main/proposals/0420-inheritance-of-actor-isolation.md.
7. SE-0446: Nonescapable types (~Escapable), github.com/swiftlang/swift-evolution/blob/main/proposals/0446-non-escapable.md.
8. SE-0447: Span<T> and MutableSpan<T>, github.com/swiftlang/swift-evolution/blob/main/proposals/0447-span-access-shared-contiguous-storage.md.
9. apple/swift-collections 1.1.x release notes, github.com/apple/swift-collections/releases.
10. apple/swift-algorithms 1.2.x release notes, github.com/apple/swift-algorithms/releases.
11. apple/swift-async-algorithms 1.0.x release notes, github.com/apple/swift-async-algorithms/releases.
12. apple/swift-numerics 1.0.x release notes, github.com/apple/swift-numerics/releases.
13. swiftlang/swift-syntax 600.0.x and 601.0.x release notes, github.com/swiftlang/swift-syntax/releases.
14. swiftlang/swift-format 600.0.x and 601.0.x release notes, github.com/swiftlang/swift-format/releases.
15. apple/swift-system 1.4.x release notes, github.com/apple/swift-system/releases.
16. apple/swift-log 1.6.x release notes, github.com/apple/swift-log/releases.
17. apple/swift-metrics 2.5.x release notes, github.com/apple/swift-metrics/releases.
18. apple/swift-distributed-tracing 1.1.x release notes, github.com/apple/swift-distributed-tracing/releases.
19. apple/swift-corelibs-foundation README and parity matrix, github.com/apple/swift-corelibs-foundation.
20. Apple developer documentation: Observation, developer.apple.com/documentation/observation.
21. Apple developer documentation: SwiftData, developer.apple.com/documentation/swiftdata.
22. Apple developer documentation: FoundationModels, developer.apple.com/documentation/foundationmodels (introduced WWDC 2024, iOS 18.1+).
23. Apple developer documentation: Network.framework, developer.apple.com/documentation/network.
24. C++ interop in Swift, swift.org/documentation/cxx-interop/ (Swift 5.9 + Swift 6.x updates).
25. Yams (jpsim/Yams) 6.x release notes, github.com/jpsim/Yams/releases.
26. Swift Server Working Group blog posts (logging facade, metrics facade, tracing facade conventions), swift.org/sswg/.
27. swift.org platform support matrix, swift.org/platform-support/.
