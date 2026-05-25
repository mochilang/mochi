# MEP-49 research note 11, Testing strategy and gates for MEP-49

Author: research pass for MEP-49 (Mochi to Swift transpiler).
Date: 2026-05-23 (GMT+7).

This note specifies the test-as-spec gates that govern each MEP-49
phase. It mirrors the MEP-45 (C), MEP-46 (BEAM), MEP-47 (JVM), and
MEP-48 (.NET) gate patterns: one deterministic Go test per phase,
plus a differential gate against vm3, plus a swiftc clean gate, plus
a platform matrix.

## 1. Gate philosophy

A **gate** is a Go test (`Test...` in the Mochi repo, under
`tests/transpiler3/swift/`) that:

1. Iterates fixtures in `tests/transpiler3/swift/...`.
2. Compiles each fixture with the MEP-49 toolchain
   (`mochi build --target=swift`).
3. Runs the resulting binary (executable for executable targets,
   `swift test` for library targets).
4. Compares actual stdout against `<fixture>.out`.
5. Compares against the vm3 oracle (running the same Mochi source on
   the reference VM).

A gate **fails closed**: any fixture mismatch fails the test. Phases
land only when every fixture in the phase's matrix is green.

The Swift-specific additions:
- **swiftc clean gate.** Every emitted `.swift` file must compile
  under `swiftc -strict-concurrency=complete -warnings-as-errors`
  with Swift 6 language mode. A Mochi program producing
  uncompilable Swift is a transpiler bug, not a runtime issue.
- **swift-format gate.** Mochi output, after `swift-format format
  --in-place`, must be a fixed point: running swift-format again
  produces no diff.
- **App Store gate** (Apple platforms). The emitted .ipa or .app
  must pass `xcrun altool --validate-app` against App Store
  Connect. This catches forbidden API usage and missing entitlements.

## 2. Phase gates (planned matrix)

Following [[01-language-surface]] and the MEP body's phase plan:

| Phase | Gate                       | Fixture count target | Surface covered                                  |
|-------|----------------------------|----------------------|--------------------------------------------------|
| 1     | `TestPhase1Hello`          | 5                    | hello world, print, basic let, basic int         |
| 2     | `TestPhase2Scalars`        | 20                   | int/float/bool/string ops, comparisons           |
| 3.1   | `TestPhase3Lists`          | 25                   | list literal, index, len, for-each               |
| 3.2   | `TestPhase3Maps`           | 25                   | map literal, index, len, keys, values, has       |
| 3.3   | `TestPhase3Sets`           | 20                   | set literal, add, has, len                       |
| 3.4   | `TestPhase3ListOfRecord`   | 20                   | list[record], comprehensions over records        |
| 4     | `TestPhase4Records`        | 25                   | records, methods, equality, `with`               |
| 5     | `TestPhase5Sums`           | 25                   | sum types, pattern matching, exhaustiveness      |
| 6     | `TestPhase6Funs`           | 25                   | closures, higher-order, @Sendable                |
| 7     | `TestPhase7Query`          | 30                   | from/where/select, group_by, order_by, joins     |
| 8     | `TestPhase8Datalog`        | 20                   | facts, rules, recursion                          |
| 9     | `TestPhase9Agents`         | 25                   | actor definitions, spawn, call, cast             |
| 10    | `TestPhase10Streams`       | 20                   | streams, AsyncSequence, await foreach            |
| 11    | `TestPhase11Async`         | 15                   | async colouring, typed throws                    |
| 12    | `TestPhase12FFI`           | 25                   | C interop via module maps, @_silgen_name         |
| 13    | `TestPhase13LLM`           | 10                   | generate (FoundationModels on Apple, mock elsewhere) |
| 14    | `TestPhase14Fetch`         | 10                   | fetch (URLSession, against local test server)    |
| 15    | `TestPhase15iOSAppBundle`  | 20                   | .ipa bundle, codesign, xcodebuild archive        |
| 16    | `TestPhase16Reproducible`  | 10                   | reproducible build (byte-identical .o / .ipa)    |
| 17    | `TestPhase17StaticLinux`   | 20                   | static Linux SDK, single binary                  |
| 18    | `TestPhase18AppStoreValidate` | 15                | App Store / Mac App Store validation             |

Total target by Phase 18: ~400 fixtures, all green on Swift 6.0 +
Swift 6.1 on Linux x86-64 / arm64, macOS arm64, Windows x86-64, plus
iOS Simulator and physical iPhone for Phases 15 and 18.

## 3. Differential testing vs vm3

The vm3 oracle is **the reference Mochi interpreter**. For each
fixture:

1. Run vm3: `mochi run <fixture>.mochi > <fixture>.vm3.out`.
2. Run Swift build:
   `mochi build --target=swift-macos-arm64 <fixture>.mochi -o /tmp/f &&
    /tmp/f > <fixture>.swift.out`.
3. `diff <fixture>.vm3.out <fixture>.swift.out` must be empty.

The fixture's checked-in `<fixture>.out` file is the **vm3 oracle
output**; CI verifies vm3 produces it (catching vm3 regressions)
and then verifies the Swift target matches.

For non-deterministic fixtures (random, time, streams, agents,
LLM), the fixture is excluded from differential testing and runs
only the static check (stdout vs checked-in `.out`).

The MEP-45 (C), MEP-46 (BEAM), MEP-47 (JVM), and MEP-48 (.NET)
targets share the fixture pool. Per-target overrides via
`*.swift.skip` files for fixtures that exercise target-specific
behavior we choose to exclude.

## 4. swiftc clean gate

`TestSwiftcClean`:

1. Build all fixtures with `mochi build --target=swift-source`.
2. Compile with `swiftc -strict-concurrency=complete -swift-version 6
   -warnings-as-errors`.
3. Verify zero warnings on Mochi-generated code.

Warning categories that must be zero:
- Unused variable (`-Wunused-variable`).
- Implicit conversions.
- Sendable violations under strict concurrency.
- Deprecated API uses.
- Non-exhaustive switch (Swift catches automatically).
- Optional force-unwrap in shipped code (codegen never emits `!`).

Vendor code in `Sources/MochiRuntime/` may have its own warnings
(suppressed via `// swift-format-ignore` per file with a tracked
entry in `SUPPRESSIONS.md` explaining why).

## 5. swift-format fixed-point gate

`TestSwiftFormatFixedPoint`:

1. For each emitted .swift file: run `swift-format format
   --in-place`.
2. Run `swift-format format --in-place` again.
3. Diff must be empty.

Swift-format runs once during codegen. The gate verifies idempotence:
a non-fixed-point output means a Mochi codegen bug (e.g., emitting
trailing whitespace that format strips, then re-emits on the next
codegen).

## 6. Swift version matrix

| Swift version | Released | Floor / ceiling      | Required CI status |
|---------------|----------|----------------------|--------------------|
| Swift 6.0     | 2024-09-17 | Floor (Xcode 16)   | required           |
| Swift 6.1     | 2025-03-27 | Ceiling (Xcode 16.3) | required         |
| Swift 6.2     | 2025-09-15 | Advisory (Xcode 17)| warning-only       |
| Swift 5.10    | 2024-03-05 | not supported (pre-6.0 lang mode) | rejected |

Every gate runs on `6.0` and `6.1` in parallel. A phase lands only
when both are green. The `6.2` matrix runs advisory; a regression
triggers a warning, not a block.

Matrix implementation:

```yaml
strategy:
  matrix:
    swift: [6.0, 6.1]
    os: [macos-15, ubuntu-24.04, windows-2025]
    arch: [arm64, x86_64]
    exclude:
      - os: macos-15
        arch: x86_64    # macOS-15 runners are arm64 only
      - os: windows-2025
        arch: arm64     # Windows Swift toolchain is x86_64 only as of 6.1
```

## 7. Apple platform packaging gate

`TestPhase15iOSAppBundle`:

1. For each phase-15-eligible fixture (declares `@app` in Mochi):
   - Emit Swift sources plus Xcode project.
   - `xcodebuild -scheme MochiApp -configuration Release -destination
     "generic/platform=iOS" archive -archivePath /tmp/MochiApp.xcarchive`.
   - `xcodebuild -exportArchive -archivePath /tmp/MochiApp.xcarchive
     -exportOptionsPlist export.plist -exportPath /tmp/`.
   - Verify the resulting `.ipa` exists, is non-empty, and contains
     a signed Mach-O binary.
2. For macOS apps:
   - Build with `xcodebuild ... -destination "platform=macOS"`.
   - Verify `.app` bundle structure.
   - `codesign --verify --verbose /tmp/MochiApp.app` succeeds.

This gate requires Apple Silicon CI runner with Xcode 16.3+ and a
valid Developer ID certificate in the keychain (for codesign).

## 8. App Store validation gate

`TestPhase18AppStoreValidate`:

1. For each phase-18-eligible fixture:
   - Build and sign as Phase 15.
   - `xcrun altool --validate-app -f /tmp/MochiApp.ipa
     --type ios --apiKey <key> --apiIssuer <issuer>`.
   - Exit code 0 means App Store validation passed.

Validation catches:
- Missing required Info.plist keys.
- Forbidden API usage (private SPI, `UIWebView`).
- Asset catalog issues.
- Missing entitlements.
- Architecture mismatches.

Requires App Store Connect API key (stored as CI secret). Runs on
PR only, not on every commit.

## 9. Static Linux SDK gate

`TestPhase17StaticLinux`:

1. For each phase-17-eligible fixture:
   - `swift build --swift-sdk x86_64-swift-linux-musl --configuration
     release`.
   - Verify the produced binary is statically linked: `ldd <binary>`
     reports "not a dynamic executable".
   - Run the binary, compare stdout against vm3.
2. Repeat for `aarch64-swift-linux-musl`.

This gate validates the single-binary Linux story. Requires the
Static Linux SDK installed (`swift sdk install
https://download.swift.org/swift-6.0-RELEASE/static-sdk/...`).

## 10. Reproducible build gate

`TestPhase16Reproducible`:

1. For each fixture:
   - Build with `mochi build --target=swift --deterministic`.
   - Record the binary's SHA-256.
   - Build again on a different machine (or with a different temp
     directory).
   - Verify the SHA-256 matches.

Reproducibility requires:
- Pinned Swift toolchain (`.swift-version` file).
- `swift build` with `-Xswiftc -enable-deterministic-output`.
- A canonical `Package.resolved` checked into the fixture.
- swift-format applied deterministically.
- No `__DATE__`/`__TIME__`/`__FILE__` macros in emitted code.

## 11. Swift test gate

For Mochi `test` blocks, the codegen emits XCTest-compatible
methods or Swift Testing (the new framework introduced in Swift 6.0):

```swift
import Testing

@Test func phase1Hello() async throws {
    let result = await myFunction()
    #expect(result == "hello")
}
```

The gate runs `swift test` and verifies all tests pass.

XCTest is the legacy framework; Swift Testing is the new one. Mochi
defaults to Swift Testing for new projects and supports XCTest for
backward compatibility.

## 12. Cross-target differential gate

Beyond vm3-equality, the five backends (C, BEAM, JVM, .NET, Swift)
all produce byte-equal stdout on every fixture in their shared
matrix. The cross-target gate:

```
TestCrossTargetDifferential
```

1. For each fixture:
   - Run on vm3, C, BEAM, JVM, .NET, Swift (whichever are eligible).
   - Verify every pair produces byte-equal stdout.
2. Report any divergence as a "target divergence" error.

A divergence is *always* a bug in at least one target (assuming the
fixture is deterministic).

## 13. Memory and resource gates

`TestPhase18MemoryStable`:

For streams and agent phases, a long-running fixture (e.g., 1M
agent messages) must not leak heap. The gate:

1. Run the fixture for N seconds.
2. Capture peak RSS via `/usr/bin/time -v` (Linux) or
   `dtrace -n 'syscall::mmap*:entry' ... ` (macOS) or
   `Get-Process | Select-Object WorkingSet64` (Windows).
3. Verify the steady-state heap is bounded (no monotonic growth).

For actor-heavy fixtures, also capture Task counts via
`Task.localCounter` or runtime introspection.

## 14. Security gates

Per the threat model and memory-safety spec:

- **TLS pinning gate.** The Mochi `fetch` runtime, when configured
  with a pinned certificate, must reject mismatched cert chains.
  The gate uses a local test server with a known-bad cert and
  verifies URLSession rejects.
- **FFI sandbox gate.** Mochi C calls require a manifest entry. The
  gate verifies that a call not in the manifest is rejected at
  compile time (Mochi-level check) and at runtime (when reflection
  bypasses).
- **No `unsafe` code from user surface.** Mochi codegen never emits
  `unsafeBitCast`, `Unsafe*Pointer.assumingMemoryBound`, or similar
  in user-facing code. The runtime library may use these
  (`MochiRuntime.Unsafe`) but contains them.
- **Sandbox entitlements.** Mac App Store binaries require sandbox
  entitlements. Mochi emits a default `entitlements.plist` with
  minimal entitlements (no network unless `@network` declared, no
  filesystem unless `@filesystem` declared).

## 15. Performance gates

Soft gates (regression-warning, not block):

- Hello-world AOT cold-start: ≤ 50ms on macOS arm64, ≤ 100ms on
  Linux arm64 static.
- Hello-world binary size: ≤ 800 KB macOS arm64, ≤ 12 MB Linux
  static, ≤ 4 MB Windows.
- iOS app launch (cold): ≤ 200ms on iPhone 15 Pro.
- 1M-element list iteration: ≤ 1.5x vm3.
- 1M-element map insert: ≤ 1.5x vm3.
- Actor message dispatch: ≤ 400ns.

Regressions of >10% fail the soft gate with a warning; >25% fail
hard.

## 16. Embedded Swift gate (v2, deferred)

Reserved for MEP-49 v2 (when Embedded Swift is unlocked):

`TestPhase19Embedded`:

1. For each Embedded-eligible fixture:
   - Build with `--embedded` flag.
   - Verify the resulting binary uses no String, no protocol
     existentials, no Foundation.
   - Flash to a target microcontroller (RP2040 / ESP32-S3 in CI)
     and verify execution.

Not in v1 scope.

## 17. SwiftWasm gate (v2, deferred)

Also reserved:

`TestPhase20SwiftWasm`:

1. Build with `--target=wasm32-unknown-wasi`.
2. Verify the WebAssembly module loads in wasmtime / Node.js.
3. Compare stdout against vm3.

Not in v1 scope.

## 18. Phase gate summary

For Phase N to land:

1. `TestPhaseN` passes on Swift 6.0 and 6.1, Linux x64/arm64, macOS
   arm64, Windows x64, plus iOS Simulator for Phase 15+.
2. `TestSwiftcClean` passes on the Phase N fixtures.
3. `TestSwiftFormatFixedPoint` passes.
4. Cross-target gate green where applicable.
5. App Store validation gate green for Phase 15 fixtures.
6. Static Linux SDK gate green for Phase 17 fixtures.
7. The phase commit includes:
   - The new gate test under `tests/transpiler3/swift/`.
   - The new fixtures under
     `tests/transpiler3/swift/fixtures/phase<NN>/`.
   - The phase entry in the MEP-49 progress log.

A phase that misses any of these is "in-progress", not "landed".

## 19. Out of scope for the testing notes

- Property-based testing (SwiftCheck): documented as v2 follow-up.
- Fuzzing (libFuzzer via Swift, swift-syntax fuzzer): future work.
- Mutation testing (Stryker for Swift): future work.
- UI testing (XCUITest): future work, not a Mochi codegen concern.

These are documented in [[12-risks-and-alternatives]].

Cross-references: [[01-language-surface]] for the phase plan,
[[06-type-lowering]] for the type regression matrix,
[[09-agent-streams]] for the actor/stream test patterns,
[[10-build-system]] for the build driver under test.
