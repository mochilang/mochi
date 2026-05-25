---
name: 5600-research-12-risks-and-alternatives
description: "MEP-45 research note 12. Risks (semantic, build, supply chain, performance, ergonomic), explicit alternatives considered (LLVM IR, WASM, Rust, JIT, C++), and the kill switches that demote the transpiler back to optional."
---

# MEP-45 research note 12, Risks and alternatives

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

This note is the "what could go wrong, and what would we do instead"
discussion that PEP-style proposals are expected to carry.

## 1. Risks

### 1.1 Semantic divergence

Risk: the C target diverges from the VM on edge cases (integer
overflow, float NaN handling, string comparison, map iteration
ordering, GC observable behaviour).

Mitigation: byte-equal differential testing against the VM (note 11
§1) is the master gate. Every fixture in the corpus must produce
identical stdout. Edge cases that escape (e.g. allocations observable
through `mochi_runtime_stats`) are documented as non-portable.

Residual risk: a divergence in a corner case not in the fixture
corpus. The MEP body proposes a *fuzz-and-diff* harness that
generates random valid programs and runs both backends.

### 1.2 Build environment fragility

Risk: a user's `cc` is too old, missing C23 features, or producing
incompatible output (e.g., a custom MSVC with incomplete attributes).

Mitigation: the driver bundles `zig cc` for cross-compilation and
defaults to it on every target. The user's native `cc` is the *opt-in*
path. zig is small (~80 MB) and MIT-licensed; bundling is sustainable.

Residual risk: zig itself ships with bugs or pace changes; the driver
pins a known-good zig version per Mochi release.

### 1.3 Supply chain

Risk: the runtime depends on third-party libraries (BDWGC, mimalloc,
minicoro, yyjson, libfyaml, libcurl, utf8proc, simdutf, cwisstable),
each with its own security history and release cadence.

Mitigation:
- Vendor every dep at a pinned commit. The repo's `runtime/deps/`
  has SHA-256-locked tarballs.
- Reproducible builds (note 10 §15) so a third party can audit.
- Annual review of CVE feeds per dep.
- Where a dep has a strong alternative (libcurl vs. picohttpparser+nghttp2),
  the runtime layer is library-agnostic so swaps are tractable.

Residual risk: a transitive dep (OpenSSL pulled in by libcurl) has a
zero-day. We accept the standard mitigation timeline (patch within 72h).

### 1.4 Performance

Risk: the C output is slower than the VM (unexpected) or slower than
the Go backend (more expected on some workloads).

Mitigation: phase 16 perf gate (note 11 §11) tracks regressions. The
MEP body sets a soft target of "median fixture within 2x of Go
backend wall-time" for v1; phase-2 targets parity or better.

Residual risk: a class of programs (e.g. tight string-manipulation
loops) is permanently slower due to UTF-8 cursor cost or boxed
strings. The MEP body lists Phase-2 optimisations (SSO, interning, SIMD
UTF-8) to close the gap.

### 1.5 Binary size

Risk: even small Mochi programs produce 5+ MB binaries because the
runtime is amalgamated.

Mitigation:
- Aggressive dead-code elimination at link (`-Wl,--gc-sections`,
  `-Wl,--icf=safe`, `-flto=thin`).
- `--small` profile uses `-Os` and strips aggressively; documented
  baseline ~3 MB for hello-world.
- Phase 3 considers per-feature opt-in (the runtime is split into
  `mochi-core`, `mochi-query`, `mochi-stream`, etc., linked on demand).

Residual risk: still bigger than Go's binaries (~2 MB hello-world).
Acceptable trade for the C-target story.

### 1.6 Debugging UX

Risk: stack traces show generated C frames; users get confused.

Mitigation: `#line` directives plus debug-info path rewriting (note
07 §10, note 10 §15) so gdb/lldb/DAP show Mochi source. The driver
post-processes C compiler errors to Mochi spans.

Residual risk: tricky concurrency bugs (TSan reports on stream/agent
code) surface in runtime files; the docs need a "how to read runtime
TSan reports" page.

### 1.7 FFI surface area

Risk: the Go FFI through a Unix-domain RPC is slow (30-80us per call)
and the c-archive alternative requires Go toolchain at build time.

Mitigation: document both paths and let users pick. For Python, the
embedded libpython3 path is fast but adds 30MB to the binary; the
out-of-process path is slow but lightweight. Both supported, default
chosen by `import` declaration shape.

### 1.8 GC choice lock-in

Risk: BDWGC is conservative and has known false-retention issues.
Switching later breaks user code that relies on observable GC
behaviour.

Mitigation: the language spec forbids relying on GC observable
behaviour (no destructors, no `__del__` analogue, no weak refs in v1).
This keeps the GC swap as an implementation-detail change.

Residual risk: users build mental models around BDWGC's pause-time
profile; switching to a precise GC perturbs those.

## 2. Alternatives considered and rejected

### 2.1 LLVM IR directly

Pros:
- Best codegen quality.
- Skip the C front-end entirely.
- Direct access to LLVM passes (vectoriser, inliner).

Cons:
- LLVM IR is a moving target across versions; we'd need to track each.
- The build dep is heavy (~300 MB).
- Cross-compilation requires more work than `zig cc` (we'd write our
  own driver for every target).
- No source-readable output for review.

Verdict: rejected. The "boring C output a human can read" advantage is
worth more than the codegen quality gap.

### 2.2 WASM (WAT or WAT-via-Walrus) as primary IR

Pros:
- Portable by construction.
- Strong sandbox story.

Cons:
- Native performance gap (WAMR / wasmtime are 20-40% behind native).
- No threads without wasi-threads (still unstable in 2026).
- GC story (wasm-gc proposal) is improving but not universal in 2026
  runtimes.
- Strings and varints require encoding work that's free in C.

Verdict: rejected as *primary*. WASM is a target *of* the C path
(via wasi-sdk or emscripten), not the IR itself.

### 2.3 Rust as the target language

Pros:
- Memory safety in the generated code itself.
- Good libraries (tokio, serde) we could lean on.

Cons:
- Rust compile times (10-20x C's) destroy the "fast iteration" story.
- The borrow checker would reject most generated code (we'd have to
  emit a sea of `Rc` and `RefCell`, defeating the safety).
- Cross-compilation depends on Rust toolchains; smaller install base
  than C toolchains.
- "Boring Rust" is much less mature than "boring C".

Verdict: rejected.

### 2.4 JIT (a la JVM HotSpot, LuaJIT, V8)

Pros:
- Best peak performance possible.
- Single binary, no compile step at deploy.

Cons:
- Per-platform JIT engines (x86/arm64/riscv) are months of work each.
- W^X is increasingly hard on macOS, iOS, Android.
- No precompile / AOT story without doubling the implementation.
- Hard to debug; hard to sandbox.

Verdict: rejected as primary. A JIT for the *VM* is a separate MEP.

### 2.5 C++ as target

Pros:
- Templates would let us emit fewer monomorphised types.
- Standard library has containers we could reuse.

Cons:
- C++ compile times are slower than C.
- Template error messages are worse than C errors.
- ABI is messier (name mangling, exceptions, RTTI).
- We do not gain anything we cannot do in C with a bit more code.

Verdict: rejected.

### 2.6 Zig as target

Pros:
- Cleaner type system than C.
- Built-in cross-compilation.

Cons:
- Zig has no 1.0 in 2026; ABI and syntax still moving.
- Generated Zig is less readable to most reviewers than generated C.
- C interop is already first-class in Zig, so Zig-as-output gains
  nothing over C-as-output.

Verdict: rejected.

### 2.7 Custom bytecode + AOT compiler

This is the current VM path. The MEP-45 transpiler does *not*
replace the VM; it adds a parallel "AOT to native" path. Users keep
`mochi run` for fast iteration and use `mochi build` for shipping.

## 3. Kill switches

If MEP-45 fails to meet its gates, here is how the project
recovers without disrupting users:

### 3.1 Demote to "experimental"

- The CLI keeps the C backend but flags `mochi build --target=c` as
  experimental.
- Default `mochi build` falls back to the Go backend.
- Documentation marks the feature as preview.

### 3.2 Drop a tier

If a specific architecture or OS is too costly to support (say,
Windows MSVC keeps breaking), demote it to tier 2 or tier 3.

### 3.3 Drop a feature

If FFI to Python proves intractable, drop it from the v1 scope and
ship without. Users who need it stay on the VM (which has Python FFI)
or use the Go backend (which embeds CPython via cgo).

### 3.4 Switch to plan B GC

If BDWGC's WASM story stays broken, switch to a precise GC (MMTk) for
WASM only. The runtime layer is GC-agnostic by design.

## 4. Non-risks (explicitly)

These are *not* risks the MEP body needs to mitigate:

- "C is unsafe", the generated C is sanitiser-clean and follows a
  restricted style guide (note 07 §9). The user writes Mochi, not C.
- "Users will have to debug C", the diagnostics layer rewrites paths
  back to Mochi (note 07 §10). Stack traces show Mochi frames.
- "C is dying", C usage grew in 2025, not shrank. C23 is the most
  ambitious C standard in 30 years. Toolchain support is the strongest
  it has ever been. The 2026 baseline is healthy.

## 5. Comparable industrial precedent

The "transpile to C" path has been chosen by:

- Nim (very mature; C is the default backend).
- Crystal (LLVM by default, but the compiler is C-shaped).
- Vala (default backend is C; widely deployed in GNOME).
- TinyGo (LLVM, but with a C-style runtime philosophy).
- Cython (Python -> C, ships at scale).
- Faust (DSP language -> C).
- ATS (dependent types -> C).
- GHC's STG -> C / -> LLVM duality.
- MLton (whole-program SML -> C historically).

The path is well-trodden. The novelty in MEP-45 is not the path but
the integration shape: a transpiler tightly coupled to a single
maintained runtime, with byte-equal differential testing against an
existing VM as the master gate.

## 6. Open questions for the MEP body

1. Whether the v1 GC choice (BDWGC) deserves a kill-switch documented
   in the spec, or is treated as load-bearing for v1 only.
2. Whether the supply-chain mitigation (vendoring) extends to libcurl
   (which has its own deep dep graph).
3. Whether the Python FFI is a v1 deliverable or pushed to v2.
4. Whether the perf gate (2x of Go) is the right target or too lax.
5. Whether the threat model considers a hostile `cc` (a tampered host
   compiler); this is a class of risk most projects punt on.
