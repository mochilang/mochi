---
name: 5600-research-04-runtime
description: "MEP-45 research note 04. Runtime building blocks: GC, allocator, coroutines, channels, effects, strings, lists, maps, datalog, JSON/CSV/YAML, HTTP, time, cross-compilation."
---

# MEP-45 research note 04, Runtime building blocks

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

This note inventories the third-party and home-grown components the C
runtime can stand on. It does not prescribe which goes into v1 (that
is the MEP body); it captures what is actually available in 2026, with
licence, portability, and integration cost recorded so that the MEP
can defend its choices.

## 1. Memory management

### 1.1 Conservative GC: BDWGC (Boehm-Demers-Weiser)

- Repo: ivmai/bdwgc (active in 2026, maintained by Ivan Maidanski).
- Licence: BDWGC-style (MIT-like, permissive).
- Languages that use it: GNU/GCJ, Mono (old), D (option), Crystal, Vala,
  Inko (historic), nearly every Scheme-on-C transpiler.
- Properties: precise on the C heap roots it is told about, conservative
  on the C stack and registers. Incremental and generational modes.
  Works on macOS, Linux, Windows, BSDs, WASI (with patches), Android,
  iOS. Threads supported via `GC_THREADS`.
- Cost: ~150 KB code size for the static lib. Pause times on the BG
  corpus: low single-digit ms.
- Limitations: false retention on tagged pointers; not friendly to
  WASM (linear memory model). For WASM we either disable GC and lean on
  Asyncify/region allocation, or switch to a precise allocator with
  shadow stack.

### 1.2 Precise tracing GC: MMTk

- Repo: mmtk/mmtk-core (Rust framework), with bindings for OpenJDK,
  V8, Ruby, Julia, RPython, and an experimental C binding.
- Licence: MIT.
- Properties: plan-based (NoGC, MarkSweep, SemiSpace, Immix, GenImmix,
  StickyImmix, GenCopy, MarkCompact). State-of-the-art Immix collector
  (Blackburn & McKinley, PLDI 2008).
- Cost: ~3 MB Rust static lib, requires a precise root visitor on the
  client side. The Rust dependency is the biggest portability question
  (it does land cleanly via zig cc on every target we care about, but
  it pulls a `libstd` baseline).
- Use: Phase 3 candidate when the BG corpus shows pause-time wins from
  GenImmix would pay back the integration cost.

### 1.3 Reference counting: Perceus

- Paper: Reinking, Xie, de Moura, Leijen, "Perceus: Garbage Free
  Reference Counting with Reuse" (PLDI 2021).
- Implemented by: Koka (canonical), Lean 4 (with adaptations), Roc
  (variant).
- Properties: compiler infers reference counts and emits inc/dec at
  precise points. Cycle freedom is a *language obligation* (Koka and
  Roc both forbid cycles statically); for Mochi, with mutable records
  and closures that can cycle, we need a cycle collector.
- Follow-up papers:
  - "Frame-Limited Reuse" (ICFP 2022) reduces RC traffic by reusing
    record slots in-place.
  - "FP² Fully in-Place Functional Programming" (ICFP 2023) extends
    in-place updates further.
- Cost: deep compiler integration; not a drop-in library.
- Use: Phase 3+ alternative if we can prove the BG corpus stays
  cycle-free or if we add a Bacon-Rajan cycle collector.

### 1.4 Bacon-Rajan cycle collection

- Paper: Bacon & Rajan, "Concurrent Cycle Collection in Reference
  Counted Systems" (ECOOP 2001).
- Implemented by: CPython's `gc` module, Vala for objects with `weak`
  references, Cone's pluggable backend.
- Pairs with RC: catches cycles missed by inc/dec.
- Cost: scanning candidate set; can be triggered on allocation pressure.

### 1.5 Allocator: mimalloc

- Repo: microsoft/mimalloc.
- Licence: MIT.
- Properties: free-list sharded by size class, per-thread heaps,
  excellent small-alloc throughput, optional secure mode with guard
  pages.
- Cost: ~80 KB. Available on macOS, Linux, Windows, BSDs, WASI.
- Use: default `malloc` replacement under both BDWGC (BDWGC calls
  `mi_malloc` for non-GC allocation paths) and any future precise GC.

### 1.6 Allocator: scudo

- LLVM's hardened allocator. Used by Android, Fuchsia.
- Use: opt-in `-Drt-alloc=scudo` for security-sensitive builds.

### 1.7 Region / arena allocation

- Implemented inline in `mochi_arena` (home-grown). Used for the query
  pipeline (note 08) so that an entire dataset query's intermediate
  state lives in one arena freed at the pipeline boundary.
- Cyclone (PLDI 2002) and Tofte-Talpin (TOPLAS 1997) are the reference
  designs. We do not do region inference; we expose `with arena { ... }`
  blocks the codegen targets for query bodies.

## 2. Coroutines and fibers

### 2.1 minicoro

- Repo: edubart/minicoro.
- Licence: MIT.
- Properties: ~600 LOC, header-only or small static lib, portable
  across x86_64, arm64, riscv64, wasm32 (with Asyncify shim),
  ucontext, fcontext, Windows fibers. No Boost dependency.
- Use: default fiber backend for the M:N scheduler.

### 2.2 libaco

- Repo: hnes/libaco.
- Properties: very fast on x86_64 and arm64 (~16 ns context switch),
  but no first-class WASM support.
- Use: opt-in for native-only high-frequency switch workloads.

### 2.3 boost.context fcontext

- The Boost.Context primitive that minicoro and many others wrap.
- Use: indirectly via minicoro; not a direct dep.

### 2.4 ucontext

- POSIX, deprecated on macOS, slow.
- Use: fallback when stuck on a platform with no better option (no
  practical case today; minicoro covers everything).

### 2.5 Asyncify (WASM)

- Binaryen's transform that turns C call/return into resumable state
  machines.
- Use: required for any WASM build that uses coroutines.

## 3. Event loop / I/O

### 3.1 libuv

- Repo: libuv/libuv.
- Licence: MIT.
- Properties: cross-platform async I/O (epoll, kqueue, IOCP, io_uring
  on Linux 5.10+), timers, child processes, DNS, file I/O.
- Used by: Node.js, Julia, pyuv, neovim.
- Cost: ~500 KB static. Portable to macOS, Linux, Windows, BSDs.
  WASI: partial.
- Use: default I/O substrate for `mochi-net`.

### 3.2 libxev

- Repo: mitchellh/libxev (Zig with C API).
- Properties: io_uring on Linux, kqueue on macOS/BSD, IOCP on Windows.
  Designed by the Ghostty author; lighter than libuv but newer.
- Use: candidate for future `mochi-net2` if libuv proves heavy on
  embedded targets.

### 3.3 libdill / libmill

- Channels and goroutines in C, by Martin Sustrik.
- Properties: structured concurrency primitives, CSP-style channels.
- Use: not a dep, but a reference for channel API design.

## 4. Channels

Home-grown `mochi_chan` based on the Go channel design:

- Buffered ring with sender and receiver wait queues.
- `chan T` lowers to `mochi_chan*` plus a per-T trampoline for typed
  send/recv.
- `select` lowers to a polling loop with random ordering on ready
  cases, matching the Go semantics the language docs describe.

Reference reads:
- Pike's "Go concurrency patterns" talks.
- Plan 9 alef channels (the historical ancestor).
- Concurrent ML primitives (Reppy).

## 5. Effects compilation

The Mochi spec does not surface algebraic effects in v1. This note
records the menu so the runtime can be extended without re-architecting:

### 5.1 Multicore OCaml fibers

- Paper / impl: Sivaramakrishnan et al., "Retrofitting Effect Handlers
  onto OCaml" (PLDI 2021), arXiv:2104.00250.
- Properties: one-shot continuations on top of fibers. Production-grade
  in OCaml 5.x.
- Use: pattern to copy for a future `effect` feature.

### 5.2 Koka evidence-passing

- Paper: Xie & Leijen, "Generalized Evidence Passing for Effect
  Handlers" (ICFP 2021).
- Properties: closed-form CPS-free compilation. No fibers needed.
- Use: alternative if we want effects without OS-thread-shaped
  context switches.

### 5.3 Effekt capability-passing

- Paper: Schuster, Brachthäuser, Ostermann, "Compiling Effect Handlers
  in Capability-Passing Style" (ICFP 2020).
- Use: the static-capability variant; lightweight when the effect set
  is closed at compile time.

### 5.4 Plain CPS

- Always available; expensive in C without TCO. Not preferred.

## 6. Strings

### 6.1 utf8proc

- Repo: JuliaLang/utf8proc.
- Licence: MIT.
- Properties: Unicode normalisation (NFC, NFD, NFKC, NFKD), case
  folding, grapheme cluster iteration, Unicode 15.1 tables (~400 KB).
- Use: backs `mochi_str` normalisation, `string.upper`, `string.lower`,
  `string.fold_case`, and grapheme-aware indexing if we promote indexing
  past code-point granularity.

### 6.2 simdutf

- Repo: simdutf/simdutf.
- Properties: SIMD UTF-8 validation and transcoding. Used by Node.js,
  Bun.
- Use: `mochi_str` construction from raw bytes; validates input in
  parallel.

### 6.3 PCRE2

- Standard regex library. Used by PHP, R, many others.
- Use: backing for any future `regex` module. v1 of Mochi does not
  surface regex in the language docs, so this is Phase 2.

## 7. Lists and maps

### 7.1 Swiss tables: cwisstable

- Repo: google/cwisstable.
- Licence: Apache-2.
- Properties: pure C port of Abseil's flat_hash_map, the same SIMD
  scan structure.
- Use: backing for `mochi_map__K_V` (note 06 §7).

### 7.2 Robin Hood: emhash, ankerl::unordered_dense

- Strong alternatives if cwisstable proves slow on a benchmark.
- Use: not a v1 dep.

### 7.3 Persistent / functional maps

- Bagwell HAMTs as in Clojure / Scala.
- Use: not in language scope (Mochi's map is mutable, eager).

## 8. JSON / YAML / CSV / TOML

### 8.1 yyjson

- Repo: ibireme/yyjson.
- Licence: MIT.
- Properties: fastest JSON parser known as of 2026, header + one C
  file, MIT, supports streaming, mutation, pretty-print, and a
  zero-copy "immutable doc" mode.
- Use: backing for `std/json`. Default.

### 8.2 simdjson

- C++; we would expose a thin C shim.
- Use: alternative when measured faster on a specific corpus.

### 8.3 libfyaml

- Repo: pantoniou/libfyaml.
- Licence: MIT.
- Properties: YAML 1.2 with anchors, merge keys, schemas.
- Use: backing for `std/yaml`.

### 8.4 libcsv / xsv / home-grown

- CSV is small enough that a 300-LOC home-grown reader is the default.
- Reference: rfc4180.

### 8.5 toml++

- C++17. We would prefer a pure-C TOML parser; tomlc99 is acceptable.

## 9. HTTP and networking

### 9.1 libcurl

- Properties: HTTP/1, HTTP/2 (via nghttp2), HTTP/3 (via ngtcp2 +
  nghttp3 or quiche), WebSockets, FTP, TLS via OpenSSL / mbedTLS /
  rustls.
- Use: backing for `std/net.fetch` and `std/net.client`.
- Cost: ~1 MB+ static. Trim with `./configure --disable-...`.

### 9.2 cpr / hyper-c / picohttpparser

- picohttpparser is the H2O parser, small and fast; suitable for a
  custom client.
- Use: if libcurl is too heavy on a target.

### 9.3 TLS: mbedTLS, rustls (via FFI), OpenSSL, BoringSSL

- mbedTLS: portable, small, MIT licensed.
- rustls: modern, safe, but requires Rust on the host.
- OpenSSL / BoringSSL: industrial; large.
- Use: mbedTLS for v1 default; allow `-Drt-tls=...` swap.

### 9.4 QUIC: ngtcp2 + nghttp3, msquic, quiche

- All MIT-or-similar; pick based on `mochi-net` profile.

## 10. Time, dates, locales

### 10.1 stdlib

- `clock_gettime(CLOCK_REALTIME)` / `clock_gettime(CLOCK_MONOTONIC)`
  on POSIX.
- `GetSystemTimePreciseAsFileTime`, `QueryPerformanceCounter` on
  Windows.

### 10.2 tzdb: tz / IANA

- Bundled via Howard Hinnant's date library (C++) or a slim home-grown
  reader. We pick the latter for v1.
- Use: backs `std/time.in_tz`.

### 10.3 ICU

- Big, comprehensive Unicode + locale lib.
- Use: not in v1. utf8proc covers enough Unicode for now.

## 11. Datalog / logic

### 11.1 Soufflé

- Repo: souffle-lang/souffle.
- Properties: Datalog compiler to C++; production-grade for program
  analysis. Indexed evaluation, magic sets, parallel evaluation.
- Use: reference for our `fact/rule/query` lowering, not a dep.

### 11.2 ddlog (Differential Datalog)

- VMware project, archived but reference-quality for incremental
  evaluation.
- Use: design read.

### 11.3 Home-grown

For v1 we generate a forward-chaining evaluator per-program based on
the rules in the source. Indexes are picked from the rule shape (`p(X,
Y) :- q(X, Z), r(Z, Y)` builds an index on `r` by first arg). The
evaluator is iterative semi-naive (Bancilhon, Naughton, Ramakrishnan,
Sagiv, "Magic Sets and Other Strange Ways to Implement Logic
Programs", PODS 1986). For corner cases (negation as failure, stratified
semantics) we use Apt-Pugin stratification.

## 12. AI / LLM clients

The language exposes `generate text { model: "openai/gpt-4o" ... }`,
`generate T { ... }`, `generate embedding { ... }`. The runtime needs a
provider abstraction:

```c
typedef struct {
    const char *name;             // "openai", "anthropic", "google", "local-llama"
    mochi_error (*text)(mochi_str prompt, struct mochi_llm_opts opts, mochi_str *out);
    mochi_error (*structured)(mochi_str prompt, mochi_type *schema, struct mochi_llm_opts opts, mochi_value *out);
    mochi_error (*embedding)(mochi_str text, struct mochi_llm_opts opts, mochi_list__f64 *out);
} mochi_llm_provider;
```

Backends:

- HTTP-based providers (OpenAI, Anthropic, Google, Mistral, ...) via
  libcurl + yyjson; this covers most users.
- Local providers via llama.cpp's C API (`llama.h`). Loaded on demand
  via `dlopen`.
- The `tool { ... }` blocks in the language docs lower to a tool-router
  registered with the provider before each `generate` call.

Determinism: when the provider supports a seed and a temperature of 0,
the runtime passes them. Tests gated by `--llm-record-mode=replay`
use a cassette file (analogous to VCR).

## 13. FFI subsystems

The language docs show `import "go/strings" as gs` and similar for
Python and TypeScript. The C target compiles each `extern` declaration
into a typed thunk:

### 13.1 Go FFI

- We do *not* embed a Go runtime. The transpiler emits a small Go
  helper program at build time that exposes the imported symbols
  through a Unix-domain RPC (length-prefixed CBOR, MessagePack, or
  JSON). The C runtime spawns this helper and talks to it.
- Cost: helper process startup ~10ms; per-call overhead 30-80us via
  Unix-domain stream.
- Alternative for static linking: build the Go helper as a c-archive
  (`go build -buildmode=c-archive`) and link directly. We accept this
  for closed sets of imports.

### 13.2 Python FFI

- We embed CPython via libpython3 (`-lpython3.X`). The thunk uses the
  C API (`PyImport_ImportModule`, `PyObject_CallObject`).
- Conversion table per language docs §types is generated by the
  transpiler.
- Alternative: out-of-process via the same RPC scheme.

### 13.3 TypeScript FFI

- We embed QuickJS-NG (a maintained QuickJS fork) or v8 via libv8 (rare
  in C-only builds). The thunk transpiles the imported TS to JS at
  build time using esbuild as a build dep.

### 13.4 C FFI

- Direct call. The `extern` is a normal C prototype. No marshalling.

## 14. Build, packaging, cross-compilation

### 14.1 zig cc

- Repo: ziglang/zig.
- Properties: drop-in C/C++ compiler driver wrapping LLVM + bundled
  libcs (musl, glibc multi-version, mingw-w64, msvc-compatible, wasi,
  macOS SDK). Cross-compiles from any host to any target. The mochi
  cross-AOT system already uses this (see git log on
  hotfix/cross-aot-zig-url).
- Use: default toolchain for `mochi build`.

### 14.2 cosmocc / Cosmopolitan

- Justine Tunney's actually-portable executable system. One binary
  runs on macOS, Linux, Windows, FreeBSD, NetBSD, OpenBSD without
  modification (using the PE/ELF + system call multiplexer trick).
- Use: optional `mochi build --apex` produces a Cosmopolitan binary.

### 14.3 wasi-sdk

- Properties: wasi-libc + clang + binaryen, configured for WASI 0.2.
- Use: WASM/WASI builds.

### 14.4 emscripten

- Properties: WASM + JS glue + JS-FS + DOM bindings.
- Use: only for the browser playground build; CLI prefers wasi-sdk.

### 14.5 CMake / Meson / xmake / plain make

- Plain make is sufficient for the runtime (single libmochi.a target,
  amalgamated). The MEP body recommends a tiny `Makefile.toolchain`
  per platform plus an outer `mochi build` driver in Go.

## 15. Observability

### 15.1 ftrace, perf, eBPF

- Linux only; we expose USDT probes via `<sys/sdt.h>` for hot points
  (GC pause start/end, scheduler step, query step).

### 15.2 dtrace

- macOS / FreeBSD; same USDT probes are visible.

### 15.3 Windows ETW

- Phase 3.

### 15.4 Built-in tracing

- `MOCHI_TRACE=1` env var makes the runtime emit Chrome trace events
  to a file for later viewing in `chrome://tracing` or Perfetto.

## 16. Test harnesses

### 16.1 unity / cmocka / greatest

- We pick `greatest` for the runtime self-tests: single header, MIT,
  trivial to vendor.

### 16.2 hypothesis-style property tests

- `theft` (single header C lib) is our property-testing backbone.
- Use: shrinks counterexamples; needed for the pattern-matcher and
  the Swiss-table implementations.

### 16.3 Differential testing against the VM

- For the corpus already in `tests/vm/valid`, the MEP body requires
  byte-equal stdout from the AOT C binary versus the VM.

## 17. Security

### 17.1 Sanitisers

- ASan, UBSan, TSan, MSan are required for the CI matrix. Clang on
  Linux/macOS, MSVC ASan on Windows. UBSan in particular catches the
  signed-overflow class of bugs that the language considers undefined
  if `--mochi-fast-int` is on.

### 17.2 CFI

- Clang's `-fsanitize=cfi-icall` is enabled in `mochi build
  --secure`. It catches type-confused indirect calls (closures across
  ABI boundaries).

### 17.3 SafeStack and shadow stack

- Clang `-fsanitize=safe-stack`; Intel CET on supporting CPUs.
- Use: `--secure` profile.

### 17.4 Stack canaries, PIE, RELRO, BIND_NOW

- Default on the release profile.

### 17.5 Sandbox

- The threat-model document mentions `pledge`/`unveil` on OpenBSD,
  `seccomp-bpf` on Linux, App Sandbox on macOS. We expose
  `mochi_sandbox_apply(profile)` at runtime entry, with profiles named
  in the threat model.

## 18. Reproducibility

- `SOURCE_DATE_EPOCH` honoured by the codegen and the C compiler.
- `-ffile-prefix-map`, `-fdebug-prefix-map` strip absolute paths from
  debug info.
- Static linking of all non-libc deps so the binary digest is host-
  agnostic.
- The MEP body cites reproducible-builds.org/specs as the reference.

## 19. Open questions for the MEP body

1. Default GC choice: BDWGC (boring, works everywhere, easy) vs. a
   precise GC (less false retention, better WASM story, more work).
2. Default coroutine backend: minicoro (works everywhere) vs.
   libaco (faster on hot loops).
3. JSON: yyjson (default) vs. simdjson via C++ shim.
4. Whether to bundle libcurl or write a slim HTTP/2 client over
   picohttpparser + nghttp2.
5. Python FFI: embed libpython3 (heavy) vs. out-of-process (slow per
   call). Likely both, behind a flag.
6. Whether the Datalog evaluator is part of the always-on runtime or
   pulled in only when the program uses `fact/rule/query`.
7. AI provider abstraction: ship with OpenAI + Anthropic + local
   llama.cpp by default; everything else is an optional plug-in.

All other choices can be made by reading this note plus note 05 and
note 06.
