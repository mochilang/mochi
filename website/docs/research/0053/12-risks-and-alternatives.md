---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks"
description: "Risk register (macOS LC_UUID, generic closure storage, wasm32 no-TCP, cc-rs detection, cassette drift, embedded surface gap) and rejected alternatives (tokio, reqwest, serde, sha2, Arc/Mutex, impl Fn, syn/quote, inlined runtime, no published crate)."
---

# 12. Risks and alternatives

This note collects the open risks in MEP-53's design and the alternative designs that were considered and rejected.

## Risk register

### R1. macOS LC_UUID non-determinism

Mach-O binaries on macOS carry an `LC_UUID` load command that ld64 randomises per link. There is no flag to disable it (the `-no_uuid` flag was removed in Xcode 12+). Phase 16 (reproducible builds) skips macOS for this reason.

Impact: macOS CI cannot assert reproducibility. Linux CI can; this is the load-bearing platform for the property.

Mitigation: document the limitation. A future sub-phase could post-process Mach-O binaries to zero the LC_UUID bytes; this would break code-signing but is acceptable for non-signed distribution.

### R2. Generic closure storage

Mochi closures lower to `Box<dyn Fn(T) -> U>` (or `FnMut` / `FnOnce`). This forces heap allocation and vtable dispatch per call. The colour pass elides clones of Copy captures but does not eliminate the box.

Impact: closure-heavy programs (e.g., functional-style transformations) run slower than monomorphised `impl Fn` would. Benchmarks show ~2-3x slowdown vs monomorphised on tight loops.

Mitigation: a future codegen sub-phase could opt into `impl Fn` for closures stored in local variables (where the concrete type is known at use site). Closures stored in struct fields or returned from functions would remain boxed.

### R3. wasm32-wasip1 lacks TCP

WASI Preview 1 does not expose TCP sockets. `mochi_runtime::http::get` (which uses `std::net::TcpStream`) compiles for wasm32-wasip1 but panics at run-time on first TCP connect.

Impact: fixtures that use `fetch` cannot run under wasm. The wasm fixture set excludes them.

Mitigation: WASI Preview 2 has a `wasi:sockets` interface but is not stable on Rust 1.95. A future sub-phase could add a feature flag to route http through `wasi:sockets` when the target is wasm32-wasip2.

### R4. cc-rs C compiler detection

The cffi/ sidecar (phase 12) requires a host C compiler. cc-rs auto-detects via the CC env var or PATH (cc, gcc, clang, msvc). If no C compiler is present, the build fails with "cc not found."

Impact: hosts without a C toolchain cannot compile extern-fn-using Mochi programs.

Mitigation: macOS ships cc as part of Xcode CLI tools; Linux distros usually have gcc; Windows requires msvc-build-tools install. The driver could detect missing cc at lower-time and emit a clear pre-build error; currently the error surfaces at cargo time.

### R5. LLM cassette drift

The LLM cassette replay (phase 14) stores recorded request / response pairs in `MOCHI_LLM_CASSETTE_DIR`. If the cassette format changes or a prompt changes between record and replay, the replay returns the stale recorded response without warning.

Impact: subtle test failures where the replay drifts from the live API.

Mitigation: cassettes are content-addressed by SHA-256 of the prompt. A prompt change generates a new cassette key, which surfaces as a cache miss (no replay available) rather than a silent stale hit. Documented contract: cassettes are tied to a specific prompt; re-record when the prompt changes.

### R6. Embedded surface gap

The embedded gate (phase 18) exposes only `conv` and `strings`. Mochi programs using collections, channels, streams, panic, fetch, JSON, LLM, or runtime-checked arithmetic do not compile under embedded.

Impact: the embedded target is a "Mochi subset" rather than full Mochi.

Mitigation: this is documented as the intentional design. Adding more modules to the embedded surface would require careful work to remove `std::sync`, `std::thread`, `std::net`, `std::io`, and `std::time` deps and replace them with no_std-compatible alternatives (e.g., `core::cell::RefCell`, `alloc::collections::BTreeMap`). A future sub-phase could expand the embedded surface to include `chan`, `stream`, and bounded collections.

### R7. Toolchain version drift

The cache key (phase 0) does not include the rustc or cargo version. Bumping the Rust toolchain while keeping the cache could give stale binaries.

Impact: `cargo --version` change is invisible to the cache.

Mitigation: `Driver.NoCache=true` after a toolchain bump. A future sub-phase could include `cargo --version` in the cache key.

### R8. Cross-compilation toolchain install

`cargo-zigbuild` requires the Zig SDK; `wasmtime` requires the wasmtime binary; both must be installed separately. Hosts without them cannot exercise the cross / wasm gates.

Impact: developer-machine setup friction.

Mitigation: the driver detects missing tools and emits a clear "tool not found; install via cargo install X" message. CI installs them via the workflow setup step.

## Rejected alternatives

### A1. tokio for async

Considered and rejected. Mochi's `async` is a typecheck-time colour pass, not a runtime concurrency request. Adding tokio would (1) introduce a ~50 MB binary size overhead per emitted program, (2) require an executor entry point (`#[tokio::main]`), (3) force `Send + 'static` on every closure captured in an async block. The single-thread blocking lowering is correct for Mochi's source semantics and far simpler.

### A2. reqwest for HTTP

Considered and rejected. reqwest pulls in tokio (40+ transitive deps) for HTTP/1.1 GET. MEP-53 only needs HTTP GET against `http://` URLs (no TLS, no auth, no streaming). A hand-rolled `TcpStream` + manual request format is ~60 LOC and has zero deps.

### A3. serde / serde_json for JSON

Considered and rejected. serde / serde_json are heavy compile-time deps (~25 seconds first-build) and require `#[derive(Serialize)]` on every type. Mochi values are dynamically typed at the JSON boundary; a hand-rolled ~90 LOC object decoder targeting Mochi values directly is simpler.

### A4. sha2 crate for SHA-256

Considered and rejected. The sha2 crate is ~3 transitive deps and pulls in `digest`, `block-buffer`, etc. Phase 14's SHA-256 use is only for content-addressing cassettes (the cassette key). A ~80 LOC inline implementation of SHA-256 has zero deps and zero compile cost.

### A5. Arc / Mutex for concurrency primitives

Considered and rejected. See [[agent-streams]] for the full argument: (1) syscall cost per uncontended lock, (2) `Send + 'static` requirement on captured values, (3) embedded gate breakage (Arc requires CAS not present on all MCUs). `Rc<RefCell>` is the load-bearing choice.

### A6. impl Fn instead of Box dyn Fn for closures

Considered and rejected. `impl Fn` is monomorphised (zero-overhead) but cannot be stored in struct fields (the size is unknown at struct definition time) nor returned from functions of varying closure types. Mochi closures freely flow into struct fields (e.g., `record Handler { f: fun(int) -> int }`), which forces `Box<dyn Fn>`. Using `impl Fn` for some closures and `Box<dyn Fn>` for others would split the type system and force a colour-pass discipline that is more work than the saving justifies.

### A7. syn / quote for codegen

Considered and rejected. syn / quote are designed for proc-macros, where the input is already a Rust TokenStream. MEP-53's input is Mochi `aotir.Stmt` / `aotir.Expr`; lowering to syn would require an extra intermediate step (Mochi AST → syn AST → token stream → string). The direct path (Mochi AST → rtree IR → string with stable formatter) is simpler and gives byte-stable output by construction.

The rtree IR (see [[codegen-design]]) is a thin, structural Rust AST tuned for Mochi's emission needs, with no pretensions to parse arbitrary Rust.

### A8. Inlined runtime instead of mochi-runtime crate

Considered and rejected. Inlining the runtime into each emitted program (~1200 LOC of Rust prepended to every file) would (1) blow up build times (rustc parses the same 1200 LOC every time), (2) break the publish-to-crates.io story (no shared library, every program ships its own copy), (3) lose the embedded-feature-flag mechanism. The separate runtime crate is right.

### A9. No published runtime crate

Considered and rejected. Without a published runtime crate, downstream Rust consumers (e.g., third-party Mochi tooling, embedded crates that want to pull in `mochi_runtime::strings`) would have to vendor the path-dependency. Publishing under `mochi-runtime` on crates.io gives a stable consumption point. Phase 15 establishes the publish-side metadata; an actual publish is a separate manual step.

### A10. Generated Cargo.lock vs no Cargo.lock

Considered and rejected (in favor of: emit Cargo.lock when Deterministic=true). Without a Cargo.lock, cargo resolves deps fresh each build, which can produce different binaries when itertools or cc gets a patch release. Emitting a frozen Cargo.lock (with pinned itertools and cc versions) gives bit-stable builds across time.

### A11. Path dependency vs git dependency on mochi-runtime

Considered (and current choice: path dependency). Path dependency requires the user to have the mochi source tree available. A git dependency (`mochi-runtime = { git = "..." }`) would let users build emitted Rust against a published or git-hosted runtime without the Mochi tree. The trade-off: git deps require network and have stale-cache concerns. Path is simpler for the in-tree gate; a future sub-phase could add a `--published` flag that swaps in a crates.io version dep.

## Cross-references

- [[design-philosophy]] for the foundational decisions.
- [[runtime]] for the dep-list rationale.
- [[agent-streams]] for the Arc/Mutex rejection.
- [[testing-gates]] for the gates that surface the risks.
- [MEP-53 §6](/docs/mep/mep-0053#6-alternatives) for the normative alternative-rejection list.
