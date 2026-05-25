# MEP-46 research note 12, Risks and alternatives

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).

This note catalogues the major risks of the BEAM target choice, the alternative architectures we considered and rejected, and the migration paths if one of the risks materialises.

---

## 1. Risk: Core Erlang API drift

**Risk**: The OTP team marks `cerl` records as "internal" in a future release, breaks the API, and Mochi's codegen breaks.

**Likelihood**: Low. The Core Erlang spec has been stable for 20 years (Carlsson et al. 2004); the `cerl` module API is documented (https://www.erlang.org/doc/man/cerl.html) and is the supported plug-in point per the EEF Compiler Workgroup (see [[03-prior-art-transpilers]] §18). LFE, Clojerl, Hamler, Alpaca all rely on it.

**Impact if it happens**: High. We'd need to migrate to Erlang abstract format (the Elixir/Gleam route).

**Mitigation**:
- Pin the OTP minimum version and validate `cerl` API stability at each minor release in CI.
- Maintain a parallel "emit abstract format" path in the codegen as a safety net (initially behind a feature flag). The cost is ~2 weeks of engineer time to maintain.
- Engage with the EEF Compiler Workgroup; sponsor LFE's continued maintenance (LFE is the canary for `cerl` stability).

## 2. Risk: BeamAsm JIT regressions

**Risk**: The JIT mis-compiles certain Mochi-emitted Core Erlang patterns, producing wrong output or crashes.

**Likelihood**: Low. The JIT operates at BEAM bytecode level; if our Core Erlang validates and our BEAM validates, the JIT input is well-formed. JIT bugs in OTP 24-28 have been rare and fixed quickly.

**Impact if it happens**: Medium. We'd disable the JIT for affected modules (`erl +JMsingle false` or `code:set_modules_compile_options/1` with `no_jit`) until OTP fixes upstream.

**Mitigation**:
- All gates run with `+JMsingle true` (JIT default) and `+JMsingle false` (interpreter) in a weekly cron, catching divergence.
- Report any divergence to the OTP team; they're responsive (OTP 25.0.4, 26.0.2, 27.0.1 all included JIT correctness fixes within weeks of report).

## 3. Risk: NIF temptation

**Risk**: A user or future MEP-46 contributor adds a NIF for performance, introducing memory-safety bugs and platform-specific build dependencies.

**Likelihood**: Medium. The temptation is real; raw Erlang for hot loops is slower than NIF C.

**Impact if it happens**: Could destabilise the VM; complicates portability (cross-arch builds need NIF binaries per target).

**Mitigation**:
- Document the "no new NIFs" stance in MEP-46 spec.
- Default deny on PRs that add `erl_nif.h` includes; require an EEP-style proposal with justification.
- If a NIF is genuinely needed (e.g. zstd compression), use **Rustler** (https://github.com/rusterlium/rustler) so the NIF is in safe Rust.

## 4. Risk: Atom exhaustion

**Risk**: A long-running Mochi node accumulates atoms (from dynamic FFI calls, hot-reloaded modules, etc.) and crashes when the atom table is exhausted (1M limit).

**Likelihood**: Medium for long-lived services; Low for CLI tools.

**Impact if it happens**: Node crash, requires restart.

**Mitigation**:
- `mochi_atoms` module pre-registers all known atoms at boot.
- Codegen **never** emits `binary_to_atom/1` on user data; only `binary_to_existing_atom/2`.
- Document the limit; recommend `+t 4194304` for users running large Mochi clusters.
- A `mochi inspect atoms` CLI command lists atom table usage.

## 5. Risk: Process leak from streams

**Risk**: `subscribe` filters that never terminate accumulate as the publisher's subscriber list grows; eventually nodes run out of processes (default 262144 per node).

**Likelihood**: Medium. A common bug pattern in OTP.

**Impact if it happens**: Node OOM or process-limit crash.

**Mitigation**:
- All subscribe processes are supervised by `mochi_stream_sup`; supervisor restarts fail if too many crashes happen.
- Subscribe processes auto-terminate when the subscribing process dies (link semantics).
- Documentation prominently covers the "subscribe in a long-lived process, not a request handler" rule.
- A `mochi inspect streams` command lists subscribers per stream.

## 6. Risk: Performance vs C target

**Risk**: Users compare BEAM-target Mochi vs C-target Mochi (MEP-45) and find the BEAM target 5-10x slower, ditching it for everything except concurrency-heavy workloads.

**Likelihood**: High. BEAM is faster than CPython but slower than C for raw arithmetic and bulk array work.

**Impact if it happens**: BEAM target relegated to "the concurrency option" rather than a general-purpose backend.

**Mitigation**:
- This is **acceptable** and matches reality. The MEP body explicitly positions BEAM as the concurrency/distribution target, not the numerics target.
- Document the trade-off in user-facing docs; show benchmark tables comparing C vs BEAM for both styles of workload.
- Provide an FFI path so Mochi-on-BEAM users can call a C-target Mochi library via NIFs (a future MEP).

## 7. Risk: Atom table sharing across distribution

**Risk**: A distributed cluster includes a node with a maliciously crafted module that emits atoms designed to push other nodes over the atom limit.

**Likelihood**: Low (requires a hostile distributed peer).

**Impact if it happens**: Network-wide atom exhaustion.

**Mitigation**:
- Document that distribution should run only over trusted networks (with TLS).
- Recommend `epmdless` or named `-name` with `-setcookie` minimums.
- Out of scope for v0.1 to enforce; documentation only.

## 8. Risk: AtomVM divergence

**Risk**: AtomVM diverges from OTP semantics enough that Mochi-on-AtomVM and Mochi-on-OTP produce different results for the "compat profile" subset.

**Likelihood**: Medium. AtomVM has had subtle divergences (e.g. integer overflow handling on 32-bit pre-0.6).

**Impact if it happens**: AtomVM target becomes unsupported.

**Mitigation**:
- Test the AtomVM compat profile in CI on every release.
- Maintain a small allowlist of fixtures known to differ; require a fix before promoting AtomVM out of Tier 2.

## 9. Risk: Hex dependency on `mochi` runtime

**Risk**: Users build a rebar3 project from Mochi output; the `mochi` Hex package becomes unmaintained or has a CVE; downstream users are stuck.

**Likelihood**: Low. The `mochi` runtime is small (~3000 lines of Erlang), pure Erlang, no native deps. Maintenance is straightforward.

**Impact if it happens**: Forks or vendoring required.

**Mitigation**:
- The `mochi` runtime ships under Apache-2.0 (matching the rest of the Mochi project).
- The Mochi compiler can optionally **vendor** the runtime modules into the user's project (`mochi build --vendor-runtime`); the rebar3 project then has no Hex dep at all.

## 10. Risk: Diagnostic mismatch

**Risk**: BEAM stack traces (which point at generated Erlang source lines) confuse users who write Mochi; they file bugs against `mochi_user_main.erl:42` when they meant `main.mochi:17`.

**Likelihood**: High; this is unavoidable in any transpiler.

**Impact if it happens**: User-experience degradation, more "unhelpful stack trace" issues.

**Mitigation**:
- The `Line` chunk maps generated Erlang lines back to Mochi source lines via a sidecar `.mochi-map` file.
- A `mochi stacktrace` helper takes a raw OTP stack trace and rewrites it in Mochi-source coordinates. Build into `mochi_log`, the default OTP error handler.
- Documentation: every error message points to the Mochi source line, never the generated Erlang.

## 11. Alternative architecture: target Erlang abstract format

**Architecture**: Emit Erlang abstract format (the AST that `erl_parse` produces) instead of Core Erlang.

**Pros**:
- Better warning messages (the `erl_lint` pass works at this level).
- Slightly easier to pretty-print as `.erl` for handoff to Erlang devs.
- This is what Elixir and Gleam do.

**Cons**:
- More complex than Core Erlang (operator precedence, multi-clause functions, list syntactic sugar).
- Less stable as an API (the `erl_parse` term shape changes between OTP versions, e.g. column info added in OTP 27).
- The EEF Compiler Workgroup explicitly recommends Core Erlang as the plug-in point (see [[03-prior-art-transpilers]] §18).

**Decision**: Reject. Core Erlang is better-suited for a clean compiler with its own type/check system (we don't need `erl_lint`'s warnings; Mochi's checker has already produced them). The MEP keeps abstract format as a **fallback** if Core Erlang ever breaks.

## 12. Alternative architecture: emit Erlang source text

**Architecture**: Pretty-print Erlang source code to `.erl` files, then run `erlc`.

**Pros**:
- Generated code is human-readable.
- Caramel, Purerl, Hamler all chose this path.
- Stack traces point at the generated `.erl`, which is at least viewable.

**Cons**:
- Pretty-printing is error-prone; operator precedence, special characters in atoms, line continuation issues.
- Slower compile (an extra parse step).
- Less compact intermediate representation.

**Decision**: Reject as primary path; **emit `.erl` alongside `.beam`** in rebar3-project mode for handoff but compile directly via Core Erlang. Best of both worlds.

## 13. Alternative architecture: target Elixir

**Architecture**: Transpile Mochi to Elixir source, then run `mix compile`.

**Pros**:
- Elixir has the largest BEAM library ecosystem.
- Mix is the most polished build tool.
- Elixir-style structs map cleanly to Mochi records.

**Cons**:
- Elixir as a target language means depending on the Elixir compiler version (separate concern from OTP version).
- Elixir's macro system is invisible to a transpiler emitting source; certain features (Phoenix LiveView, Ecto schemas) are macro-driven and not accessible.
- Adds a layer of indirection: Mochi → Elixir → BEAM.

**Decision**: Reject as primary. Offer `mochi build --target=beam-mix-project` for users who want to integrate with Elixir codebases, but the canonical path is Core Erlang → BEAM directly.

## 14. Alternative architecture: target Gleam

**Architecture**: Transpile Mochi to Gleam source, ride Gleam's typed Erlang emission.

**Pros**:
- Gleam's type system is closest to Mochi's of any BEAM language.
- Gleam handles ADT lowering, pattern matching, OTP wrappers.

**Cons**:
- Three-stage compilation (Mochi → Gleam → Erlang → BEAM) is slow and error-prone.
- Gleam as a target language means a moving dependency (Gleam pre-1.0 was unstable; 1.0+ is stable but still evolving).
- The Gleam compiler is in Rust; cross-language dependency.

**Decision**: Reject. Direct Core Erlang emission is cleaner.

## 15. Alternative architecture: write a Mochi VM in Erlang

**Architecture**: Implement a Mochi interpreter in Erlang; ship `.mochi` files directly.

**Pros**:
- Conceptually simplest.
- Same surface as vm3, just on BEAM.

**Cons**:
- 10-50x slower than compiled `.beam`.
- Loses all of BEAM's optimization (JIT, pattern-match compiler, etc.).
- Defeats the purpose of having a BEAM *target*.

**Decision**: Reject. The point of MEP-46 is to use BEAM as a *compilation* target, not a hosting platform for the Mochi interpreter.

## 16. Alternative architecture: NIF-only runtime

**Architecture**: Implement the Mochi runtime as a single C NIF; Erlang code is a thin shim.

**Pros**:
- Maximum performance.
- Mochi could share the libmochi C runtime between MEP-45 (C target) and MEP-46 (BEAM target).

**Cons**:
- Scheduler-blocking issues require very careful yielding.
- Platform-specific binaries (different NIF per arch).
- Loses BEAM's process model (NIF code is single-threaded within a scheduler slot).
- Memory safety risk.

**Decision**: Reject for v0.1. Keep the door open for v0.2+ if specific hot paths warrant it (e.g. binary-heavy parsing).

## 17. Alternative IR: skip `aotir`

MEP-45 has a separate AOT IR (`transpiler3/c/aotir`) between the typed Mochi AST and C codegen. For MEP-46, we considered both options.

**Decision** (from [[02-design-philosophy]] §10): **Reuse `aotir` from MEP-45**. The IR is target-agnostic; both the C and BEAM backends consume it. This saves implementation effort and ensures shared optimizations. The lowering pass from `aotir` to Core Erlang lives in `transpiler3/beam/lower/`, mirroring `transpiler3/c/lower/`.

## 18. Migration strategy: if Core Erlang breaks

If OTP marks `cerl` as truly internal and removes it from the documented API:

1. Move to **Erlang abstract format** (the `erl_parse` AST shape). The `epp_dodger` and `erl_syntax` modules give us a stable manipulation API.
2. Refactor `transpiler3/beam/lower/` to emit abstract format instead of `cerl` records.
3. Use `compile:forms/2` without `from_core`; the default path is abstract format.
4. Re-validate all gates.

Effort estimate: 4-6 weeks for one engineer. The codegen changes are localised to `lower.go` and `lower_*.go`; the type lowering and runtime stay unchanged.

## 19. Migration strategy: if BEAM goes away

If for some reason BEAM is deprecated (extremely unlikely; the platform has 25+ years of momentum and is the basis for WhatsApp, Discord, RabbitMQ, Riak, etc.), Mochi would lean harder on the C target (MEP-45) and add a Wasm target (likely MEP-47).

## 20. Migration strategy: if performance is unacceptable

If real-world benchmarks show BEAM target ≥10x slower than C target on Mochi's reference workloads:

1. Investigate **NIF acceleration** for specific hot paths (binary parsing, integer arithmetic). Use Rustler for safety.
2. Investigate **persistent_term** for shared read-only state.
3. Add a `mochi tune` profiler that reports which operations are slow and suggests refactorings.

The goal is not to make BEAM as fast as C; it's to make BEAM fast enough for its intended use cases (concurrency, distribution, hot reload).

## 21. Open questions for the MEP body

1. **Should we support OTP 26?** Currently no; revisit if OTP 27 adoption is slow.
2. **Should we ship a Mochi-flavored supervisor surface?** Currently no; FFI is the escape hatch. Revisit if users complain.
3. **Should hot reload be a first-class Mochi feature?** Currently no; users opt in via the build system.
4. **Should we publish `mochi` to Hex.pm?** Yes, by Phase 14.
5. **Should we contribute upstream to OTP (e.g. add Mochi-friendly Core Erlang constructors)?** Maybe, if patterns emerge. Coordinate with EEF.
6. **Should we use Gleam's `gleam_otp` as a model for our OTP wrappers?** Yes; cite their patterns. Don't depend on the library.
7. **Should we maintain a Mochi → Gleam translator for ecosystem interop?** No; out of scope.

These questions are answered (where applicable) in the main MEP body.

## 22. Summary risk table

| Risk                          | Likelihood | Impact | Mitigation owner               |
|-------------------------------|------------|--------|--------------------------------|
| Core Erlang API drift         | Low        | High   | Codegen team + EEF liaison     |
| BeamAsm JIT regression        | Low        | Medium | OTP upstream reports           |
| NIF temptation                | Medium     | Medium | PR review + EEP process        |
| Atom exhaustion               | Medium     | High   | mochi_atoms + lint rules       |
| Process leak                  | Medium     | High   | Supervisor invariants          |
| Perf vs C target              | High       | Low    | Document trade-off             |
| AtomVM divergence             | Medium     | Low    | Per-release validation         |
| Diagnostic mismatch           | High       | Medium | Line-chunk + stack rewriter    |

This risk profile is acceptable for a Phase 1 deliverable. Major risks (Core Erlang drift) have low likelihood and known migration paths.

---

## Sources

1. EEF Compiler Workgroup notes. https://erlef.org/wg/compiler
2. OTP 24-28 release notes. https://www.erlang.org/news
3. Rustler. https://github.com/rusterlium/rustler
4. Joe Armstrong, "Why OO Sucks." https://www.cs.kent.ac.uk/people/staff/sjt/TS_Sem/Armstrong_on_OO.html (historical context)
5. Fred Hébert, "Erlang in Anger" §4 (production tuning). https://www.erlang-in-anger.com/
6. WhatsApp scaling reports (1996-2014). https://blog.whatsapp.com/1-million-is-so-2011
