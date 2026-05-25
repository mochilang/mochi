# MEP-46 research note 02, Design philosophy

Author: research pass for MEP-46 (Mochi → Erlang/BEAM transpiler).
Date: 2026-05-22 (GMT+7).

This note records the *why*. It is the design-rationale charter for MEP-46
and explicitly contrasts the BEAM target with the C target (MEP-45). The two
backends share a frontend (parser + type checker), share a correctness gate
(byte-equal stdout vs vm3), and target the same fixture corpus, but their
runtime models, optimisation strategies, and operational profiles are
deeply different. This note states the position MEP-46 takes on each axis.

The TL;DR position:

- **BEAM is the right second target after C** because it complements rather
  than duplicates: C buys distribution shape (single-file native binary)
  and ceiling performance; BEAM buys fault tolerance, hot code reload,
  distribution-transparent concurrency, and a 35-year-proven runtime for
  long-running services. Mochi's stream/agent core was *designed* in the
  actor-model idiom and lands on BEAM with very little impedance.
- **Target Core Erlang, not Erlang source text**, with `compile:forms/2`
  as the entry point. Core Erlang is documented, stable (the most stable
  IR layer past the parser), and the `cerl` module exposes a clean AST
  constructor API. Going through Erlang source costs nothing in
  correctness but adds a parse round-trip and forfeits direct AST control.
- **Reuse OTP wholesale**. Agents are gen_server processes; streams are
  gen_event-shaped hubs; supervision is the supervisor behaviour; storage
  is ETS for in-memory queries and Mnesia for distributed cases (Phase 2).
  The runtime layer is a thin shim, not a re-implementation.
- **Single-file ship target is escript by default, OTP release for
  long-running services**. `mochi build --target=beam-escript` produces a
  self-contained executable runnable with `escript`; `--target=beam-release`
  produces a `relx`-style release tarball with embedded ERTS.
- **Differential testing against vm3 is the master gate**, exactly as in
  MEP-45. vm3 is the recording oracle; the BEAM artefact's stdout must
  diff clean against `expect.txt` for every fixture, on every supported OTP
  major version.

## 1. Why BEAM is the right second target

Mochi's existing surface includes streams, agents, intent calls, and a
deterministic-replay test mode for stream programs. These constructs are
the actor model with mailbox-typed message passing — exactly the model
BEAM has implemented for 35 years and refined through every long-running
telecom, fintech, and chat system in production today (Erlang shipped
1986; Open Telecom Platform 1996; Ericsson AXD301 famously hit nine 9s of
availability through the late '90s; WhatsApp's BEAM-backed messaging
served 900M users on 50 engineers in 2014; Discord's voice and message
infrastructure remains BEAM-backed in 2026; Phoenix LiveView is the
canonical real-time UI framework with millions of concurrent connections
per node).

Concretely, every Mochi `agent T { ... }` declaration maps to one BEAM
gen_server. Every `on T as x { ... }` handler block becomes one process
attached to a stream hub. Every `intent f()` becomes a `gen_server:call`.
The supervision tree, the OTP application lifecycle, the link/monitor
fault-isolation model — none of it has to be invented; we get to inherit
35 years of operational maturity.

For everything else (LINQ queries, ADTs, pattern matching), BEAM is also a
good fit because BEAM is itself a strongly-immutable, single-assignment,
pattern-matching language under the hood, and its compiler is competent
at the optimisations Mochi wants (TCO unconditional in tail position;
pattern compilation via Maranget-equivalent decision trees in the kernel
pass; binary build/match optimised by the BEAM JIT since OTP 24).

The MEP-45 C target solves the *distribution shape* problem (one static
binary, every triple, no runtime dependencies). The MEP-46 BEAM target
solves the *operational profile* problem (long-lived services, fault
tolerance, hot upgrades, distributed clustering). They are
complementary; users pick by deployment context.

## 2. Why Core Erlang, not Erlang source, not BEAM bytecode

Five candidate IR layers (full analysis in note 05):

1. **Erlang source text** — Elixir's choice; Gleam's choice. Forces the
   transpiler to emit syntactically valid Erlang source, parse-roundtrip
   it through `erl_parse`, lose direct control over the AST.
2. **Erlang abstract format** (the tree `erl_parse` produces; documented
   as the type of forms accepted by `compile:forms/2`). One step deeper.
   Stable. The natural target for an AST-driven backend.
3. **Core Erlang** — documented in `cerl` module's reference; stable
   across OTP versions (a 2007 paper defines it; OTP 27 still uses the
   same shape). Cleaner separation of letrec / let / case / try / receive
   than Erlang source.
4. **Kernel Erlang** — internal, undocumented, unstable across OTP
   versions. Not viable.
5. **BEAM assembly (.S) and bytecode (.beam)** — requires us to do our
   own register allocation and instruction selection. Forfeits 30+ years
   of BEAM compiler optimisation work, including the JIT.

MEP-46 picks **Core Erlang** with `cerl_to_icode` and downstream passes
unchanged. Rationale:

- The `cerl` module's constructor API (`cerl:c_module`, `c_letrec`,
  `c_case`, `c_call`, `c_var`, `c_apply`, …) is a clean AST builder.
- Stable across OTP versions; the spec has not had a breaking change
  since the early 2000s.
- Preserves source-level structure for Dialyzer and the kernel pass.
- The BEAM JIT (BeamAsmJIT, OTP 24+, materially improved through OTP
  27/28) gets to do its work; we don't bypass it.
- LFE generates Core Erlang. Caramel generates Core Erlang. Both prove
  the path is real.

The choice is documented in detail in note 05 §3. The runner-up is the
Erlang abstract format (one layer up), which is also a viable target and
is what we fall back to if the `cerl` API exhibits version-specific
breakage (the abstract format is the most-stable layer per OTP's
documented compatibility policy).

## 3. Why reuse OTP wholesale

The Mochi runtime layer for BEAM (`mochi_*` applications) is a *thin
shim*, not a re-implementation:

| Mochi concept | OTP construct used |
|---------------|-------------------|
| agent | gen_server |
| stream hub | bespoke process modelled on gen_event |
| supervision | supervisor (one_for_one for agents) |
| in-memory query state | ETS (set / ordered_set tables) |
| Datalog fact tables | ETS (set) |
| persistent in-memory config | persistent_term (OTP 21.2+) |
| HTTP client (fetch) | gun or hackney (third-party, well-maintained) |
| JSON | OTP 27's stdlib `json` (added 2024) |
| YAML | yamerl (third-party) |
| CSV | hand-rolled binary scanner |
| TLS, crypto | ssl, crypto (stdlib) |
| logging | logger (stdlib) |
| telemetry | telemetry (de-facto standard) |
| OpenTelemetry | opentelemetry_api / opentelemetry (third-party) |
| escript packaging | escript (stdlib) |
| OTP release packaging | rebar3 + relx |

The runtime adds:

- `mochi_runtime` (helpers: format, panic, error-record conversion, type
  guards)
- `mochi_str` (string ops over UTF-8 binaries, layered on `string` and
  `unicode`)
- `mochi_list` (a few helpers around `lists`, mostly pass-through)
- `mochi_map` (helpers around `maps`, plus `omap` insertion-order)
- `mochi_query` (LINQ runtime: group_by, hash_join, sort, set ops)
- `mochi_stream` (stream hubs and subscription protocol)
- `mochi_agent` (gen_server template with the intent/handler dispatch)
- `mochi_datalog` (ETS-backed semi-naive evaluator)
- `mochi_llm` (LLM provider abstraction, HTTP under the hood)
- `mochi_fetch` (HTTP fetch wrapper with JSON decode shim)
- `mochi_ffi_port` (port-based subprocess RPC for Go / Python / TypeScript)
- `mochi_test` (eunit-compatible expect/test driver)
- `mochi_io` (variadic print, with per-type formatter dispatch)

The list above is the entirety of the runtime layer. Total LOC target is
~3000 lines of Erlang for v1 (note 04 closes with a per-module estimate).
This compares favourably with MEP-45's C runtime, which is ~15000 LOC by
the same accounting because C makes us write a GC integration shim, a
scheduler, a Swiss-table implementation, and a fiber library — all of
which BEAM gives us free.

## 4. Why escript for default ship, OTP release for services

Two ship targets:

### 4.1 `mochi build --target=beam-escript`

Produces a single executable runnable by `escript`. The executable is a
shell-script wrapper plus a base64-encoded `.beam` archive plus the
runtime modules. It runs anywhere with an `erl` (Erlang runtime) on the
PATH.

Hello-world size: ~3-5 KB. Realistic-app size: ~50-200 KB. Comparable to
Python script shipping if the Erlang runtime is already on the host;
significantly smaller than a Go binary because we don't bundle ERTS.

The user-facing requirement: install Erlang/OTP on the host. This is
analogous to "install JDK" for Java's `jar` workflow. Most Linux distros
ship OTP in their package manager; macOS has `brew install erlang`;
Windows has the Erlang Solutions installer.

Use case: scripts, batch jobs, ad-hoc programs.

### 4.2 `mochi build --target=beam-release`

Produces a `relx`-style OTP release tarball with embedded ERTS (the full
Erlang runtime baked in). Self-contained; no runtime dependency on the
host. Size: ~30-60 MB (ERTS plus stdlib plus user code). Comparable to a
Go binary.

The release includes:
- A start script (`bin/<app>`).
- The `releases/<vsn>/<app>.rel` and `relup` files for hot upgrades.
- An `erts-<version>/` directory with the embedded VM and stdlib.
- All Mochi-generated `.beam` files plus the `mochi_*` runtime.

Use case: long-running services, production deployment, multi-node
clusters, hot upgrades.

### 4.3 `mochi build --target=beam-component`

Phase 2. Produces a `.tar.gz` package suitable for inclusion in another
OTP application's `deps/`, with no `start.boot` or release machinery.
Lets a host Erlang/Elixir application embed Mochi-generated modules as a
library.

## 5. Why differential testing against vm3 is the master gate

vm3 is the existing reference implementation. Byte-equal stdout from the
BEAM artefact versus vm3, on every fixture, is the strictest behaviour
check available. vm3 is used here only as the recording oracle for
`expect.txt`; the transpiler does not consume any of vm3's IR, runtime,
or codegen. Property tests, fuzzing, and reproducibility are secondary
gates layered on top.

This is the same gate MEP-45 uses; sharing the gate means we share the
fixture corpus and the recorded goldens. A change to a Mochi source file
re-records both targets' `expect.txt` from vm3 in one pass, and both
backends are validated against the same byte sequence.

For BEAM-specific test infrastructure we add:

- A per-OTP-major matrix: OTP 26, 27, 28 (LTS branches). Newer-OTP
  features may not exist on older releases; we cap features at the
  OTP-26 floor for v1.
- A Dialyzer pass on the emitted Erlang. The pass must complete with no
  warnings of severity ≥ `warn_return_no_exit`. Achieving this requires
  the codegen to emit accurate `-spec` declarations from Mochi types,
  which it does (note 06 §15).
- An `eunit` pass on the test functions emitted from Mochi `test`
  blocks. Equivalent to MEP-45's `--debug` test runner.
- A `common_test` integration in Phase 2 for the stream/agent test
  fixtures, which need deterministic replay across multiple processes.

## 6. Why a separate IR is unnecessary

MEP-45 introduces `aotir`, its own lowering IR, because C codegen
benefits from explicit closure conversion, explicit match-to-decision-
tree expansion, explicit setjmp insertion at try sites, and explicit
monomorphisation. None of these survive into the C output unchanged.

For BEAM, every one of those passes is either unnecessary or handled by
the BEAM compiler itself:

- **Closure conversion**: BEAM funs are first-class with native capture.
  No conversion needed.
- **Match-to-decision-tree**: BEAM kernel pass does this from `case`
  expressions. We emit the `case`.
- **Setjmp insertion**: BEAM has native `try`. We emit the `try`.
- **Monomorphisation**: BEAM is dynamically typed; there is nothing to
  monomorphise. We emit one function clause; BEAM dispatches at runtime.

Therefore MEP-46 has no equivalent of `aotir`. The codegen is one pass:
typed AST → Core Erlang AST (via `cerl` constructors). The pipeline is
shallower than MEP-45's by three passes.

This is deliberate. Mochi's frontend (parser + type checker) already does
the heavy lifting. The BEAM compiler does the optimisation work
downstream. The transpiler is the thin glue layer between.

## 7. Why not just compile to Elixir or Gleam

A reasonable alternative would be to lower Mochi to Elixir source and let
the Elixir compiler do the rest. This was considered and rejected:

- **Elixir has its own type system** (currently set-theoretic types,
  Phase 2 of José Valim's incremental typing). Mochi's types would have
  to be reconciled, losing strictness.
- **Elixir macros** would tempt us to encode Mochi forms as Elixir
  macros, but the macro layer adds a parse / expand round-trip that
  hides codegen bugs from Mochi error messages.
- **Distribution shape**: Elixir adds the `Mix` build system as a hard
  dependency. The escript path is more cumbersome.
- **Performance**: an extra compilation layer slows iteration.

Gleam similarly: a fine language, statically typed, but compiling
Mochi → Gleam source → Erlang adds a layer we don't need.

Going straight to Core Erlang via the `cerl` module is one fewer
layer than either alternative, and gives us byte-identical control over
the emitted code.

## 8. Why BEAM is *not* the right primary target

Symmetric to §1: things BEAM cannot do that the C target can.

- **CPU-bound performance**. BEAM ships a JIT (BeamAsmJIT, OTP 24+) that
  closes much of the gap, but tight numeric loops still trail native C
  by 2-5x in 2026. Mochi's MEP-39 work pushed vm3 to 3-5x of Go; a BEAM
  artefact lands in roughly the same band. The C target promises 1.5x of
  hand-written C; BEAM cannot match this.
- **Memory footprint**. ERTS plus stdlib is ~30 MB baseline; even a
  hello-world OTP release weighs ~30 MB. The C target's hello-world is
  ~3 MB. Embedded and resource-constrained environments need C.
- **Single-file ship without an Erlang prerequisite**. The escript path
  needs `erl` on the host. The release path embeds ERTS but balloons to
  30+ MB. The C target's static binary needs nothing.
- **Cross-architecture coverage outside BEAM's ports**. BEAM runs on
  most platforms but not all (e.g. niche embedded RTOS without a POSIX
  layer). The C target via `zig cc` reaches further (e.g. armv6 with
  newlib).
- **WASM**. BEAM's WASM story (via AtomVM-WASM, lumen-style projects,
  experimental ports) is immature in 2026. The C target → wasm32-wasi is
  production-ready.

The two targets are *complementary*. Users with operational profile
needing 24/7 uptime, hot upgrades, distribution, and high-concurrency
fan-out pick BEAM. Users needing portable native binaries, tight CPU
performance, and embedding into existing native systems pick C. Many
non-trivial Mochi programs will eventually ship both.

## 9. Why this is not "just transpile to Erlang"

A common shorthand for the project is "transpile to Erlang." That's
broadly accurate but obscures three load-bearing design choices:

1. **Target Core Erlang via `cerl`**, not Erlang source text. The choice
   is non-trivial; see note 05 §3.
2. **The runtime layer is a Mochi-controlled OTP application**, with
   ~12 modules in the `mochi_*` namespace. Without it, Mochi's
   higher-level features (queries, streams, agents) have no place to
   land. See note 04.
3. **The build driver owns the ship-format story** (escript, release,
   component), the cache, the cross-OTP-version matrix, and the
   reproducibility gate. See note 10.

"Transpile to Erlang" without these three is a toy. MEP-46 specifies all
three.

## 10. Position on hot code loading

Hot code loading is the BEAM feature that defines its operational
profile. It is also a feature Mochi has never promised at the language
level. The position MEP-46 takes:

- **v1 does not expose hot reload as a Mochi language feature.** Users
  who care can rebuild and `relup` themselves via the underlying OTP
  release machinery; the emitted code is hot-loadable by construction
  (we don't violate any module-versioning rules), but Mochi has no
  syntactic surface for it.
- **v2 may add a `live` modifier** on functions and agents that explicitly
  opts in to hot-reload-safe code generation (no module-local state in
  closures captured before reload; intent dispatch through the module
  attribute, not direct call). This is research; do not commit to v1.

Mochi programs that rely on the BEAM's hot-reload behaviour at v1 do so
at their own risk; the byte-equal-vs-vm3 gate cannot validate hot reload
because vm3 has no equivalent feature.

## 11. Position on Dialyzer

Dialyzer is the BEAM-side success-typing analyser. Mochi's frontend
already does strict type checking, so Dialyzer is redundant for
*correctness*. It is still useful for:

- Catching bugs in the *generated* code (transpiler bugs that produce a
  Mochi-correct-but-Erlang-wrong shape).
- Validating the `-spec` declarations the codegen emits.
- Giving Erlang/OTP users who consume Mochi modules a sanity check via
  the standard toolchain.

The gate is: Dialyzer must complete on the generated code with zero
warnings, run as part of the CI matrix. This is documented as a
secondary gate in note 11 §4.

## 12. Position on Elixir interoperability

Elixir is the dominant BEAM language by usage. A Mochi module compiled
to a `.beam` is callable from Elixir via standard module syntax:

```elixir
:my_mochi_module.my_function(args)
```

For this to be ergonomic, the Mochi codegen must:

- Use atom keys in maps where Elixir conventions expect them (Mochi
  records optionally lower to `%Struct{}`-shaped maps when the
  `@elixir_struct` annotation is present; default is tagged tuples).
- Emit `-spec` declarations so the Elixir Language Server can type-check
  call sites.
- Provide a `mix` task (`mix mochi.compile`) that drives the Mochi build
  from an Elixir project. Phase 2.

v1 does not aim for "Elixir feels native" interop; it aims for "Elixir
can call Mochi". The deeper integration is Phase 2 (note 04 §16).

## 13. Position on AtomVM

AtomVM is a minimal BEAM re-implementation for microcontrollers (ESP32,
STM32, RPi Pico). It supports a subset of BEAM and the standard
libraries.

MEP-46 v1 does not target AtomVM. Phase 2 may add a `--target=atomvm`
flag that:

- Restricts the runtime to AtomVM-supported OTP modules (no `httpc`,
  no `ssl`, limited `string`).
- Disables features that require unsupported BIFs (LLM call, fetch).
- Cross-compiles via the AtomVM build flow.

This is recorded for completeness; it is not in scope for the main 19-phase
plan.

## 14. Summary of position

MEP-46 is a focused, complementary target that:

- Inherits the parser, type checker, and fixture corpus from the shared
  Mochi frontend.
- Picks Core Erlang via `cerl` as the IR layer, for stability and
  optimisation preservation.
- Reuses OTP wholesale, with a thin `mochi_*` runtime application.
- Ships via escript (default) or OTP release (services).
- Validates against vm3 byte-equal as the master gate, with Dialyzer and
  eunit as secondary gates, across an OTP version matrix.
- Does not duplicate MEP-45's distribution-shape story; complements it.

The next eleven notes flesh out each axis of this position.
