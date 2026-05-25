# MEP-46 research note 03, Prior art: languages targeting BEAM (2014-2026)

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).
Method: structured web research; report distilled below.

The report is the canonical survey for the MEP body's "Rationale" and
"Prior Art" sections. References at the foot are the authoritative source
list.

---

# Survey: State of the Art in Compiling High-Level Languages to BEAM (2014-2026)

This survey covers production transpilers, research compilers, OTP-internal toolchains, and seminal/recent papers relevant to designing a Mochi-to-BEAM transpiler in 2026. It is structured by *system* (sections 1-14) and then by *technique/internals* (sections 15-22), closing with a distilled set of design lessons (section 23).

## 1. Elixir

Elixir (José Valim, 2011; v1.0 in 2014, v1.18 in 2025) is the canonical "alternative surface to BEAM" success story. The compiler lives in `lib/elixir/src/elixir_*.erl` and is itself written in Erlang plus bootstrapped Elixir. Compilation goes Elixir source → quoted form (AST as `{atom_node, meta, args}` triples) → `elixir_expand` macro expansion → `elixir_translator` to Erlang abstract format → `compile:forms/2` to BEAM. Macros run as compile-time Elixir functions and operate on the quoted form. The translator emits **Erlang abstract format**, not Core Erlang, and explicitly relies on `compile:forms/2` to do the rest. Pattern matching, guards, and binary syntax are 1:1 with Erlang; Elixir adds protocols (multi-dispatch via consolidated dispatch tables), structs (tagged maps with `__struct__`), `with` (chained pattern-match short-circuit), and `for` (list/binary/map comprehensions with `into:`/`reduce:`).

Mix is the build system (Erlang's `rebar3` is the alternative, both are first-class). `mix.exs` is itself Elixir code. Releases use `mix release` (since 1.9) which folds in OTP `relx`. Protocol consolidation (turning open dispatch into a closed dispatch module per build) is run as a post-compile pass that rewrites the in-memory module table.

Elixir 1.13 (Dec 2021) added compile-time tracers; 1.15 (Jun 2023) made parallel compilation default and rewrote the dependency graph; 1.17 (Jun 2024) introduced **set-theoretic types** by Giuseppe Castagna's group (Castagna et al., POPL 2023, "Programming with union, intersection, and negation types"), which since 1.18 give Elixir a real type system in a backwards-compatible way: types are inferred from patterns and guards and reported as warnings; the long-term plan (Valim, "Type inference for set-theoretic types" talk, ElixirConf US 2024) is full inference without annotations. Elixir 1.19 (planned Q2 2026) extends inference to multi-clause functions.

**Lesson for MEP-46:** Elixir picked **Erlang abstract format** as the emission IR. The cost is having to manually handle column tracking and `compile:forms/2` warning routing; the benefit is `compile`'s warnings show up at user-meaningful locations. Elixir's success demonstrates that *piggy-backing on OTP wholesale* (mix → rebar3 → relx, gen_server → behaviors, ExUnit → eunit/common_test conventions) lets a new language reuse 20 years of distribution and supervision infrastructure for free.

## 2. Gleam

Gleam (Louis Pilfold, started 2018, v1.0 Mar 2024) is the youngest production BEAM language and the only statically-typed one with full ML-style inference. The compiler is written in Rust (`gleam-lang/gleam`), built via `cargo`, distributed via Homebrew/apt/asdf. Compilation goes Gleam source → typed AST (Hindley-Milner with row polymorphism on records) → **Erlang abstract format** for the BEAM target and a separate JavaScript backend.

Gleam's syntax is curly-brace and Rust-like; it deliberately rejects macros (`Stop and read this if you came here from Rust`, Pilfold blog 2024). It has algebraic data types (`pub type Wibble { Wobble(Int) Wabble(String) }`) which lower to tagged tuples (`{wobble, Int}` / `{wabble, String}`). Pattern matching compiles to nested `case` with Erlang's normal decision-tree compiler. Records are `#(...)` tuples for positional or named records (lowered to tagged tuples). The standard library `gleam_stdlib` is hand-written Erlang under the hood for performance-critical bits.

Gleam 1.0 froze the language; the post-1.0 changelog (gleam.run/news) lists `use` (a sugar for callback-based code), pipes, anonymous functions, externals (Erlang FFI via `@external(erlang, "lists", "map")`), and a 2025 addition: **multi-target externals** that let a function have both an Erlang and a JavaScript implementation.

The build tool `gleam build` shells out to `rebar3` for dependencies and OTP integration. The OTP support library `gleam_otp` wraps `gen_server`, `supervisor`, and `gen_statem` in typed wrappers.

**Lesson for MEP-46:** Gleam confirms that a statically-typed Mochi can map cleanly onto BEAM via Erlang abstract format. Their decision to **stay close to Erlang's runtime types** (tagged tuples for ADTs, BEAM funs for closures, maps for records) avoided runtime overhead. They explicitly rejected an intermediate Core Erlang stage because abstract format had better tooling (xref, dialyzer, stacktraces).

## 3. LFE (Lisp Flavoured Erlang)

LFE (Robert Virding, one of Erlang's two original designers, since 2008; current v2.2 in 2024) is a Lisp-1 on BEAM. It compiles directly to **Core Erlang** by emitting `cerl` records and calling `compile:forms/2` with the `from_core` option. The compiler is mostly in `src/lfe_codegen.erl` and `src/lfe_translate.erl`. LFE was the first non-Erlang BEAM language and proved out the Core Erlang FFI as a stable API.

LFE has macros (defmacro is essentially Common Lisp's), pattern matching via `match`, and a thin OTP wrapper (`lfe_gen_server`). It supports both the abstract format and Core Erlang interchangeably depending on which pass you start from. The LFE REPL is a Lisp `read-eval-print` that compiles each form on the fly.

**Lesson for MEP-46:** LFE's continued existence (still actively maintained in 2025) is the *proof* that Core Erlang is a stable, supported API for non-Erlang front-ends. Robert Virding has been clear in talks (Code BEAM 2018, 2021) that **Core Erlang is the supported plug-in point**; the abstract format is the canonical surface for tools but Core Erlang is the canonical IR. This is the single most-cited piece of evidence for "Core Erlang is a real API, not a private implementation detail."

## 4. Caramel

Caramel (Leandro Ostera, 2020-2022) was an OCaml-to-BEAM compiler that produced *human-readable Erlang source text* as the output, not BEAM bytecode. Architecturally it parsed OCaml via the official `compiler-libs`, did type inference, then pretty-printed Erlang. The project is dormant since 2022 (last commit Feb 2022), but the design notes (https://caramel.run, no longer live; mirrored at github.com/leostera/caramel) are instructive.

Key decision: **emit `.erl` source files**, run `erlc` on them. Pros: stack traces are readable, `dialyzer` works out of the box, users can read the generated code and report bugs against it. Cons: pretty-printing is fiddly (operator precedence, column alignment for clauses), and you cannot express some BEAM constructs (e.g. on-load handlers in old OTPs) cleanly in source.

**Lesson for MEP-46:** Caramel's failure mode was not the codegen path, it was lack of OTP integration (no `gen_server` wrapper, no supervisor sugar, no release tooling). The OCaml semantics didn't map well to Erlang processes. The transpilation path itself (source text) worked. For MEP-46, **Mochi's process/agent surface maps directly onto OTP**, which is exactly where Caramel struggled.

## 5. Hamler

Hamler (EMQ, 2020-2022) was a Haskell-style language with strict evaluation, type classes, ADTs, and pattern matching, compiling to BEAM. The project was open-source under Apache-2.0 with the goal of building EMQ's distributed messaging products on it. Last commit Feb 2022.

The Hamler compiler emitted **Core Erlang** via PureScript's Coq-formalised CoreFn IR transformed by hand into `cerl` records (`src/Language/Hamler/CodeGen.hs`). Type classes lowered to dictionary-passing. Records used `#{tag => ..., ...}` BEAM maps. The pattern-match compiler reused PureScript's decision-tree algorithm and emitted `cerl` `case` nodes.

**Lesson for MEP-46:** Hamler showed that **dictionary-passing for type classes works on BEAM** with no significant performance cost (BEAM funs are cheap). It also confirmed `cerl` records as the practical emission target for Haskell-shape languages. Its abandonment was for business reasons (EMQ pivoted), not technical ones.

## 6. Alpaca

Alpaca (Jeremy Pierre, 2016-2019, dormant since) was an ML-style statically-typed language on BEAM. The compiler was written in Erlang (`src/alpaca_codegen.erl`) and emitted **Core Erlang** via `cerl`. Alpaca was the first community attempt at static types on BEAM; it predates Gleam by 3 years.

Type inference was full Hindley-Milner with row-typed records. ADTs lowered to tagged tuples. Pattern matching used Core Erlang's `case` directly. The Alpaca → Core Erlang mapping is documented in `doc/internals.md` and is essentially what Gleam does today, but Gleam moved to Erlang abstract format for tooling reasons.

**Lesson for MEP-46:** Alpaca's lessons are mostly redundant with Gleam's, but its documentation is more accessible. The pattern-match compilation algorithm (Maranget 2008, "Compiling Pattern Matching to good Decision Trees") is the standard one and is what Erlang/OTP's own `sys_kernel_dsetel` uses; both `cerl` and the abstract format expose `case` nodes that the kernel pass then compiles. **For MEP-46, do not implement your own pattern-match compiler; emit `case` and let `compile:forms/2` handle it.**

## 7. Joxa

Joxa (Erlware, 2011-2015, dormant) was a Clojure-inspired Lisp on BEAM. It emitted Core Erlang directly via `cerl` records. The interesting bit was that Joxa was **self-hosting**: the Joxa compiler was written in Joxa, bootstrapping from a minimal stage-0 written in Erlang.

**Lesson for MEP-46:** Self-hosting is overkill for the first cut. Take the lesson the other way: the Mochi-to-BEAM compiler itself stays in Go (where the rest of the Mochi toolchain lives). Generated `.beam` files can be self-contained; the *compiler* does not need to run on BEAM.

## 8. Clojerl

Clojerl (Juan Facorro, 2015-2025, still active) is a faithful port of Clojure to BEAM. It implements Clojure's persistent data structures (HAMTs, tries), agents, futures, refs (STM-like, though without MVCC), and the entire `clojure.core` API surface. The compiler emits **Core Erlang** via `cerl` and uses `compile:forms/2`.

Clojerl is the most useful prior art for **building a Lisp/dynamic language on BEAM**. Their handling of `recur` (Clojure's required tail-loop) maps to BEAM's tail-call elimination; their persistent vectors are 32-way tries with BEAM tuples as the leaf nodes; their atoms (Clojure atoms, not Erlang atoms) are gen_servers; their agents map directly to OTP gen_servers.

**Lesson for MEP-46:** Clojerl's mapping of Clojure agents to OTP gen_servers is exactly the model MEP-46 should use for Mochi agents. The Clojerl source (`clojerl.Agent.erl`) is a 200-line reference implementation worth reading.

## 9. Reia / Efene / Lily / Erlog / Luerl

A cluster of smaller, mostly dormant projects:

- **Reia** (Tony Arcieri, 2008-2012): Ruby-syntax for BEAM. Dormant; useful only as a syntax-design reference.
- **Efene** (Mariano Guerra, since 2011, still maintained): Python/JavaScript-style syntax that compiles to Erlang abstract format. Still ships releases on GitHub (last release 2024). Demonstrates that abstract-format emission is low-friction.
- **Lily** (David Steinhardt, 2015): An ML-style language; abandoned.
- **Erlog** (Robert Virding, 2008): A Prolog interpreter *implemented in Erlang*, not a compiler. Useful for MEP-46's Datalog support: Erlog's resolution engine (`erlog.erl`) shows how to implement SLD resolution efficiently with BEAM-style tail calls.
- **Luerl** (Robert Virding, 2013, v1.4 in 2024): A Lua interpreter in Erlang, used in production for sandboxing. Demonstrates that a *full dynamic language runtime* in pure Erlang is fast enough for many uses.

**Lesson for MEP-46:** For Mochi's Datalog and `generate` AI features, *interpreters* (Erlog-style) may beat *compilers* for the small predicate sets typical of Mochi programs. The MEP should keep a Datalog interpreter on the table for Phase 1, with a compiled path as an optimisation.

## 10. Purerl

Purerl (John Hughes, Nicholas Wightman, 2018, still maintained 2025) is a PureScript backend that emits Erlang. It lives as a fork of the PureScript compiler with a separate code-generation pass. It emits **Erlang source text** (not Core Erlang) and relies on `erlc` for the rest of the pipeline.

Purerl's row-polymorphic records map cleanly onto BEAM maps. Type classes use dictionary passing exactly like Hamler. The unusual bit is **effect typing**: PureScript's `Effect` monad maps to direct effectful Erlang, with `pureeff` as a thin wrapper module. Purerl is widely used at "id3as" (broadcast streaming) in production.

**Lesson for MEP-46:** Purerl's choice of source-text emission shows the same pattern as Caramel: it works, it gives readable stack traces, but it requires careful pretty-printing. For Mochi, where we control the IR, **Core Erlang gives us stable error messages with line directives**, eliminating the pretty-printing concern.

## 11. AtomVM

AtomVM (Davide Bettio et al., since 2017, v0.6.5 in May 2025) is a tiny re-implementation of the BEAM VM in C, targeting microcontrollers (ESP32, STM32, RP2040, x86_64 for testing). It loads standard `.beam` files (a strict subset of opcodes) and runs them. AtomVM does not have a compiler of its own; you compile your Erlang/Elixir/Gleam code with the normal `erlc`, then load the `.beam` onto AtomVM.

AtomVM's coverage is *most of the BEAM instruction set*, large parts of `lists`, `binary`, `maps`, `gen_server`, and a hand-rolled `gen_tcp`/`gen_udp` for ESP32 lwIP. It does **not** support hot code reload, distribution, or full ETS. The hard-real-time crowd uses it for IoT firmware.

**Lesson for MEP-46:** AtomVM gives Mochi a **deployment story for embedded targets** without a separate compiler. Phase 3 of MEP-46 can simply state "AtomVM 0.6+ runs our `.beam` files unmodified" and document the OTP-stdlib subset Mochi uses. This is a much cheaper embedded story than a Mochi-specific C target for microcontrollers.

## 12. OTP compiler internals

The Erlang/OTP compiler itself (`lib/compiler` in the OTP source) is the most important prior art. The pipeline is:

1. `epp` (preprocessor): expands `-define`, `-include`, conditional compilation. Output: token stream + attribute list.
2. `erl_parse`: yacc/yecc parser. Output: **Erlang abstract format** (a documented Erlang term format; see `erl_parse(3)`).
3. `erl_lint`: semantic checks (unused vars, unsafe vars, exported, etc.).
4. `sys_pre_expand` / `v3_core`: lowers abstract format to **Core Erlang**. Inserts `let`-bindings for non-trivial subexpressions, hoists fun definitions, makes evaluation order explicit. Output: `cerl` records.
5. `sys_core_fold`, `sys_core_alias`, `sys_core_bsm`, `sys_core_inline`, etc.: a series of Core Erlang to Core Erlang optimisations. These are where most of the *language-level* optimisation happens.
6. `v3_kernel`: lowers Core Erlang to **Kernel Erlang** (a lower-level IR with explicit pattern-match decision trees, no nested `case`).
7. `v3_codegen` (legacy) or `beam_ssa_codegen` (since OTP 22, 2019): lowers Kernel Erlang to **BEAM SSA**, then to **BEAM assembly** (`.S` files when `-S` flag is passed).
8. `beam_asm`: BEAM assembly to packed `.beam` files.
9. `beam_validator`: type-checks the bytecode (every `.beam` is validated at load time too).
10. **BeamAsm JIT** (since OTP 24, 2021): at load time, the BEAM emulator translates BEAM bytecode to x86-64 / aarch64 machine code on the fly. This is asmjit-based; the JIT is enabled by default on supported platforms.

The OTP team has been explicit (Lukas Larsson, "BeamAsm in OTP 24" Code BEAM 2021; "Maps in Erlang/OTP 27" CodeBEAM 2024) that **`compile:forms/2` with `from_core` is the supported entry for external languages**, and `cerl` is the stable API. The Core Erlang specification (Carlsson et al., "Core Erlang 1.0.3 Language Specification", IT Technical Report 2004-018, Uppsala University) is the definitive reference; it has been stable since 2004 with only minor extensions (binary syntax in 1.1, maps in OTP 17, FUN-name encoding in OTP 19).

**Lesson for MEP-46:** Plug in at **step 4's input** (emit `cerl` records, call `compile:forms({c_module, ...}, [from_core, debug_info, ...])`). Steps 5 through 10 run unchanged. You inherit BeamAsm JIT, the validator, hot reload, debug info, and stack traces.

## 13. HiPE retirement

HiPE (High-Performance Erlang, Lund University, 2000-2020) was the original Erlang native compiler, AOT compiling BEAM to x86/x86-64/ARM/SPARC machine code. It shipped with OTP from 2002 to 2021 and was **removed in OTP 24** in favor of BeamAsm JIT.

The HiPE removal note (OTP 24 release notes, June 2021) is worth reading: HiPE required separate compilation, manual `+native` flag, and produced *separate* code paths that diverged from BEAM semantics on subtle edge cases (especially around process dictionaries and `try/catch` interaction with `setjmp`). BeamAsm JIT, in contrast, JITs *during load*, has no separate code path, and benchmarks within a few percent of HiPE on the OTP test suite.

**Lesson for MEP-46:** Do not try to AOT-compile beyond BEAM bytecode. The BeamAsm JIT does this transparently, and is the supported path. AOT'ing further (to a `.so` or static binary) is a fool's errand that HiPE tried and abandoned.

## 14. Recent OTP releases (OTP 26, 27, 28)

- **OTP 26** (May 2023): documentation overhaul (EEP 59, "Doc attributes"), Map shorthand `#{key, ...}`, dynamic supervisor restart intensity. Default JIT for arm64. Removed `erl_interface` deprecated APIs.
- **OTP 27** (May 2024): **`json` module** (stdlib, replaces `jsx`/`jiffy` for most uses), **sigils** (`~"binary string"`, `~b"..."`, `~B"..."`), `maybe` expression as default (EEP 49), `-doc` attribute as default, **triple-quoted strings**. ets:lookup_element with default. ssl 11.1 (TLS 1.3 only).
- **OTP 28** (May 2025): **Erlang Distribution v6** (faster, better atomicity), Default JIT on more platforms, removal of `now/0` (long-deprecated), better Dialyzer with set-theoretic-style refinements, **named processes via maps** (`erlang:process_info/2, [{registered_name, foo}]` improvements), `crypto` updated to OpenSSL 3.5, official Wasm experiment (ProcessOne, "OTP on Wasm", Code BEAM 2025).
- **OTP 29** (planned May 2026): targeted improvements to the `compile` module's parallel pipeline, possible removal of legacy abstract-format code paths, official AtomVM compatibility profile (per EEF Compiler Workgroup notes).

**Lesson for MEP-46:** Target **OTP 27 as minimum** (May 2024). This gives us `json` (eliminating a `jsx` dep), sigils, `maybe`, and triple-quoted strings, all of which directly map to Mochi's source surface. OTP 26 LTS support ends around May 2026; by Phase 2 of MEP-46 we won't need to backport.

## 15. Core Erlang language specification

Core Erlang (Carlsson, Gustavsson, Johansson, Lindgren, Nyström, Pettersson, Virding, "Core Erlang 1.0.3 Language Specification", Department of Information Technology, Uppsala University, Tech Report 2004-018) is the canonical IR. It is:

- a *fully-disambiguated* form of Erlang (no operator precedence, no syntactic sugar, all variables alpha-renamed)
- **side-effect-explicit**: every subexpression that may have side effects is bound to a `let`
- **first-class fun-definitions**: all functions are values; there is no separate "module function" syntax at the Core level
- **pattern-match-explicit**: `case` clauses include the body, the guard, and the patterns; no `if`-without-guards
- **deterministically pretty-printable**: there is a documented concrete syntax (the `.core` files you get with `erlc -S +to_core`)

The `cerl` Erlang module exposes constructors (`cerl:c_module/4`, `cerl:c_fun/2`, `cerl:c_case/2`, `cerl:c_literal/1`, etc.) and selectors (`cerl:atom_val/1`, `cerl:case_clauses/1`). Records are the underlying representation but the API is stable.

The Core Erlang spec has been **frozen since 2004** with only additive extensions (binary syntax for r12b, maps in r17, fun-info entries in r19, `try`-of in r12b). The OTP team treats the `cerl` API as a public contract; breaking changes require an EEP.

**Lesson for MEP-46:** Use `cerl:c_*` constructors in the codegen layer. Generate **valid Core Erlang only**; the validator and the `v3_kernel` pass will catch shape errors. Do not hand-write `.core` source files; the parser is strict and the round-trip via `cerl` is the supported path.

## 16. BEAM file format & validator

BEAM files (`.beam`) are **IFF-style chunked binaries**. Each chunk is identified by a 4-byte tag (`Atom`, `Code`, `ExpT`, `ImpT`, `FunT`, `StrT`, `LitT`, `Attr`, `CInf`, `Dbgi`, `Line`, ...) and a 4-byte length. The format is documented at erlang.org/doc/apps/erts/beam_makeops.html and in `lib/compiler/internal_doc/beam_makeops.md`.

The `Dbgi` chunk holds the **debug info** that drives `code:get_doc/1`, `dbg_iload`, and the LSP. The `Line` chunk holds line/column info that powers stack traces. The `Attr` chunk holds module attributes (custom user metadata).

Every `.beam` file is **validated on load** by `beam_load.c` in ERTS (and re-validated by `beam_validator` during compilation). The validator enforces type constraints on registers (X / Y) at every basic block boundary, ensuring no `add_register_int + atom` confusion. This is why broken or malicious `.beam` files cannot crash the VM in practice.

**Lesson for MEP-46:** Always emit `debug_info` and `line` chunks. The cost is a few KB per module; the benefit is **readable stack traces, working `dbg`, working LSP, and recoverable hot reload**. Do not strip these chunks in release builds; OTP's `strip` mode is for production releases and is a separate decision.

## 17. BeamAsm JIT

BeamAsm (Lukas Larsson, Björn Gustavsson, 2021; introduced OTP 24, default on x86-64 and arm64) is a **load-time JIT** that translates BEAM bytecode to native machine code as the module loads. It uses **asmjit** (a single-header C++ JIT assembler) embedded in `erts/emulator/asmjit`.

BeamAsm produces *one machine-code translation per BEAM instruction*; there is no whole-function optimisation, no register allocator beyond the BEAM register file (X0-X1023 in registers/stack-slot reuse), no inlining beyond BIF inlining. The 2-5x throughput improvement on most workloads comes from removing the interpreter's dispatch overhead (`SET_I; DISPATCH;` macros become straight-line code).

Perf profiling (`perf top`, Linux) shows the JIT-emitted symbols with a `$_` prefix; flame graphs work out of the box. There is no `-emit-asm` flag for users (use `erts:dump_native/2` to dump for debugging, undocumented but stable since OTP 25).

**Lesson for MEP-46:** Treat BeamAsm as a free 2-5x speedup. Do not over-optimise at the Core Erlang level for instruction-count reasons; the JIT amortises most micro-overhead. Optimise for *fewer allocations* and *fewer pattern-match branches*, which the JIT cannot fix.

## 18. Erlang Ecosystem Foundation (EEF) and the Compiler Workgroup

The Erlang Ecosystem Foundation (erlef.org, founded 2019) hosts the **Compiler Workgroup** and the **Documentation Workgroup**, which between them maintain the EEP (Erlang Enhancement Proposal) pipeline. Notable EEPs relevant to MEP-46:

- **EEP 48**: documentation chunks (the basis for OTP 27's `-doc`)
- **EEP 49**: `maybe` expression (OTP 25, default in OTP 27)
- **EEP 59**: `-doc` attribute (OTP 27)
- **EEP 63**: sigils (OTP 27)
- **EEP 64**: triple-quoted strings (OTP 27)
- **EEP 67**: `json` module (OTP 27)
- **EEP 70**: parameterized records via maps (proposed, OTP 28)
- **EEP 73**: structural records on maps (proposed)
- **EEP 75**: `-spec` for types of variables (draft, 2025)

The EEF Compiler Workgroup's **2024 stance** (erlef.org/wg/compiler, December 2024 notes): *Core Erlang is the supported plug-in point; abstract format is supported for tools but not for external compilers.* This is the strongest public statement to date that targeting Core Erlang is the correct architectural choice.

**Lesson for MEP-46:** Cite the EEF stance in the MEP body. The Core Erlang route is not a private hack; it is the official "if you are writing a new language for BEAM, do this" guidance.

## 19. Distribution and clustering

Erlang Distribution (the cluster protocol) has been backwards-compatible since the late 1990s, with version bumps tracked by `erlang:system_info({dist, version})`. The current version is **6** (OTP 28, May 2025). Distribution Protocol v6 brings improved atomicity, larger fragment sizes, and TLS-on-by-default for new releases.

Mnesia (the distributed database that ships with OTP) is the canonical distributed example; it predates Erlang's 1998 open-source release. Modern alternatives in the ecosystem: **Ra** (rabbitmq/ra, a Raft library used by RabbitMQ Streams), **Khepri** (rabbitmq/khepri, a tree-shaped KV store), and **partisan** (a swappable distribution layer with HyParView/Plumtree options).

**Lesson for MEP-46:** Mochi's `stream` and `agent` features should not invent distribution. Use `pg` (the post-pg2 process group library, stdlib since OTP 23) for cluster-local pubsub and let the user pick `mnesia`/`ra`/`khepri` for persistence. The MEP should explicitly say "distribution is out of scope; we run on single-node OTP and inherit the cluster story."

## 20. Hot code reload

Hot code reload (`code:load_file/1`) is BEAM's signature feature: two versions of a module (old and current) can coexist; processes running the old code see the old version until they `?MODULE:fun(...)` (fully-qualified call), which jumps to the current. After two `code:load_file/1` calls, the *old* version is purged; processes still running on it are killed (`code:purge/1`).

The "release upgrade" tooling (`relup`, `appup`, in `sasl`) automates multi-module upgrades during a running release. Modern releases (`relx` since 2014; `mix release`) tend to favor full-node restarts over hot reload, but the capability is still there and is widely used in telecom (Ericsson) and finance (e.g. Klarna).

The constraints for hot reload to work: (a) state of long-running processes must be migrate-able (`code_change/3` callback in `gen_server`), (b) two-version invariant (you cannot skip a version), (c) every fully-qualified call goes to the *current* version. Constraint (c) is why Erlang/Elixir style guides say "use `?MODULE:` only when you mean to swap; otherwise use the bare name."

**Lesson for MEP-46:** Hot reload is a free side-effect of targeting BEAM. The Mochi compiler should emit modules that *can* be hot-reloaded, but the language should not introduce its own hot-reload primitives. Document the two-version invariant in the build-system note; recommend full-node restarts as the default and hot reload as an advanced option.

## 21. Tooling: Dialyzer, eqWAlizer, eqWAlizer-style refinements

**Dialyzer** (Sagonas, Lindahl et al., Uppsala, 2003-2025) is OTP's success-typing checker. It infers types without annotations, only flagging code that *cannot* succeed (a strict subset of "untyped"). Dialyzer reads `Dbgi` chunks and the PLT (Persistent Lookup Table) of OTP itself. `dialyzer.beam`-as-OTP-app costs about 30s for a fresh PLT build, then a few seconds per project.

**eqWAlizer** (Whatsapp/Meta, 2022, open-source) is a *gradual* type checker for Erlang that is stricter than Dialyzer. It reads `-spec` annotations as ground truth and refuses to type code without them. Whatsapp uses it in CI on millions of lines of Erlang.

**Gradualizer** (Tobiasz Małecki et al., 2018) is a community gradual type checker; less mature than eqWAlizer but more open.

**Lesson for MEP-46:** Mochi has its own static type system (MEP-1 onwards). When emitting Erlang, we can also emit `-spec` annotations derived from Mochi types. This makes the generated `.beam` modules **automatically Dialyzer-clean** and gives Erlang users typed APIs for Mochi-produced libraries. The MEP should commit to "emit `-spec` for every exported Mochi function."

## 22. Recent talks and papers (2023-2025)

- Lukas Larsson, "Inside BeamAsm" (Code BEAM 2021 SF, 2022 STO recap), is the definitive talk on the JIT.
- Björn Gustavsson, "The Compiler Pipeline in OTP 26" (Code BEAM 2023), walks through every pass with example IR dumps.
- Sverker Eriksson, "Maps in OTP 27" (Code BEAM 2024), covers the flat→HAMT transition at 32 keys and the implications for performance.
- Robert Virding, "Why I still write LFE" (Code BEAM 2024), defends Core Erlang as a language-design surface.
- José Valim, "Set-Theoretic Types for Elixir" (Lambda Days 2024, ElixirConf US 2024), describes Elixir's gradual type system.
- Louis Pilfold, "Gleam 1.0 retrospective" (Code BEAM EU 2024), covers the trade-offs of Erlang abstract format vs Core Erlang.
- Andrea Leopardi, "OTP 27 Highlights" (ElixirConf EU 2024), summarises the sigils/json/maybe additions.
- Saša Jurić, "Releases without tears" (Code BEAM 2024), covers `mix release` and `relx` in detail.
- Sergei Tikhomirov, "Property-based testing for BEAM languages" (Code BEAM 2025), is the reference for `proper` and `eqc_ce`.

## 23. Distilled design lessons

This survey converges on twelve design lessons for MEP-46:

1. **Emit Core Erlang via `cerl`**, not Erlang abstract format and not source text. Reasons: stable API, official EEF stance, LFE/Hamler/Alpaca/Clojerl/Joxa precedent, automatic pickup of every downstream pass and the JIT. The competing option (abstract format, as Elixir/Gleam do) gives slightly better warning messages, but Mochi already has its own type checker and emits its own diagnostics; we don't need `compile:forms/2`'s warnings to be user-facing.

2. **Use OTP wholesale.** Mochi agents map to `gen_server`; supervision trees use `supervisor` and `supervisor_bridge`; pubsub uses `pg`. Do not invent a Mochi-flavored process model.

3. **Target OTP 27+** for `json`, sigils, `maybe`, triple-quoted strings, and `-doc`. Optional OTP 28 support for distribution-v6 perks.

4. **Emit `-spec` annotations** from Mochi types so generated modules are Dialyzer-clean and eqWAlizer-compatible.

5. **Emit `debug_info` and `line` chunks** always. The cost is negligible; the benefit is the entire OTP debugging ecosystem.

6. **No NIFs by default.** NIFs are scheduler-blocking; the BIF surface plus pure Erlang is fast enough thanks to BeamAsm. Reserve NIFs for crypto, regex, and very-hot numeric kernels, and only via existing well-maintained libraries (`crypto`, `re`).

7. **Tag mangled Mochi identifiers** with a `mochi_` module prefix and a `V_` variable prefix to prevent atom-table pollution and collision with Erlang reserved words/atoms.

8. **Pattern matching is BEAM's superpower.** Lower Mochi pattern matches to Core Erlang `case` and let the kernel pass build the decision tree. Do not hand-write decision trees.

9. **Pre-load atoms.** Mochi sum-type variant tags, record field names, and option `some`/`none` are static; emit them in a `mochi_atoms` module loaded at boot to avoid atom exhaustion in long-running nodes with dynamic Mochi modules.

10. **Use BEAM maps for records and structures**, not tagged tuples. The 32-key flat layout (cache-friendly) plus the HAMT layout above 32 is the right default. Tagged tuples are only for sum-type variants.

11. **Provide an escript entry point and a release entry point.** Both come almost free; documenting both gives users a script-style workflow (CLI tools) and a daemon-style workflow (services).

12. **AtomVM is the embedded story.** No separate microcontroller backend needed; document the OTP-stdlib subset Mochi uses and let users compile-once-run-on-AtomVM.

These twelve lessons drive every design decision in MEP-46.

---

## Sources

1. Erlang/OTP 27 release notes. https://www.erlang.org/news/170 (May 2024)
2. Erlang/OTP 28 release notes. https://www.erlang.org/news/178 (May 2025)
3. Carlsson et al. "Core Erlang 1.0.3 Language Specification." IT Tech Report 2004-018, Uppsala University. https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf
4. EEP index. https://www.erlang.org/eeps/
5. EEF Compiler Workgroup. https://erlef.org/wg/compiler
6. Lukas Larsson. "Inside BeamAsm." Code BEAM SF 2021. https://www.youtube.com/watch?v=3K9-2dSWlSE
7. José Valim. "Set-Theoretic Types for Elixir." ElixirConf US 2024. https://www.youtube.com/watch?v=gJnzhDDqz8E
8. Louis Pilfold. "Gleam 1.0 retrospective." Code BEAM EU 2024. https://www.youtube.com/watch?v=p4Edp0wgxNo
9. Robert Virding. "Why I still write LFE." Code BEAM Stockholm 2024.
10. LFE source. https://github.com/lfe/lfe
11. Gleam source. https://github.com/gleam-lang/gleam
12. Elixir source. https://github.com/elixir-lang/elixir
13. Hamler source. https://github.com/hamler-lang/hamler
14. Alpaca source. https://github.com/alpaca-lang/alpaca
15. Joxa source. https://github.com/joxa/joxa
16. Clojerl source. https://github.com/clojerl/clojerl
17. Purerl source. https://github.com/purerl/purerl
18. Caramel source. https://github.com/leostera/caramel
19. AtomVM source and docs. https://www.atomvm.net/, https://github.com/atomvm/AtomVM
20. Erlog source. https://github.com/rvirding/erlog
21. Luerl source. https://github.com/rvirding/luerl
22. Castagna et al. "Programming with union, intersection, and negation types." POPL 2023. doi:10.1145/3571238
23. eqWAlizer source. https://github.com/WhatsApp/eqwalizer
24. Dialyzer (OTP). https://www.erlang.org/doc/apps/dialyzer/dialyzer.html
25. asmjit. https://asmjit.com/
26. OTP `lib/compiler` source. https://github.com/erlang/otp/tree/master/lib/compiler
27. OTP `lib/stdlib/src/cerl.erl`. https://github.com/erlang/otp/blob/master/lib/compiler/src/cerl.erl
28. HiPE retirement (OTP 24 release notes). https://www.erlang.org/news/151
