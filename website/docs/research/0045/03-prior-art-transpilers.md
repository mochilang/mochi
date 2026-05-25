# MEP-45 research note 03, Prior art: language → C compilation (2024–2026)

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).
Method: structured web research by a delegated agent; verbatim report
preserved below with light section-anchor edits.

The report is the canonical survey for the MEP body's "Rationale" and
"Prior Art" sections. References at the foot are the authoritative source
list.

---

# Survey: State of the Art in Compiling High-Level Languages to C (2024-2026)

This survey covers production transpilers, research compilers, and seminal/recent papers relevant to designing a Mochi-to-C transpiler in 2026. It is structured by *system* (sections 1-11) and then by *technique* (sections 12-21), closing with a distilled set of design lessons (section 22).

## 1. Nim and its C backend

Nim has historically used C and C++ as its primary backends; in Nim 2.x ARC/ORC is the default memory model (Nim Blog 2020, "Introduction to ARC/ORC"). The classic pipeline lives in `injectdestructors.nim`, `lambdalifting.nim`, `closureiters.nim`, and `transf.nim`. ARC injects `=destroy`, `=copy`, `=sink` hooks via a per-scope owned-temporary table plus data-flow last-read analysis; when a variable is last-read, the compiler rewrites the use as `=sink` and skips the destructor at scope exit. Closures are eliminated by lambda lifting into explicit `(env*, fn*)` pairs; iterators are transformed by `closureiters` into resumable state machines (a switch-on-resume-PC closure environment, similar to C# iterators). Exceptions can be lowered three ways (`setjmp`, `cpp`, or `--exceptions:goto`); the goto mode produces ANSI-C-only output where every potentially-throwing call sets a thread-local error flag and jumps to a generated dispatch label, which is what most cross-compilation users pick.

The 2024-2026 architecture is a clean rewrite called **Nimony** organised around **NIF** (Nim Intermediate Format) and its C-shaped dialect **NIFC** (nim-lang/RFCs #556, "Nim Roadmap 2025"; nim-lang/nifspec). NIF is essentially S-expressions with separate tag and identifier namespaces, designed to be the on-disk module format and the lingua franca between compiler stages. Lowering ("Gear 2") performs eleven explicit passes in order: parse, sem1 (lookup/type), sem2 (effect inference), iterator inlining, lambda lifting, deref injection, dup injection, control-flow expression lowering, destructor injection, builtin mapping, exception translation; only then does "Gear 3" generate NIFC. The key lesson is that the old monolithic `ccg*` generator hit a complexity wall (issues #21579, #23897 show painful interactions between `--exceptions:goto`, ARC, and C++ goto-past-initialiser rules), and the team explicitly decided to *desugar everything into low-level Nim before touching C*.

Concrete code-shape: each Nim proc emits a flat C function whose locals all sit at the top, with structured `if/while/goto` for control flow. Closure environments are heap-allocated `tyObject_envXX` structs whose fields are the captured locals; refcounted types use a hidden header word holding the count plus type info pointer (`TNimType*`). Generics are monomorphised eagerly, which causes the documented compile-time explosion when many type parameters meet ARC's per-type hook generation.

## 2. Crystal

Crystal is LLVM-only, there is no C textual backend, but its codegen *IR shape* is instructive. The compiler emits LLVM IR that treats every reference type as a tagged pointer, with method dispatch realised as inlined virtual-call sequences after Crystal's whole-program type inference has narrowed union types. Memory is managed by **bdwgc** (Boehm-Demers-Weiser) linked as a runtime library (`src/gc/boehm.cr`), which avoids the need for `@llvm.gcroot`/stack maps. Allocations split into `GC_malloc` (may contain pointers) and `GC_malloc_atomic` (pointer-free, e.g. `String` payload, `Bytes`), exploiting Boehm's atomic-block optimisation to avoid scanning. Finalisers route through `GC_register_finalizer_ignore_self`.

A continuing pain point is multithreading: Boehm versions before 8.2.0 require an upstream patch, and Boehm "does not guarantee to scan thread-local storage" (issue tracker), so Crystal stores any pointer destined for TLS *also* on the thread's stack. Migration to **Immix** has been discussed since issue #5271 (2017) but blocked by the lack of LLVM-level safepoints and the fact that conservative scanning would have to be replaced by precise stack maps, a major investment. The lesson for a *transpiler*-style compiler is that Boehm gives you 80% of a GC for one library link, and that the "atomic vs non-atomic allocation" distinction is a cheap, high-impact optimisation when you control the type system.

## 3. Vala

Vala is the canonical "transpile to readable C" success story (`valac` man page, Vala FAQ, Wikipedia). It targets GLib/GObject; classes become `GTypeInstance` subtypes with generated `_class_init`, `_instance_init`, `_get_type` functions; properties become `g_param_spec_*` plus generated getters/setters; signals are `g_signal_new` calls plus generated marshalers, so the Vala `signal foo (int x)` line becomes one C macro registration and a stub `foo_emit` wrapper. Memory uses GObject reference counting; `weak` annotations turn into raw pointers, and Vala inserts `g_object_ref`/`g_object_unref` along all paths via a simple scope analysis (Vala's analysis is admittedly less precise than Nim's, hence the FAQ note "Vala uses reference counting in more places than most GObject/C-based applications do").

Two profile knobs matter: `gobject` (default, depends on libgobject) and `posix` (no GLib, no runtime types, useful for microcontrollers and minimal containers). Cross-language interop is essentially free because every Vala class *is* a GObject; GObject-Introspection metadata is emitted automatically. ABI stability requires `--abi-stability` to force public members in source order. The retrospective lesson: piggy-backing on a *existing* object system (GObject in Vala's case) saved years of design work but locked the language into GLib's worldview. A Mochi-to-C compiler that does not want that lock-in must invent its own minimal runtime, see §15.

## 4. OCaml and Multicore

OCaml's primary backend is `ocamlopt` to native, not C; `ocamlc` produces bytecode. The headline 2021-2026 work is Multicore OCaml (Sivaramakrishnan et al., PLDI 2021, "Retrofitting Effect Handlers onto OCaml"; arXiv:2104.00250) which retrofitted effect handlers with three hard requirements: (R1) backward compatibility, (R2) DWARF/debugger compatibility, and (R3) millions of live continuations. The implementation uses *segmented fiber stacks* where each handler frame contains a `handler_info` struct pointing back to its parent fiber plus three closures (value, exn, effect cases). Context blocks emit DWARF callbacks so unwinders stitch parent fibers transparently. The benchmark result, 1% mean overhead on non-effect code, set the bar for "effects without regret."

OCaml 5.0 shipped this in 2022; 5.2 (May 2024) restored POWER64 and added ThreadSanitizer; 5.3 (Jan 2025) added *dedicated syntax* for deep effect handlers (Leo White, Tom Kelly, et al.). The 2024 **Oxidizing OCaml with Modal Memory Management** paper (Lorenzen, White, Dolan, Eisenberg, Lindley, ICFP 2024, PACMPL 8/ICFP/253) introduced *modes* on three axes, affinity, uniqueness, locality, with backwards-compatible inference; field-level modalities, regions (a function or loop body is a region), and the `exclave` construct for escape, enabling stack-allocated closures and in-place updates of immutable structures without `unsafe`. The interaction with tail calls is the subtle part: a region must close *before* the tail jump, so locals in that region cannot be passed as tail-call arguments. **OCaml is also the first industrial-strength language targeting the WASM-GC extension** (ocaml.org/releases).

## 5. Roc

Roc compiles via LLVM, but its design notes are the most-cited modern reference on platform/host split and refcount elision. The **platform** (which a Roc app embeds against) provides *all* I/O primitives, `Stdout.line` lives in the platform, not in `std`. The Roc runtime exports a tiny C ABI surface (`roc__mainForHost_1_exposed_generic`, etc.); platforms can be written in Zig, Rust, Swift, or C and link the Roc-compiled app object in. This is exactly the "two-tier compilation" model a Mochi-to-C transpiler should consider: emit the app as a `.o` with a fixed C ABI, let the host write `int main()`.

Roc uses Perceus (see §13) for refcount elision plus **Morphic** mutation analysis to discover in-place updates on persistent data structures (roc-lang.org/fast). Closures are compiled as `{void* fn; void* env}` fat pointers (rwx.com/blog/how-roc-compiles-closures), but the compiler aggressively converts them to *tagged unions over known function sets* when whole-program analysis can enumerate the closure values, eliminating the indirect call. Defunctionalisation is a stated long-term goal (still gappy in 2025). The 2024 Utrecht thesis "Reference Counting with Reuse in Roc" formalises Roc's variant of the λ1n calculus.

## 6. Cone, Koka, Lean 4, Eff

**Koka** (Daan Leijen, Microsoft Research) is the most relevant single system to study. It compiles directly to portable C99 with *no GC and no runtime system*, using compiler-guided reference counting (Perceus; Reinking, Xie, de Moura, Leijen, PLDI 2021, distinguished paper; doi 10.1145/3453483.3454032). Effects compile via the line in Schuster, Brachthäuser et al.'s "Compiling effect handlers in capability-passing style" (ICFP 2020, doi 10.1145/3408975) and Xie & Leijen's "Generalized evidence passing for effect handlers" (ICFP 2021, doi 10.1145/3473576), where each handler in scope is passed as an *evidence vector* and `yield`-bubbling unwinds to the matching handler frame. The C library `libhandler` (github.com/koka-lang/libhandler) implements the same scheme as a portable C99 library using `setjmp`/portable stack copying.

The Koka research line through 2024 stacks: Frame-Limited Reuse (Lorenzen & Leijen, PACMPL 6/ICFP 2022, doi 10.1145/3547634) generalised reuse analysis with bounded-extra-memory soundness; **FP² Fully in-Place Functional Programming** (Lorenzen, Leijen, Swierstra, ICFP 2023, PACMPL 7/ICFP/198) introduced the `fip` keyword and the λfip calculus with unboxed tuples, borrowed parameters, and a proof that FIP functions run in constant stack and zero allocation when args are unique. The Koka binary-trees benchmark beats C++ by ~20%, mainly thanks to TRMC (tail-recursion modulo cons) and 8-byte vs C++'s 16-byte alignment.

**Lean 4** (de Moura & Ullrich 2021, CADE) ships both a C and LLVM backend. The pipeline is Lean → LCNF (A-normal form, Flanagan 1993; with join-point insertion à la Maurer 2017) → IR (explicit `inc`/`dec`/`reuse`/`isShared`) → C or LLVM (`EmitC.lean`, `EmitLLVM.lean`). Perceus is implemented at the IR layer; the compiler marks parameters `borrowed` to elide refcount traffic and detects destructive-update opportunities via `isShared`. Threading uses sign-bit-tagged counts: positive = unsynchronised (thread-local), negative = atomic (shared); once a block goes shared it never goes back.

**Eff** (Bauer & Pretnar) and the optimising Eff compiler (Karachalias, Pretnar et al., PACMPL 5/ICFP 2021, doi 10.1145/3485479) compile Eff to OCaml using type-and-effect-directed rewrites that aggressively reduce handler applications, then erase effect info and emit "tight" OCaml. "Eff Directly in OCaml" (Kiselyov & Sivaramakrishnan, arXiv:1812.11664) instead *embeds* Eff using delimited continuations or Multicore OCaml's native effects.

**Cone** (Jonathan Goodwin; github.com/jondgoodwin/cone) targets LLVM but its design, *gradual* memory management mixing single-owner, refcounted, traced, lifetime-bound, and arena strategies in the same program plus lockless/locked permissions for sharing, is a useful menu for what a Mochi could expose. Cone is research-grade, not production-ready, but the design notes are the clearest articulation of "let each allocation pick its strategy."

POPL/PLDI 2024-2025 effect-handler progress: **Soundly Handling Linearity** (POPL 2024) combined linear types with multi-shot handlers via *control-flow linearity*; **Affect** (POPL 2025) uses affine types on continuations so the compiler knows which optimisations are sound; **Handling the Selection Monad** (PLDI 2025) extended handlers with choice continuations; "Tracing JIT for Effects and Handlers" (Gaißert, Bolz-Tereick, Brachthäuser, OOPSLA2 2025) is the first JIT-focused work. The actionable takeaway: tracking continuation linearity in the type system pays for itself in code quality.

## 7. Hare, Zig, Odin

These are C-replacement systems languages, not C-targeting transpilers, but they teach portability lessons. **Zig** bundles LLVM + Clang + headers for ~97 libcs (~130 MiB uncompressed) and exposes `zig cc` as a turn-key cross-compiler (ziglang.org/learn/overview). Its compile-time evaluation (`comptime`) emulates the target architecture, which is the same trick a transpiler can use to fold target-specific intrinsics at compile time. **Hare** uses **QBE** as its backend (small enough to fit on a floppy), the lesson is that you do not need LLVM if you accept the optimisation hit; QBE is auditable in days. **Odin** has no methods, leading to C-style `parser_parse_statement(Parser *p)` prefixed-function code; this is the cost of avoiding method dispatch and an argument *for* keeping method syntax in Mochi even at the C-generation layer.

Common design discipline across all three: no hidden control flow, no hidden allocations, explicit allocators passed as arguments. Zig's "comptime emulates target" plus "no preprocessor / no macros" is the cleanest demonstration that you can have rich metaprogramming without textual hacks.

## 8. MLton

MLton (mlton.org) is the canonical whole-program SML compiler and the classic reference for the **defunctorize → monomorphise → defunctionalize** pipeline. Defunctorisation duplicates each functor at every application and renames variables to eliminate structures. Monomorphisation duplicates each polymorphic type/function at every instantiation. Defunctionalisation (Reynolds 1972) replaces higher-order functions with a tagged record of free variables plus a top-level apply function dispatching on the tag. The output IR is simply-typed first-order SSA; both a native and a C backend exist.

Reported code-size blowup is bounded, empirically ~30% max in MLton. Recent work (Lambda Set Specialization, Brandon et al.) extends defunctionalisation to lift restrictions on how function values may be used, recasting it as type monomorphisation over a polymorphic function-flow type system; evaluations target MLton, OCaml, and Morphic. The implication for Mochi: whole-program defunctionalisation is a credible, *implementable-by-a-small-team* alternative to LLVM-style polymorphic optimisation if you accept whole-program compilation.

## 9. Cosmopolitan libc

Cosmopolitan (jart/cosmopolitan; ape/specification.md; justine.lol/cosmopolitan/) reconfigures stock GCC/Clang to emit **APE**, a polyglot of ELF, Mach-O, PE, and bootable disk image where the magic bytes are valid x86 instructions in all modes. A single binary then runs on Linux, macOS, Windows, FreeBSD, OpenBSD, NetBSD, and BIOS on x86-64; macOS arm64 was added recently. **The `cosmocc` wrapper** (tool/cosmocc/) is essentially `gcc` with a different linker script and `cosmopolitan.a`. The `apelink` tool weaves multiple native binaries plus tiny APE loaders into a self-extracting shell-script header so each platform extracts and re-execs the right binary at first run. As of v3.5.8 (mid-2024) hello-world bytes are byte-identical across all platforms, *that is the reproducibility win*.

The major limitation: no cross-platform `dlopen` (DLL+DYLIB+SO ABIs cannot be polyglotted), worked around in projects like `llamafile` by manually loading a platform-specific binary that then uses the OS's own `dlopen`. For Mochi: if you emit ANSI C99 and link with `cosmocc`, you get a *single binary* artifact for free, extraordinary value-per-line.

## 10. emscripten and wasi-libc

Emscripten (emscripten.org) is LLVM-clang-to-WASM with an emulated POSIX layer. The 2019 switch to the upstream LLVM Wasm backend (Emscripten 1.39) removed fastcomp. Two output modes: standard (`.wasm + .js` glue, depends on Emscripten's JS runtime for `longjmp`, C++ exceptions, time, console, WebGL) and `STANDALONE_WASM` (no JS, uses WASI APIs, runnable in Wasmer/Wasmtime/WAVM). **wasi-libc** is the alternative, pure musl-derived libc with WASI syscalls, usable directly with `clang --target=wasm32-wasi --sysroot=$WASI_SYSROOT`.

The WASI ecosystem in 2024-2025 underwent a major iteration: **WASI Preview 2 / 0.2** (Bytecode Alliance, early 2024) added the Component Model and "worlds" (`wasi-cli`, `wasi-sockets`, `wasi-clocks`, `wasi-random`); **WASI 0.3 / Preview 3** is expected in 2025 with native async I/O via the Component Model (eunomia.dev WASI status, Feb 2025). Wasmtime was the first runtime with full WASI 0.2 support. Threading remains experimental (`wasi-threads` proposal; not in Preview 2 mainstream). For Mochi-to-C-to-WASM, the cleanest path is to emit ANSI C with no exotic libc dependencies and let `clang --target=wasm32-wasi` handle the rest.

## 11. TigerBeetle, Bun, `zig cc` in practice

`zig cc` is now mainstream as a portable cross-compiler. **TigerBeetle** uses `zig cc` for its Go client library CI on Windows and Linux (tigerbeetle/src/clients/go/ci.zig) because `gcc` is not available on Windows out of the box. **Bun** is built largely on top of Zig + WebKit. **Uber** uses Zig as the cross-compiler for its Go monorepo (Jakštys blog). A July 2024 guide (jcbhmr.com) walks through CMake + `zig cc` cross-compilation; a small `zig-ar` wrapper is needed because CMake's `CMAKE_AR` does not accept commands with arguments.

The reproducibility angle: reproducible-builds.org reports through 2024 (March, September, October) document toolchain hermeticity gains; the September 2024 Android D8 bug ("DEX output depends on core count") is a cautionary tale for Mochi, *any* parallelism in the build can poison reproducibility unless ordering is forced. `cosmocc` and `zig cc` together cover the entire matrix: `zig cc` for cross-arch, `cosmocc` for single-binary multi-OS, both deterministic. **A 2026 Mochi-to-C transpiler should target `clang -std=c99 -pedantic` as the canonical compiler and validate against `zig cc` and `cosmocc` as portability oracles.**

## 12. Conservative GC in C: bdwgc and what's new

bdwgc (github.com/bdwgc/bdwgc) is at 8.2.10 (Oct 2025). The core algorithm, mark-sweep over the heap, conservative pointer identification on stacks and registers, has not changed in a decade, but incremental and generational modes (under VM support) plus prefetching strategies do meaningful work. Multithreading is supported with two caveats: full signal-safety costs two syscalls per `malloc` (usually unacceptable, default-off), and **TLS is not scanned**, pointers in `pthread_getspecific` must be duplicated onto the thread stack. CMake is the cross-platform build path (`cmake.md`).

Practical experience (jank Clojure dialect, HN comment 2024): "stupid simple to integrate and surprisingly fast … MPS, MMTk, others are worlds apart in dev work." For a Mochi-to-C transpiler the calculus is: bdwgc gives you a working memory manager in one link line; you trade ~10-30% throughput vs precise/generational and lose moving/compacting; you keep ANSI-C portability.

## 13. Precise reference counting: Perceus and successors

**Perceus** (Reinking, Xie, de Moura, Leijen, PLDI 2021) is the modern canonical algorithm. The compiler inserts `dup`/`drop` operations such that *cycle-free* programs are *garbage free*, objects are freed at the exact moment of last use. Key rules: delay `dup` to leaves, generate `drop` immediately after the binding goes dead; the linear resource calculus λ1 proves soundness. **Reuse analysis** pairs a `drop` of size-S with a fresh allocation of size-S into a single in-place update when refcount is 1 at runtime, which enables the **FBIP** (functional but in-place) programming style, purely-functional code that runs imperative-fast. **Frame-Limited Reuse** (Lorenzen & Leijen, ICFP 2022) generalises this to "drop-guided reuse" with provable bounded extra memory per call; **FP²** (Lorenzen, Leijen, Swierstra, ICFP 2023) adds the static `fip` keyword guaranteeing constant stack + zero allocation; **Oxidizing OCaml** (ICFP 2024) brings the same affinity/uniqueness/locality modes to OCaml.

Companion techniques: **Lobster** (Wouter van Oortmerssen) reportedly elides 95% of refcount ops via compile-time flow-sensitive lifetime analysis (strlen.com/lobster), closer to move semantics than to traditional RC. **Swift ARC** uses an SSA-with-ownership IR (OSSA) where the compiler inserts `retain`/`release` after type checking; ARC optimisations include retain-release pairing, retain motion across barriers, and "observed object lifetimes are emergent" (WWDC 2021 ARC session). **Lean 4** uses the same Perceus algorithm but exposed via the `borrowed` attribute and `isShared` IR primitive (lean-lang.org/doc/reference/latest/Run-Time-Code/Reference-Counting).

Cycle collection: **Bacon-Rajan** (ECOOP 2001, "Concurrent Cycle Collection in Reference Counted Systems") is the standard solution. Two insights: (1) cycles can only form when a decrement leaves a non-zero count, so candidate roots are exactly those decrements, and (2) inside a cycle all RCs are internal, so subtracting them via local DFS finds the cycle. The synchronous version is used in PHP 5.3+; the concurrent version achieves 6 ms max pause in Jalapeño. PHP-style "generational" cycle collection (Python) is a heuristic on top. For Mochi: Perceus + FBIP-style reuse + Bacon-Rajan cycle collection as a backup gives a *garbage-free in the common case* system with a safety net.

## 14. Region/arena inference

**Tofte-Talpin** (TOPLAS 1997, "A region inference algorithm", doi 10.1145/291891.291894; implemented in the MLKit) is the seminal type-and-effect system inferring region lifetimes for ML; the runtime stack pushes/pops regions with all objects deallocated together. The big practical complaint: small program changes can cause "drastic, unintuitive effects on object lifetimes."

**Cyclone** (Grossman, Morrisett et al., PLDI 2002, "Region-Based Memory Management in Cyclone") adapted regions to a low-level explicit-region language. Key contributions: region subtyping (LIFO scoping induces an "outlives" relation), simplified effects without effect variables, *dynamic* regions (not tied to a lexical scope) for data that needs to escape, and default effects computed from function prototypes to preserve separate compilation. **"Linear Regions Are All You Need"** (Fluet, Morrisett, Ahmed, ESOP 2006) shows λrgnUL, a substructural type system that can encode Tofte-Talpin AND Cyclone dynamic regions, removing the LIFO restriction.

Recent: **Spegion** (arXiv:2506.02182, 2025) introduces implicit, non-lexical regions with sized allocations; **Elsman's "Double-Ended Bit-Stealing for Algebraic Data Types"** (ICFP 2024); **Koparkar et al., "Garbage Collection for Mostly Serialized Heaps"** (ISMM 2024); **"Optimistic Stack Allocation and Dynamic Heapification for Managed Runtimes"** (Anand et al., PLDI 2024), JITs optimistically stack-allocate based on escape analysis and *dynamically heapify* if the optimism fails, with a stack-ordering analysis to amortise the check. The lesson for Mochi: region inference is brittle as a *primary* memory strategy, but arena-per-request or arena-per-handler is the cheapest possible "GC" and pairs beautifully with Roc-style platform/host split.

## 15. Closures in C

The exhaustive treatment is Matt Might's "Compiling Scheme to C with flat closure conversion," Hokstad's "How to implement closures," and the Roc post "How Roc Compiles Closures." Four strategies, each with concrete trade-offs:

1. **Fat pointer** (`struct { void* fn; void* env; }` passed everywhere). Portable, simple; cost is one extra register/argument per call site and an explicit unpack at the call. Roc uses this with the optimisation that `env` is `union` over known environments and can live on the stack when the closure does not escape.
2. **Thunk** (mmap a small RWX page, write machine code that pushes the env then jumps to the body, return a plain `void(*)()`). Callers see a regular function pointer; cost is W^X-unfriendly, architecture-specific code generation. Apple Silicon and many hardened Linuxes now forbid this without `mprotect` dance plus `PT_GNU_STACK` exemptions.
3. **Object dispatch** (closure is an object with a `call` method). Uniform with an object system; full vtable cost.
4. **Tagged-union dispatch** (when whole-program analysis enumerates the closure values, emit `switch(tag)` calling the right top-level function directly). No indirect call; Roc's preferred path when defunctionalisation succeeds.

For Mochi: pick fat-pointer as the default *but* implement tagged-union dispatch where the analysis is trivial (e.g. callbacks visible in scope), and never emit thunks (they break W^X and reproducibility).

## 16. Coroutines and stack switching in C

Three viable approaches in 2026 (Wikipedia "Coroutine," Boost.Context, libdill issue #30, Wikipedia "Protothread"):

- **`ucontext`** family (`makecontext`, `swapcontext`) is *obsolete on macOS* (deprecated POSIX 1.2008) and not in many embedded libcs.
- **Boost.Context / libdill / libco style**, hand-written asm per architecture (x86-64 SysV, x86-64 Win64, aarch64, riscv64) that swaps callee-saved registers + stack pointer. libdill benchmarks ~13-16ns context switch, ~25-44ns coroutine creation. Caveats: signal-mask preservation costs a `rt_sigprocmask` syscall (kills perf), and signal handlers run in the current coroutine's stack.
- **Stackless / Protothreads / Duff's device / computed-goto**, uses `switch` to fake resumption from yield points; locals do not survive across yield (must be in an explicit state struct). Very cheap (bytes of memory per coroutine); cannot yield from within a nested switch unless using GCC computed gotos. This is the model **C++20 coroutines** chose, and what async/await ends up being.

For Mochi: the stackless model is the right default for `async`/generators because it survives sandboxes (WASM, BIOS, embedded), and the per-target asm switching can be added later as an optimisation for true threads.

## 17. Effect handlers compiled to C

The literature breaks into three families. **Direct stack capture**, what Multicore OCaml does, copy or segment the native stack on `perform`, resume by jumping back to a saved context. Best performance at runtime, requires DWARF cooperation and per-arch asm. **Capability-passing / evidence-passing** (Schuster, Brachthäuser, Ostermann, ICFP 2020, PACMPL 4/ICFP/93; Xie & Leijen, ICFP 2021, PACMPL 5/ICFP/127), pass the handler implementation as a runtime value (a capability) so calls to effect operations are direct calls into the handler; combine with iterated CPS to enable optimisation across effect calls. This is what Koka's C backend uses and what makes the Koka benchmarks competitive with hand-written C++. **Monadic translation**, Eff compiles to OCaml via type-and-effect-directed rewrites (Karachalias et al. ICFP 2021), erases effect info, emits tight code.

For Mochi-to-C, evidence passing is the right pick: pure C99, no asm, predictable perf, integrates naturally with refcounted environments. The Koka `libhandler` library (github.com/koka-lang/libhandler) is a literal blueprint. Recent POPL/PLDI/OOPSLA work (Soundly Handling Linearity, POPL 2024; Affect, POPL 2025; Tracing JIT for Effects, OOPSLA2 2025) consistently shows that *tracking continuation linearity in the type system* (one-shot vs multi-shot) enables much better codegen, Mochi should expose this distinction.

## 18. Sum types in C

The portable C lowering of an ADT is a discriminated union: `struct { tag_t tag; union { variant_A a; variant_B b; ... } u; }`. Size = `sizeof(largest_variant) + sizeof(tag) + padding`. Three optimisations matter:

- **Niche-filling** (Rust's `Option<&T>` taking zero extra bytes by using the null pointer as the `None` tag; RFC 2195), detect "forbidden" bit patterns in payload types and reuse them as discriminators. Implement by tracking per-type `niche` metadata: `&T` has one niche (null), `bool` has 254, `enum Direction { N, E, S, W }` as u8 has 252.
- **Pointer tagging**, exploit alignment: on 64-bit, an 8-byte-aligned pointer has the low 3 bits free. OCaml's classic value representation uses bit 0 as the int tag. Lean 4 uses tagged pointers for small constructors.
- **Repr-C escape hatch**, for FFI, emit a fixed-layout `{int tag; union body;}`, lose niche optimisation, gain ABI stability.

For Mochi: implement niche-filling for the common cases (`Option<&T>`, `Option<Box<T>>`, single-niche enums), it is high-impact and the analysis is local.

## 19. Pattern matching compilation

The standard algorithm is Luc Maranget's **decision-tree** algorithm (Maranget, ML Workshop 2008, "Compiling Pattern Matching to Good Decision Trees," doi 10.1145/1411304.1411311). The compiler maintains a pattern matrix + occurrence + action vectors; recursively chooses a column to split on using a *necessity* heuristic ("number of rows for which this column is needed"); shares the resulting DAG to avoid code blowup; produces a tree that never tests the same subterm twice. The alternative, *backtracking automata* (Le Fessant & Maranget 2001), is more compact but may re-test. Modern decision-tree compilers (e.g. Tobin-Hochstadt's Racket `match`) combine both.

For Mochi: a single-pass Maranget decision tree, emitted as a chain of `switch`/`if` over discriminator fields, gives O(1)-per-test code that the C compiler can further optimise. Crumbles.blog (Nov 2025, "Tour of a pattern matcher: decision trees") has a current implementation walkthrough.

## 20. Other stream / dataflow / Datalog targets

**Lustre** (Halbwachs et al. 1991) is the synchronous-dataflow language behind SCADE; its classic compiler synthesises a finite-state machine in C where the "step" function is called once per clock tick. **Vélus** (Bourke et al., PLDI 2017, "A formally verified compiler for Lustre," doi 10.1145/3062341.3062358) is a CompCert-verified compiler from normalised Lustre to Clight, handling sampling, nodes, and delays, with the dataflow→imperative transformation proven correct. The **CoCompiler** (miniKanren 2025, arXiv:2510.00210) re-encodes Vélus in the Walrus relational language so the Lustre↔C compilation runs *both directions*, interesting for round-tripping codegen.

**Soufflé** (souffle-lang.github.io) translates Datalog to heavily-templated parallel C++ via a relational-algebra machine (Futamura projection on semi-naive evaluation). Magic-set transformation is available behind `-m`; data-structure choices (B-trees vs tries) are auto-selected via VLDB-published index analysis. Compiled mode beats interpreted by 10-100× for large workloads.

**Pony / ORCA** (Clebsch et al., OOPSLA 2017, "Orca: GC and Type System Co-Design for Actor Languages," doi 10.1145/3133896), fully concurrent per-actor GC with no global synchronisation, requiring only that (i) actor behaviours are atomic and (ii) messages are causally delivered. Each actor owns its heap; cross-actor references use weighted reference counting via INC/DEC messages sent only to the owning actor. Actor collection itself handles dead-cycle detection. This is the right reference for any Mochi *actor* extension.

## 21. Escape analysis for C codegen

Standard textbook material (Wikipedia "Escape analysis"; Park & Goldberg 1992 "Higher order escape analysis"), track pointer flow, prove a pointer does not outlive the procedure, stack-allocate. The 2024 PLDI "Optimistic Stack Allocation and Dynamic Heapification for Managed Runtimes" paper (CSE IIT Bombay preprint) is the recent SOTA: combine static escape analysis with JIT-time optimistic stack allocation plus a "stack walk" run-time check that *heapifies* objects (moving them to the heap and patching references) if the optimism turns out to be wrong. Microsoft's 2026 HotSpot JEP proposes true stack allocation that preserves object shape (vs scalar replacement) for objects at control-flow merges; claims up to 15% heap allocation reduction on Renaissance and Scala DaCapo.

For Mochi: a simple intra-procedural escape analysis (does any reference flow into a global, a return value, a heap field, or a closure environment?) catches the easy wins; combine with the FBIP / Perceus reuse path for the hard cases.

## 22. Distilled lessons for a Mochi-to-C transpiler in 2026

1. **Lower in stages, not in the C emitter.** Nim's painful experience and the Nimony rewrite agree: do iterator inlining, lambda lifting, deref injection, destructor injection, exception lowering as *separate passes over your own IR* before you ever touch C. NIF's separation of tag and identifier namespaces is a clean trick worth borrowing.

2. **Pick Perceus + FBIP as the default memory model**, with Bacon-Rajan cycle collection as the safety net and bdwgc as the "off switch" for users who do not care. This matches Koka's proven design, integrates with effect handlers via evidence passing, and gives you predictable, sub-millisecond pause behaviour without a moving GC.

3. **Closures as fat pointers, defunctionalise where you can.** Default to `{fn*, env*}`; emit MLton/Roc-style defunctionalisation when whole-program analysis succeeds; *never* generate thunks (W^X, code-signing, WASM all hate them).

4. **Effect handlers via evidence passing**, not stack capture. Pure C99 output, no per-architecture asm, blueprint exists in Koka's libhandler. Track continuation linearity (one-shot vs multi-shot) in the type system per POPL 2024/2025, the optimisation payoff is large.

5. **Sum types: niche-fill `Option<&T>`, `Option<Box<T>>`, and small enums**; otherwise lay out as `{tag; union}`. Pattern match via Maranget decision trees emitted as cascaded `switch`.

6. **Coroutines: stackless by default** (computed-goto state machines in the Protothread / C++20 style), stack-switching as an opt-in for true threads, preserves WASM and embedded portability.

7. **Two-tier compilation à la Roc.** Emit the program as a `.o` with a small, documented C ABI; ship a default "host" platform written in C that does `main()` + I/O; let users replace the host. This is the cleanest model for embedding Mochi in larger systems.

8. **Target `clang -std=c99 -pedantic` as canonical, validate with `zig cc` and `cosmocc`.** This gives you (a) cross-architecture cross-compilation via `zig cc -target ...`, (b) single-binary multi-OS via `cosmocc`, (c) WASM via `clang --target=wasm32-wasi`, and (d) byte-reproducible builds if you discipline the codegen (deterministic name mangling, sorted iteration, no time/PID in output).

9. **Region/arena allocation as a programmer-visible escape hatch**, not as the primary strategy. Tofte-Talpin region inference is too brittle for a primary model (per MLKit experience); but per-request arenas + Spegion-style sized regions integrate cleanly with Roc-style host platforms.

10. **Generate readable, debuggable C.** Vala's success rests on its C output being legible and using familiar idioms (GObject, `g_signal_emit`, etc.). Mochi-to-C should emit code that a curious user can read, with `#line` directives back to Mochi source so `gdb` and sanitisers work.

11. **Reproducibility is a first-class feature.** The 2024 Reproducible Builds reports (Android D8 core-count bug, Bazel hermeticity issues) prove that any nondeterminism, iteration order, time, PID, parallelism, will eventually bite. Bake hermetic codegen in from day one.

12. **Borrow Lean 4's pipeline diagram**: source → A-normal form → IR with explicit `inc`/`dec`/`reuse`/`isShared` → C/LLVM. Make the IR introspectable (`trace.compiler.ir.result`) so users can debug refcount surprises.

## Sources

(URLs are preserved from the source agent; cited inline above.)

Nim & NIF: Nim Roadmap 2025; Nim ARC/ORC intro; Nim compilation pipeline (DeepWiki); nifspec; NIF spec doc; issues #23897, #21579.

Crystal: Crystal Wikipedia; src/gc/boehm.cr; Crystal GC API; Immix issue #5271.

Vala: valac man; Vala FAQ; Vala Signals tutorial; Vala Wikipedia.

OCaml / Multicore / Modes: arXiv:2104.00250 (Retrofitting Effect Handlers onto OCaml); ACM PLDI 2021; OCaml 5.3 release / changelog; "Oxidizing OCaml with Modal Memory Management" ICFP 2024 (Lorenzen, White, Dolan, Eisenberg, Lindley).

Roc: Roc Fast / Platforms pages; "How Roc Compiles Closures" (rwx.com); Utrecht thesis "Reference Counting with Reuse in Roc."

Koka / Perceus / FBIP / FIP / Evidence: Perceus PLDI 2021 PDF and ACM; Frame-Limited Reuse ICFP 2022 (PACMPL 6/ICFP); FP² ICFP 2023 (PACMPL 7/ICFP/198); Schuster et al. ICFP 2020 (PACMPL 4/ICFP/93); Xie & Leijen ICFP 2021 (PACMPL 5/ICFP/127); Koka GitHub; libhandler GitHub.

Lean 4: Lean 4 paper PDF (CADE 2021); Lean 4 reference, Reference Counting; Lean DeepWiki; Lean 4.23 release notes.

Cone: github.com/jondgoodwin/cone; PLDB Cone entry.

Eff: github.com/matijapretnar/eff; "Eff Directly in OCaml" arXiv:1812.11664; "Efficient Compilation of Algebraic Effect Handlers" ICFP 2021 (PACMPL 5/ICFP).

Hare / Zig / Odin: Zig overview; colinsblog.net systems-languages II; zig cc OSDev; jcbhmr.com zig cc + CMake 2024.

MLton: mlton.org; "Whole-Program Compilation in MLton" (Weeks); SIGPLAN blog "Defunctionalization Everybody Does It, Nobody Talks About It."

Cosmopolitan: github.com/jart/cosmopolitan; ape/specification.md; justine.lol/cosmopolitan/; cosmocc README; APE DeepWiki.

Emscripten / WASI: emscripten.org/docs/compiling/WebAssembly.html; WASI Component Model status 2025 (eunomia.dev); Standalone WASM wiki.

TigerBeetle / zig cc / reproducibility: tigerbeetle Go client ci.zig; papercompute.com Cross-Compiling CGo; reproducible-builds.org reports 2024-09/10.

GC, RC, regions: bdwgc GitHub; Boehm tutorial PDF; Boehm GC Wikipedia; MMTk about; Bacon-Rajan ECOOP 2001 PDF; Tofte-Talpin TOPLAS 1997; "Linear Regions Are All You Need" ESOP 2006; "Optimistic Stack Allocation and Dynamic Heapification for Managed Runtimes" PLDI 2024 (CSE IIT Bombay preprint); Spegion arXiv:2506.02182.

Lobster / Swift ARC: strlen.com/lobster; aardappel.github.io/lobster reference; WWDC 2021 ARC session.

Closures, coroutines: Matt Might "Compiling Scheme to C"; Hokstad closures; Wikipedia Coroutine and Protothread; Boost.Context; libdill issue #30.

Tagged unions, niche: Rust RFC 2195; Wikipedia "Tagged union"; patshaughnessy.net on tagged unions.

Pattern matching: Maranget ML 2008 PDF + ACM doi 10.1145/1411304.1411311; crumbles.blog Nov 2025.

Datalog, dataflow, actors: Soufflé docs and magic-set; Vélus PLDI 2017; Lustre Verimag; CoCompiler arXiv:2510.00210; Orca OOPSLA 2017 (Clebsch et al., doi 10.1145/3133896); Pony GC tutorial.

Effects 2024-2025: Soundly Handling Linearity POPL 2024; Affect POPL 2025; Handling the Selection Monad PLDI 2025; "Simplifying explicit subtyping coercions" arXiv 2024.
