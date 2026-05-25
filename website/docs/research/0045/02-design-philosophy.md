# MEP-45 research note 02, Design philosophy

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

This note records the *principles* the C target should be designed against,
written before any code is touched, so that the implementation can be
audited against them later. The philosophy is distilled from three places:
the language surface (note 01), the Mochi project's own existing
priorities (`README.md` "small, statically typed, … zero-dependency single
binary, … built for clarity, safety, and expressiveness"), and the modern
state of the art that the background research agents are gathering.

## 1. Why a C target at all

There are five concrete reasons a Mochi → C transpiler is worth building
in 2026, even though Mochi already has a fast bytecode VM and a JIT:

1. **AOT distribution.** A C output gives Mochi programs an unencumbered
   path to a single, statically-linked native binary on every triple the
   user's C compiler supports, Linux amd64/arm64, macOS arm64, Windows
   x64, FreeBSD, wasm32-wasi, and embedded targets that the Go-based vm3
   cannot reach (microcontrollers, kernels, hypervisors, restricted
   environments where a Go runtime is unwelcome).
2. **Smaller artifacts.** A linked native binary that pulls in only the
   runtime objects it actually uses can be ~100 KB for a hello-world; a
   Go-runtime binary cannot. Important for CDN-deployed scripts and for
   future "Mochi on your fridge" stories.
3. **Cold-start latency.** Native C, with statically-linked initial heap,
   starts in <1 ms. The Mochi VM cold-start is dominated by Go runtime
   init. This matters for serverless and CLI usage.
4. **Interop with native C libraries.** A C-emitting backend can
   `#include` and call libcurl, libsodium, libsqlite, BLAS, etc. without
   the marshalling cost of `import go`. This unblocks systems-programming
   use cases the VM target cannot reach.
5. **An alternate verifiable lowering.** The threat-model document names
   the verifier as the single point of policy. Having a *second* lowering
   path (vm3 bytecode and C source) is a structural check against
   verifier blind spots: bugs that survive both paths are real semantic
   bugs, not lowering bugs.

These reasons do not override the VM. They make C an additional output
target that the user selects at build time (`mochi build --target=c
hello.mochi`), the same way Go is built or interpreted as the user
prefers.

## 2. Non-goals

What the C target deliberately does **not** try to do:

- **Replace the VM as the canonical runtime.** The VM keeps the
  hot-iterate, REPL, agent-sandbox, and verified-bytecode positions. The
  C target is for AOT shipping.
- **Match the VM's startup cost on multi-megabyte programs.** Optimised C
  link times will be slower than `mochi run`.
- **Expose C-level UB to user code.** A Mochi program that compiles to C
  must remain memory-safe at the source language level: every dereference
  is bounds-checked, every union access is tag-checked, every `int`
  overflow traps in debug builds.
- **Be a "Mochi as a C library" embedding API.** That is a different MEP.
  The C target *emits* C, it is not an API for C callers to invoke Mochi
  ad hoc.
- **Encode every language feature in the first release.** The phasing
  plan in the MEP body explicitly defers `generate`, full agents, full
  Datalog, and the `import go|python|typescript` FFI to later phases. The
  first ship is a *Mochi-Core* subset that covers ~80 % of the SPOJ
  corpus.

## 3. Five guiding principles

### 3.1 Spec-first

Every code-generation rule is documented in the MEP body before it is
implemented. The MEP is the unit of review; the code lands in PRs that
cite the MEP section they implement. This is the "spec-in-sync" rule
already imposed on the Mochi MEP corpus (memory `feedback_spec_in_sync`)
and it applies to MEP-45 from day zero.

### 3.2 Boring C

The C we emit should look like something a competent human would write,
not like the inscrutable output of a bytecode-flattener. Concretely:

- Named locals, not `t1`, `t2`, `t3`.
- Comments that point back to the source line (`// from main.mochi:42`).
- One C function per Mochi function, not a giant trampoline.
- C control flow (`if`, `for`, `while`) for Mochi control flow.
- No goto except for `break`/`continue` to labelled enclosing loops, and
  for exception unwinding where setjmp/longjmp is in play.

This is non-negotiable: the C target is also a teaching tool, a debugging
aid, and an emergency-egress route for users who need to read the output.
Nim's `nimcache` output and Vala's C output both demonstrate that this is
achievable.

### 3.3 No surprises in the C ABI

The runtime is one library (`libmochi.a` / `libmochi.so`) with a stable
header (`mochi.h`). The emitted user code links against it. The header is
*the* contract; semantic versioning applies and the MEP body fixes the
v1 surface.

### 3.4 Portability over performance, where they conflict

The first release targets *correctness on every triple zig cc supports*.
Performance comes from a second pass once the targets are green. This
ordering matters because portability bugs are caught by the CI matrix,
and adding portability later requires rewrites that performance work
makes harder. (Memory `feedback_umbrella_phase_targets`: an umbrella
phase only lands when every target in the matrix is green.)

### 3.5 The C output is verifiable

Every C output should compile under:

- `-std=c23 -Wall -Wextra -Wpedantic -Werror`
- `-fsanitize=address,undefined`
- `-D_FORTIFY_SOURCE=3`
- `-fstack-protector-strong`

with no warnings, on the matrix of (gcc-15, clang-19, zig cc 0.16). Any
warning surfaces a bug in the codegen, not a feature of the user code.
The CI gate enforces this on the BG corpus.

## 4. The shape of the runtime

The runtime split is the design's main lever. The current plan, before
the background-research results land, is:

- **mochi-core**: GC, value-tagging, list, map, set, string, time,
  duration, error, panic, longjmp-based try/catch, print, format.
- **mochi-query**: dataset, group, sort, join, set ops; loaders (CSV,
  JSON, JSONL, YAML, Parquet stub).
- **mochi-net**: fetch (libcurl), generate (provider shims), webhook
  helpers.
- **mochi-stream**: stream/agent scheduler, fiber pool, channel.
- **mochi-logic**: facts, rules, query engine (semi-naive eval).
- **mochi-ffi-go**, **mochi-ffi-python**, **mochi-ffi-ts**: optional
  sidecar processes (one binary per host language) speaking the same
  JSON-over-pipe protocol the existing Go runtime uses.
- **mochi-ffi-c**: header-only macros for direct `#include` + extern
  decl.

Each layer is a separate static archive so that a hello-world program
links only mochi-core (target: <100 KB). A program that uses `generate`
brings in mochi-net + the LLM provider's TLS/HTTP code (~500 KB). A
program that uses streams brings in mochi-stream (~80 KB). This linker
slicing matters because the C target's distribution story depends on
keeping the small case small.

## 5. The taxonomy of what "compiling to C" actually means

A literature review (see notes 03 and 04) shows three distinct
flavours of "language → C" transpilation, and we have to pick:

1. **Fully type-erased**: emit `void *` everywhere, dispatch through a
   uniform value representation. This is what classical Scheme-to-C
   compilers (chicken, gambit) do. Lowest implementation cost, worst
   cache behaviour, hardest to debug.
2. **Boxed-by-default, unboxed-by-analysis**: every value is a tagged
   union by default; escape analysis unboxes when safe. This is the
   Nim / Crystal / OCaml-mlton style.
3. **Monomorphised**: every type instantiation produces its own C type
   and its own C function copy. Generics are specialised. This is the
   Rust / Cone / Roc / TigerBeetle style; closest to "human-readable
   C".

MEP-45 picks **#3 with selective boxing for sum types and `any`-typed
ports**. Concretely:

- `list<int>`, `list<string>`, `list<Point>` are three separate C types.
- A method `area` on `Circle` and on `Square` becomes two separate C
  functions.
- A sum-type variant payload is boxed when the variant carries a non-trivial
  payload, inline when nullary.
- A closure carrying an `int` captures it by value; carrying a `list<T>`
  captures by handle.

Rationale: monomorphisation gives us the small clean C output we need
for principle 3.2, eliminates indirect calls in the hot loops the BG
corpus measures, and aligns with how Roc and TigerBeetle structure their
C-adjacent output. The cost is code-size growth from type-parametric
helpers; the mitigation is amalgamation + LTO, which collapses
identical instantiations.

## 6. Memory management

The principle: **GC by default, opt-in regions for systems users.**

Default heap: a conservative or precise tracing GC. The default option
in note 04 is BDW-GC because it works on every target zig cc supports,
ships in every distro, integrates with C trivially, and Crystal /
Nim-refc-fallback / many Schemes all rely on it for the same reasons.

A second option, considered seriously in note 04, is **Perceus-style
reference counting** in the Koka tradition: the type-checker tracks
unique vs shared values, RC operations are elided where the analysis
proves uniqueness, and a Bacon-Rajan cycle collector cleans up. This
generates predictable-latency code and gives systems users the
"deterministic free" they care about.

The MEP body proposes: ship v1 with BDW-GC behind a single `mochi_gc.h`
abstraction, and reserve `mochi_gc_perceus` as a Phase-3 alternative
that flips at link time. Same C output, different runtime.

## 7. Concurrency

Two layers:

- **Stream/agent dispatch** maps to lightweight tasks. The plan: an
  M:N scheduler over OS threads with assembly-stack-switched
  fibers (minicoro or libco; both BSD-licensed, both portable to
  every triple including wasm via Asyncify). Per-agent serial
  execution; cross-agent parallel.
- **Test replay** uses the same scheduler with `MOCHI_SCHED=det`
  forced; tasks are popped FIFO and stepped one at a time so a `test`
  block sees a deterministic interleaving.

C11 atomics + pthreads underlie the scheduler. Optional io_uring /
kqueue / IOCP / wasi-poll under the libuv-like abstraction.

## 8. Error model

`try/catch` lowers to setjmp/longjmp around a per-fiber exception
buffer. Caught values carry a `mochi_error` struct. This is the same
mechanism Lua, OCaml-bytecode, Nim, and Crystal use; the cost is one
setjmp per `try` (typically <10 ns on M-class hardware) and one frame
of buffer overhead per active try.

The alternative, full algebraic effect handlers compiled in
capability-passing style, is the more elegant lowering and the
background research is gathering papers on it. The MEP body proposes
shipping setjmp/longjmp for v1 and re-evaluating once Phase 2 has
landed; the lowering is sufficiently localised that swapping in effect
handlers later is a Phase-N change rather than an architectural one.

## 9. Build system

One command (`mochi build --target=c -o app foo.mochi`) is the entire
user-facing surface. Under the hood:

1. The transpiler emits one or more `.c` and `.h` files into
   `.mochi-build/`.
2. A bundled `cc` (default: `zig cc`, fallback: the system compiler)
   compiles them.
3. The runtime archives (`libmochi-core.a`, `libmochi-query.a`, ...) are
   shipped inside the `mochi` binary as embedded blobs (Phase 2 uses
   `#embed` from C23).
4. The resulting binary is the user's artifact.

Reproducibility: set `SOURCE_DATE_EPOCH`, pin the zig version, emit a
`.mochi-build-manifest.json` recording every input hash, compiler flag,
and linker order.

## 10. The "what does the C look like?" answer

A sample to anchor the rest of the design. Given:

```mochi
fun double(x: int): int { return x * 2 }
let xs = [1, 2, 3]
for n in xs { print(double(n)) }
```

The emitted C should look approximately like:

```c
// hello.mochi.c, generated by mochi 0.x.y on 2026-05-22T17:00:00Z
#include "mochi/core.h"

static mochi_int hello_double(mochi_int x) {
    return mochi_imul(x, 2);
}

int main(int argc, char **argv) {
    mochi_init(argc, argv);
    mochi_list_int xs = mochi_list_int_of3(1, 2, 3);
    mochi_for_int(n, xs, {
        mochi_print_int(hello_double(n));
        mochi_print_newline();
    });
    mochi_list_int_drop(xs);
    return mochi_shutdown();
}
```

Notes embedded in this sketch that the MEP body must fix:

- `mochi_int` is `int64_t`. Always.
- `mochi_imul` is a checked-multiply macro that traps in debug, plain
  `*` in release (using `<stdckdint.h>` from C23 where present).
- `mochi_list_int` is the monomorphised list type. The `_of3` constructor
  is a varargs-free fixed-arity helper; longer literals call
  `mochi_list_int_from_array(arr, n)`.
- `mochi_for_int` is a macro that wraps a `for` loop with the right
  cursor type; no hidden allocation.
- `mochi_list_int_drop` is RC-aware in the Perceus variant, no-op under
  GC.

This sample passes principle 3.2 (boring C), 3.4 (portable, no
extensions), and 3.5 (compiles under sanitisers).

## 11. Closing, the philosophy in one sentence

> A Mochi-to-C transpiler should emit C that a Mochi-fluent C programmer
> would have written by hand, that runs on every target zig cc supports,
> and that preserves every safety guarantee Mochi makes at the source
> level.

Everything else in MEP-45 is downstream of that statement.
