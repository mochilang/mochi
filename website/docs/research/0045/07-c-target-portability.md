---
name: 5600-research-07-c-target-portability
description: "MEP-45 research note 07. The C target itself. C23 features used, compiler matrix, ABI per arch, libc matrix, sanitisers, reproducibility, hardening, style guide for generated C."
---

# MEP-45 research note 07, The C target and portability

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

This note covers the *output side* of the transpiler. What flavour of
C we emit, what compilers must accept it, which architectures and OSes
get binary artefacts, what hardening is on by default, and what style
rules the generated code follows so that a reviewer (or a tool like
`clang-tidy`) sees consistent output.

## 1. C standard

Target: **C23 (ISO/IEC 9899:2024)**, with a clean fallback to C17 (ISO/IEC
9899:2018) when a target compiler is too old. The hard floor is C11.

Rationale:

- C23 is the first ISO C with `nullptr`, `constexpr`, `typeof`,
  `[[attributes]]`, `_BitInt(N)`, the `<stdckdint.h>` checked-arithmetic
  header, the `<stdbit.h>` bit-manipulation header, `#embed`, and an
  unambiguous two's-complement mandate. Every one of these maps to
  something the transpiler wants to emit:
  - `nullptr` -> the obvious literal for `?T` lowering.
  - `constexpr` -> emit compile-time constants for `let x: int = 42` at
    file scope without `static const` shenanigans.
  - `typeof` -> macros for `mochi_swap(a, b)` and the generic helpers
    without `_Generic` selection.
  - `[[fallthrough]]`, `[[maybe_unused]]`, `[[nodiscard]]` -> emit
    cleanly without compiler-specific pragmas.
  - `_BitInt(N)` -> a future arbitrary-precision integer story.
  - `<stdckdint.h>` `ckd_add/sub/mul` -> integer overflow checks in
    debug mode without home-grown intrinsics.
  - `<stdbit.h>` `stdc_count_ones` etc -> bit ops without compiler
    intrinsics.
  - `#embed` -> a no-fuss way to bake the standard library's `.mochi`
    sources into the binary.
  - Two's complement guarantee -> we can rely on bit-level integer
    semantics without per-platform conditionals.

The C17 fallback uses, in order: compiler-specific equivalents
(`__typeof__`, `__attribute__`, `__builtin_*`), then home-grown
intrinsics in `mochi/core/compat.h`. The transpiler picks the floor
based on the detected compiler, not the user (so the user gets the
best output their compiler can take).

## 2. Compiler matrix

| Compiler | Version floor | C23 status (2026) | Notes |
|---------|---------------|---|---|
| clang | 18.0 | Most of C23 in `-std=c23`. Missing: full `#embed` in some 18.x; complete in 19+ | Default driver for `mochi build`; clang is what `zig cc` ships. |
| gcc | 14.0 | Most of C23 in `-std=c23`. `#embed` since 15. | Default on most Linux distros by 2026. |
| msvc | 19.40 (VS 2022 17.10) | Partial. `nullptr`, `constexpr`, attributes mostly there. `_BitInt` no. | We compile through `clang-cl` on Windows in the supported config; `cl.exe` direct is best-effort. |
| zig cc | 0.16 | Whatever the bundled clang has; 0.16 carries clang 19+. | The cross-compilation driver we use. |
| cosmocc | 4.x | Bundled clang or gcc; whichever Justine ships. | Used for the APE build. |
| tcc | mob | C11 + extensions; no C23. | `--ffast-build` fallback for `mochi run`-style quick iteration. Not for releases. |
| chibicc, cproc | various | C11 baseline | Reference targets to keep the generated C *un-clever*. |

Concretely: if the generated C compiles under `gcc -std=c17 -Wall -Wextra
-Wpedantic` it must also compile under `clang -std=c23` and `zig cc
-std=c23`. The CI matrix bakes this in.

## 3. Architectures and OSes

### Tier 1 (gated on every PR)

- `x86_64-linux-gnu` (glibc), `x86_64-linux-musl`
- `aarch64-linux-gnu`, `aarch64-linux-musl`
- `aarch64-darwin` (macOS 14+ on Apple silicon)
- `x86_64-darwin` (macOS 13+; still alive in 2026 for a while)
- `x86_64-windows-msvc` (clang-cl), `x86_64-windows-gnu` (mingw via zig
  cc)
- `wasm32-wasi` (WASI 0.2)

### Tier 2 (nightly)

- `aarch64-windows-msvc`
- `riscv64-linux-gnu`
- `armv7-linux-gnueabihf`
- `x86_64-freebsd`, `x86_64-openbsd`, `x86_64-netbsd`
- `wasm32-emscripten` (browser playground)

### Tier 3 (best effort, no gate)

- `aarch64-android`, `x86_64-android`
- `aarch64-ios`, `aarch64-ios-sim`
- `loongarch64-linux-gnu`
- `s390x-linux-gnu`, `powerpc64le-linux-gnu`

## 4. ABI and calling conventions

The transpiler emits *standard system V*, *AAPCS64*, *Windows x64*, or
*RISC-V LP64D* depending on target, by virtue of using the system C
compiler. We do not pick a custom calling convention. Closure pointers
follow the host pointer width.

For variadic-style internal helpers we never use C variadics (`...`)
because of the AAPCS64 surprise on Apple silicon (variadic args go on
the stack, not in registers). Instead, we emit explicit overloads or
wrap the args in a struct.

### Pointer width

`mochi_int` is always `int64_t` regardless of host pointer width.
`mochi_uint` is `uint64_t`. We do not use `intptr_t` semantics for
language integers. Pointers stay opaque.

### Endianness

We assume little-endian for the BG corpus byte-equality fixtures; on
big-endian targets the runtime byteswaps on JSON and on the binary
fixture compare. The codegen does not generate endian-specific code.

### Alignment

Records align to the largest field's natural alignment. `mochi_list__T`
data buffers align to 16 bytes (SIMD-friendly, fits cwisstable's
contract).

## 5. libc matrix

| libc | Properties | Notes |
|------|------------|---|
| glibc 2.38+ | full POSIX, GNU extensions | Linux desktop and server default. |
| musl 1.2.5+ | small, static-friendly | Alpine, embedded; lacks some GNU bits. |
| Bionic | Android | Different sigaction, dlfcn quirks. |
| Apple libSystem | macOS | Variadic ABI quirk; mostly POSIX. |
| ucrt / Universal C Runtime | Windows | clang-cl + ucrt is our path. |
| WASI libc | wasi-libc | Roughly POSIX-shaped; no signals, no fork. |
| Cosmopolitan libc | Justine's portable libc | Mostly POSIX-compatible. |

The runtime's portability shim `mochi/core/sys.h` smooths over:

- `clock_gettime` vs. `mach_absolute_time` vs. `QueryPerformanceCounter`.
- `pthread_*` vs. Windows threads (via a thin wrapper; we do not use
  C11 threads because MSVC ucrt is patchy).
- `mmap` vs. `VirtualAlloc`.
- `dlopen`/`dlsym` vs. `LoadLibraryW`/`GetProcAddress`.
- File path handling: UTF-8 in, the runtime widens to UTF-16 on
  Windows.

## 6. Sanitisers and dynamic checks

| Sanitiser | Compiler | Use |
|-----------|----------|---|
| AddressSanitizer | clang, gcc, msvc | mandatory in CI matrix |
| UndefinedBehaviorSanitizer | clang, gcc | mandatory; catches signed overflow, OOB, alignment |
| ThreadSanitizer | clang, gcc | nightly, with scheduler-stress fixtures |
| MemorySanitizer | clang | nightly, finds uninit reads |
| LeakSanitizer | clang, gcc | mandatory; rules out runtime leaks under BDWGC false retention thresholds |
| ControlFlowIntegrity | clang | `--secure` profile only |
| SafeStack | clang | `--secure` profile only |
| ShadowCallStack | clang | aarch64 only |

The transpiler's output is *sanitiser-friendly*: no pointer
arithmetic across array boundaries, no `union` punning across struct
boundaries, no implicit integer conversions that lose precision (the
codegen always emits explicit casts), no `memcpy` of overlapping
ranges (the codegen uses `memmove` when overlap is possible).

## 7. Reproducibility

- `SOURCE_DATE_EPOCH` honoured throughout: the codegen never embeds
  `__DATE__`/`__TIME__`; the build driver passes
  `-D__DATE__=\"redacted\"` `-D__TIME__=\"redacted\"`.
- `-ffile-prefix-map=$PWD=.`, `-fdebug-prefix-map=$PWD=.` strip absolute
  paths.
- Static-link everything except libc; the libc itself is the only
  per-system varying piece.
- The codegen orders functions and globals by sorted MIR identifier,
  not by hash-map iteration order.
- The build driver sorts source files before passing to the C compiler
  to keep `-flto` cache hits stable.
- Sample artefact digest: we publish SHA-256 of the produced binary
  for each tier-1 target on every release.

## 8. Hardening defaults

- `-fstack-protector-strong`
- `-D_FORTIFY_SOURCE=3` (glibc 2.34+; falls back to `2` on older)
- `-fPIE` for executables, `-fPIC` for shared libs
- `-Wl,-z,relro,-z,now` on Linux
- `-Wl,--icf=safe` for dead-code merging (clang/gold/lld)
- `/guard:cf` on MSVC
- `-Wl,-pie`, `-Wl,--dynamicbase`, `-Wl,--nxcompat` on Windows
- `-fcf-protection=full` on x86 CET-capable; `-mbranch-protection=standard`
  on aarch64 (PAC + BTI)
- Stripped debug info into a `.dSYM`/`.debug` sidecar; the main binary
  is small and stable.

Profiles:

- `--debug`: sanitisers on, no LTO, `-O0 -g3`, asserts on.
- `--dev` (default): UBSan on, `-O1 -g`, asserts on.
- `--release`: LTO on, `-O2`, asserts off, debug info to sidecar.
- `--secure`: release + CFI + SafeStack + heap quarantine
  (mimalloc-secure or scudo).
- `--small`: `-Os`, no LTO, strip aggressively (kiosk / embedded).

## 9. Style guide for emitted C

The generated C is meant to be *human-readable*. Rules:

1. Two-space indent. No tabs. (LLVM style, easier to diff.)
2. One declaration per line. `int x = 1; int y = 2;` is two lines.
3. Always braces on `if`/`else`/`for`/`while`, including single-stmt.
4. `goto` only for the cleanup-cascade pattern (`goto err_close;`).
5. No macros except in `mochi/core/*.h` runtime headers. Generated code
   uses inline functions.
6. No `static inline` in `.c` files; only in headers.
7. Every function gets a leading source-comment with the Mochi source
   span: `// mochi: foo.mochi:42:1-46:1 fn add`.
8. `#line` directives on every emitted statement so that gdb's `list`
   shows Mochi source.
9. Identifier mangling per note 05 §3; no two emitted identifiers
   collide.
10. No reserved-identifier reuse: never `_Foo`, never `__bar`, never
    identifiers in the implementation's namespace.
11. The first lines of every generated `.c` file are:
    ```c
    /* Generated by mochi {version} for target {triple}.
       Do not edit. */
    #include "mochi/core.h"
    #include "{module}.h"
    ```
12. Header guards use `MOCHI__{pkg}__{module}__H_` form.
13. No trigraphs, no digraphs, no `<iso646.h>`-style identifiers.
14. No K&R declarations. ANSI prototypes only.
15. Integer literals carry the right suffix: `42L`, `42LL`, `42U`,
    `42ULL` as needed; never rely on default-int rules.

## 10. Diagnostics

The C compiler's diagnostics are *almost never* what the user wants to
see. Two halves of the design:

- The transpiler runs its own type-checker first; nearly all errors
  surface there with proper Mochi spans and good messages.
- For errors that escape (rare: macro expansions, header-only library
  conflicts, linker errors), the build driver post-processes the C
  compiler's output and rewrites file:line refs back to Mochi spans
  using a side-map generated at codegen time.

The driver also collapses include-cascade noise (`In file included
from ... In file included from ...`) into a single line. Inspired by
Rust's diagnostic UX.

## 11. Test gate per target

For tier 1, every PR runs:

1. Build the runtime + amalgamated `libmochi.a` under each compiler.
2. Build every fixture under `tests/cross-aot/bg/<target>/`.
3. Run every binary under each tier-1 target where the host can run
   it (`qemu-user-static` for cross-arch on Linux CI).
4. Byte-equal stdout vs. the VM-recorded `expect` files.
5. Sanitiser pass on a curated subset (the full corpus is heavy).
6. Diff-stat the generated C against the previous release commit; large
   regressions on identical source flagged for review.

## 12. Open questions for the MEP body

1. Whether to require C23 or accept C17 fallback as supported
   forever. Cost of supporting C17 is per-feature compat shims.
2. Whether MSVC direct (not via clang-cl) is tier 1 or tier 2.
3. Whether `wasm32-wasi` gets the same test gate as native or a slimmer
   one (no fork/exec).
4. How aggressively to apply hardening on the `--release` profile;
   stack canaries cost throughput.
5. Whether the `--secure` profile is a CI gate (it currently is not).

These need MEP-body resolutions.
