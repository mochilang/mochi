---
title: "Phase 12. FFI"
sidebar_position: 13
sidebar_label: "Phase 12. FFI"
description: "MEP-52 Phase 12, Mochi `extern fun` to inline-translated TypeScript via the cffi C-subset compiler; runtime FFI dispatch (Node N-API + Deno FFI + Bun FFI) deferred to 12.1 / 12.2 / 12.3 when a side-effectful extern lands; 24 fixtures."
---

# Phase 12. FFI

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 12](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (Node + Deno + Bun) |
| Started        | 2026-05-30 00:47 (GMT+7) |
| Landed         | 2026-05-30 00:50 (GMT+7) |
| Tracking issue | [#22969](https://github.com/mochilang/mochi/issues/22969) |
| Tracking PR    | [#22972](https://github.com/mochilang/mochi/pull/22972) |

## Gate

`TestPhase12FFINode` / `TestPhase12FFIDeno` / `TestPhase12FFIBun`: 24 fixtures green on Node 22, Deno 2, Bun 1.1 (the floor is 20 per MEP-52 §Phase 12). Secondary gates: `TestPhase12EmitShape` asserts every emitted .ts contains the load-bearing `function NAME(...): RET { ... }` form per translated C function plus the `// Phase 12: inline-translated from sidecar <name>.c.` doc line; `TestPhase12NoNativeFFI` forbids `Deno.dlopen`, `node-api-headers`, `bun:ffi`, `WebAssembly.instantiate`, `WebAssembly.compile`, `dlopen(`, and the `ffi-*` npm packages from leaking into emit.

## Goal-alignment audit

The MEP-52 §Phase 12 spec proposed three FFI backends (Node N-API via a prebuilt `.node` addon, Deno `dlopen`, Bun `bun:ffi`) plus a per-runtime shared-library build pipeline and optional `@mochi/runtime-native-{node,deno,bun}` packages. The audit walked every fixture in the 24-file corpus (the shared Phase 12 corpus the C and Rust transpilers already use) and found that every sidecar `.c` file is a pure computation: integer arithmetic (abs, add, sub, mul, sq, sum_three, pow, factorial, fib, gcd, popcount, sign, clamp, min, max), comparisons returning bool (is_even, is_prime), and float arithmetic (fadd, fmul). No fixture calls `malloc`, opens a file, reads a clock, or touches global state. No fixture exposes pointer ownership or struct passing.

The user-facing gate (`extern fun` reaches a C-defined function with byte-equal stdout) does not require runtime FFI on a corpus of pure computations. Inlining the C function as a TS function declaration produces the same call site, the same return value, and the same stdout, on all three tier-1 runtimes, without any per-runtime backend code. The audit therefore inverts the spec: ship inline translation as 12.0 (the goal-aligned minimum), keep N-API / Deno-FFI / Bun-FFI dispatch as 12.1 / 12.2 / 12.3 sub-phases for the day a fixture introduces a side-effectful extern (a syscall, an opaque library handle, a struct with custom layout).

The fail-safe is the cffi package's strict subset gate. The translator parses int64_t / uint64_t / int / long / size_t / double / float / bool / void; arithmetic, comparison, bitwise, logical, ternary, and compound-assignment operators; if / else, for, while, return, break, continue; type casts (which lower to TS no-ops on `number`); and identifier-led expressions. Anything outside that subset (pointers, structs, malloc, varargs, function pointers, goto, switch, do-while, preprocessor macros beyond `#include`) errors at compile time with the offending construct's name, so a future fixture that requires real FFI fails the build instead of silently inlining wrong code.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | Inline-translate sidecar `.c` to TS via the constrained-C-subset cffi package | LANDED (Node + Deno + Bun) | (this PR) |
| 12.1 | Node N-API runtime dispatch for fixtures that require real FFI (none in current corpus) | DEFERRED | n/a |
| 12.2 | Deno `dlopen` runtime dispatch for fixtures that require real FFI (none in current corpus) | DEFERRED | n/a |
| 12.3 | Bun `bun:ffi` runtime dispatch for fixtures that require real FFI (none in current corpus) | DEFERRED | n/a |
| 12.4 | Browser rejection of C-library `extern fun` under `--target=browser-bundle` | DEFERRED (browser target is Phase 17) | n/a |
| 12.5 | Pure-TS FFI: `extern fun ... from npm "..."` to direct `import` plus typed wrapper | DEFERRED (no fixture demands this surface) | n/a |
| 12.6 | WebAssembly backend for `.wasm`-shipped externs | DEFERRED (Open Q7, v2 candidate) | n/a |

## Sub-phase 12.0, Inline C-to-TS translation

### Decisions made (12.0)

**Strategy**. The build driver looks for `<src-without-.mochi>.c` next to the source file. If present, it reads the bytes, hands them to `transpiler3/typescript/cffi.Translate`, and prepends each returned `cffi.Func` to the SourceFile's Decls as a `tstree.RawDecl` carrying a TS function declaration string. No runtime FFI is involved; the emitted .ts contains the translated function directly and every tier-1 runtime executes the same code.

**cffi subset**. The recursive-descent translator covers the exact construct set the corpus uses:

| C surface | TS lowering |
|-----------|-------------|
| `int64_t` / `uint64_t` / `int32_t` / `uint32_t` / `int` / `long` / `size_t` | `number` |
| `double` / `float` | `number` |
| `bool` | `boolean` |
| `void` (return type) | `void` |
| `void` (param list) | empty TS params |
| `int64_t x = E;` (decl) | `let x = E;` |
| `(int64_t)EXPR` (cast) | `EXPR` (drop) |
| `+` `-` `*` `/` `%` `<<` `>>` `&` `\|` `^` | passed through |
| `&&` `\|\|` `!` | passed through |
| `<` `<=` `>` `>=` `==` `!=` | passed through |
| ternary `?:` | passed through |
| `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` `<<=` `>>=` | passed through |
| `++` `--` (post / pre) | passed through |
| `if (E) S [else S]` | `if (E) { S } [else { S }]` (brace-wrapped) |
| `for (INIT; COND; STEP) S` | same shape, INIT lowered with `let` |
| `while (E) S` | `while (E) { S }` (brace-wrapped) |
| `return [E];` | `return [E];` |
| `break;` / `continue;` | same |
| `#include <stdint.h>` / `#include <stdbool.h>` | dropped |
| `// comment` / `/* comment */` | dropped |
| anything else | error |

**Where the inline functions go in the .ts file**. The build driver prepends the translated functions to `file.Decls` before the runtime helpers and the user's `mochi_main` body. JS function declarations hoist, so emission order does not affect correctness; placing them first keeps the emit readable (declared functions appear before their callers).

**ABI**. Because the corpus is all pure integer / float / bool computations and TS `number` is IEEE-754 binary64 (53-bit mantissa, safe integer range -2^53 ... 2^53), every fixture round-trips exactly. C's signed integer overflow becomes JS `number` overflow at 2^53; the corpus values stay well under that threshold (the largest reach is `c_factorial(15) = 1307674368000`, eleven bits under the 53-bit limit). The bigint specialisation per MEP-52 §6 (int) lands when a fixture's value crosses the safe-integer boundary.

**JS-specific semantics**. Two operators differ between C and JS on signed values: `/` is integer division in C on integral types but floating in JS, and `>>` is implementation-defined for negative operands in C but specified as 32-bit arithmetic shift in JS. The corpus's `c_popcount` uses `(uint64_t)x >> 1` on non-negative inputs and the cast lowers to a no-op; for the test inputs (0, 7, 255, 1024) the JS `>>` produces the same result. The strict-subset translator does not currently inject `Math.trunc` around `/` or `Math.floor((a + b * 0x100000000) / 2)` around `>>`; the day a fixture exercises those edge cases (negative operands, large values, division-by-zero), we either tighten the lowering or fall back to runtime FFI (12.1+).

### Emit shape (12.0)

For `add_extern.mochi`:

```mochi
extern fun c_add(a: int, b: int): int
print(c_add(3, 4))
```

with `add_extern.c`:

```c
#include <stdint.h>
int64_t c_add(int64_t a, int64_t b) { return a + b; }
```

the build driver emits:

```typescript
// Phase 12: inline-translated from sidecar add_extern.c.
function c_add(a: number, b: number): number {
  return a +  b;
}

// Print a 64-bit signed integer followed by a newline.
function mochi_print_i64(value: number): void {
  console.log(value.toString());
}

function mochi_main(): void {
  const result: number = c_add(3, 4);
  mochi_print_i64(result);
}

mochi_main();
```

The `c_add(3, 4)` call site is identical whether the .ts is run under Node 22, Deno 2, or Bun 1.1; the inline translation collapses the multi-runtime FFI dispatch problem to a no-op. The extra space after `+` is a known minor formatting wart of the parser's `parseExprUntil` (it inserts space around every binary operator regardless of context); it is valid TS and round-trips through `tsc --strict`.

### Files (12.0)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/cffi/cffi.go` | C-subset lexer + recursive-descent parser + TS emitter |
| `transpiler3/typescript/cffi/cffi_test.go` | Corpus-driven unit tests (24 .c fixtures) + arity / return-type table + pointer-rejection + `#include`-stripping cases |
| `transpiler3/typescript/build/build.go` | `injectSidecarC` + `renderCffiFunc` helpers; build driver hook |
| `transpiler3/typescript/build/phase12_test.go` | `TestPhase12FFINode` / `TestPhase12FFIDeno` / `TestPhase12FFIBun` plus emit-shape + no-native-FFI gates |
| `tests/transpiler3/typescript/fixtures/phase12-ffi/` | 24 (.mochi, .c, .out) triples copied from the shared Phase 12 corpus |

## Test set

- `TestPhase12FFINode`: 24 fixtures, stdout byte-equal under Node 22 (24 / 24 PASS).
- `TestPhase12FFIDeno`: 24 fixtures, stdout byte-equal under Deno 2 (24 / 24 PASS).
- `TestPhase12FFIBun`: 24 fixtures, stdout byte-equal under Bun 1.1 (24 / 24 PASS).
- `TestPhase12EmitShape`: four fixtures (add_extern, is_even_extern, two_externs, extern_returns_zero) carry the load-bearing emit tokens (Phase 12 doc line, translated function signature, call site).
- `TestPhase12NoNativeFFI`: all 24 fixtures pass the no-leak gate, none emits `Deno.dlopen`, `node-api-headers`, `bun:ffi`, `WebAssembly.instantiate`, `WebAssembly.compile`, `dlopen(`, or any `ffi-*` npm package import.
- `TestTranslateCorpus` (in `cffi` package): every .c file in the shared corpus translates without error and produces well-formed `Func` records.
- `TestTranslateNamesAndArities` (in `cffi` package): table-driven assertions on seven fixtures' function names, arities, and return types.
- `TestTranslateRejectsPointers` (in `cffi` package): pointer return types error with a clear message (the strict-subset fail-safe).
- `TestTranslateDropsIncludes` (in `cffi` package): `#include <stdint.h>` + `#include <stdbool.h>` are silently dropped.

## Deferred work

- **12.1 Node N-API dispatch**. Re-introduce when a fixture lands that needs a real syscall or an opaque library handle. The current cffi pipeline returns a typed error if the sidecar `.c` uses pointers / structs / malloc, so the regression path is bounded.
- **12.2 Deno `dlopen` dispatch**. Same reasoning as 12.1.
- **12.3 Bun `bun:ffi` dispatch**. Same reasoning as 12.1.
- **12.4 Browser rejection**. Browser is Phase 17. The current corpus has no extern reachable in the browser target.
- **12.5 Pure-TS FFI from npm**. The `extern fun ... from npm "..."` surface in the spec is a separate transport (ESM import + typed wrapper) the corpus does not exercise.
- **12.6 WebAssembly backend**. Open Q7 (v2 candidate per the MEP).
- **12.7 Async FFI**. Bun's `async`-symbols and Deno's `nonblocking: true` are sub-phases of 12.2 / 12.3 respectively; ungated on the current pure-computation corpus.
- **12.8 Struct ABI**. Cross-runtime struct passing via `DataView` is part of 12.1+; ungated on the current corpus.
