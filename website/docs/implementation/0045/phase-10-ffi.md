---
title: "Phase 10. FFI shells"
sidebar_position: 12
sidebar_label: "Phase 10. FFI"
description: "MEP-45 Phase 10 tracking: C-direct FFI in v1, boxed mochi_value at boundary; Go/Python/TS via deferred sub-phases."
---

# Phase 10. FFI shells

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 10](/docs/mep/mep-0045#phase-10-ffi-shells) |
| Status         | IN PROGRESS |
| Started        | 2026-05-25 23:52 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

C-direct FFI fixture suite (~15 cases: call a vendored C function, pass scalars + strings + records, return scalars + records, error propagation) compiles + runs byte-equal vs vm3 on host triple.

## Goal-alignment audit

C-direct FFI is the natural FFI for a C-AOT target: the generated C and the user C share an address space, so calls are zero-overhead and marshalling is trivial for scalar + string types. Without FFI, Mochi AOT programs cannot call any external C library, which limits their practical usefulness. Phase 10.0 lands the minimum viable binding path (scalar + string args/returns, neighbour `.c` file); later sub-phases extend to boxed values and other language runtimes. Aligns directly with the user-facing goal.

## Sub-phases

| #    | Scope                                                                                                                       | Status      | Commit | PR |
|------|-----------------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 10.0 | `extern fun` declarations lower to `extern <ctype> <name>(<params>);` in the C prologue; calls to extern funcs use `CallExpr` (direct C call, no closure ABI); driver compiles `<stem>.c` neighbour alongside `main.c`; verifier extended to accept extern calls; `TestPhase10FFIDirect` gate (2 fixtures: `add_extern`, `str_len_extern`) | LANDED 2026-05-25 23:52 (GMT+7) | — | — |
| 10.1 | Boxed `mochi_value_t` tagged union (`nil`, `bool`, `int`, `float`, `str`, `handle`); `ValueType` in aotir + type-checker; `"value"` type alias in `extern fun` declarations; `mochi/value.h` + `src/value.c` in runtime; six constructor + six predicate + five accessor + one tag helper; `TestPhase10BoxedValue` gate (8 fixtures) | LANDED 2026-05-26 06:04 (GMT+7) | — | — |
| 10.2 | Go FFI via Unix-domain RPC (deferred sub-phase; ships after C-direct is green)                                              | NOT STARTED | —      | — |
| 10.3 | Python FFI via embedded libpython3 (deferred sub-phase)                                                                     | NOT STARTED | —      | — |
| 10.4 | TypeScript FFI via QuickJS-NG (deferred sub-phase)                                                                          | NOT STARTED | —      | — |

## Phase 10.1 decisions

**`mochi_value_t` is a tagged union, not a void pointer.** A void pointer would be smaller (8 bytes) but would require the C side to know the layout of every Mochi type. A tagged union (16 bytes on LP64: 4-byte tag + 4-byte padding + 8-byte payload) is self-describing: any C function that receives a `mochi_value_t` can check the tag and route to the correct payload field without extra type information.

**Six tags: nil, bool, int, float, str, handle.** Covers all five Mochi scalar kinds plus an opaque C pointer. Records and collections are not yet supported at the FFI boundary (they would require deep marshalling); that is deferred to a later sub-phase. `nil` is a first-class tag so that C functions can return "no result" without using a sentinel value.

**`value` is a reserved type keyword in `extern fun` declarations only.** The type-checker (`types/resolve.go`, `types/unify.go`, `types/infer.go`, `types/subtype.go`) recognises `ValueType` as a concrete type. At the Mochi language level, `value` cannot be used in user function signatures or arithmetic expressions; it is only legal in `extern fun` parameter and return type positions (the type-checker does not enforce this restriction in Phase 10.1, but the lower pass will reject `value` in positions that don't map to a scalar C type).

**Pass `mochi_value_t` by value (16 bytes).** All architectures in the tier-1 matrix support passing 16-byte structs in registers (two 64-bit registers on x86_64 via System V AMD64 ABI; Q register pair on AArch64). No pointer indirection required.

**`mochi/value.h` included unconditionally in the prologue.** Matches the pattern of `mochi/strings.h`, `mochi/fileio.h`, etc. The linker strips unused symbols; including the header unconditionally avoids conditional logic in the emitter.

**`value.c` added to embed.FS.** `runtime/embed.go` lists the new files so `writeRuntimeFiles` stages them into every build's work directory automatically.

## Decisions made

**Phase 10.0: `extern fun` declarations use direct C call ABI (not closure ABI).** User-defined Mochi functions use a `static <type> <name>(...)` definition and are called via `CallExpr.Func` which emits `name(args...)`. Extern functions use the same `CallExpr` path; the emitter emits `name(args...)` without any `mochi_` prefix since the extern name is the C symbol name directly. The closure ABI (`FunCallExpr`) is not used for extern functions because there is no env pointer.

**Phase 10.0: dotted extern names map to underscored C identifiers.** `extern fun math.sin(x: float): float` emits `extern double math_sin(double x);` in C (replacing `.` with `_`). This preserves namespacing from the Mochi side while producing a valid C identifier.

**Phase 10.0: neighbour `.c` is copied to workDir root (not `src/`).** The workDir `src/` directory is reserved for the Mochi libmochi runtime sources. The neighbour `.c` is written to `workDir/extern_<stem>.c` and added to the cc command after the runtime sources. The `-I <workDir>/include` flag gives it access to `mochi/print.h` and other libmochi headers.

**Phase 10.0: verifier extended to accept extern calls.** The aotir verifier builds `externFns` from `prog.ExternFuncs` and makes it available via `verifyCtx`. `resolveCallSig` checks `ctx.externFns` as a third fallback (after builtins and user functions). The `verifyExprCtx` switch for `*CallExpr` accepts extern calls in both statement and expression positions.

## Deferred work

- Phase 10.1: boxed `mochi_value` (deferred; Phase 10.0 covers scalar + string which covers the main use cases)
- Phase 10.2-10.4: multi-language FFI (deferred; each needs a sub-phase after 10.1 lands)
- Go c-archive route (in-process, no RPC): v2, alongside 10.2 review.

## Closeout notes

Phase 10.0 LANDED. The remaining sub-phases (10.1-10.4) are deferred until the gate test corpus is expanded to cover records and the boxed `mochi_value` type.
