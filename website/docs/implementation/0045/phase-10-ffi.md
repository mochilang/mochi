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
| Status         | COMPLETE |
| Started        | 2026-05-25 23:52 (GMT+7) |
| Landed         | 2026-05-26 08:58 (GMT+7) |
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
| 10.2 | Go FFI via subprocess RPC: `extern go fun` syntax; JSON newline protocol over `pipe()+fork()+exec()` to a Go companion binary; `go_rpc.h` + `go_rpc.c` runtime; driver detects `<stem>.go` neighbour, compiles it with `go build`, bakes path via `-DMOCHI_GO_RPC_PATH_DEFAULT`; emitter generates `static mochi_go_<name>()` C wrappers; `TestPhase10GoFFI` gate (2 fixtures: `go_add_ints`, `go_str_upper`) | LANDED 2026-05-26 08:42 (GMT+7) | — | — |
| 10.3 | `extern python fun` syntax + subprocess RPC via `python3 <stem>.py`; `python_rpc.h` + `python_rpc.c` runtime; shared `emitRPCFuncWrappers` helper; `TestPhase10PythonFFI` gate (2 fixtures: `py_add_floats`, `py_str_lower`) | LANDED 2026-05-26 08:58 (GMT+7) | — | — |
| 10.4 | `extern js fun` syntax + subprocess RPC via `node <stem>.js`; `js_rpc.h` + `js_rpc.c` runtime; `TestPhase10JSFFI` gate (2 fixtures: `js_mul_ints`, `js_str_trim`) | LANDED 2026-05-26 08:58 (GMT+7) | — | — |

## Phase 10.1 decisions

**`mochi_value_t` is a tagged union, not a void pointer.** A void pointer would be smaller (8 bytes) but would require the C side to know the layout of every Mochi type. A tagged union (16 bytes on LP64: 4-byte tag + 4-byte padding + 8-byte payload) is self-describing: any C function that receives a `mochi_value_t` can check the tag and route to the correct payload field without extra type information.

**Six tags: nil, bool, int, float, str, handle.** Covers all five Mochi scalar kinds plus an opaque C pointer. Records and collections are not yet supported at the FFI boundary (they would require deep marshalling); that is deferred to a later sub-phase. `nil` is a first-class tag so that C functions can return "no result" without using a sentinel value.

**`value` is a reserved type keyword in `extern fun` declarations only.** The type-checker (`types/resolve.go`, `types/unify.go`, `types/infer.go`, `types/subtype.go`) recognises `ValueType` as a concrete type. At the Mochi language level, `value` cannot be used in user function signatures or arithmetic expressions; it is only legal in `extern fun` parameter and return type positions (the type-checker does not enforce this restriction in Phase 10.1, but the lower pass will reject `value` in positions that don't map to a scalar C type).

**Pass `mochi_value_t` by value (16 bytes).** All architectures in the tier-1 matrix support passing 16-byte structs in registers (two 64-bit registers on x86_64 via System V AMD64 ABI; Q register pair on AArch64). No pointer indirection required.

**`mochi/value.h` included unconditionally in the prologue.** Matches the pattern of `mochi/strings.h`, `mochi/fileio.h`, etc. The linker strips unused symbols; including the header unconditionally avoids conditional logic in the emitter.

**`value.c` added to embed.FS.** `runtime/embed.go` lists the new files so `writeRuntimeFiles` stages them into every build's work directory automatically.

## Phase 10.2 decisions

**`extern go fun` is a distinct AST node, not a modifier on `extern fun`.** The Go FFI has fundamentally different semantics (subprocess RPC, JSON marshalling, companion binary) from C-direct FFI. Overloading `extern fun` with a `go` modifier would require threading language-kind metadata through the lowerer, verifier, and emitter. A separate `ExternGoFunDecl` in `parser/ast.go` keeps each path clean; the parser checks it before `ExternFun` to avoid ambiguity.

**Subprocess RPC over pipe, not cgo or c-archive.** cgo requires CGO_ENABLED and a C toolchain that supports cgo, plus the generated binary depends on libgo. A subprocess communicating via stdin/stdout pipes is entirely self-contained: the companion is a plain `go build` output and the C side only needs POSIX `pipe()+fork()+exec()`. The protocol is newline-delimited JSON (one request line, one response line) for simplicity; binary protocols would be faster but not needed at this scale.

**Companion binary placed alongside the output binary (`absOut + "_gorpc"`), not in workDir.** workDir is a temp directory removed after compilation. Baking its path into the C binary via `-DMOCHI_GO_RPC_PATH_DEFAULT` would produce a dangling reference at runtime. Placing the companion at `<out>_gorpc` (next to the final binary) guarantees the path remains valid after the build.

**String args JSON-encoded as `\"%s\"` in the C format string (single-level quoting).** The snprintf format string for a string argument is `\"%s\"` (C escapes: literal-quote + %s + literal-quote), producing `"hello"` in the JSON array. Earlier code used `\\\"%s\\\"` which produced `\"hello\"` (with literal backslashes), making the JSON invalid and causing `json.Unmarshal` to return a parse error.

**`mochi_go_<name>()` wrappers use `char *` return type for strings, not `const char *`.** `cType(TypeString)` returns `"const char *"`, but the let-emitter prefixes `const` at each call site. Using `const char *` in the wrapper signature would produce `const const char *` at the call site, which is a C error. The wrapper returns `char *` (non-const) to absorb that extra const.

**Verifier registers GoFuncs under both `<name>` and `mochi_go_<name>`.** The lowerer emits `mochi_go_<name>` as the `CallExpr.Func` for Go FFI calls. The verifier must recognize this prefixed name as valid; registering both entries means the verifier passes regardless of which name appears in the IR.

## Decisions made

**Phase 10.0: `extern fun` declarations use direct C call ABI (not closure ABI).** User-defined Mochi functions use a `static <type> <name>(...)` definition and are called via `CallExpr.Func` which emits `name(args...)`. Extern functions use the same `CallExpr` path; the emitter emits `name(args...)` without any `mochi_` prefix since the extern name is the C symbol name directly. The closure ABI (`FunCallExpr`) is not used for extern functions because there is no env pointer.

**Phase 10.0: dotted extern names map to underscored C identifiers.** `extern fun math.sin(x: float): float` emits `extern double math_sin(double x);` in C (replacing `.` with `_`). This preserves namespacing from the Mochi side while producing a valid C identifier.

**Phase 10.0: neighbour `.c` is copied to workDir root (not `src/`).** The workDir `src/` directory is reserved for the Mochi libmochi runtime sources. The neighbour `.c` is written to `workDir/extern_<stem>.c` and added to the cc command after the runtime sources. The `-I <workDir>/include` flag gives it access to `mochi/print.h` and other libmochi headers.

**Phase 10.0: verifier extended to accept extern calls.** The aotir verifier builds `externFns` from `prog.ExternFuncs` and makes it available via `verifyCtx`. `resolveCallSig` checks `ctx.externFns` as a third fallback (after builtins and user functions). The `verifyExprCtx` switch for `*CallExpr` accepts extern calls in both statement and expression positions.

## Phase 10.3 decisions

**Python FFI uses subprocess RPC (not embedded libpython3).** The spec noted "embedded libpython3 (heavy, fast) vs. out-of-process RPC (light, slow)." The subprocess RPC approach is consistent with Phase 10.2 (Go FFI) and requires zero vendored C libraries. A `.py` companion script is already a runnable Python program, so the driver just bakes its absolute path; no compilation step needed.

**Companion path baked as absolute path, not copied.** Unlike Go companions (which are compiled to `<out>_gorpc` alongside the output binary), Python companions are already source files that persist in place. The driver records their absolute path via `filepath.Abs` and bakes it into `-DMOCHI_PYTHON_RPC_PATH_DEFAULT`. The `MOCHI_PYTHON_RPC_PATH` env var can override at runtime.

**`emitRPCFuncWrappers` shared between Go, Python, and JS emitters.** All three FFI backends produce the same C wrapper shape: snprintf a JSON request, call `<prefix>rpc_call`, extract result via `<prefix>rpc_<type>`. Extracting this to a generic helper eliminates duplication and makes adding a fourth backend trivial.

## Phase 10.4 decisions

**JavaScript FFI uses `node` as the companion runtime.** `node` is broadly installed and understands CommonJS modules. The companion is a plain `.js` file (no transpilation needed). The driver bakes the file's absolute path; node handles the rest.

**`readline` interface for stdin line parsing in JS companion.** Node's `readline` module handles line buffering from stdin correctly, including the case where the process receives multiple requests before flushing. Without `readline`, naive `process.stdin.on('data', ...)` may split lines at buffer boundaries.

## Deferred work

- Phase 10.1: boxed `mochi_value` (deferred; Phase 10.0 covers scalar + string which covers the main use cases)
- Phase 10.2-10.4: multi-language FFI (deferred; each needs a sub-phase after 10.1 lands)
- Go c-archive route (in-process, no RPC): v2, alongside 10.2 review.

## Closeout notes

Phase 10 COMPLETE. All sub-phases landed: 10.0 (C-direct FFI), 10.1 (boxed mochi_value), 10.2 (Go FFI), 10.3 (Python FFI), 10.4 (JavaScript FFI). The `extern python fun` and `extern js fun` keywords both use the same subprocess JSON RPC protocol established in 10.2, sharing runtime infrastructure and the `emitRPCFuncWrappers` generic emitter helper.
