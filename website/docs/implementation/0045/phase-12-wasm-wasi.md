---
title: "Phase 12. WASM / WASI"
sidebar_position: 14
sidebar_label: "Phase 12. WASM / WASI"
description: "MEP-45 Phase 12 tracking: wasm32-wasi target via zig cc (ships wasi-libc); wasmtime run gate in CI."
---

# Phase 12. WASM / WASI

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-45 §Phases · Phase 12](/docs/mep/mep-0045#phase-12-wasm--wasi) |
| Status         | IN PROGRESS |
| Started        | 2026-05-26 00:12 (GMT+7) |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

Every Phase 1-10 fixture compiles via `mochi build --target=wasm32-wasi` and runs byte-equal vs vm3 under `wasmtime`.

## Goal-alignment audit

WASM/WASI is the user-facing payoff for sandboxed and serverless deployment: the same Mochi source that produces a native binary also produces a portable WASM module that runs on any wasmtime/wasmer/browser runtime. Without a WASM target, Mochi cannot reach the growing class of environments that run WASM natively (Fastly Compute, Cloudflare Workers, WASM components). Phase 12.0 uses the zig cc path (which already ships wasi-libc) to add WASM compilation without vendoring a separate wasi-sdk. Aligns directly with user-facing goal.

## Sub-phases

| #    | Scope                                                                                                              | Status      | Commit | PR |
|------|--------------------------------------------------------------------------------------------------------------------|-------------|--------|----|
| 12.0 | `wasm32-wasi` triple routes through `zig cc -target wasm32-wasi` (zig ships wasi-libc, no separate wasi-sdk needed); driver skips darwin-only `-Wl,-no_uuid` and sanitiser flags for wasm targets; `TestPhase12WasmWasi` gate (add_ints compile + wasmtime run); CI: wasmtime install + gate step in `cross-linux` job | LANDED 2026-05-26 00:12 (GMT+7) | — | — |
| 12.1 | Precise allocator + shadow-stack root scanning (currently GC-less; wasi-libc malloc is used directly) | DEFERRED | —      | — |
| 12.2 | Stream/agent surface narrowed: no threading; M:N scheduler collapses to single-fibre cooperative loop              | NOT STARTED | —      | — |
| 12.3 | Full fixture corpus subset under wasmtime in CI (all Phase 1-10 fixtures excluding file_io)                        | NOT STARTED | —      | — |

## Decisions made

**Phase 12.0: zig cc path replaces wasi-sdk vendoring.** The MEP spec originally called for wasi-sdk to be vendored under `transpiler3/c/toolchain/wasi-sdk/`. However, zig cc already bundles wasi-libc and wasm-ld internally; passing `-target wasm32-wasi` to `zig cc` produces a complete WASM binary without any additional SDK. Since Phase 11 already vendors zig cc via `transpiler3/c/toolchain/zig/`, Phase 12.0 reuses the same path. wasi-sdk vendoring is deferred unless a gap in zig's wasi-libc coverage is found.

**Phase 12.0: driver guards for wasm targets.** Three driver flags are skipped for `wasm32*` targets:
1. `-Wl,-no_uuid`: Apple's linker flag; wasm-ld rejects it with an error.
2. `-static`: wasm-ld links statically by default; the flag is redundant and may cause errors.
3. `-fsanitize=address,undefined` (debug profile): sanitisers are not supported for wasm32-wasi.

An `isWasm` boolean derived from `strings.HasPrefix(target, "wasm32")` guards all three. The `-ffile-prefix-map`/`-fdebug-prefix-map` flags are accepted by clang for wasm targets (they affect DWARF sections in the embedded debug info).

**Phase 12.0: runtime is GC-less and wasi-libc compatible.** The mochi runtime uses malloc/free (wasi-libc provides these), setjmp/longjmp (wasi-libc supports), printf (wasi-libc I/O layer), and no platform-specific syscalls. Phase 12.1 (precise GC) is deferred because the runtime already works correctly for the current test corpus with the "leak on exit" model.

**Phase 12.0: wasmtime run gate via `exec.LookPath`.** `TestPhase12WasmWasi` runs `wasmtime run <binary>`. If wasmtime is not on PATH, the test logs a message and returns (compile-only check). CI installs wasmtime via `curl https://wasmtime.dev/install.sh | bash` and adds `~/.wasmtime/bin` to PATH.

**Phase 12.0: file_io excluded from WASM gate.** WASI file I/O requires preopened directories (the `--dir` flag to wasmtime). Phase 12.0 limits the gate to the `primitives/add_ints` fixture; Phase 12.3 will extend to the full corpus with appropriate WASI dir flags.

## Deferred work

- Phase 12.1: precise GC (currently GC-less; malloc/free leaks on exit; deferred until GC design is locked in).
- Phase 12.2: streams/agents narrowed surface (deferred; Phase 9 not yet landed).
- Phase 12.3: full corpus gate (file_io needs `--dir` preopens; csv_adapters similar; query fixtures rely on the arena allocator which should be WASM-clean).
- WasmGC: still drafting on common runtimes in 2026; revisit when WasmGC stabilises in wasmtime + wasmer.

## Closeout notes

Sub-phase 12.0 LANDED. Sub-phases 12.1-12.3 are deferred.
