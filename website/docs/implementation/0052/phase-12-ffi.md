---
title: "Phase 12. FFI"
sidebar_position: 13
sidebar_label: "Phase 12. FFI"
description: "MEP-52 Phase 12, Mochi extern fun to Node N-API + Deno FFI + Bun FFI runtime-dispatched call; pure-TS FFI via npm imports with DefinitelyTyped stubs; browser rejection at codegen; 20 fixtures."
---

# Phase 12. FFI

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 12](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase12FFI`: 20 fixtures green on Node 22, Deno 2, Bun 1.1 (browser is skipped: FFI is rejected at codegen under `--target=browser-bundle`). Secondary gates: tsc strict zero diagnostics including the typed-FFI declarations; the optional native-bindings packages (`@mochi/runtime-native-{node,deno,bun}`) build cleanly on x86_64-linux-gnu, aarch64-linux-gnu, aarch64-darwin.

## Goal-alignment audit

FFI is how Mochi reaches C, Rust, Zig, or any shared-library symbol that the JavaScript runtime cannot expose natively. Each tier-1 runtime exposes a different FFI surface: Node's stable interface is N-API via a pre-compiled `.node` addon; Deno's is `Deno.dlopen(path, symbols)`; Bun's is `bun:ffi` (`dlopen`, `CFunction`, `JSCallback`). MEP-52 surfaces a single `extern fun ...` declaration that compiles to a typed wrapper which dispatches at runtime to the appropriate backend, plus optional pre-built `@mochi/runtime-native-{node,deno,bun}` packages users opt into when native acceleration is needed.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 12.0 | Runtime detection: `mochiRuntime()` returns `"node" \| "deno" \| "bun" \| "browser"` via `globalThis` probes | NOT STARTED | n/a |
| 12.1 | C-library FFI: `extern fun ...` lowers to a typed wrapper that dispatches to Node N-API, Deno `dlopen`, or Bun `bun:ffi` per runtime | NOT STARTED | n/a |
| 12.2 | Pure-TS FFI: `extern fun foo(...) -> ... from "npm-package"` to direct `import { foo } from "npm-package"` plus typed wrapper | NOT STARTED | n/a |
| 12.3 | Browser rejection: `--target=browser-bundle` errors at codegen if any `extern fun` from a C library is reachable | NOT STARTED | n/a |
| 12.4 | Optional native packages `@mochi/runtime-native-{node,deno,bun}` with prebuilt binaries | NOT STARTED | n/a |

## Sub-phase 12.0, Runtime detection

### Decisions made (12.0)

```typescript
// @mochi/runtime/runtime
export type RuntimeName = "node" | "deno" | "bun" | "browser";

export function mochiRuntime(): RuntimeName {
  if (typeof (globalThis as any).Deno !== "undefined") return "deno";
  if (typeof (globalThis as any).Bun !== "undefined") return "bun";
  if (typeof (globalThis as any).process !== "undefined" &&
      typeof (globalThis as any).process.versions?.node === "string") return "node";
  return "browser";
}
```

Probe order matters: Bun defines `process.versions.node` for compatibility, so Bun's own marker must be checked first. Same for Deno (which also implements `process` partially).

**Inlining**: most FFI dispatch sites read `mochiRuntime()` once at module load and store the choice in a `const` for reuse. The emitter generates this idiom at any module that references FFI.

## Sub-phase 12.1, C-library FFI

### Decisions made (12.1)

**Mochi**: `extern fun lz4_compress(input: bytes) -> bytes from "liblz4.so"`

**TypeScript wrapper**:

```typescript
// src/generated/lz4.ts (emitted)
import { mochiRuntime } from "@mochi/runtime/runtime";

interface Lz4Backend {
  compress(input: Uint8Array): Uint8Array;
}

let backend: Lz4Backend | undefined;
async function loadBackend(): Promise<Lz4Backend> {
  if (backend !== undefined) return backend;
  switch (mochiRuntime()) {
    case "node": {
      const mod = await import("@mochi/runtime-native-node/lz4");
      backend = mod.default;
      break;
    }
    case "deno": {
      const lib = (globalThis as any).Deno.dlopen("liblz4.so", {
        LZ4_compress_default: { parameters: ["pointer", "pointer", "i32", "i32"], result: "i32" },
      });
      backend = { compress: (input) => /* call lib.symbols.LZ4_compress_default */ };
      break;
    }
    case "bun": {
      const { dlopen, FFIType } = await import("bun:ffi");
      const { symbols } = dlopen("liblz4.so", {
        LZ4_compress_default: { args: [FFIType.ptr, FFIType.ptr, FFIType.i32, FFIType.i32], returns: FFIType.i32 },
      });
      backend = { compress: (input) => /* call symbols.LZ4_compress_default */ };
      break;
    }
    case "browser":
      throw new Error("FFI not available in browser bundle");
  }
  return backend!;
}

export async function lz4_compress(input: Uint8Array): Promise<Uint8Array> {
  const b = await loadBackend();
  return b.compress(input);
}
```

**Why per-runtime branch in the emitted file**: bundlers tree-shake the unreachable branches under the `"browser"` export condition, so the browser bundle never contains the Node or Bun FFI code paths. For Node, Deno, and Bun the file ships intact; the runtime detection routes once per process.

**Calling convention**: pointers, ints, floats, structs follow each backend's native marshalling. The emitter generates the marshalling per declared signature; complex (struct) types route through a runtime helper that uses `DataView` for cross-runtime portability.

## Sub-phase 12.2, Pure-TS FFI

### Decisions made (12.2)

**Mochi**: `extern fun stripe_charge(req: ChargeReq) -> ChargeResp from npm "stripe"`

**TypeScript**:

```typescript
import Stripe from "stripe";

const stripe = new Stripe(process.env.STRIPE_SECRET_KEY ?? "");

export async function stripe_charge(req: ChargeReq): Promise<ChargeResp> {
  const c = await stripe.charges.create(req);
  return { id: c.id, status: c.status };
}
```

Type stubs come from DefinitelyTyped (`@types/stripe`) or the package's own `.d.ts`. The Mochi-side `extern fun` declaration must list the field types so the emitter can generate the marshalling on both sides.

**`package.json` dependency**: pure-TS FFI declarations register the npm package as a Mochi-side dependency; the emitter writes it into the emitted `package.json` `dependencies` map.

## Sub-phase 12.3, Browser rejection

### Decisions made (12.3)

`--target=browser-bundle` runs a reachability check from the user's entry point. If any reachable `extern fun` declares a C-library backend (not pure-TS), codegen errors:

```
error: FFI declaration `lz4_compress from "liblz4.so"` is reachable from main but
cannot run in the browser. Either gate the call behind `if mochiRuntime() != "browser"`
or move the call to a Node/Deno/Bun-only sub-module.
```

Pure-TS FFI declarations (via `npm`) pass the check; they are normal imports as far as the bundler is concerned.

## Sub-phase 12.4, Native package binaries

### Decisions made (12.4)

`@mochi/runtime-native-node`: Node N-API addon. Compiled via `node-gyp` or `cmake-js`. Prebuilt binaries (`linux-x64.node`, `linux-arm64.node`, `darwin-arm64.node`, `win32-x64.node`) shipped via the `prebuildify` pattern; the package's `index.js` picks the right binary at install.

`@mochi/runtime-native-bun`: Bun FFI binding scripts in TypeScript; no compile step needed at install (Bun FFI dlopens at runtime).

`@mochi/runtime-native-deno`: Deno FFI binding scripts in TypeScript; no compile step needed at install.

**Each is a separate npm package the user opts into**. The default `@mochi/runtime` has zero native dependencies; users invoking `extern fun` on a C library must `npm install @mochi/runtime-native-node` (or the relevant runtime's package).

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/lower/ffi.go` | `extern fun` to per-backend dispatch wrapper |
| `transpiler3/typescript/lower/ffi_reachability.go` | Browser-target reachability check |
| `runtime3/typescript/src/runtime/index.ts` | `mochiRuntime()` probe |
| `runtime3/typescript-native-node/` | Node N-API addon source (separate package) |
| `runtime3/typescript-native-bun/` | Bun FFI bindings |
| `runtime3/typescript-native-deno/` | Deno FFI bindings |
| `transpiler3/typescript/build/phase12_test.go` | `TestPhase12FFI` |
| `tests/transpiler3/typescript/fixtures/phase12-ffi/` | 20 fixtures |

## Test set

- `TestPhase12FFI`, 20 fixtures Node + Deno + Bun (browser skipped).
- `TestPhase12BrowserRejection`, a fixture using `extern fun` from a C library is rejected at codegen under `--target=browser-bundle`.
- `TestPhase12NativeBuild`, `@mochi/runtime-native-node` builds cleanly on linux-x64, linux-arm64, darwin-arm64.

## Deferred work

- WebAssembly backend (`extern fun` from a `.wasm` module). Open Q7 (v2 candidate).
- Async FFI (`bun:ffi` supports `async` symbols since 1.1; Deno FFI has `nonblocking: true`). Phase 12 ships sync FFI; async is v1.5.
- Cross-runtime ABI normalisation for struct passing. Phase 12 ships the simple int/ptr/u8-array path; struct ABI is v1.5.
