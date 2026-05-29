---
title: "11. TinyGo, embedded, and wasm"
sidebar_position: 12
sidebar_label: "11. TinyGo and embedded"
description: "The TinyGo subset of pkg.go.dev, the no-cgo embedded path, the wasm-js and wasm-wasip1 surface, what kind of Go modules Mochi can consume on bare metal, the firmware story, and how the wasm targets interact with wazero."
---

# 11. TinyGo, embedded, and wasm

This note covers the constrained targets: TinyGo (alternative Go compiler for microcontrollers), wasm-wasip1, and wasm-js. The bridge's behaviour on these targets is a strict subset of the host-binary behaviour documented in [[09-abi-stability]].

## TinyGo: alternative compiler for constrained targets

[TinyGo](https://tinygo.org), first released in 2018 and stable since 2020, is an alternative Go compiler targeting microcontrollers (Cortex-M, RISC-V, AVR), wasm, and bare-metal x86. TinyGo implements a subset of the Go language and stdlib:

- **Supported**: most language features (functions, structs, interfaces, goroutines, channels via a cooperative scheduler).
- **Not supported**: full reflect (TinyGo has a partial implementation), cgo (TinyGo replaces cgo with `wasm_import`-style calls in wasm targets and direct linker symbols in MCU targets), `runtime/debug` package, large parts of `net/http` (no TLS by default; can be added via tinyusb), goroutines under heavy contention (the scheduler is cooperative, not preemptive).
- **Modified**: the GC is conservative-mark-sweep (smaller than Go's tri-colour concurrent GC); `runtime.NumCPU()` is always 1.

The bridge's TinyGo subset (phase 16) consumes Go modules that compile under TinyGo. Modules that depend on reflect-heavy or cgo-using packages are rejected at lock time with a clear diagnostic.

## The TinyGo-compatible module subset

Of pkg.go.dev's top 1,000 modules (April 2026), an estimated 8-15% compile under TinyGo. The subset is biased toward:

- Pure-algorithm modules: hashing (xxhash, sha2, blake3), encoding (base64, hex, gob), parsing (json without reflect-based codegen).
- Embedded-system modules: tinygo.org/x/drivers (sensors, displays), tinygo.org/x/tinyfs (filesystem), tinygo.org/x/bluetooth.
- Pure-data-structure modules: container/list, slices, sort, math, golang.org/x/exp/constraints.

The subset excludes:

- `net/http` (modulo TinyGo's partial implementation).
- gRPC and protobuf (reflect-heavy).
- Any module pulling in `reflect.Value.Call`-based dispatch.
- Most observability stacks (zap, logrus, prometheus).

The bridge's lock-time TinyGo gate runs the synthesised wrapper through `tinygo build -target=wasm-wasi -o /dev/null .` (or the target architecture); a non-zero exit indicates the module is not TinyGo-compatible. The lock fails with a diagnostic naming the incompatible item.

## The no-cgo embedded path

For MCU and bare-metal targets, cgo is not available (no C compiler in the target's toolchain; no shared library loader). The bridge's embedded path emits the wrapper without `//export` directives and instead uses TinyGo's `//go:linkname` directives to expose the wrapper functions as linker symbols:

```go
//go:build tinygo && embedded

package gowrap_<module>

//go:linkname mochi_go_<module>_Foo Foo
func mochi_go_<module>_Foo(arg int64) int64 {
    return Foo(arg)
}
```

The Mochi side links against these symbols directly (no cgo runtime). The cost: the Mochi side has to know the symbol naming convention statically (the bridge writes a small `.mochi-embed-symbols.json` file alongside the wrapper that records the symbol-to-name mapping).

## wasm-wasip1 target

wasm-wasip1 (the WebAssembly System Interface, preview 1) is a wasm target with a POSIX-like syscall surface. Go 1.21 added official support via `GOOS=wasip1 GOARCH=wasm`. TinyGo has supported wasi for longer.

The bridge's wasm-wasip1 behaviour:

- The wrapper is compiled to `.wasm` via `GOOS=wasip1 GOARCH=wasm go build -o wrap.wasm`.
- The Mochi side embeds [wazero](https://wazero.io) (a pure-Go wasm runtime) to host the wrapper.
- Cross-boundary calls go through wazero's host-function call surface rather than cgo.
- Goroutines work via Go's cooperative scheduler embedded in the wasm module.
- Channels work (with bounded buffers).
- cgo is not available; the bridge refuses Go modules with `import "C"`.
- The Go GC runs inside the wasm module; the embedding host has no GC interaction.

The cost-per-call on wasm-wasip1 is higher than on native (the wasm runtime adds ~500ns per host-function call). The bridge's batched-variant optimisation is more impactful here.

## wasm-js target

The wasm-js target (`GOOS=js GOARCH=wasm`) is for browser execution. The bridge supports it via the `syscall/js` package, with two notable constraints:

- **No goroutine preemption.** The wasm-js scheduler is cooperative; a goroutine spinning in a tight loop never yields. The bridge documents this in the per-function notes; functions known to spin-loop are not exposed.
- **Async dispatch.** wasm-js callbacks from JS into Go are inherently async; the wasm-js `js.FuncOf` callback registers a JS function that calls the Go side. The bridge's callback handle pattern adapts to this: instead of cgo `_call`, the wrapper exposes a `js.FuncOf`-registered JS function per callback.

The Mochi side, when targeting wasm-js, links the wrapper as a JS module loaded by the Mochi-wasm-js runtime. The cost-per-call is dominated by the JS-to-wasm transition (~300ns on V8, May 2026).

## What kind of Go modules Mochi can consume on each target

| Target | cgo | reflect | net | goroutines | TLS |
|--------|------|---------|------|--------------|------|
| native host (darwin/linux/windows) | yes | yes | yes | yes | yes |
| wasm-wasip1 | no | yes | partial (no TLS by default) | yes (cooperative) | no (without tinyusb) |
| wasm-js | no | yes | partial (via fetch) | yes (cooperative) | yes (via fetch's underlying browser) |
| tinygo embedded MCU | no | partial | no | yes (cooperative) | no |

The bridge's gate (phase 16 for tinygo, phase 17 for wasm) validates per-target compatibility at lock time.

## The firmware story

For embedded users targeting actual hardware (ARM Cortex-M, ESP32, RISC-V boards), the bridge's flow is:

1. Mochi program written normally, with `import go "tinygo.org/x/drivers/bme280" as bme280`.
2. `mochi pkg lock` validates that `bme280` compiles under TinyGo.
3. `mochi build --target=tinygo-cortex-m4 --runtime=tinygo` invokes the bridge's TinyGo-aware build path:
   - Synthesises the wrapper using `//go:linkname` instead of `//export`.
   - Invokes `tinygo build -target=stm32f4 -o firmware.elf .`.
   - The output is a flashable ELF.

The bridge does not own the flashing step (the user invokes `tinygo flash` or `st-link` themselves).

## The wazero embedding strategy

For wasm-wasip1 consumers, the Mochi wasm-wasip1 binary embeds wazero and hosts every imported Go module's wrapper as a wasm module inside the wazero runtime. The plumbing:

- The Mochi-wasm-wasip1 main binary contains the wazero runtime initialised at startup.
- Each `import go "<module>" as <alias>` becomes a wazero `runtime.InstantiateModule(...)` call that loads the wrapper's `.wasm`.
- Cross-boundary calls go through wazero's host-function call surface.
- Each wasm module's Go runtime is independent (each module has its own scheduler, GC).

The cost: each wrapper module carries ~200 KB of Go runtime + stdlib in its `.wasm`. A Mochi program importing 5 Go modules adds ~1 MB to the wasm binary size.

The benefit: full Go module compatibility on wasm targets (modulo the TinyGo subset for the wrapper itself).

## Future direction: wasm component model

The WebAssembly Component Model, stabilising through 2026-2027, will let wasm modules expose typed interfaces (WIT) that consumers call without per-module host-function plumbing. The bridge's wasm-wasip1 path is ready to migrate to the component model once it stabilises; the migration path is documented in [[12-risks-and-alternatives]] §A12.

## Cross-references

- [[02-design-philosophy]] §3 for why cgo (and its absence on wasm) drives the wrapper-vs-direct decision.
- [[09-abi-stability]] for the cgo ABI the host targets use.
- [[12-risks-and-alternatives]] §R6 for the TinyGo-subset-size risk.
- [The TinyGo documentation](https://tinygo.org/docs/) for the upstream compatibility table.
- [The wazero documentation](https://wazero.io/docs/) for the wasm-host story.
- [The Go wasm-wasip1 documentation](https://go.dev/wiki/WebAssembly) for the official wasm target.
