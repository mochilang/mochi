---
name: 5600-research-10-build-system
description: "MEP-45 research note 10. Build pipeline. `mochi build`, output layout, amalgamated runtime, cross-compilation via zig cc, APE via cosmocc, WASM via wasi-sdk, caching, reproducibility."
---

# MEP-45 research note 10, Build system

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

This note covers the build driver. The driver is the thing that takes
a Mochi source tree and turns it into a binary. The transpiler is one
stage of the driver.

## 1. Command surface

```
mochi build [PATH]                              # build the current module
mochi build --target=x86_64-linux-musl PATH     # cross-compile
mochi build --target=wasm32-wasi PATH           # WASM/WASI
mochi build --apex PATH                         # Cosmopolitan portable binary
mochi build --release PATH                      # release profile
mochi build --secure PATH                       # release + CFI + safe-stack
mochi build --debug PATH                        # sanitisers on
mochi build --emit=c PATH                       # stop after C emission
mochi build --emit=mir PATH                     # stop after MIR
mochi build --emit=ast PATH                     # stop after parse
mochi build --out PATH/bin/foo PATH             # output path
mochi build -j N PATH                           # parallelism
mochi build --no-cache PATH                     # bypass cache
mochi build --reproducible PATH                 # extra reproducibility flags
```

`mochi run` builds with `--debug` defaults into a cache and then
executes. `mochi test` is `mochi run` against `tests/*.mochi`.

## 2. Output layout

```
.mochi/cache/
  ast/
    <module-hash>.bin
  mir/
    <module-hash>.bin
  c/
    <module-hash>.c
    <module-hash>.h
  obj/
    <target-triple>/
      <module-hash>.o
  bin/
    <target-triple>/
      <output-name>
```

All filenames are deterministic functions of (source hash, compiler
flags, runtime version). A cache hit is "is the file already there?";
no metadata DB needed.

`<module-hash>` is a 128-bit BLAKE3 hash of:
- The module's source bytes.
- The hashes of every module it imports (recursive).
- The transpiler version string.
- The compilation profile name.

This makes the build "Bazel-shaped" without Bazel.

## 3. Pipeline stages

```
   source.mochi
       |
     parse    --> AST                (.mochi -> .ast.bin)
       |
   type-check --> typed AST + errors
       |
    elaborate --> MIR                (.ast.bin -> .mir.bin)
       |
   monomorphise --> MIR (specialised)
       |
      lower   --> C IR
       |
       print --> .c + .h             (.mir.bin -> .c)
       |
      cc      --> .o                 (per-target)
       |
       link   --> binary
```

Each arrow is a cacheable step. Failures surface at the producing
stage; downstream stages don't re-run.

## 4. C compiler invocation

The driver shells out to a `cc` selected per target:

| Target | Driver chosen | Flags appended |
|--------|--------------|----------------|
| host (current) | `${CC}` or `cc` | `-std=c23 -O2 -g -Wall` |
| native cross | `zig cc -target ${triple}` | same |
| WASI | `zig cc -target wasm32-wasi` | `-O2 -g` |
| APE | `cosmocc` | `-O2` |
| Windows MSVC | `clang-cl --target=x86_64-pc-windows-msvc` | `/std:c17 /O2` |

The driver does not require the user to install LLVM separately; the
default install bundles `zig` (small, MIT) for cross-compilation.

## 5. Cache strategy

- Per-module compilation. A change to one module rebuilds that module
  and re-links; modules whose hashes did not change stay cached.
- Content-addressed; no mutable index.
- The cache root is `.mochi/cache/` per project; a global root at
  `$XDG_CACHE_HOME/mochi/` is shared across projects when content
  matches.
- Cache eviction: LRU on a `--cache-max=10GiB` budget.

## 6. Parallelism

- AST/MIR/C phases parallelise across modules with no shared state.
- `cc` invocations parallelise across object files.
- The link step is serial.

`-j` defaults to the number of cores.

## 7. The runtime library

`libmochi.a` is an amalgamated static archive containing:

- `mochi/core` (memory, types, errors, panic)
- `mochi/sched` (M:N scheduler, fibers, channels, streams, agents)
- `mochi/query` (Swiss-table, omap, sort, arena)
- `mochi/io` (file, network, time, env)
- `mochi/text` (utf8proc, simdutf, regex-Phase2)
- `mochi/data` (JSON via yyjson, YAML via libfyaml, CSV)
- `mochi/net` (libcurl-backed HTTP)
- `mochi/llm` (provider abstraction)
- `mochi/ffi` (Python embedding, Go RPC, TS via QuickJS-NG)

Per-target builds of `libmochi.a` live in
`prebuilt/<triple>/libmochi.a` in the install tree. The driver picks
the right one based on `--target`. Cross-compilation of the runtime
itself happens at install time, or on first use, via zig cc.

## 8. APE / cosmocc

The `--apex` mode uses `cosmocc` instead of `zig cc`. Output is a
single `.com` file that runs on macOS, Linux, Windows, FreeBSD,
NetBSD, OpenBSD without modification. The runtime is rebuilt against
Cosmopolitan libc for this profile.

Limitations:

- No `dlopen` (so Python FFI and llama.cpp local LLM are disabled).
- No threading on Windows under the polyfill (so streams degrade to
  single-threaded).
- Binary size larger (~5 MB minimum).

This is a niche but valuable feature for "ship one binary" stories.

## 9. WASM / WASI

`--target=wasm32-wasi` produces a `.wasm` binary that runs under
`wasmtime`, `wasmer`, `wasmedge`, or in a browser via `wasi-snapshot-preview2`
shims.

Limitations:

- BDWGC disabled; we use a precise allocator with a shadow stack.
- No threads (wasi-threads is unstable in 2026); scheduler falls back
  to single-fiber cooperative mode.
- Asyncify is applied post-link for coroutine support.
- No subprocess; Go and Python FFI disabled.

A separate `--target=wasm32-emscripten` for browser playground use
emits with emscripten and Asyncify; needed because wasi-libc lacks DOM
bindings.

## 10. Static vs dynamic

Default: fully static link. Single binary, no runtime dependencies
besides libc.

Optional dynamic mode (`--shared`) builds `libmochi.so` and links the
final binary against it. Useful for memory-saving in distros that ship
many Mochi binaries.

## 11. Debug info

DWARF 5 on Linux, BSD, macOS; CodeView on Windows. The transpiler
emits `#line` directives so the C compiler attaches Mochi source spans
to every instruction. Split-debug-info (`--gsplit-dwarf`) on by
default; the binary stays small.

For `--release` builds, debug info goes to a sidecar (`foo.dSYM` on
macOS, `foo.debug` on Linux, `foo.pdb` on Windows). The main binary
is stripped.

## 12. Symbol resolution

The mangling rules in note 05 §3 guarantee no symbol collisions across
packages. The link step resolves them via the system linker's normal
rules; we do not use a custom linker.

For the `mochi-core` runtime, every public symbol is prefixed with
`mochi_` and exposed via a single header `mochi/core.h`. No
"surprise" exports.

## 13. Embedding mochi-core in a host C program

`libmochi.a` plus `mochi/core.h` is a valid C library. A host C
program can call `mochi_runtime_init`, `mochi_eval_str`, or link
against a transpiled `.o` directly.

Use case: a game engine in C++ that wants to script with Mochi.

## 14. Source maps

For the WASM target, the driver emits a `.wasm.map` (V3 source map
JSON) that points back to Mochi source. This lets browser dev tools
show the right file:line on a stack trace.

## 15. Diagnostics post-processing

The driver wraps the C compiler's invocation. On non-zero exit, it
parses the `gcc -fdiagnostics-format=sarif` (or clang's `-fdiagnostics-print-source-range-info`)
output and rewrites file:line refs from generated C to Mochi source
using the `#line` map. The user sees `foo.mochi:42:1: error: ...`,
not a generated-C path.

This is critical UX: 90% of compile errors should surface from the
type-checker; the remaining 10% (linker errors, header conflicts,
ICEs) must still be navigable.

## 16. Distribution

The Mochi CLI distribution includes:

- `mochi` (Go driver binary, ~12 MB)
- `zig` bundled (small, MIT)
- `cosmocc` bundled (optional, large; can fetch on demand)
- `runtime/include/mochi/*.h` (runtime headers)
- `runtime/lib/<triple>/libmochi.a` (per-target prebuilds for tier 1)
- `runtime/src/` (runtime source for cross-compile fallback)

Install methods:

- Single-binary download from releases.
- `brew install mochi`.
- `apt install mochi` (PPA).
- `winget install mochi`.
- `cargo binstall mochi` (since the project tracks rust-toolchain
  features anyway).

## 17. Versioning

The CLI follows SemVer. The runtime ABI is *not* stable across major
versions; users are expected to rebuild. The transpiler's IR (MIR) is
internal and unstable. The generated C is reasonably stable; we publish
diff stats per release so users tracking the C output can see drift.

## 18. Open questions

1. Whether to ship `zig` (~80 MB) bundled or fetch on first use.
2. Whether `--apex` is a tier-1 feature or experimental.
3. Whether the per-module C cache is on by default or opt-in.
4. Whether to expose a `mochi build --plugin=<so>` API for codegen
   plugins (e.g., a custom backend).
5. Whether `mochi build --emit=ir-pretty` is worth shipping (dumps a
   human-readable MIR for the user to inspect).
