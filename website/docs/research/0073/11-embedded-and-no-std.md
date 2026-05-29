---
title: "11. Embedded and no_std"
sidebar_position: 12
sidebar_label: "11. Embedded"
description: "The no_std subset of MEP-73, the alloc-feature opt-in, the embedded MEP-53 target dependencies, which crates work on bare metal vs which do not, the firmware story for Mochi-on-microcontroller, and the gate that flips behaviour at lock time."
---

# 11. Embedded and no_std

This note documents the embedded subset of MEP-73: the modes by which Mochi-imported Rust crates can run on `no_std` targets (microcontrollers, RTOS environments, WASM-no-WASI cores). The subset is narrower than the default surface, but well-defined.

## The default vs embedded modes

| Property | default mode | embedded mode |
|----------|--------------|---------------|
| Target | x86_64 / aarch64 / arm-linux / wasm32-wasip1 / etc. | thumbv7em-none-eabihf / riscv32imc-unknown-none-elf / wasm32-unknown-unknown / etc. |
| std available | Yes | No |
| alloc available | Yes (via std) | Configurable (via alloc + global allocator) |
| Heap | Yes | Optional |
| Threads | Yes | Single-thread |
| Tokio | Yes | No (refused) |
| File / network IO | Yes | No (refused) |
| Mochi runtime | Full | Minimal (mochi-runtime-core) |

The embedded mode is gated behind:

```toml
[rust]
profile = "embedded"
target = "thumbv7em-none-eabihf"
allocator = "linked-list-allocator"  # one of: linked-list-allocator, none

[rust.embedded]
alloc = true     # enable the alloc crate
panic-handler = "panic-halt"
runtime-features = ["core-numerics", "core-strings"]   # no IO, no time, no signals
```

When `profile = "embedded"`, the bridge enforces the following at lock time:

- Every imported crate must compile with `#![no_std]`. The bridge inspects the crate's `Cargo.toml` for a `[features] default = ["std"]` config and skips it; the user must opt into a `default-features = false` mode.
- The dependency graph is walked for `std`-only sub-deps; any such transitive dep produces a hard error.
- The wrapper crate is emitted with `#![no_std]` and uses the alloc crate (when `alloc = true`) for MochiString / MochiSlice / opaque-handle boxing.

## Which crates work on no_std

The 24-crate fixture set has the following no_std support:

| Crate | no_std with alloc | no_std without alloc | Notes |
|-------|--------------------|----------------------|-------|
| anyhow | Yes | No | Uses `Box<dyn Error>`. |
| thiserror | Yes (1.0.50+) | No | Conditional. |
| serde | Yes | Yes | Core serde is no_std; serde_json needs alloc. |
| regex | Yes | No | regex-syntax compiles no_std; regex needs alloc. |
| rayon | No | No | Requires threads. |
| itertools | Yes | Yes | Pure iterator combinators. |
| once_cell | Yes | Yes | The race crate. |
| time | Yes | No | Conditional. |
| uuid | Yes | Yes | Conditional. |
| url | Yes | No | Needs alloc for String. |
| base64 | Yes | Yes | The `display` mode is alloc-free. |
| hex | Yes | Yes | Pure conversion. |
| sha2 | Yes | Yes | Pure hashing. |
| blake3 | Yes | Yes | Pure hashing. |
| rand | Yes | Yes | Conditional. |
| rand_chacha | Yes | Yes | Pure PRNG. |
| num_cpus | No | No | Requires syscall to detect CPUs. |
| bytes | Yes | No | Uses alloc for Bytes. |
| smallvec | Yes | Yes | Inline buffer; alloc for spill. |
| indexmap | Yes | No | Uses HashMap. |
| ahash | Yes | Yes | The `compile-time-rng` feature is conditional. |
| parking_lot | No | No | OS synchronisation. |
| crossbeam | No | No | Threading. |
| tokio | No | No | Refused unconditionally on embedded. |

The bridge does not maintain this table at runtime; it derives the compatibility from cargo metadata at lock time. The above is informational.

## The alloc opt-in

When `alloc = true`, the bridge emits the wrapper crate with:

```rust
#![no_std]
extern crate alloc;

use alloc::string::String;
use alloc::vec::Vec;
use alloc::boxed::Box;
```

The wrapper still uses MochiString / MochiSlice with the same heap-pointer layout as the default mode. The runtime side links against a Mochi-provided alloc shim that bridges to the firmware's allocator (linked-list-allocator, embedded-alloc, etc.).

When `alloc = false`, the bridge refuses any imported item whose translation requires alloc:

- Any item with a String / Vec / HashMap parameter.
- Any item that returns a String / Vec / HashMap.
- Any item that internally allocates (the bridge consults the crate's docs.rs annotations; crates without `#![forbid(alloc)]` are treated as alloc-using unless the user overrides per-item).

The alloc=false mode is suitable for sensor-firmware contexts where the firmware uses a stack-allocated arena and the wrapper exposes only stack-shaped types (i32, f32, bool, repr(C) structs of those).

## The runtime split

The Mochi runtime is split into three crates for embedded distribution:

- `mochi-runtime-core` (no_std + alloc): integer arithmetic, string concatenation, list operations, GC for opaque handles. ~150 KiB compiled, --opt-level=z.
- `mochi-runtime-io` (std-only): file IO, network IO, time, signals.
- `mochi-runtime-full` (std + std-process): everything above plus tokio bridge and panic catching.

In embedded mode, only `mochi-runtime-core` is linked. The wrapper crate's Cargo.toml depends on `mochi-runtime-core` exclusively:

```toml
[dependencies]
mochi-runtime-core = { version = "0.6", default-features = false, features = ["alloc"] }
# upstream crate:
my-no-std-crate = { version = "0.1", default-features = false }
```

The runtime split is invisible to the user: the bridge selects the right runtime crate based on `[rust] profile`.

## Firmware target story

For Mochi-on-microcontroller, the build flow:

1. The user authors Mochi code in `<package>/src/*.mochi`.
2. `mochi build --target=thumbv7em-none-eabihf` runs through MEP-53's TargetRust, then `cargo build --target=thumbv7em-none-eabihf` against the embedded toolchain.
3. The output is an `.elf` binary suitable for `cargo embed` / `probe-run` / OpenOCD upload.

The cargo-embed integration is out of scope for MEP-73; the user runs `probe-run target/thumbv7em-none-eabihf/release/<binary>` by hand.

A worked example: a Mochi program that uses `sha2` to hash a sensor reading and sends it over UART:

```mochi
import rust "sha2@0.10" as sha2
import rust "embedded-hal@1.0" as hal

fun hash_and_send(reading: list<int>): unit {
    let mut hasher = sha2.Sha256.new()
    hasher.update(reading)
    let digest = hasher.finalize()
    let uart = hal.Uart.from_pin(2)
    uart.write(digest)
}
```

The bridge generates wrapper code that links sha2's no_std mode and embedded-hal's trait surface (the latter requires a future sub-phase for trait import). The .elf builds for thumbv7em-none-eabihf and runs on a Cortex-M4 firmware target.

## WASM no_std

`wasm32-unknown-unknown` (the no-WASI variant) is a no_std target by default. The bridge supports it as a special-case of embedded mode:

```toml
[rust]
profile = "embedded"
target = "wasm32-unknown-unknown"
allocator = "wee_alloc"   # 1 KiB allocator for size-constrained WASM
```

The wrapper crate links against `mochi-runtime-core` + the wee_alloc global allocator. The output is a minimal `.wasm` module suitable for browser embedding via `wasm-bindgen` (though MEP-73 does not produce the wasm-bindgen JS glue; that is MEP-13's domain).

## Panic handler

A no_std binary requires a panic handler. The bridge emits:

```rust
use panic_halt as _;  // or panic-abort, configurable
```

The panic handler is a wrapper-crate-level dependency:

```toml
[dependencies]
panic-halt = { version = "0.2", optional = true }
panic-abort = { version = "0.3", optional = true }
```

The user selects via:

```toml
[rust.embedded]
panic-handler = "panic-halt"   # or panic-abort, panic-semihosting, panic-rtt-target
```

The bridge enforces that exactly one panic handler is selected.

## Memory layout

A no_std Mochi binary's memory layout (on Cortex-M4):

```
+------------------+
| .vector_table    | 0x08000000  (firmware reset / interrupt vectors)
+------------------+
| .text            |             (Mochi-emitted code + wrapper code + Mochi runtime + Rust crate code)
+------------------+
| .rodata          |             (string literals, const tables)
+------------------+
| .data (in flash) |             (initial values for .data in RAM)
+------------------+
                                  -- flash / RAM boundary --
+------------------+
| .bss             | 0x20000000  (zero-initialised statics)
+------------------+
| .data            |             (initialised statics, copied from flash on boot)
+------------------+
| heap             |             (linked-list-allocator pool)
+------------------+
| .stack           | 0x20020000  (grows down)
+------------------+
```

The bridge generates a `memory.x` linker script that the embedded toolchain consumes. The user can override the memory map for their specific MCU.

## Gate at lock time

When `profile = "embedded"`, `mochi pkg lock` runs additional validation:

- Every imported crate's `Cargo.toml` is scanned for `[features] default = ["std"]` or `[dependencies] std`.
- Any crate with a hard std dependency that cannot be disabled via features fails with `ERROR: crate <name> requires std; not usable in embedded profile`.
- Crates with conditional no_std support (gated behind `default-features = false`) are accepted; the bridge writes the required feature list to the wrapper's Cargo.toml.

The lockfile records the embedded selection:

```toml
[[rust-package]]
name = "sha2"
version = "0.10.8"
profile = "embedded"
features-selected = ["asm"]
features-disabled = ["std", "default"]
```

A subsequent `mochi pkg lock --check` validates that the selected features still satisfy the embedded constraint; a change to the upstream crate's `default-features` is flagged.

## Cross-references

- [[01-language-surface]] for the `[rust.embedded]` manifest table.
- [[09-abi-stability]] §static-link-vs-cdylib for the staticlib mode required on embedded.
- [MEP-53 phase 16](/docs/implementation/0053/phase-16-embedded) for the underlying embedded emit on Rust.
- [embedonomicon](https://docs.rust-embedded.org/embedonomicon/) for the no_std Rust toolchain background.
