---
title: "Phase 12. FFI"
sidebar_position: 14
sidebar_label: "Phase 12. FFI"
description: "MEP-53 Phase 12, FFI via a sidecar C library compiled by cc-rs."
---

# Phase 12. FFI via sidecar C + cc-rs

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-53 §Phases](/docs/mep/mep-0053#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 (GMT+7) |
| Landed         | 2026-05-29 (GMT+7) |
| Tracking issue | [#22597](https://github.com/mochilang/mochi/issues/22597) |
| Tracking PR    | [#22499](https://github.com/mochilang/mochi/pull/22499) |
| Commit         | 2b9ad7bd38 |

## Gate

`TestPhase12FFI` walks `tests/transpiler3/rust/fixtures/phase12-ffi/` (24 fixtures) and asserts byte-equal stdout. Coverage: `extern fn` with int args, with float args, with string args (UTF-8 round-trip), return-by-pointer, side-effect functions (printf-style logging via C), multiple extern fns in one program.

## Lowering decisions

Mochi's `extern fn name(a: T): U from "header.h"` directive lowers to a Rust `extern "C"` block plus a sidecar `cffi/` directory carrying the user-supplied C source and a `build.rs` that runs cc-rs:

```rust
extern "C" {
    fn add(a: i64, b: i64) -> i64;
}

fn main() {
    let r = unsafe { add(2, 3) };
    mochi_runtime::io::print_i64(r);
}
```

The emitted crate layout:

```
workdir/
  Cargo.toml           # adds cc = "1" as build-dependency
  build.rs             # cc::Build::new().file("cffi/extern.c").compile("mochi_cffi");
  src/main.rs          # the emitted Rust source
  cffi/
    extern.c           # the user-supplied C body
    extern.h           # the header (declarations)
```

`build.rs` runs cc-rs to compile `cffi/*.c` into a static archive named `libmochi_cffi.a` and emits `cargo:rustc-link-lib=static=mochi_cffi`. cc-rs auto-detects the host C compiler (cc, gcc, clang, msvc); when no C compiler is on PATH, the build fails clean and phase-12 fixtures are skipped at the per-fixture gate (not the per-phase gate, because some fixtures don't actually use FFI but live in the FFI directory for cohabitation).

String arguments lower with explicit `CString` round-trip:

```rust
let c_s = std::ffi::CString::new(s.clone()).unwrap();
let r = unsafe { print_c(c_s.as_ptr()) };
```

This adds an allocation per FFI call but matches the standard Rust idiom and is the only way to guarantee NUL-termination without changing C-side semantics.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/rust/lower/extern.go` | extern fn lowering |
| `transpiler3/rust/emit/cffi.go` | sidecar cffi/ + build.rs emit |
| `transpiler3/rust/build/build.go` | cc-rs detection (best-effort) |
| `transpiler3/rust/build/phase12_test.go` | 24-fixture gate |
| `tests/transpiler3/rust/fixtures/phase12-ffi/*.mochi` + `.out` + `*.c` + `*.h` | 24 fixtures |

## Test set

- `TestPhase12FFI/<fixture>` for each `.mochi` in the fixture directory (24 fixtures).

## Closeout notes

cc-rs detection was the most fragile piece. On Apple Silicon with Xcode CLT installed, `cc` resolves to `/usr/bin/cc` (clang); without Xcode, there is no cc. The build script does not require cc-rs at Mochi build time (`mochi build --target=rust-source` works without a C toolchain); cc is only needed when cargo invokes the build script during `cargo build`. The per-phase gate is skipped on CI runners without cc, which is detected by the `Driver.Build` precheck.

The cc-rs version pinned in the emitted Cargo.toml is `cc = "1"`; we follow the major-version contract because cc-rs has been very stable post-1.0 (April 2018).
