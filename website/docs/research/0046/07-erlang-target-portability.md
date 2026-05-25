# MEP-46 research note 07, BEAM target portability and OTP version matrix

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).

This note covers the platform/OS matrix Mochi-on-BEAM supports, the OTP version policy, BeamAsm JIT availability per arch, AtomVM-compat profile, reproducibility, and cross-compilation concerns.

---

## 1. OTP version policy

Mochi-on-BEAM **minimum** OTP version is **27** (May 2024). Reasons:

- `json` stdlib module (eliminates `jsx`/`jiffy` dependency)
- Sigils `~"..."` for binary string literals (cleanest mapping for Mochi strings)
- `maybe` expression as a default-enabled feature (used in lowered Mochi `?`-propagation)
- Triple-quoted strings (for Mochi multi-line string literals)
- `-doc` attribute (for `mochi doc` HTML output)
- `ets:lookup_element/4` with default (cleaner Datalog lowering)

**Tested / supported** range:
- OTP 27.x (LTS through May 2026): primary supported.
- OTP 28.x (LTS through May 2027): supported, runs all gates.
- OTP 29.x (planned May 2026): tested on pre-release; will be added to gates when GA.

**Unsupported**:
- OTP 26 and earlier: no `json`, no sigils. We do not work around their absence; users on OTP 26 must upgrade.

The CI matrix runs `OTP 27.0`, `OTP 27.latest`, `OTP 28.latest`, on Linux x86-64 and macOS arm64. Windows is best-effort (see §5).

## 2. CPU architecture matrix

BEAM runs natively on:

| Architecture     | BeamAsm JIT   | Status                                  |
|------------------|---------------|-----------------------------------------|
| x86-64           | yes (default) | Tier 1: full CI, all gates              |
| aarch64 (ARM64)  | yes (default) | Tier 1: full CI on macOS arm64          |
| ppc64le          | no            | Tier 2: interpreter only; tested weekly |
| riscv64          | no (OTP 28)   | Tier 3: best-effort; experimental       |
| s390x            | no            | Tier 3: best-effort                     |
| 32-bit x86, ARM  | no            | not supported (memory pressure)         |

Tier 1: all gates run, blocking. Tier 2: weekly cron job, non-blocking. Tier 3: when the user asks for it.

The JIT-vs-interpreter split affects performance but not correctness; Mochi-emitted `.beam` files run identically on either.

## 3. Operating system matrix

| OS                  | Notes                                                            |
|---------------------|------------------------------------------------------------------|
| Linux (glibc)       | Tier 1. Ubuntu 22.04, 24.04; Debian 12; Alpine via musl (Tier 2) |
| macOS arm64         | Tier 1. macOS 13+ (Ventura).                                     |
| macOS x86-64        | Tier 1. macOS 13+ (Ventura). Mostly Intel CI hosts.              |
| Windows x86-64      | Tier 2. MSVC build of ERTS; some gates skipped.                  |
| FreeBSD             | Tier 3. Community-supported.                                     |
| Linux (musl/Alpine) | Tier 2. Static linking via OTP's `--without-system-libs`.        |

Mochi-emitted `.beam` files are **architecture-independent** (BEAM is a VM); the OS matrix reflects ERTS support, not our codegen.

## 4. AtomVM (embedded) profile

AtomVM (https://www.atomvm.net/) is a tiny BEAM reimplementation in C for microcontrollers. It loads stock `.beam` files but supports only a subset of the OTP standard library and BEAM opcodes.

The **AtomVM compatibility profile** for Mochi-emitted code is:

- Allowed: pure Mochi functions, ADTs, records, pattern matching, basic agent and stream code.
- Allowed stdlib: `lists` (most), `maps` (most), `binary`, `gen_server`, `supervisor`, `gen_tcp`, `gen_udp`, `lwip` (via AtomVM-specific bindings).
- **Disallowed**: `pg` (no process groups), `ets` (limited), `mnesia` (absent), `json` (workaround: use a pure-Erlang JSON lib bundled with AtomVM), `httpc`/`gun` (no full TLS), `code:load_file` (no hot reload), `crypto` (limited), distribution.

A Mochi program that does not use FFI, datalog (which uses ETS), or `fetch` (which uses gun) should run on AtomVM unmodified. The Mochi compiler emits a `mochi atomvm-check` lint that scans for AtomVM-incompatible calls.

This is **best-effort**: AtomVM is rapidly evolving; the compat profile must be reviewed each AtomVM release.

## 5. Windows support

ERTS runs on Windows (MSVC and recently MinGW); BeamAsm JIT is available on x86-64 Windows since OTP 26.

Limitations for Mochi:
- `gun`/`cowboy` work but file-descriptor handling differs.
- The `escript` shebang trick (`#!/usr/bin/env escript`) does not work; Windows users get a `.cmd` wrapper.
- Path handling: Mochi's `path:` module uses POSIX semantics; on Windows, paths are normalised in `mochi_str` helpers.

The build driver emits `mochi-windows.cmd` alongside `mochi-windows.escript` for the `escript` target. The release target emits a `.zip` with `bin/mochi.cmd`.

CI runs the test suite on `windows-2022` (GitHub Actions) for OTP 27 and 28, on x86-64 only. Failures are logged but do not block release.

## 6. WSL2

WSL2 (Windows Subsystem for Linux v2) is fully supported via the Linux x86-64 path. No special handling.

## 7. Docker / containers

Mochi releases ship a base Docker image (`mochilang/mochi:beam-otp27`) based on `erlang:27-alpine`, with the Mochi runtime pre-loaded. User builds produce a slim image (~50MB) by layering `COPY` of the user release on top.

The Alpine variant (musl libc) is the default; glibc variants are available as `:beam-otp27-glibc`.

Multi-arch images are built via `docker buildx` for `linux/amd64` and `linux/arm64`.

## 8. Reproducibility

BEAM `.beam` files contain:
- Compiled bytecode (deterministic from the same source + compiler version)
- A `Dbgi` chunk with debug info (deterministic)
- A `Line` chunk with line numbers (deterministic if line numbers don't change)
- An optional `CInf` chunk with compile-time info: **this includes a hash of the source path and the compile time**, breaking reproducibility.

Mochi-emitted `.beam` files **strip the timestamp** from `CInf` (set to a fixed epoch) and use **relative source paths** (relative to the project root). This produces bit-identical `.beam` files from the same source on different machines.

The `mochi build --target=beam-release` produces a release tarball with a manifest including a SHA-256 of each `.beam`; manifests are reproducible across CI agents.

## 9. Cross-compilation

You cannot cross-compile BEAM bytecode in the traditional sense, because BEAM is platform-independent. You can:
- Build `.beam` files on one machine, ship to another.
- Build a release on the **target architecture** (because the release bundles ERTS, which is platform-specific). This is the standard practice; release builds run on the target arch (or via Docker buildx).

For `mochi build --target=beam-release` on a foreign arch, the user must specify `--target-erts=<path-to-erts>` pointing at a pre-built ERTS for the target. Default behavior is to require building on the target machine.

## 10. Distribution as a binary

Two distribution paths:

**escript** (`mochi build --target=beam-escript`):
- Output: a single executable file, ~5-15 MB depending on user code.
- Bundles user `.beam` + Mochi runtime `.beam` files only; does **not** bundle ERTS. Requires `erl` on `$PATH` at runtime.
- Startup: ~50ms.
- Use case: CLI tools.

**release** (`mochi build --target=beam-release`):
- Output: a `.tar.gz` (default) or directory with `bin/`, `releases/`, `lib/`, `erts-<version>/`.
- **Bundles ERTS**: self-contained, runs without `erl` on `$PATH`.
- Size: 30-80 MB depending on which OTP apps are included.
- Startup: ~300ms cold, ~50ms warm.
- Use case: daemons, services.

For embedded targets, **AtomVM** runs raw `.beam` files; no escript or release wrapping. The Mochi build emits `.beam` files into an `atomvm/` directory; the AtomVM packer (`atomvm_packbeam`) bundles them into a single `.avm` for flashing.

## 11. ERTS version pinning

OTP releases include a **specific ERTS version** (e.g. OTP 27.0 ships ERTS 15.0). The Mochi runtime application requires `{erts, ">= 15.0"}` in its `.app` resource file; this is the OTP-canonical way to express a minimum ERTS version.

Mochi releases pin to the exact ERTS that was tested in CI. Users on a different OTP minor version may upgrade or downgrade, but the release tooling will warn.

## 12. Source-level portability

Mochi source code itself is platform-independent. The BEAM target adds:
- `pid` type (only meaningful in BEAM context; rejected by C target)
- `monitor`/`link` operators (only emitted to BEAM)
- `pg` stream backing (BEAM-only)

These features have no portability concerns because they are only available on the BEAM target.

The Mochi build system tags each emitted file with `% target: beam` in a comment header so users can grep for BEAM-only files.

## 13. Release upgrade reproducibility

Release upgrades (`relup`/`appup`) require a "before" and "after" release directory. Mochi releases ship with `appup` files generated automatically from a diff of the previous release's exported functions. Manual override is supported via a user-provided `appup.exs` (Mochi-style appup spec, compiled to standard appup).

`appup` generation is a Phase 5+ feature; v0.1 does not generate appups (users manually write them or restart the node).

## 14. Summary table: portability

| Feature                  | Status        |
|--------------------------|---------------|
| OTP 27/28                | Tier 1        |
| x86-64 Linux/macOS       | Tier 1        |
| arm64 macOS/Linux        | Tier 1        |
| Windows x86-64           | Tier 2        |
| ppc64le, riscv64, s390x  | Tier 2-3      |
| Docker images            | Tier 1        |
| AtomVM (compat profile)  | Tier 2        |
| Reproducible builds      | Yes           |
| Cross-arch release build | via Docker    |

This matrix is reviewed at each major Mochi release.

---

## Sources

1. OTP release notes 27.0–28.0. https://www.erlang.org/news
2. AtomVM compatibility matrix. https://www.atomvm.net/doc/master/api-reference/
3. ERTS internals: `Dbgi`, `CInf`, `Line` chunks. https://www.erlang.org/doc/apps/erts/beam_makeops.html
4. OTP-on-Wasm experiment, ProcessOne, Code BEAM 2025.
5. `relx` documentation. https://erlware.github.io/relx/
6. `mix release` documentation. https://hexdocs.pm/mix/Mix.Tasks.Release.html
7. Docker official Erlang image. https://hub.docker.com/_/erlang
