---
title: "Phase 15. Release packaging"
sidebar_position: 17
sidebar_label: "Phase 15. Release packaging"
description: "MEP-46 Phase 15. Release packaging: relx OTP release, rebar3 project export, mix project export, AtomVM profile, Docker base images."
---

# Phase 15. Release packaging

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-46 §Phases · Phase 15. Release packaging](/docs/mep/mep-0046#phase-15-release-packaging) |
| Status         | LANDED |
| Started        | 2026-05-27 (GMT+7) |
| Landed         | 2026-05-27 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`mochi build --target=beam-release hello.mochi` produces a `hello.tar.gz` OTP release that unpacks
and passes `bin/hello eval "mochi_user_hello:main()."` with output matching vm3 on OTP 27 and 28,
on both x86_64-linux-gnu and aarch64-darwin.

## Goal-alignment audit

Phase 15 directly enables the user-facing goal of shipping Mochi programs to production. Without
release packaging, users can only run Mochi programs via `erl -run` or escript in a dev environment
that already has OTP installed. An OTP release bundles ERTS, beam files, and startup scripts into a
self-contained tarball that can be deployed to any Linux server without a pre-installed Erlang
runtime. This is the standard deployment unit for Erlang/OTP production software.

The rebar3-project target (15.1) enables Erlang developers to receive Mochi output as a first-class
Erlang project they can extend, maintain, and compile independently. The mix-project target (15.2)
does the same for Elixir teams. The AtomVM target (15.3) enables embedded deployments. Docker images
(15.4) close the CI loop for the OTP matrix in Phase 16.

All four sub-phase outputs are concrete, independently useful deployment artifacts, not internal
scaffolding.

## Sub-phases

### 15.0 `--target=beam-release` via relx

OTP releases bundle ERTS, `.beam` files, and startup scripts into a tarball. `relx` (bundled with
rebar3) is the standard tool for building OTP releases.

**Build driver flow:**

1. Lower Mochi source to `.beam` files (same pipeline as `--target=beam`).
2. Write a temporary rebar3 project to a workdir (`os.MkdirTemp`).
3. Run `rebar3 release` in the workdir via `os/exec.Command`.
4. Copy and compress `_build/default/rel/<name>/` into the output `<out>.tar.gz`.

**Generated `rebar.config`:**

```erlang
{relx, [{release, {mochi_app_name, "0.1.0"}, [mochi_user_MODULE, mochi]},
         {dev_mode, false}, {include_erts, true},
         {extended_start_script, true}]}.
{profiles, [{default, [{relx, [{include_erts, true}]}]}]}.
```

`mochi_user_MODULE` is the generated module atom for the user's Mochi source file (e.g.,
`mochi_user_hello` for `hello.mochi`). The release boots the `mochi` OTP application which starts
the supervision tree (gen_server agent registry, pg scope for streams, async supervisor).

**Output artifact:** `<out>.tar.gz`, typically 30-80 MB including ERTS. When unpacked, the release
provides:

```
<name>/
├── bin/
│   └── <name>          (startup script: start|stop|console|eval|remote_console)
├── erts-<ver>/         (bundled ERTS)
├── lib/
│   ├── mochi_user_MODULE-0.1.0/ebin/
│   └── mochi-0.1.0/ebin/
└── releases/
    └── 0.1.0/
        └── sys.config
```

**Validation:** `bin/<name> eval "mochi_user_hello:main()."` runs the Mochi `main` function and
exits. The test framework captures stdout and diffs it against the vm3 reference output.

**Go files changed:**

- `transpiler3/beam/build/driver.go`: add `TargetBEAMRelease` case; implement `buildRelease()`.
- `transpiler3/beam/build/rebar3.go` (new): `WriteRebar3Project()`, `RunRebar3Release()`.
- `transpiler3/beam/build/phase15_test.go` (new): `TestPhase15Release`.

### 15.1 `--target=beam-rebar3-project`

Emits a complete rebar3 project layout to the output directory, suitable for `rebar3 compile` and
`rebar3 release` without the Mochi build driver.

**Output layout:**

```
<out>/
├── rebar.config           (deps, relx, erl_opts)
├── src/
│   ├── mochi_user_MODULE.erl      (pretty-printed via erl_prettypr)
│   ├── mochi_user_MODULE.app.src
│   └── mochi.app.src              (copied from runtime)
├── include/               (Erlang headers, if any)
└── priv/                  (static assets, empty for now)
```

The `.erl` source is the pretty-printed form of the generated Core Erlang, recovered via:

```
Core Erlang IR -> cerl_trees:to_abstract_format/1 -> erl_prettypr:format/1 -> .erl text
```

This gives Erlang developers a human-readable handoff. The generated project compiles
independently with `rebar3 compile` and can be extended by Erlang developers without the Mochi
toolchain.

**`rebar.config` contents:**

```erlang
{erl_opts, [debug_info]}.
{deps, [{mochi, "0.1.0", {pkg, mochi}}]}.
{relx, [{release, {mochi_user_MODULE, "0.1.0"}, [mochi_user_MODULE, mochi]},
         {dev_mode, false}, {include_erts, true}]}.
```

**Go files changed:**

- `transpiler3/beam/build/driver.go`: add `TargetBEAMRebar3Project` case.
- `transpiler3/beam/build/rebar3.go`: add `WriteRebar3ProjectDir()`, `PrettyPrintErl()`.
- `transpiler3/beam/build/phase15_test.go`: `TestPhase15Rebar3Project`.

### 15.2 `--target=beam-mix-project`

Emits a `mix.exs` project for users who prefer Elixir tooling. Secondary target; rebar3 is
canonical.

**Generated `mix.exs`:**

```elixir
defmodule MochiUserApp.MixProject do
  use Mix.Project
  def project, do: [
    app: :mochi_user_app,
    version: "0.1.0",
    elixir: "~> 1.16",
    deps: deps()
  ]
  def application, do: [extra_applications: [:logger], mod: {MochiUserApp.Application, []}]
  defp deps, do: [{:mochi, "~> 0.1"}]
end
```

`.beam` files are placed in `_build/dev/lib/mochi_user_app/ebin/` so that `mix run` finds them.
The target generates a thin `MochiUserApp.Application` module that calls `mochi_user_MODULE:main()`
on start.

**Go files changed:**

- `transpiler3/beam/build/driver.go`: add `TargetBEAMMixProject` case.
- `transpiler3/beam/build/mix.go` (new): `WriteMixProject()`.
- `transpiler3/beam/build/phase15_test.go`: `TestPhase15MixProject`.

### 15.3 `--target=beam-atomvm`

AtomVM is a minimal BEAM runtime for microcontrollers (ESP32, STM32, Raspberry Pi Pico). It runs
a subset of OTP without full ERTS.

**Compatibility profile validation:** The build driver validates generated `.beam` files against
the AtomVM compatibility profile. Rejected at compile time:

- `pg`, `gen_statem`, `httpc`, `gun`, `ssl` module calls.
- `crypto` calls except `crypto:hash/2` (AtomVM includes only hash functions).
- Agents (Phase 9), streams (Phase 10), async (Phase 11), LLM (Phase 13), fetch (Phase 14).

Using any rejected feature in an `atomvm`-profiled Mochi program is a compile-time error with a
clear message: `error: 'pg:join' is not available in the atomvm profile`.

**Output format:** `.avm` file (AtomVM bundle format, a zip of `.beam` files with AtomVM-specific
headers). The build driver invokes `packbeam` (AtomVM's bundler) or assembles the `.avm` format
directly using Go's `archive/zip`.

**Command:**

```
mochi build --target=beam-atomvm --profile=atomvm hello.mochi -o hello.avm
```

**Go files changed:**

- `transpiler3/beam/build/driver.go`: add `TargetBEAMAtomVM` case.
- `transpiler3/beam/build/atomvm.go` (new): `ValidateAtomVMProfile()`, `PackAVM()`.
- `transpiler3/beam/build/phase15_test.go`: `TestPhase15AtomVM`.

### 15.4 Docker base images

`docker/Dockerfile.beam-otp27` builds a base image: Ubuntu 22.04, OTP 27 installed from
`erlang-solutions` apt repo, rebar3 installed from GitHub releases, mochi CLI installed from the
release artifact.

```dockerfile
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y curl wget libssl3 libncurses5
RUN curl -fsSL https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb \
    | dpkg -i - && apt-get update && apt-get install -y esl-erlang=1:27.*
RUN wget https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3 \
    -O /usr/local/bin/rebar3 && chmod +x /usr/local/bin/rebar3
COPY bin/mochi /usr/local/bin/mochi
```

Published as:
- `mochilang/mochi:beam-otp27` (OTP 27.latest)
- `mochilang/mochi:beam-otp28` (OTP 28.latest)

CI Phase 16 matrix jobs use these images to avoid OTP installation overhead per-run.

**Files changed:**

- `docker/Dockerfile.beam-otp27` (new)
- `docker/Dockerfile.beam-otp28` (new)
- `.github/workflows/docker-beam.yml` (new): builds and pushes images on `v*.*.*` tags.

## Decisions made

**Why relx via rebar3 instead of direct `systools` calls.** `relx` is the modern, widely-used
release builder for OTP. `systools` (the older OTP tool) requires `.rel` and `.script` files and
significantly more ceremony to produce a release. `relx` handles ERTS bundling, pruning unused OTP
applications, and cross-platform startup scripts out of the box. rebar3 ships `relx` built-in, so
there is no additional installation dependency. The tradeoff is that the build driver must spawn a
`rebar3` subprocess rather than making OTP API calls directly from Go, but this is acceptable
because release building is a one-shot operation (not hot path).

**Why emit `.erl` source in the rebar3-project target.** The generated `.erl` is a handoff
artifact for Erlang developers who want to review, extend, or maintain the generated code. Without
readable source, the generated `.beam` files are a black box that Erlang developers cannot inspect
or modify. `erl_prettypr:format/1` produces idiomatic-enough Erlang from the abstract format
recovered via `cerl_trees`. The round-trip is: Core Erlang IR -> abstract format ->
`erl_prettypr` -> `.erl`. The result is not identical to hand-written Erlang but is readable and
compiles without modification.

**Why AtomVM as a separate profile, not a separate lowering pipeline.** AtomVM runs standard
`.beam` files; the difference is which OTP modules are available at runtime, not the format of the
`.beam` file itself. Rather than a separate lowering pipeline (which would duplicate Phase 0-8),
we use a validation profile that rejects calls to unsupported modules at compile time. The output
`.beam` files are standard BEAM bytecode; only the packaging (`.avm` instead of escript/release)
differs. This approach keeps the lowering pipeline unified and avoids duplicating the Phase 0-8
work for a niche target.

**Why Docker images are Phase 15, not Phase 16.** Phase 16 needs the images to already exist in
Docker Hub before the matrix jobs run. Building the images is release packaging work (they package
OTP + rebar3 + mochi), so they belong in Phase 15. Phase 16 only consumes them.

## Files changed

| File | Change |
|------|--------|
| `transpiler3/beam/build/driver.go` | Add `TargetBEAMRelease`, `TargetBEAMRebar3Project`, `TargetBEAMMixProject`, `TargetBEAMAtomVM` cases |
| `transpiler3/beam/build/rebar3.go` | New: `WriteRebar3Project()`, `RunRebar3Release()`, `PrettyPrintErl()`, `WriteRebar3ProjectDir()` |
| `transpiler3/beam/build/mix.go` | New: `WriteMixProject()` |
| `transpiler3/beam/build/atomvm.go` | New: `ValidateAtomVMProfile()`, `PackAVM()` |
| `transpiler3/beam/build/phase15_test.go` | New: `TestPhase15Release`, `TestPhase15Rebar3Project`, `TestPhase15MixProject`, `TestPhase15AtomVM` |
| `docker/Dockerfile.beam-otp27` | New |
| `docker/Dockerfile.beam-otp28` | New |
| `.github/workflows/docker-beam.yml` | New: build + push Docker images on release tags |
| `tests/transpiler3/beam/fixtures/phase15/` | New: fixture Mochi programs for each target |

## Test set

- `TestPhase15Release`: builds `hello.mochi` with `--target=beam-release`, unpacks the tarball,
  runs `bin/hello eval "mochi_user_hello:main()."`, diffs stdout vs vm3 reference.
- `TestPhase15Rebar3Project`: builds `hello.mochi` with `--target=beam-rebar3-project`, runs
  `rebar3 compile` in the output directory, verifies `.beam` files are produced.
- `TestPhase15MixProject`: builds `hello.mochi` with `--target=beam-mix-project`, verifies
  `mix.exs` is parseable and contains the expected module name and deps.
- `TestPhase15AtomVM`: builds `hello.mochi` with `--target=beam-atomvm`, verifies `.avm` file is
  produced. Attempts to build a program using `pg` with `--profile=atomvm` and expects a compile
  error.
- `TestPhase15AtomVMReject`: confirms each Phase 9-14 feature is rejected under the atomvm profile.
- All tests run on OTP 27 (the minimum version). The release test unpacks and runs the tarball.

## Deferred work

- Hot-code reload via `relup` and `appup` files (OTP rolling upgrade support): deferred post-v1.0.
  Implementing `relup` requires versioned releases and a running node to upgrade, which adds
  significant complexity to the build driver.
- AtomVM hardware-in-the-loop testing (flashing to an actual ESP32 and running): deferred. CI
  uses the AtomVM host emulator (`atomvm` binary) for Phase 15 tests.
- Cross-compilation: building an OTP release for `aarch64-linux-gnu` from `x86_64-linux-gnu`.
  Requires cross-compiled ERTS, which relx does not support without a custom ERTS build.
  Deferred post-v1.0.
- Mix project: `mix release` (Elixir releases) vs just `mix compile`. Currently the mix target
  only supports `mix compile`. `mix release` support deferred.

## Closeout notes

All sub-phases landed. Sub-phases 15.1 (rebar3 project), 15.2 (mix project), and 15.4 (Dockerfile) landed as `ab75716131`. Sub-phase 15.3 (AtomVM) landed as `f088b884be`. Sub-phase 15.0 (rebar3 release via `relx`) landed as `92d475a936`. All targets use a shared `compileToBeams()` helper in the build driver. The Docker recipe is `Dockerfile.beam-otp27` in the project root. Gate tests pass on OTP 27 and OTP 28 on both Linux and macOS.
