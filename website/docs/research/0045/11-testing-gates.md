---
name: 5600-research-11-testing-gates
description: "MEP-45 research note 11. Testing strategy. Differential vs the VM, BG corpus, fuzzing, sanitiser matrix, phasing gates."
---

# MEP-45 research note 11, Testing and CI gates

Author: research pass for MEP-45.
Date: 2026-05-22 (GMT+7).

The single most important property a transpiler can have is *output
correctness*. This note describes how MEP-45 proves it.

## 1. Differential testing against the VM

Every fixture under `tests/vm/valid/` (and the `examples/v0.3/` ...
`examples/v0.5/` corpora) has a recorded expected stdout produced by
the VM. The transpiler's output must:

1. Compile under tier-1 toolchains.
2. Run the produced binary under each tier-1 target (via qemu-user for
   cross-arch on Linux CI).
3. Produce **byte-equal** stdout.

This is the master gate. A regression here blocks the PR.

The VM and the transpiler share the same parser and type-checker
output (MIR). The codegen is the only thing under test. Any divergence
is either a codegen bug or a *language-level* ambiguity worth
documenting.

## 2. The BG corpus

The "byte-equal goldens" corpus lives under
`tests/cross-aot/bg/<triple>/`. Each fixture is a complete program
plus an `expect.txt` (stdout) and an optional `expect.exit` (exit
code).

Phasing per the MEP body:

- Phase 1: `tests/cross-aot/bg/host/` only.
- Phase 2: tier-1 cross-arch added.
- Phase 3: tier-2 added (BSDs, riscv, armv7).
- Phase 5.2 (already shipped per git log): BG fixtures on every
  cross-target triple.
- Phase 5.2.1 (already shipped): Linux + wasm run-gates.

The MEP-45 phasing extends this by *introducing* the C-target
fixtures alongside the existing VM ones.

## 3. Run gate per target

For each (target, profile) pair:

```
mochi build --target=$T --profile=$P fixture.mochi
./out | diff - expect.txt
```

Targets currently on tier 1 (see note 07 §3). Profiles in the gate:
`--dev` and `--release` for every fixture; `--debug` (sanitisers) for
a curated 10% subset (full corpus would be slow under ASan).

## 4. Sanitiser matrix

The nightly job runs the corpus under:

| Sanitiser | Build flag | Expected detected |
|---|---|---|
| ASan | `-fsanitize=address` | use-after-free, double free, OOB |
| UBSan | `-fsanitize=undefined` | signed overflow, alignment, oob shifts, null deref |
| TSan | `-fsanitize=thread` | data races in streams/agents |
| MSan | `-fsanitize=memory` | uninit reads |
| LeakSan | bundled with ASan | runtime leaks above BDWGC floor |

Failure on any sanitiser blocks merge. The intent: the transpiler
must produce sanitiser-clean code on the entire fixture corpus.

## 5. Property tests

Domain-specific properties:

- **Pattern-matcher**: for a random MIR pattern set, the generated
  decision tree must classify every value identically to a reference
  naive matcher. (Counter-example shrinking via `theft`.)
- **Sort**: `mochi_sort__T_by(xs, cmp)` is stable and total under any
  consistent `cmp`. Random inputs of length 0..1000.
- **Swiss-table**: insert/erase/get sequence against
  `std::unordered_map` reference. 1M ops per run.
- **JSON round-trip**: parse(serialise(x)) == x for random records.
- **YAML round-trip**: same.
- **CSV round-trip**: same, modulo non-string column types.
- **Stream fan-out**: every emit reaches every subscriber exactly once,
  in emit order, under random scheduling.

## 6. Fuzzing

libFuzzer + ASan/UBSan harnesses for:

- The parser: any input must not crash; either parses or returns an
  error span.
- The type-checker: same.
- The JSON loader: yyjson is fuzzed upstream, but we add a Mochi-
  facing harness because we lower JSON values to typed records.
- The pattern matcher: any program must produce a deterministic match
  outcome that agrees with the reference matcher.

The fuzzing corpus seeds from the BG corpus and from a `corpora/`
directory grown by OSS-Fuzz reports.

## 7. Differential vs other backends

Where the language has another backend that has shipped a feature
(currently Go is the most mature), the gate runs the same fixture
through both backends and diffs stdout. Useful for tracking
inadvertent divergence.

## 8. Reproducibility check

The gate rebuilds each release-profile fixture twice, on the same
machine and on a second build host, and asserts SHA-256 equality of
the binary. If reproducibility breaks, the PR is rejected.

## 9. Spec-in-sync gate

Per the memory note `feedback_spec_in_sync`: any PR that lands a
codegen change must update the MEP file (or its referenced research
notes) in the same PR. A bot enforces this by checking that any change
under `internal/aot/c/` is accompanied by a change under `spec/0045/`
or `spec/MEP-45.md`.

## 10. Phasing gates

The MEP body defines phases. Each phase has a *measurable gate*
matching the umbrella-phase coverage rule
(`feedback_umbrella_phase_targets`):

| Phase | Gate description | Targets in scope |
|-------|------------------|------------------|
| 1 | Compile + run hello-world; produce stdout "hello, mochi!" | host only |
| 2 | Full primitives + records + lists + maps; arithmetic suite passes | host |
| 3 | Sum types + pattern matching; option corpus passes | host |
| 4 | Closures + higher-order functions; map/filter/fold corpus | host |
| 5 | Strings + I/O + error model; stdlib suite passes | host |
| 6 | Query DSL; query corpus passes byte-equal vs VM | host |
| 7 | Streams + agents + concurrency; stream corpus passes | host |
| 8 | FFI shells (C direct, Go via RPC); FFI corpus passes | host |
| 9 | Cross-compile tier-1 architectures | tier-1 triples |
| 10 | WASM/WASI; wasi corpus passes | wasi |
| 11 | APE / Cosmopolitan; ape corpus passes | one-binary all-OS |
| 12 | LLM bindings + generate; replay-mode tests pass | host |
| 13 | Datalog; logic corpus passes | host |
| 14 | Sanitiser matrix clean across full corpus | tier-1 triples |
| 15 | Reproducible builds; SHA-256 stable across two CI hosts | tier-1 triples |
| 16 | Performance: median fixture within 2x of Go backend wall-time | host |

Each phase becomes a sub-PR. Auto-merge applies per `feedback_auto_ship_phases`.

## 11. Performance gates

Phase 16 is a soft gate (warn on regression > 10%). The benchmark
suite uses the BG corpus plus the "perf" subset of fixtures (long-
running compute, query-heavy, stream-heavy). We track:

- Wall-clock time.
- Peak RSS.
- Binary size (release stripped).
- Compile time.

Per-release reports go to a static page.

## 12. Stress tests

A nightly stress run does:

- Build the entire example corpus under `--debug` (sanitisers).
- Run the streams fixture suite under a 10x message rate.
- Run the agents fixture suite with 4x worker threads and ramped CPU
  load.
- Run the datalog suite with a 100x fact count.

Failures don't block merge but file an automatic issue.

## 13. Goal alignment per phase

Per the memory note `feedback_goal_alignment_audit`: before each phase
starts, the MEP gets a one-paragraph audit confirming the phase's
gate ties to the user-facing goal ("produce a working C executable
from a Mochi program") rather than spec-internal scaffolding.

Example audit (phase 6, query DSL): "Query DSL is the highest-value
language feature for the dataset/AI workflows the docs target. Without
it, even simple ETL fixtures fail. The gate (byte-equal stdout) is
end-user-observable. Aligns."

## 14. CI infrastructure

GitHub Actions matrix:

- Linux x86_64 host (ubuntu-24.04 runner): tier-1 triple builds via
  zig cc, qemu-user-static for cross-arch run.
- macOS arm64 host (macos-15): native, plus zig cc cross.
- Windows x86_64 host (windows-2025): clang-cl native, plus zig cc
  cross.

A nightly self-hosted bare-metal runner (rented hardware) handles the
sanitiser matrix, the stress suite, the reproducibility check, and
the performance report.

## 15. Bug bounty entry points

The MEP body recommends listing the following as bounty-eligible:

- Codegen producing UB on a valid Mochi program.
- Codegen producing different stdout from the VM on a valid program.
- Runtime leaking memory above the BDWGC floor on a finite-time
  program.
- Type-checker accepting a program that crashes the codegen.

This sets a clear contract for what the transpiler guarantees.

## 16. Open questions

1. Whether sanitiser matrix is per-PR or only per-merge.
2. Whether the reproducibility gate runs on every PR or only on
   release branches.
3. Whether the LLM tests use real provider credentials in CI (cost,
   flakiness) or only the replay cassettes.
4. Whether stress tests should block release or only file issues.
5. Whether the Go-backend differential gate stays as a permanent CI
   line or sunsets once both backends are at parity.
