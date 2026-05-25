# MEP-46 research note 11, Testing strategy and gates for MEP-46

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).

This note specifies the test-as-spec gates that govern each MEP-46 phase. Mirrors MEP-45's `TestPhase*` pattern: a single deterministic Go test per phase, plus a differential gate against vm3, plus a Dialyzer gate, plus an OTP version matrix.

---

## 1. Gate philosophy

A **gate** is a Go test (`Test...` in the Mochi repo) that:
1. Iterates fixtures in `tests/transpiler3/beam/...`
2. Compiles each fixture with the MEP-46 toolchain.
3. Runs the resulting `.beam` (via escript or via `erl -noshell -eval`).
4. Compares actual stdout against `<fixture>.out`.
5. Compares against the vm3 oracle (running the same Mochi source on the reference VM).

A gate **fails closed**: any fixture mismatch fails the test. Phases land only when every fixture in the phase's matrix is green.

Mochi's tradition (from MEP-45) is one gate per phase, gates remain in CI forever, and the fixture matrix only grows. We follow the same pattern for MEP-46.

## 2. Phase gates (planned matrix)

Following [[01-language-surface]] and the MEP body's phase plan:

| Phase | Gate                      | Fixture count target | Surface covered                                |
|-------|---------------------------|----------------------|------------------------------------------------|
| 1     | `TestPhase1Hello`         | 5                    | hello world, print, basic let, basic int       |
| 2     | `TestPhase2Scalars`       | 20                   | int/float/bool/string ops, comparisons         |
| 3     | `TestPhase3Lists`         | 25                   | list literal, index, len, for-each             |
| 3.2   | `TestPhase3Maps`          | 25                   | map literal, index, len, keys, values, has     |
| 3.3   | `TestPhase3Sets`          | 20                   | set literal, add, has, len                     |
| 3.4   | `TestPhase3ListOfRecord`  | 20                   | list[record], comprehensions over records      |
| 4     | `TestPhase4Records`       | 25                   | records, methods, equality                      |
| 5     | `TestPhase5Sums`          | 25                   | sum types, pattern matching                     |
| 6     | `TestPhase6Funs`          | 25                   | closures, higher-order, partial app             |
| 7     | `TestPhase7Query`         | 30                   | from/where/select, group_by, order_by, joins   |
| 8     | `TestPhase8Datalog`       | 20                   | facts, rules, recursion                         |
| 9     | `TestPhase9Agents`        | 25                   | agent definitions, spawn, call, cast            |
| 10    | `TestPhase10Streams`      | 20                   | streams, subscribe, publish                     |
| 11    | `TestPhase11Async`        | 15                   | async, await, futures                            |
| 12    | `TestPhase12FFI`          | 15                   | extern, FFI marshalling                          |
| 13    | `TestPhase13LLM`          | 10                   | generate (mocked LLM provider)                  |
| 14    | `TestPhase14Fetch`        | 10                   | fetch (HTTP, against local test server)         |

Total target by Phase 14: ~290 fixtures, all green on OTP 27 and 28, both Linux x86-64 and macOS arm64.

## 3. Differential testing vs vm3

The vm3 oracle is **the reference Mochi interpreter** (the original tree-walker in `interp/`). For each fixture:

1. Run vm3: `mochi run <fixture>.mochi > <fixture>.vm3.out`.
2. Run BEAM build: `mochi build --target=beam-escript <fixture>.mochi -o /tmp/f && /tmp/f > <fixture>.beam.out`.
3. `diff <fixture>.vm3.out <fixture>.beam.out` must be empty.

The fixture's checked-in `<fixture>.out` file is the **vm3 oracle output**; CI verifies vm3 produces it (catching vm3 regressions) and then verifies the BEAM target matches.

For non-deterministic fixtures (random, time, streams), the fixture is excluded from differential testing and runs only the static check (stdout vs checked-in `.out`).

## 4. Dialyzer gate

`TestDialyzerClean`:
1. Build all fixtures to a rebar3 project layout.
2. Run `rebar3 dialyzer` with `-Werror`.
3. Verify zero warnings on Mochi-generated code (vendor code may have its own).

This gate validates the **`-spec` emission** from MEP-46: every Mochi-exported function gets a `-spec` matching its Mochi type, and Dialyzer confirms the implementation matches.

False positives (Dialyzer over-conservative on opaque types) are documented in `tests/transpiler3/beam/dialyzer_allowlist.txt` with a rationale.

## 5. eqWAlizer gate (optional)

For users opting into stricter typing, `TestEqWAlizerClean` runs eqWAlizer against generated code. Initially **disabled in CI** (eqWAlizer is stricter than Dialyzer and may reject valid Mochi patterns); promoted to required gate in Phase 15+.

## 6. OTP version matrix

CI runs every gate on:
- OTP 27.0 (minimum)
- OTP 27.latest
- OTP 28.latest

A nightly job adds:
- OTP 29 RC (when available)
- OTP 26.latest (in *non-blocking* mode, just to surface regressions early; not a supported target)

Architecture: x86-64 Linux and arm64 macOS for blocking. Windows x86-64 nightly only.

## 7. Property tests

Mochi `property "..." for <var> in <gen> { <assertion> }` blocks compile to **PropEr** (the de facto Erlang QuickCheck) test cases via the `proper_test_format` mode. Example:

```mochi
property "addition is commutative" for a in int, b in int {
  a + b == b + a
}
```

becomes:

```erlang
prop_addition_commutative() ->
    ?FORALL({A, B}, {integer(), integer()},
        A + B =:= B + A).
```

PropEr runs 100 cases by default; `--proper-iter N` overrides.

The build driver wires PropEr into the rebar3 project as a `test` profile dep.

## 8. eunit and common_test integration

Mochi `test "..." { ... }` blocks generate `mochi_test_<sanitised>_eunit.erl`:

```erlang
-module(mochi_test_user_main_eunit).
-include_lib("eunit/include/eunit.hrl").

add_test() ->
    ?assertEqual(3, mochi_user_main:add(1, 2)).
```

For tests that need supervised setup (e.g. requiring the `mochi` application to be started), the generated module is a **Common Test suite** instead:

```erlang
-module(mochi_test_streams_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
all() -> [test_publish_subscribe].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(mochi),
    Config.
end_per_suite(_) -> application:stop(mochi).

test_publish_subscribe(_) ->
    %% ...
    ok.
```

The decision (eunit vs CT) is automatic: tests in async/stream/agent code use CT; pure tests use eunit.

## 9. Output capture

vm3 prints to stdout; BEAM `io:format/2` prints to the group leader (which for escripts is stdout, but for releases may be a logger). To capture identically, the BEAM target's `print` and `log` lower to:

```erlang
print(V) -> io:put_chars([V, "\n"]).
log(V) -> io:format(standard_error, "~ts~n", [V]).
```

This matches vm3's "stdout for `print`, stderr for `log`" convention.

## 10. Time and random determinism

For fixtures involving time or random:
- `now()` is overridable via the `MOCHI_TIME_FIXED=<unix-ms>` env var; the runtime checks at boot.
- `random()` is overridable via `MOCHI_RANDOM_SEED=<int>`; the runtime calls `rand:seed(exsss, {seed, seed, seed})`.

vm3 honors the same env vars. Both produce identical output when the env is set, making differential testing of time/random-sensitive fixtures possible.

## 11. Test harness organisation

```
tests/transpiler3/beam/
├── README.md
├── golden/                              % Checked-in `.out` files
├── fixtures/
│   ├── phase1/
│   │   ├── 001_hello.mochi
│   │   ├── 001_hello.out                % Oracle output
│   │   └── ...
│   ├── phase2/
│   └── ...
├── phase_gate_test.go                   % Go test driver
├── dialyzer_gate_test.go
├── otp_matrix_test.go
└── support/
    ├── runner.go                        % Spawns erl + builds escripts
    ├── differ.go                        % Diffs against vm3
    └── ...
```

Naming convention matches `tests/transpiler3/c/` (the MEP-45 C target's test tree).

## 12. Coverage

For each phase fixture, we collect:
- **Line coverage** of the Mochi source (which lines were exercised).
- **Branch coverage** of the generated Core Erlang (via `cover`).
- **Mochi-language-feature coverage** (which language features the fixture exercised).

Coverage minimums:
- Phase 1-5: ≥80% feature coverage.
- Phase 6+: ≥70%.
- Overall by Phase 14: ≥75% line coverage of `libmochi_erl`.

Coverage is **reported but not gating** in CI. Hard gates are correctness only.

## 13. CI duration budget

Each gate must run in ≤2 minutes on standard CI hardware (4-core, 8GB). The full BEAM test matrix should complete in ≤10 minutes per arch.

Beyond ~10 minutes, we shard fixtures across CI workers.

## 14. Continuous regression: nightly differential

A nightly job runs every fixture (~290 by Phase 14) on:
- All supported OTP versions (27.0, 27.latest, 28.latest, 29 RC)
- All supported arches (x86-64 Linux, arm64 macOS, x86-64 Windows)
- Both `prod` and `dev` profiles

Total: ~290 × 4 OTP × 3 arch × 2 profile = 6960 test runs. Sharded across 30 runners, each runner does 232 runs in ~5 minutes.

## 15. Failure reporting

When a gate fails:
- The Go test prints the fixture path, vm3 output, BEAM output, and a diff.
- The `_build/beam-test/<fixture>/` directory contains intermediate Core Erlang and generated `.erl`, retained for forensics.
- For Dialyzer failures, the warning is shown verbatim with a link to the Mochi source line via the `Line` chunk.

The MEP body documents the standard debugging recipe: read the diff, inspect the generated `.erl`, attach `dbg:tracer()` to a re-run, compare with vm3's interpretation.

## 16. Manual smoke tests

Beyond automated gates, a manual smoke checklist for each MEP-46 release:
- Build and run the Mochi tutorial (10 sample programs) on each target.
- Build the Mochi self-test (which tests Mochi by running Mochi programs that test Mochi).
- Build a representative real-world Mochi project (TBD, likely a small web service or CLI tool).

Smoke tests run on macOS (locally by the release manager) before tagging.

## 17. Bench regression

Per-phase bench tests live in `tests/transpiler3/beam/bench/`:
- Each fixture has a `_bench.mochi` companion that runs the operation 1M times.
- The benchmark runs on a fixed AWS instance (m7i.large) and reports ns/op.
- Regressions of >20% block the PR.

These are advisory only in v0.1; promoted to blocking by v0.3.

## 18. Hot reload tests

`TestHotReload`:
- Build a fixture's v1 to `.beam`.
- Start an Erlang node and load v1.
- Build v2 with a method body change.
- `code:load_file/1` v2.
- Verify the running agent's state survived and the new behavior is in effect.

This validates the two-version invariant and the `code_change/3` callback emission.

## 19. AtomVM gate (Phase 7+)

`TestAtomVMCompat`:
- For each fixture in the "AtomVM compat" subset, build with `--target=beam-atomvm`.
- Run via the AtomVM x86-64 simulator (`atomvm.elf`) in QEMU.
- Compare stdout against the oracle.

Initial subset: Phase 1-5 fixtures + a curated Phase 6 subset that does not use `pg`/`httpc`/distribution.

## 20. Documentation gate

`TestDocsBuild`:
- For each fixture using `///` or `doc "..."` blocks, build to a rebar3 project and run `rebar3 ex_doc`.
- Verify the HTML output is well-formed (W3C nu validator).
- Verify cross-references resolve.

This catches doc rot.

---

## Sources

1. MEP-45 testing gates (the source of this pattern). https://www.mochi-lang.org/docs/mep/mep-0045
2. PropEr documentation. https://proper-testing.github.io/
3. eunit reference. https://www.erlang.org/doc/man/eunit.html
4. Common Test user guide. https://www.erlang.org/doc/apps/common_test/users_guide.html
5. `cover` reference. https://www.erlang.org/doc/man/cover.html
6. Dialyzer reference. https://www.erlang.org/doc/man/dialyzer.html
7. eqWAlizer. https://github.com/WhatsApp/eqwalizer
8. AtomVM simulator. https://www.atomvm.net/
