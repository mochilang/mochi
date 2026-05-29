---
title: "Testing gates: two-tier strategy, fragment tests, PHP execution tests, DJB2, Phar, reproducibility"
description: "The two-tier testing strategy for MEP-55: fragment gates (no PHP needed) and PHP execution gates. Covers runPhpFixture, TestPhaseNEmitFragments, TestPhase13DJB2HashMatchesCassetteFilenames, runPhpLLMFixture, runPharFixture, TestPhase17AllTargetsTogether, TestPhase16NonDeterministicBuildsAlsoMatch."
sidebar_position: 11
---

# Testing gates: two-tier strategy, fragment tests, PHP execution tests

Author: research pass for MEP-55 (Mochi-to-PHP 8.4 transpiler).
Date: 2026-05-29 15:00 (GMT+7).
Sources: `transpiler3/php/build/build_test.go`,
`transpiler3/php/build/phase09_test.go`,
`transpiler3/php/build/phase10_test.go`,
`transpiler3/php/build/phase13_test.go`,
`transpiler3/php/build/phase16_test.go`,
`transpiler3/php/build/phase17_test.go`,
`transpiler3/php/build/phase07_test.go`.

## 1. Two-tier testing strategy

MEP-55 uses a two-tier approach:

**Tier 1: Fragment gates** run without PHP being installed. They lower
a Mochi fixture to PHP source text and call `strings.Contains` on the
emitted text. These tests run on any machine with Go and are fast (< 1
second per fixture).

**Tier 2: PHP execution gates** require `php` on PATH (or `PHP_PATH`
set). They run the emitted `main.php` under PHP and diff stdout against
a `.out` cassette file. These tests skip gracefully if PHP is absent and
are gate by the `go-side` CI job which installs PHP 8.4.

The two tiers complement each other: fragment tests catch lowerer shape
regressions without needing a PHP install; execution tests catch runtime
semantic regressions that generate syntactically valid but wrong output.

## 2. `runPhpFixture` helper

Defined in `build_test.go` (lines 15-50). Pattern:

```go
func runPhpFixture(t *testing.T, mochiPath, wantFile string) {
    t.Helper()
    if _, err := exec.LookPath("php"); err != nil {
        if p := os.Getenv("PHP_PATH"); p == "" {
            t.Skipf("php not on PATH: %v", err)
        }
    }
    want, _ := os.ReadFile(wantFile)
    outDir := t.TempDir()
    d := &Driver{CacheDir: t.TempDir(), NoCache: true}
    emittedPath, _ := d.Build(mochiPath, outDir, TargetPhpSource)
    cmd := exec.Command("php", emittedPath)
    var stdout bytes.Buffer
    cmd.Stdout = &stdout
    cmd.Stderr = os.Stderr
    _ = cmd.Run()
    if !bytes.Equal(stdout.Bytes(), want) {
        t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", ...)
    }
}
```

Key characteristics:
- PHP-skip pattern: tests skip when `php` is not on PATH and `PHP_PATH`
  is not set.
- `t.TempDir()` for output isolation: each sub-test gets its own
  directory; the Go test framework cleans up on exit.
- `NoCache: true`: ensures each test runs a fresh pipeline without
  sharing cached state.
- Stdout diff: `bytes.Equal` on raw stdout bytes vs `.out` file content.
  Any trailing newline difference is a failure.

`runPhpFixture` is called by `TestPhaseNAgents`, `TestPhaseNStreams`,
`TestPhase7Query`, etc. — the per-phase "run all fixtures in this
directory" walkers.

## 3. `TestPhaseNEmitFragments` pattern

Each phase has an `EmitFragments` test alongside its runner test.
The pattern (illustrated by `TestPhase9EmitFragments` in phase09_test.go):

```go
cases := []struct {
    fixture string
    wants   []string
}{
    {
        fixture: "agent_counter.mochi",
        wants: []string{
            `final class Counter`,
            `public int $count,`,
            `public function increment(): void`,
            `$this->count = ($this->count + 1);`,
        },
    },
    // ...
}
for _, c := range cases {
    t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
        mochiPath := filepath.Join(repoRoot(t), ..., c.fixture)
        outDir := t.TempDir()
        d := &Driver{CacheDir: t.TempDir(), NoCache: true}
        p, _ := d.Build(mochiPath, outDir, TargetPhpSource)
        data, _ := os.ReadFile(p)
        src := string(data)
        for _, want := range c.wants {
            if !strings.Contains(src, want) {
                t.Errorf("%s: emitted source missing %q", c.fixture, want)
            }
        }
    })
}
```

Each `wants` entry is a substring that must appear in the emitted PHP
source. The tests do not require PHP to be installed. They run in the
`go-side` CI job alongside the Go build.

Fragment tests exist for: Phase 7 (query), Phase 9 (agents), Phase 10
(streams), Phase 13 (LLM), Phase 15 (composer), Phase 16 (repro), and
Phase 17 (packaging structure).

## 4. `repoRoot` helper

`repoRoot(t)` (build_test.go lines 52-56) delegates to
`repoRootForBuild(t)` (build.go lines 220-240). It walks up from the Go
source file's directory until it finds `go.mod`. This ensures fixture
paths resolve correctly regardless of the Go test runner's working
directory. Every `phase*_test.go` uses it for fixture discovery.

## 5. TestPhase13DJB2HashMatchesCassetteFilenames

`TestPhase13DJB2HashMatchesCassetteFilenames` (phase13_test.go lines
236-281) is a pure-Go test (no PHP required) that pins the cassette
lookup algorithm:

1. For each known (provider, model, prompt) tuple, computes the DJB2
   hash using `djb2CassetteKey` (phase13_test.go lines 219-226), a Go
   reimplementation of the PHP GMP-based `mochi_llm_cassette_key`.
2. Checks that the expected hash string matches the computed value.
3. Checks that `<cassetteDir>/<hash>.txt` exists on disk.

Example:
```go
{"generate_text", "openai", "", "Say hello.", "15023835511162652990"},
```

This test catches:
- Wrong DJB2 concat order (provider, model, prompt with NUL separators).
- Missing NUL separators between components.
- Wrong mask (should be 64-bit unsigned, not 32-bit).
- Signed-int overflow in a Go or PHP implementation.
- A renamed cassette file.
- A wrong default model value passed by the lowerer.

The Go `djb2CassetteKey` uses `uint64` (which wraps modulo 2^64), and
the PHP `mochi_llm_cassette_key` uses GMP with a 64-bit mask. Both must
agree on every known tuple for the cassette system to work across the C,
PHP, and other targets.

## 6. `runPhpLLMFixture` helper

`runPhpLLMFixture(t, mochiPath, wantFile, cassetteDir)` (phase13_test.go
lines 40-73) is like `runPhpFixture` but sets `MOCHI_LLM_CASSETTE_DIR`:

```go
cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
```

The cassette directory contains `<djb2-hash>.txt` files (one per LLM
call in the fixture). The `mochi_llm_generate` PHP helper reads from
this directory using `getenv('MOCHI_LLM_CASSETTE_DIR')`.

Each Phase 13 fixture lives in its own subdirectory under
`phase13-llm/`: `generate_text/generate_text.mochi`,
`generate_text/generate_text.out`, `generate_text/cassette/`.

## 7. `runPharFixture` helper

`runPharFixture(t, mochiPath, wantFile)` (phase17_test.go lines 51-93):

1. Builds `main.php` via `Driver.Build(TargetPhpSource)`.
2. Calls `emitPharStager(outDir, mainPhp, pharPath)` to generate the
   stager script.
3. Runs `php -d phar.readonly=0 build_phar.php` to produce `out.phar`.
4. Asserts `out.phar` was created.
5. Runs `php out.phar` and diffs stdout against `wantFile`.

The stager step requires PHP; the test skips if PHP is absent.

## 8. TestPhase17AllTargetsTogether

`TestPhase17AllTargetsTogether` (phase17_test.go lines 206-246) runs
all three Phase 17 targets (Phar stager, FrankenPHP bundle,
RoadRunner bundle) for every `.mochi` fixture in `phase17-packaging/`:

```go
for _, e := range entries {
    t.Run(name, func(t *testing.T) {
        // Build main.php
        mainPhp, _ := d.Build(fixturePath, outDir, TargetPhpSource)
        // Stage phar
        emitPharStager(outDir, mainPhp, pharPath)
        // Emit FrankenPHP bundle
        EmitFrankenPHPBundle(outDir, name)
        // Emit RoadRunner bundle
        EmitRoadRunnerBundle(outDir, name)
        // Assert all five artifacts exist
        for _, want := range []string{"build_phar.php", "Caddyfile", "Dockerfile", ".rr.yaml", "worker.php"} {
            os.Stat(filepath.Join(outDir, want))
        }
    })
}
```

This is a fragment-level gate: it checks that the artifacts are created
but does not run PHP to execute the Phar. The execution gate is in
`TestPhase17Phar`.

## 9. TestPhase16NonDeterministicBuildsAlsoMatch

`TestPhase16NonDeterministicBuildsAlsoMatch` (phase16_test.go lines
86-112) verifies that even with `Deterministic = false`, two builds of
the same source from the same revision produce byte-identical PHP:

```go
d1 := &Driver{CacheDir: t.TempDir(), NoCache: true}
d2 := &Driver{CacheDir: t.TempDir(), NoCache: true}
h1 := sha256(d1.Build(mochiPath, out1, TargetPhpSource))
h2 := sha256(d2.Build(mochiPath, out2, TargetPhpSource))
if h1 != h2 { t.Errorf(...) }
```

This is a defensive check complementing `TestPhase16Repro` (which uses
`Deterministic: true`). The comment in the test explains the rationale:
the PHP lowerer has no time-, random-, or path-derived sources of
non-determinism. Any divergence means an unintentional source of
non-determinism has been introduced.

## 10. Per-phase test file structure

Every phase has at least one test file in `transpiler3/php/build/`:

| File | Tests | PHP required? |
|------|-------|---------------|
| `phase00_test.go` | skeleton emit | Yes (runPhpFixture) |
| `phase01_test.go` | hello world prints | Yes |
| `phase02_test.go` | scalars, ops, control flow | Yes |
| `phase03_test.go` | collections | Yes |
| `phase04_test.go` | records | Yes |
| `phase05_test.go` | sum types, match | Yes |
| `phase06_test.go` | closures | Yes |
| `phase07_test.go` | query DSL + EmitFragments | Fragments: No; Runner: Yes |
| `phase08_test.go` | Datalog | Fragments: No; Runner: Yes |
| `phase09_test.go` | agents + EmitFragments | Fragments: No; Runner: Yes |
| `phase10_test.go` | streams + EmitFragments | Fragments: No; Runner: Yes |
| `phase11_test.go` | async | Yes |
| `phase12_test.go` | FFI | Yes |
| `phase13_test.go` | LLM (DJB2 gate + LLM runner + EmitFragments) | DJB2: No; LLM: Yes |
| `phase14_test.go` | fetch | Yes |
| `phase15_test.go` | Composer staging | Yes |
| `phase16_test.go` | reproducibility | SHA-256 only: No; EndToEnd: Yes |
| `phase17_test.go` | packaging (Phar + FrankenPHP + RoadRunner) | Structure: No; Phar: Yes |
| `phase18_test.go` | signed releases | No (artifact structure only) |

The fragment/structure-only tests in the right column all run in the
`go-side` CI job. The PHP-execution tests also run there (PHP is
installed by `shivammathur/setup-php@v2`).
