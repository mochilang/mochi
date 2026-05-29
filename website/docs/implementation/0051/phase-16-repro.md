---
title: "Phase 16. Reproducible build"
sidebar_position: 21
sidebar_label: "Phase 16. Reproducibility"
description: "MEP-51 Phase 16, SOURCE_DATE_EPOCH plus sorted RECORD plus ast.unparse + ruff format determinism yields byte-identical wheel SHA256 across linux-x86_64, linux-aarch64, and macos-arm64 CI hosts."
---

# Phase 16. Reproducible build

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 16](/docs/mep/mep-0051#phase-plan) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase16ReproduciblePython`: 2 dedicated fixtures (`reproducibility_basic`, `reproducibility_with_extras`) built on linux-x86_64, linux-aarch64, and macos-arm64 with `SOURCE_DATE_EPOCH` pinned to the git commit timestamp; `shasum -a 256 dist/*.whl` must be byte-equal across all three hosts. Secondary gates: (a) `ast.unparse` + `ruff format` fixed-point (run the emitter twice, the second `git diff` shows zero changes); (b) the wheel `RECORD` file entries are sorted lexicographically; (c) the wheel zip contains no `__pycache__` directories. Windows reproducibility is excluded through Phase 16 (filesystem case-insensitivity delta); Phase 16.1 adds Windows.

## Goal-alignment audit

Phase 16 is a supply-chain gate. Two Mochi-emitted wheels built from the same Mochi source on different CI hosts must be byte-identical. Without this property, downstream Mochi-lock-file SHA256 pinning is meaningless, PyPI's PEP 740 attestation does not give end-users a way to verify the artifact they install matches the artifact PyPI was given, and an attacker who compromises one CI host can substitute a malicious wheel that no one will detect. Phase 16 ships the determinism stack (`SOURCE_DATE_EPOCH`, sorted `RECORD`, formatter fixed-point, lex-sorted zip entries) that makes byte-identical builds the contract.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 16.0 | `SOURCE_DATE_EPOCH` pinned to git commit timestamp; driver injects on every `uv build` invocation | NOT STARTED | — |
| 16.1 | Wheel `RECORD` file entries sorted lexicographically; zip entries sorted by name; `__pycache__` excluded | NOT STARTED | — |
| 16.2 | `ast.unparse` + `ruff format` fixed-point: emit twice, `git diff` is empty; no random salt in formatter | NOT STARTED | — |
| 16.3 | Cross-host gate: build on linux-x86_64 + linux-aarch64 + macos-arm64, assert SHA256 byte-match | NOT STARTED | — |

## Sub-phase 16.0 -- SOURCE_DATE_EPOCH

### Goal-alignment audit (16.0)

The wheel is a zip; zip stores mtime per entry; mtime is otherwise the wall-clock time of `uv build`. Two builds running at different instants produce different wheel SHA256 even when every byte of source is identical. `SOURCE_DATE_EPOCH` pins mtime to a deterministic value so the wheel SHA depends only on source content.

### Decisions made (16.0)

**`SOURCE_DATE_EPOCH`** is the reproducible-builds standard environment variable (https://reproducible-builds.org/specs/source-date-epoch/). hatchling 1.25+ honours it for wheel and sdist mtime fields.

**Source of the value**: git commit timestamp of `HEAD` at build time. The driver reads it via `git log -1 --pretty=%ct`:

```go
// transpiler3/python/build/repro.go
func GitCommitEpoch(repoDir string) (string, error) {
    cmd := exec.Command("git", "log", "-1", "--pretty=%ct")
    cmd.Dir = repoDir
    out, err := cmd.Output()
    if err != nil {
        return "", err
    }
    return strings.TrimSpace(string(out)), nil
}
```

**Driver injection**: every `uv build` call from `transpiler3/python/build/driver.go` sets `SOURCE_DATE_EPOCH` in the subprocess environment. No fallback to system time; if the value cannot be read (the source is not in a git repo), the driver exits with a clear error and does not fall back to `time.Now()`.

**CI script form** (documented for the GitHub Actions workflow that lands alongside Phase 16):

```yaml
- name: Build wheel reproducibly
  run: |
    export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
    uv build --wheel
```

**Why not the source tree mtime**: research note 10 §11 considered taking mtime from the Mochi source file but rejected it: a single Mochi project may span many files with different mtimes, and the git commit timestamp is the only value that is well-defined per build and identical across clones.

## Sub-phase 16.1 -- RECORD sorting + zip entry sorting + pycache exclusion

### Goal-alignment audit (16.1)

`os.walk()` on ext4 returns entries in inode order; on APFS in directory-block order; on NTFS in alphabetical order. Without canonicalisation the wheel zip's entry order differs across hosts even with identical mtimes. `RECORD` is generated from the same walk; its line order diverges identically. Phase 16.1 forces lex-sort on both.

### Decisions made (16.1)

**Wheel zip entry order**: hatchling 1.25+ sorts entries lexicographically before writing the zip. The emitter relies on this; no driver-side post-processing.

**`RECORD` file**: PEP 376 specifies `RECORD` as a CSV of `path,sha256=<hash>,<length>`. Lines are sorted lexicographically by path. hatchling 1.25+ enforces this; the test runner verifies by parsing the wheel and asserting `sorted(records) == records`.

**`__pycache__` exclusion**: the wheel must contain no `.pyc` files. hatchling's default file selector excludes `__pycache__/` and `*.pyc`. The emitter never writes `.pyc` to the source tree; the runner asserts `unzip -l dist/*.whl | grep -c "__pycache__"` is zero.

**Locale-independent metadata**: hatchling 1.25 writes the `WHEEL` file's `Generator:` field as a fixed string (`hatchling 1.25.0` or whatever the pinned version is). No locale formatting, no current-time stamps, no host-name leakage. The `METADATA` file's field order is fixed by PEP 643.

**Test assertions** (in `TestPhase16RecordSorted`):

```go
records := parseWheelRecord(wheelPath)
sortedRecords := append([]string{}, records...)
sort.Strings(sortedRecords)
if !reflect.DeepEqual(records, sortedRecords) {
    t.Fatalf("RECORD entries not lex-sorted")
}
```

## Sub-phase 16.2 -- ast.unparse + ruff format fixed-point

### Goal-alignment audit (16.2)

The emit chain is `aotir` → Python AST → `ast.unparse` → `ruff format` → `.py` text. If `ast.unparse` produces non-deterministic output (e.g., set iteration order in module-level imports) or `ruff format` is unstable (line wrapping picks differently on two runs), the wheel content differs even with identical `SOURCE_DATE_EPOCH`. Phase 16.2 makes the whole chain a fixed-point under repeated application.

### Decisions made (16.2)

**`ast.unparse` determinism**: CPython 3.12's `ast.unparse` is deterministic given a deterministic AST. The lower pass (`transpiler3/python/lower/`) is the source of any non-determinism. The known risk: emitting imports via a Python `set` (whose iteration order depends on `PYTHONHASHSEED`). The fix lands in 16.2: imports are accumulated into a `[]string` and explicitly sorted before `ast.ImportFrom` nodes are constructed.

**`PYTHONHASHSEED=0`**: pinned in the CI environment and in the test runner. Belt-and-braces on top of the import-sorting fix above; not load-bearing once imports are sorted at the Go-side lower pass.

**`ruff format` fixed-point**: ruff 0.7+ format is documented as a fixed-point operation (`ruff format X.py; ruff format X.py` produces no diff on the second invocation). The gate verifies:

```bash
$ uv run ruff format src/
$ git stash --keep-index
$ uv run ruff format src/
$ git diff --quiet src/  # must succeed; exit 0
```

**`ruff check --fix --select=I,F401`** (import sort + unused-import removal) is also fixed-point. The gate runs `ruff check --fix` twice and asserts no second-run changes.

**Tool version pin**: `ruff==0.7.0` exactly. Floating versions (e.g., `ruff>=0.7,<0.8`) break reproducibility when the patch release lands a formatting tweak. The pin lives in the emitted `[project.optional-dependencies].dev` and the Mochi-side dev-tools manifest.

**`mypy 1.13`** and **`pyright 1.1.380`** are also pinned exactly (research note 11 §test stability). These are gate runners, not emitters, but a tool upgrade can produce a new "strict" diagnostic that breaks the gate without any source change; pinning is the cheapest fix.

## Sub-phase 16.3 -- cross-host SHA256 byte-match

### Goal-alignment audit (16.3)

Phases 16.0-16.2 produce a deterministic single-host build. Phase 16.3 is the multi-host verification: build the same wheel on three independent CI hosts (linux-x86_64, linux-aarch64, macos-arm64) and assert byte-identical SHA256. Without this gate, the previous sub-phases could be host-deterministic but cross-host divergent (e.g., `os.path.sep` accidentally leaking into a path string written into the wheel).

### Decisions made (16.3)

**Matrix**:

| Runner | Arch | Python | Role |
|--------|------|--------|------|
| `ubuntu-24.04` | x86_64 | 3.12.7 | host A |
| `ubuntu-24.04-arm` | aarch64 | 3.12.7 | host B |
| `macos-14` | arm64 | 3.12.7 | host C |

**Workflow**:

```yaml
reproducibility:
  strategy:
    matrix:
      include:
        - { runner: ubuntu-24.04,      tag: linux-x86_64 }
        - { runner: ubuntu-24.04-arm,  tag: linux-aarch64 }
        - { runner: macos-14,          tag: macos-arm64 }
  runs-on: ${{ matrix.runner }}
  steps:
    - uses: actions/checkout@v4
    - uses: astral-sh/setup-uv@v3
      with: { version: "0.7.0" }
    - run: uv python install 3.12.7
    - name: Build wheel deterministically
      run: |
        export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
        uv build --wheel
        shasum -a 256 dist/*.whl > sha-${{ matrix.tag }}.txt
    - uses: actions/upload-artifact@v4
      with:
        name: wheel-sha-${{ matrix.tag }}
        path: sha-${{ matrix.tag }}.txt

compare:
  needs: reproducibility
  runs-on: ubuntu-24.04
  steps:
    - uses: actions/download-artifact@v4
    - run: |
        diff wheel-sha-linux-x86_64/sha-linux-x86_64.txt \
             wheel-sha-linux-aarch64/sha-linux-aarch64.txt
        diff wheel-sha-linux-x86_64/sha-linux-x86_64.txt \
             wheel-sha-macos-arm64/sha-macos-arm64.txt
```

**Windows excluded through Phase 16**: NTFS is case-insensitive by default and stores filenames with case preserved but compared insensitively; some hatchling versions normalise case in zip entries, producing windows-host wheels that diverge from linux-host wheels in entry casing. The fix is tracked but not in v1; Phase 16.1 (a future sub-phase) adds Windows once the entry-casing normaliser is wired through. Research note 10 §11 documents this constraint.

**Test runner** (`tests/transpiler3/python/phase16_test.go`):

```go
func TestPhase16ReproduciblePython(t *testing.T) {
    epoch, err := build.GitCommitEpoch(repoRoot)
    if err != nil { t.Fatal(err) }
    for _, fixture := range []string{"reproducibility_basic", "reproducibility_with_extras"} {
        sha1 := buildWheelAndSHA(t, fixture, epoch)
        cleanDist(t, fixture)
        sha2 := buildWheelAndSHA(t, fixture, epoch)
        if sha1 != sha2 {
            t.Fatalf("%s wheel SHA changed on rebuild: %s vs %s", fixture, sha1, sha2)
        }
    }
}
```

The cross-host comparison runs in CI only; the local test verifies host-determinism (same host, two builds, identical SHA).

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/repro.go` | `GitCommitEpoch`; `SOURCE_DATE_EPOCH` injection helper |
| `transpiler3/python/build/driver.go` | `uv build` invocation always sets `SOURCE_DATE_EPOCH` |
| `transpiler3/python/lower/imports.go` | Sort imports lex before constructing `ast.ImportFrom` nodes (kills `PYTHONHASHSEED` dependence) |
| `transpiler3/python/emit/ruff.go` | `ruff format` + `ruff check --fix --select=I,F401` runner; fixed-point assertion |
| `transpiler3/python/build/sha.go` | Wheel SHA256 computation helper |
| `.github/workflows/transpiler3-python-reproducibility.yml` | Three-host matrix + compare job |
| `transpiler3/python/build/phase16_test.go` | `TestPhase16ReproduciblePython`, `TestPhase16RecordSorted`, `TestPhase16FixedPoint` |
| `tests/transpiler3/python/fixtures/phase16-repro/reproducibility_basic/` | Hello-world style fixture for SHA gate |
| `tests/transpiler3/python/fixtures/phase16-repro/reproducibility_with_extras/` | Fixture with `[project.optional-dependencies]` |

## Test set

- `TestPhase16ReproduciblePython` -- builds each of 2 fixtures twice on the same host; asserts SHA256 byte-equal. Cross-host CI matrix runs in `.github/workflows/transpiler3-python-reproducibility.yml`.
- `TestPhase16RecordSorted` -- parses the wheel `RECORD` file and asserts lex-sorted entries.
- `TestPhase16FixedPoint` -- runs the emitter twice over a small fixture and asserts the emitted Python source is byte-equal on the second run.
- `TestPhase16NoPycache` -- inspects the wheel zip and asserts no `__pycache__/` entries.

## Deferred work

- Windows reproducibility (NTFS case-insensitivity + CRLF normalisation in any embedded text). Tracked as Phase 16.1, scheduled after the Linux + macOS gate is green.
- `diffoscope` integration for diff visualisation when SHA divergence is detected. Useful for debugging; the gate is binary pass/fail without it.
- sdist (`tar.gz`) reproducibility gate. tar headers include uid/gid in addition to mtime; hatchling 1.25 zeroes these but the cross-host verification is deferred to a sub-phase.
- `uv build --reproducible` flag (uv 0.5+) as a one-line replacement for the explicit `SOURCE_DATE_EPOCH` export. Once uv 0.7 stabilises this we can switch the driver over; for now the explicit env var is the contract.
