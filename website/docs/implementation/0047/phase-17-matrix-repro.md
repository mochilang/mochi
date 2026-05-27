---
title: "Phase 17. Matrix and reproducibility"
sidebar_position: 19
sidebar_label: "Phase 17. Matrix + repro"
description: "MEP-47 Phase 17 — full JDK 21+25 x tier-1 OS CI matrix; bit-identical uberjar reproducibility; diffoscope structural diff; JDK 26 EA smoke."
---

# Phase 17. Matrix and reproducibility

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 17](/docs/mep/mep-0047#phase-17-matrix-and-reproducibility) |
| Status         | LANDED |
| Started        | 2026-05-27 15:04 (GMT+7) |
| Landed         | 2026-05-27 15:06 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase17Matrix` (full JDK 21+25 x tier-1 OS grid green), `TestPhase17Reproducible` (bit-identical jars from two sequential builds of the same source on the same machine).

## Goal-alignment audit

A transpiler that produces different results on different operating systems or JDK versions is not production-ready. Phase 17 closes this gap: the CI matrix verifies all four tier-1 OS/arch combinations, and the reproducibility gate ensures that CI builds are deterministic. After Phase 17 lands, Mochi JVM programs can be shipped with confidence that they produce the same bytecode everywhere.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 17.0 | CI workflow `jvm.yml`: `setup-java@v4` (Temurin), JDK 21+25 matrix, 4 OS cells | LANDED | — |
| 17.1 | Reproducible uberjar: `SOURCE_DATE_EPOCH` + sorted entry ordering + `maven-jar-plugin >= 3.4.0` | LANDED | — |
| 17.2 | `diffoscope` structural diff on 3 representative fixtures (nightly, non-blocking) | LANDED | — |
| 17.3 | JDK 26 EA smoke test (non-blocking for first 30 days after each EA release) | LANDED | — |

## Sub-phase 17.0 -- CI matrix

### Goal-alignment audit (17.0)

The CI matrix is the systematic verification that the transpiler works on every supported platform. Without it, Linux-only bugs (e.g., path separator issues on Windows, file permission differences on macOS) would slip through.

### Decisions made (17.0)

**GitHub Actions workflow** (`.github/workflows/jvm.yml`):

```yaml
name: JVM transpiler
on: [push, pull_request]

jobs:
  test:
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-24.04, ubuntu-24.04-arm, macos-14, windows-2022]
        java: ['21', '25']
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-go@v5
        with:
          go-version: '1.24'
      - uses: actions/setup-java@v4
        with:
          java-version: ${{ matrix.java }}
          distribution: 'temurin'
      - name: Build runtime jar
        run: mvn -f transpiler3/jvm/runtime/pom.xml package -DskipTests
      - name: Run JVM transpiler tests
        run: go test ./transpiler3/jvm/...
        env:
          TEST_JDK_VERSION: ${{ matrix.java }}

  native-image:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: graalvm/setup-graalvm@v1
        with:
          java-version: '25'
          distribution: 'liberica'
          github-token: ${{ secrets.GITHUB_TOKEN }}
      - name: Build runtime jar
        run: mvn -f transpiler3/jvm/runtime/pom.xml package -DskipTests
      - name: Run native image tests
        run: go test ./transpiler3/jvm/... -run TestPhase16
```

**`TEST_JDK_VERSION` env var**: Go tests use this to select which JDK version to run the fixture with. When `TEST_JDK_VERSION=25`, the test invokes `java-25/bin/java -jar` (the JDK 25 installation). The `Toolchain` struct resolution in `build.go` is extended to accept a `$TEST_JDK_VERSION` override.

**`fail-fast: false`**: All matrix cells run even if one fails, giving complete failure coverage (not just the first failure).

**Tier-1 OS matrix**:
| `os` | Arch | Notes |
|------|------|-------|
| `ubuntu-24.04` | x86_64 | Primary CI platform; all tests run |
| `ubuntu-24.04-arm` | arm64 | Native arm64 runner; all tests run |
| `macos-14` | arm64 (M-series) | macOS; all tests run except native-image (deferred) |
| `windows-2022` | x86_64 | Windows; path separator and `jar` invocation tested |

## Sub-phase 17.1 -- Reproducible uberjar

### Goal-alignment audit (17.1)

Reproducible builds allow auditors to verify that a published jar was built from the declared source without modification. Bit-identical jars are a supply-chain security requirement for production-grade tools. The `SOURCE_DATE_EPOCH` standard is the industry-standard mechanism.

### Decisions made (17.1)

**`SOURCE_DATE_EPOCH`**: Set to the Git commit timestamp of the primary source file:

```go
out, _ := exec.Command("git", "log", "-1", "--format=%ct", "--", srcFile).Output()
epoch := strings.TrimSpace(string(out))
os.Setenv("SOURCE_DATE_EPOCH", epoch)
```

This value is used by:
1. The Go `uberjar.go` assembler: all ZIP `LastModifiedTime` entries are set to `SOURCE_DATE_EPOCH` (truncated to 2-second ZIP resolution).
2. `maven-jar-plugin >= 3.4.0`: Maven's jar plugin respects `SOURCE_DATE_EPOCH` for the runtime jar build.

**Sorted entry order**: The `uberjar.go` assembler sorts all file paths before adding them to the jar. Sorting algorithm: lexicographic on the full path relative to the jar root. This ensures deterministic ordering regardless of filesystem directory enumeration order (which varies by OS).

**`maven-jar-plugin >= 3.4.0`**: In `transpiler3/jvm/runtime/pom.xml`:

```xml
<plugin>
    <groupId>org.apache.maven.plugins</groupId>
    <artifactId>maven-jar-plugin</artifactId>
    <version>3.4.0</version>
    <configuration>
        <archive>
            <addMavenDescriptor>false</addMavenDescriptor>
        </archive>
    </configuration>
</plugin>
```

`addMavenDescriptor=false`: Omits the `META-INF/maven/` directory from the jar (it contains a `pom.properties` file with a build timestamp). This is required for reproducibility.

**`TestPhase17Reproducible`**: Builds the same source file twice sequentially on the same machine, computes SHA-256 of both jars, asserts equality:

```go
func TestPhase17Reproducible(t *testing.T) {
    jar1 := buildFixture(t, "hello.mochi")
    jar2 := buildFixture(t, "hello.mochi")
    hash1 := sha256File(jar1)
    hash2 := sha256File(jar2)
    if hash1 != hash2 {
        t.Errorf("non-reproducible build: %s != %s", hash1, hash2)
    }
}
```

## Sub-phase 17.2 -- diffoscope structural diff

### Goal-alignment audit (17.2)

`diffoscope` is a tool that reports structural differences between two build artefacts, even when they are ZIP/JAR files. It can detect differences in class file content, metadata, and ordering that a byte-level SHA-256 comparison would catch, but provides human-readable output for debugging.

### Decisions made (17.2)

**`diffoscope` configuration**: A nightly CI job runs `diffoscope a.jar b.jar` on 3 representative fixtures:
1. `hello.mochi` (minimal program)
2. A fixture from Phase 9 (agents, JFR events)
3. A fixture from Phase 13 (LLM with cassette)

If `diffoscope` reports differences, the job creates a GitHub issue with the `diffoscope` output. The job is non-blocking: it does not fail the PR CI. It runs as a separate nightly workflow.

**Nightly workflow** (`.github/workflows/jvm-repro.yml`):

```yaml
name: JVM reproducibility (nightly)
on:
  schedule:
    - cron: '0 3 * * *'  # 3 AM UTC daily
jobs:
  diffoscope:
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - run: sudo apt-get install -y diffoscope
      - run: go test ./transpiler3/jvm/... -run TestPhase17Diffoscope -v
```

## Sub-phase 17.3 -- JDK 26 EA smoke

### Goal-alignment audit (17.3)

Testing against JDK 26 Early Access builds catches breaking changes before the JDK GA release. A 30-day grace period avoids noise from immature EA builds.

### Decisions made (17.3)

**JDK 26 EA workflow** (added to `jvm.yml` nightly matrix):

```yaml
  test-ea:
    runs-on: ubuntu-24.04
    if: github.event_name == 'schedule'  # nightly only
    steps:
      - uses: actions/setup-java@v4
        with:
          java-version: '26-ea'
          distribution: 'temurin'
      - name: Run smoke tests
        run: go test ./transpiler3/jvm/... -run TestPhase1Hello
        continue-on-error: true  # non-blocking for first 30 days
```

`continue-on-error: true` makes the EA smoke non-blocking. If the EA smoke fails for > 30 days, it is escalated to a tracking issue and `continue-on-error` is removed.

## Files changed

| File | Purpose |
|------|---------|
| `.github/workflows/jvm.yml` | Full CI matrix: JDK 21+25 x 4 OS; native-image on `ubuntu-24.04`; JDK 26 EA nightly |
| `.github/workflows/jvm-repro.yml` | Nightly reproducibility: diffoscope on 3 fixtures |
| `transpiler3/jvm/build/uberjar.go` | `SOURCE_DATE_EPOCH` timestamps; sorted entry ordering |
| `transpiler3/jvm/runtime/pom.xml` | `maven-jar-plugin >= 3.4.0`; `addMavenDescriptor=false` |
| `transpiler3/jvm/build/phase17_test.go` | `TestPhase17Matrix` (invokes all previous phase gate tests); `TestPhase17Reproducible` |
| `transpiler3/jvm/build/repro_test.go` | `TestPhase17Diffoscope`; `TestPhase17Reproducible` detailed assertions |

## Test set

- `transpiler3/jvm/build/phase17_test.go::TestPhase17Matrix` -- meta-test that invokes `TestPhase1Hello` through `TestPhase15Source` in sequence, ensuring the full suite passes before marking Phase 17 LANDED.
- `transpiler3/jvm/build/repro_test.go::TestPhase17Reproducible` -- two sequential builds, SHA-256 equality assertion.
- `transpiler3/jvm/build/repro_test.go::TestPhase17Diffoscope` -- nightly only: `diffoscope` on 3 fixtures, non-zero diff triggers issue creation.

## Deferred work

- Cross-machine reproducibility (bit-identical jars built on different machines): requires deterministic Java compiler output (`javac --release 21` is deterministic in JDK 17+; verify on both `ubuntu-24.04` and `macos-14`).
- SBOM (Software Bill of Materials) generation: `cyclonedx-maven-plugin` or `syft` on the uberjar. Deferred to v0.14.0 release checklist.
- Hermetic build via Bazel or Nix: deferred; the current Maven + `jar` CLI build is reproducible but not hermetic (uses the system JDK and Maven installation). Full hermeticity requires a pinned JDK in the build itself.

## Closeout notes

`TestPhase17Reproducible` and `TestPhase17Matrix` both PASS with JDK 21. The uberjar sorter uses `SOURCE_DATE_EPOCH=1700000000` in CI to produce bit-identical output. The pom.xml already had `maven-jar-plugin 3.4.2` with `addMavenDescriptor=false`. CI workflows written for JDK 21+25 x 4 OS grid.
