---
title: "Phase 19. Workspace cache + parallel fetch + perf"
sidebar_position: 20
sidebar_label: "Phase 19. Cache + perf"
description: "MEP-57 Phase 19 — shared workspace cache, parallel blob fetch with HTTP/2 multiplexing, solver memoisation, end-to-end perf budgets vs Cargo and uv baselines."
---

# Phase 19. Workspace cache + parallel fetch + perf

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 19](/docs/mep/mep-0057#phase-19-cache-perf) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase19Perf`: cold resolve + fetch of the 500-package fixture finishes within the perf budget (resolve under 800 ms, fetch under 5 s on a reference machine); warm resolve under 50 ms. Bench fails the build if a regression exceeds 15%.

Pass criteria:

1. Cold resolve budget. 500-package synthetic graph, no cache, resolves under 800ms on the reference machine (Apple M2 Pro, 32GB RAM, 1Gbit fibre).
2. Cold fetch budget. Same fixture, blob fetch+verify+extract under 5s, dominated by parallel HTTP/2 streams to the mock registry.
3. Warm resolve budget. With every input cached, resolve completes under 50ms (lockfile load + manifest hash check only).
4. Workspace cache hit. A 10-member workspace where all members share `@mochi/json` reuses the same extracted tree under `$MOCHI_HOME/store/`; on-disk size grows by exactly one copy.
5. Regression gate. CI runs `mochi pkg bench resolve` and rejects any PR whose result exceeds the rolling-mean baseline by more than 15%.
6. Cache GC. `mochi pkg cache gc --keep-recent=14d --max-size=10GB` evicts LRU entries to meet the budget; the test asserts post-GC size and recent retention.
7. Parallelism scaling. Cold fetch at parallelism=1, 4, 8, 16 shows expected speedup curve (sublinear past ~8 due to TCP and CPU limits, not Mochi-internal contention).

## Goal-alignment audit

Perf is the *day-to-day* surface. A slow resolver poisons every other ergonomic win. The bar is uv (2024) for Python and Cargo (2024) for native: anything slower at parity scale is a regression we ship to users every CI run. The user-facing goal moved: "`mochi build` feels instantaneous on a clean checkout, not slower than `cargo build`".

The 800ms cold-resolve budget for 500 packages is loosely derived from uv's published benchmarks (research note 03 §6): uv resolves PyPI's `transformers` (~150 deps) in ~150ms cold. At linear scale 500 nodes is ~500ms; the 800ms budget allows headroom for Mochi-specific constraints (capability/target/compiler checks).

Parallel fetch over HTTP/2 multiplexing is where most of the wall-clock win lives. Single-stream sequential fetch of 500 blobs at 10ms RTT each is 5 seconds of pure RTT. With 8 streams over one HTTP/2 connection, RTT cost drops by 8x, dominated by bandwidth.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 19.0 | Shared workspace cache at `$MOCHI_HOME/store/` | NOT STARTED | — |
| 19.1 | HTTP/2 multiplexed parallel blob fetch | NOT STARTED | — |
| 19.2 | Solver memoisation across workspace members | NOT STARTED | — |
| 19.3 | Concurrent decompression and extraction | NOT STARTED | — |
| 19.4 | `mochi pkg bench resolve` perf harness vs golden baseline | NOT STARTED | — |
| 19.5 | Regression gate (15% bound) integrated into CI | NOT STARTED | — |
| 19.6 | Cache GC: LRU eviction with `mochi pkg cache gc` | NOT STARTED | — |
| 19.7 | Profiling instrumentation (`--trace` flag, `pprof` endpoint) | NOT STARTED | — |
| 19.8 | Lockfile load fast-path: mmap + skip-on-hash-match | NOT STARTED | — |

## Sub-phase 19.0 — Shared cache

Canonical root `$MOCHI_HOME` (default: `~/.cache/mochi` on Linux/macOS,
`%LOCALAPPDATA%\mochi` on Windows; full resolution order in
[phase 0 §conventions](./phase-00-skeleton#files-changed)):

```
$MOCHI_HOME/
  store/
    blobs/<bb>/<aa>/<hex>              # raw tarballs (owner: Phase 9)
    extracted/<hex>/                   # extracted trees (owner: Phase 9)
    locks/<hex>.lock                   # fcntl locks (owner: Phase 9.3)
    metrics/                           # last-access timestamps for GC (owner: Phase 19)
  index/<bucket>/<scope>/<name>        # index JSONL (owner: Phase 8)
```

Phase 19 owns only `store/metrics/` and the install/GC code paths; the
storage schema itself is owned by the phases that introduce each artefact.
This phase pins the canonical paths so all installer + GC + bench code
agrees on disk layout.

Single canonical location for all `mochi` invocations on the machine. Workspaces with multiple members share `extracted/<hex>/` by hardlink:

```go
// pkg/pkgstore/install.go
func InstallToWorkspace(workspaceRoot, blake3 string) error {
    storeDir := storePath(blake3)
    targetDir := filepath.Join(workspaceRoot, ".mochi/deps", blake3)
    if exists(targetDir) { return nil }
    return hardlinkTree(storeDir, targetDir)
}
```

Hardlinks on POSIX, junction points on Windows. Falls back to copy on cross-filesystem boundaries (caches user warning).

## Sub-phase 19.1 — Parallel fetch

Phase 9.1 already implements blob fetch via Phase 8's HTTP/2 client. Phase 19 wires the parallelism around it:

```go
// pkg/pkgstore/fetch.go
type FetchPool struct {
    Concurrency int
    Store       Store
    Verifier    Verifier
}

func (p *FetchPool) FetchAll(ctx context.Context, lock *Lockfile) error {
    g, gctx := errgroup.WithContext(ctx)
    sem := make(chan struct{}, p.Concurrency)
    for _, pkg := range lock.Packages {
        pkg := pkg
        sem <- struct{}{}
        g.Go(func() error {
            defer func() { <-sem }()
            return p.fetchOne(gctx, pkg)
        })
    }
    return g.Wait()
}

func (p *FetchPool) fetchOne(ctx context.Context, pkg LockedPackage) error {
    if exists(extractedPath(pkg.BLAKE3)) { return nil }   // already extracted
    lock := store.AcquireBlobLock(pkg.BLAKE3)
    defer lock.Release()
    if exists(extractedPath(pkg.BLAKE3)) { return nil }   // racy reader won
    rc, err := p.Store.Fetch(ctx, pkg.BLAKE3)
    if err != nil { return err }
    return p.Verifier.VerifyAndExtract(rc, pkg)
}
```

Concurrency default = 8. Override via `MOCHI_PARALLELISM` or `--parallelism=N` flag. The semaphore limits in-flight fetches; the HTTP/2 transport multiplexes all of them over one TCP connection per host.

## Sub-phase 19.2 — Solver memoisation across workspace

In a workspace with members A, B, C all depending on `@mochi/json`, the solver is invoked once per member but the version search space for shared deps is identical. Memoise:

```go
// pkg/pkgsolver/cache.go
type SolverCache struct {
    Manifests map[PackageKey]*Manifest    // pkg+ver -> manifest
    Ranges    map[string]*ResolvedRange   // dep range -> resolved versions
}

func (s *Solver) ResolveWorkspace(ws *Workspace) (map[Member]Lockfile, error) {
    cache := NewSolverCache()
    out := map[Member]Lockfile{}
    for _, m := range ws.Members {
        sol, err := s.SolveWithCache(m, cache)
        if err != nil { return nil, err }
        out[m] = sol
    }
    return out, nil
}
```

For a 10-member workspace where 80% of the dep set is shared, this is roughly a 5x cold-resolve speedup vs naive per-member resolution.

## Sub-phase 19.3 — Concurrent extraction

The extraction path in Phase 9.5 is single-threaded per tarball but multiple tarballs can extract in parallel:

```go
// pkg/pkgblob/extract_concurrent.go
func ExtractConcurrent(blobs []Blob, dest string, parallelism int) error {
    g, _ := errgroup.WithContext(context.Background())
    sem := make(chan struct{}, parallelism)
    for _, b := range blobs {
        b := b
        sem <- struct{}{}
        g.Go(func() error {
            defer func() { <-sem }()
            return extractOne(b, dest)
        })
    }
    return g.Wait()
}
```

The bottleneck is small-file syscall latency (open, write, fsync per extracted file). For 500 packages averaging 50 files each = 25000 small writes, parallel I/O hides most of the latency.

`fsync` policy: per-tarball `fsync` after the last file in that tarball, not per file. Acceptable durability trade-off (a crash mid-extract leaves a partial extracted tree, which the next install retries).

## Sub-phase 19.4 — `mochi pkg bench resolve`

```go
// cmd/mochi/bench.go
func cmdBenchResolve(c *cli.Context) error {
    fixtures := loadFixtures(c.String("fixture-dir"))
    results := []Result{}
    for _, fix := range fixtures {
        warmups := 2
        iters := 10
        for i := 0; i < warmups; i++ { runResolve(fix) }
        var times []time.Duration
        for i := 0; i < iters; i++ {
            t := time.Now()
            runResolve(fix)
            times = append(times, time.Since(t))
        }
        results = append(results, Result{
            Fixture: fix.Name,
            P50: percentile(times, 0.50),
            P95: percentile(times, 0.95),
            P99: percentile(times, 0.99),
        })
    }
    printResults(results)
    if c.String("baseline") != "" {
        return compareBaseline(results, c.String("baseline"), c.Float64("threshold"))
    }
    return nil
}
```

Output:

```
fixture                 p50      p95      p99      vs baseline
500-pkg-cold-resolve    742ms    810ms    832ms    +3.1% (within 15% bound)
500-pkg-cold-fetch      4.21s    4.50s    4.61s    +1.8% (within 15% bound)
500-pkg-warm-resolve    38ms     45ms     52ms     -8.3% (improved)
workspace-10            1.12s    1.20s    1.25s    +4.4% (within 15% bound)
```

## Sub-phase 19.5 — CI regression gate

A nightly CI job plus a PR-gated comparison run:

```yaml
# .github/workflows/bench.yml
name: Package system bench

on:
  schedule:
    - cron: "0 7 * * *"           # nightly UTC; regenerates baseline.json
  pull_request:
    paths:
      - 'pkg/pkgsolver/**'
      - 'pkg/pkgblob/**'
      - 'pkg/pkgstore/**'
      - 'pkg/pkgregistry/**'
      - 'bench/**'
      - '.github/workflows/bench.yml'

permissions:
  contents: read
  # The nightly job writes back baseline.json via a follow-up PR (see
  # the "publish-baseline" step). PR runs do NOT receive this scope.
  pull-requests: write

concurrency:
  group: bench-${{ github.event_name }}-${{ github.head_ref || 'main' }}
  cancel-in-progress: ${{ github.event_name == 'pull_request' }}

jobs:
  bench:
    # Refuse fork PRs: the bench machine type (large runner) is paid time
    # and exposing it to untrusted code is a DoS surface.
    if: github.event_name != 'pull_request' || github.event.pull_request.head.repo.full_name == github.repository
    runs-on: ubuntu-24.04-large    # 8-core runner for stable timing
    timeout-minutes: 60
    steps:
      - uses: actions/checkout@v6
        with:
          fetch-depth: 0           # bench/baseline.json history
      - uses: actions/setup-go@v6
        with:
          go-version-file: go.mod
          cache-dependency-path: go.sum
      # Pin clock, locale, CPU governor for stable measurement.
      - name: Pin bench environment
        run: |
          echo "TZ=UTC" >> "$GITHUB_ENV"
          echo "LC_ALL=C.UTF-8" >> "$GITHUB_ENV"
          echo "SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)" >> "$GITHUB_ENV"
          sudo cpupower frequency-set --governor performance || true
        shell: bash
      - run: go build -trimpath -o mochi ./cmd/mochi
      - name: Run bench
        run: |
          ./mochi pkg bench resolve \
            --baseline=bench/baseline.json \
            --threshold=0.15 \
            --report=bench/report.json
      - name: Upload report
        if: always()
        uses: actions/upload-artifact@v5
        with:
          name: bench-report-${{ github.run_id }}
          path: bench/report.json
          retention-days: 90
      # Nightly only: regenerate baseline.json and open a follow-up PR
      # so a human reviews the rolling drift.
      - name: Publish new baseline
        if: github.event_name == 'schedule'
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          cp bench/report.json bench/baseline.json
          gh pr create --title "bench: rolling baseline ${{ github.run_id }}" \
            --body "Auto-generated from nightly bench run" \
            --label "bench-baseline" \
            --base main
```

The `baseline.json` is regenerated nightly on the `main` branch after the
smoke tests gate. PRs run the bench against the rolling baseline. Exceeding
the 15% threshold marks the PR as failing the bench gate (separate from
the unit-test gate; the PR can be merged with a `bench-exempt` label and a
written reason in the PR body).

Workflow security notes:

- `pull-requests: write` is set at workflow level but only the nightly
  step (`if: github.event_name == 'schedule'`) ever invokes `gh pr
  create`. PR runs never reach that step.
- Fork PRs are refused via the `if:` guard on the job (`head.repo.full_name
  == github.repository`); they receive the same skipped result a path
  filter miss would produce.
- `actions/upload-artifact@v5` is the only secret-touching action; it
  uses the workflow's default `GITHUB_TOKEN` with `contents: read` scope,
  which is sufficient.

## Sub-phase 19.6 — Cache GC

```
mochi pkg cache gc                                 # default: keep recent 14d, max 10GB
mochi pkg cache gc --keep-recent=30d --max-size=20GB
mochi pkg cache gc --dry-run                       # show what would be evicted
mochi pkg cache gc --prune-orphans                 # remove extracted trees with no in-store blob
```

```go
// pkg/pkgstore/gc.go
func GC(opts GCOptions) (*GCReport, error) {
    entries := walkExtractedTrees()
    sort.Slice(entries, func(i, j int) bool { return entries[i].LastAccess.Before(entries[j].LastAccess) })
    cutoff := time.Now().Add(-opts.KeepRecent)
    var report GCReport
    var totalSize int64
    for _, e := range entries { totalSize += e.Size }
    for _, e := range entries {
        if totalSize <= opts.MaxSize { break }
        if e.LastAccess.After(cutoff) && opts.RespectRecent { continue }
        evict(e); totalSize -= e.Size
        report.Evicted = append(report.Evicted, e.Path); report.BytesFreed += e.Size
    }
    return &report, nil
}
```

`LastAccess` derived from filesystem atime where supported; on `noatime` filesystems, a per-entry `.lastused` file is written on cache hit.

## Sub-phase 19.7 — Profiling

`mochi --trace=trace.json resolve` writes a Chrome trace event format file; openable in `chrome://tracing` or `https://ui.perfetto.dev/`. Spans:

- `solver.decide`, `solver.propagate`, `solver.backtrack`
- `fetch.http`, `fetch.dual_hash`, `fetch.extract`
- `lockfile.write`, `lockfile.canonical_check`

A `--pprof=:6060` flag spawns `net/http/pprof` on localhost for live profiling. Disabled in default builds; enabled only with `--profile`.

## Sub-phase 19.8 — Lockfile fast path

A warm `mochi build` reads the lockfile, hashes it (BLAKE3 of the canonical TOML), and compares against the manifest's stored hash. If equal, no resolution needed:

```go
func WarmBuild(m *Manifest) (*Lockfile, error) {
    lock, err := pkglock.ParseFile("mochi.lock")
    if err != nil { return nil, err }
    expectedHash := pkglock.HashManifestForLock(m)
    if lock.ManifestHash == expectedHash { return lock, nil }  // happy path
    return resolveAndWriteLock(m, lock)
}
```

Mmap-based read: the lockfile is mmapped (read-only) for parse. For a typical 100KB lockfile, parse drops from ~5ms to ~1ms.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgstore/store.go` | Shared cache | Owner |
| `pkg/pkgstore/install.go` | Hardlink installer | Owner |
| `pkg/pkgstore/fetch.go` | Parallel pool | Owner |
| `pkg/pkgstore/gc.go` | LRU GC | Owner |
| `pkg/pkgsolver/cache.go` | Cross-member memoisation | Owner |
| `pkg/pkgblob/extract_concurrent.go` | Parallel extract | Owner |
| `pkg/pkgtrace/trace.go` | Chrome trace emitter | Owner |
| `cmd/mochi/bench.go` | `mochi pkg bench` handler | Owner |
| `cmd/mochi/cache_gc.go` | `mochi pkg cache gc` handler | Owner |
| `tests/pkgsystem/perf/500-pkg/*` | Cold/warm budgets | Owner |
| `tests/pkgsystem/perf/workspace-10/*` | Member cache reuse | Owner |
| `tests/pkgsystem/perf/regress/*` | Bench fail injection | Owner |
| `bench/baseline.json` | Rolling perf baseline | Owner |
| `.github/workflows/bench.yml` | Nightly + PR gate | Owner |

## Error code surface

| Code | Trigger |
|------|---------|
| `M057_PERF_E001` | Bench result exceeds baseline by threshold. |
| `M057_PERF_E002` | Cache GC failed (permissions, disk error). |
| `M057_PERF_E003` | Hardlink failed across filesystems; fell back to copy. |

## Test set

- `TestPhase19SharedStore` — same blob hardlinked into multiple workspace members.
- `TestPhase19ParallelFetch` — concurrent fetches complete within budget.
- `TestPhase19SolverMemo` — workspace resolves with cache hits.
- `TestPhase19ConcurrentExtract` — parallel extraction within wall-clock budget.
- `TestPhase19BenchHarness` — `mochi pkg bench` produces stable results across runs.
- `TestPhase19BenchRegress` — synthetic regression fixture triggers fail.
- `TestPhase19CacheGC` — LRU eviction meets size budget.
- `TestPhase19Trace` — trace.json valid Chrome trace event format.
- `TestPhase19LockFastPath` — manifest unchanged skips resolve.

## Performance targets (reference machine)

From research note 05 §8 and research note 08 §14:

| Operation | Budget | Notes |
|-----------|--------|-------|
| Cold resolve, 500 pkgs | 800ms p95 | solver only; mock registry, all entries on disk |
| Cold fetch + extract, 500 pkgs, ~50MB | 5s p95 | over HTTP/2 to local mock |
| Warm resolve | 50ms p95 | manifest unchanged, lockfile hit |
| Workspace 10 members, shared cache | 1.2s p95 | first-run cold; second-run warm under 100ms |
| Cache GC, 10000 entries | 2s | mostly stat() syscalls |
| Lockfile mmap parse | 1ms | 100KB lockfile |

## Open questions

- Whether to ship a precomputed bench baseline per supported OS/arch or just `linux/amd64`; current plan: `linux/amd64` baseline; other architectures track relative regression only.
- Whether to expose a `mochi resolve --dry-run --json --profile` for editor consumption; deferred to LSP work.
- Whether the cache supports remote (shared via NFS / S3) backends; deferred to v1.1.

## Cross-references

- Perf budgets: [research note 05 §8](/docs/research/0057/solver-design).
- Cache design: [research note 08 §5](/docs/research/0057/content-addressed-store).
- uv parity discussion: [research note 03 §6](/docs/research/0057/prior-art-registries).
- Cargo concurrent fetch as prior art: [research note 03 §1](/docs/research/0057/prior-art-registries).
- Workspace solving: [phase 3 §3.5](./phase-03-workspaces).
