---
title: "Phase 8. Network sparse index"
sidebar_position: 9
sidebar_label: "Phase 8. Network sparse index"
description: "MEP-57 Phase 8 — HTTPS sparse index over `index.mochi.dev`, ETag conditional fetch, exponential backoff with jitter, HTTP/2 multiplexing, mock-fronted CI gates."
---

# Phase 8. Network sparse index (HTTPS)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-57 §Phases · Phase 8](/docs/mep/mep-0057#phase-8-network-index) |
| Status         | NOT STARTED |
| Started        | — |
| Landed         | — |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase8SparseIndex`: every solver fixture from Phase 5 runs against a mock HTTPS index (`httptest.Server`) with simulated latency, 503s, and 429s; resolution matches the local-registry baseline. Conditional GET via ETag returns 304 on warm cache.

Pass criteria:

1. Solver parity. Every Phase 5 fixture re-runs under the HTTP backend (against a local `httptest.Server` mirroring the on-disk registry); the resolved tree matches the local-registry baseline byte-for-byte.
2. Conditional fetch. With a primed ETag cache, the warm fetch returns 304 and the parsed entries match what the cold fetch returned.
3. Retry semantics. A scripted 503 followed by a 200 succeeds; six consecutive 503s raise `M057_INDEX_E004` with the retry chain in the error message.
4. Rate limiting. A 429 with `Retry-After: 5` delays the next attempt by at least 5 seconds; without `Retry-After` the exponential schedule kicks in.
5. HTTP/2 multiplexing. A single resolution issues parallel GETs over one TCP connection; the test verifies via `httptest.Server.TLS.NextProto` selection and request counting.
6. Failover. With `[[registry.alternate]]` set and the primary unreachable, the secondary is queried after the retry budget.

## Goal-alignment audit

The network registry is the user-facing dependency source. Without it, no third-party Mochi package can be installed. The user-facing goal moved: "`mochi pkg add @mochi/strings` reaches the real registry, fetches the index entry, and writes a lockfile pin".

Sparse over git is the architectural decision (research note 02 §4). The "sparse" property is what makes HTTP/2 multiplexing make sense: 30 packages = 30 small concurrent GETs over one TCP connection, no clone, no upfront index download. Cargo's 2023 GA (5-20x improvement over the legacy git index) is the existence proof; Mochi inherits the pattern from day one.

The phase intentionally leaves Sigstore verification to Phase 13 (the blob fetch just verifies BLAKE3 + SHA-256 here). This keeps the Phase 8 surface small enough to test independently.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 8.0 | HTTPS client with HTTP/2; connection pool budget | NOT STARTED | — |
| 8.1 | ETag conditional GET (`If-None-Match`); cache `<bucket>/<scope>/<name>.etag` | NOT STARTED | — |
| 8.2 | Exponential backoff + jitter, 6 attempt cap | NOT STARTED | — |
| 8.3 | JSONL parser for per-package endpoint | NOT STARTED | — |
| 8.4 | Mock registry fixture harness (httptest with scripted responses) | NOT STARTED | — |
| 8.5 | Network resilience: simulated 503 / 429 / partial response retries | NOT STARTED | — |
| 8.6 | `Retry-After` header support | NOT STARTED | — |
| 8.7 | `mochi.toml` `[[registry.alternate]]` failover | NOT STARTED | — |
| 8.8 | `mochi config registry` for default / alternate URL config | NOT STARTED | — |
| 8.9 | Forward-compat unknown-field warnings | NOT STARTED | — |

## Sub-phase 8.0 — HTTPS client

```go
// pkg/pkgregistry/sparse/sparse.go
type SparseRegistry struct {
    Endpoint string         // e.g. "https://index.mochi.dev"
    client   *http.Client   // HTTP/2 enabled
    cache    *Cache
    budget   int            // max parallel streams per host
}

func New(endpoint string, opts ...Option) (*SparseRegistry, error) {
    tr := &http.Transport{
        ForceAttemptHTTP2: true,
        MaxConnsPerHost:   1,    // one TCP conn per host, HTTP/2 multiplexes
        IdleConnTimeout:   90 * time.Second,
        TLSClientConfig: &tls.Config{NextProtos: []string{"h2"}},
    }
    if err := http2.ConfigureTransport(tr); err != nil { return nil, err }
    return &SparseRegistry{
        Endpoint: endpoint,
        client:   &http.Client{Transport: tr, Timeout: 30 * time.Second},
        cache:    NewCache(os.Getenv("MOCHI_INDEX_CACHE")),
        budget:   intOr(os.Getenv("MOCHI_INDEX_PARALLELISM"), 8),
    }, nil
}
```

Parallelism budget: 8 streams per host by default (research note 07 §4.5). Configurable via `MOCHI_INDEX_PARALLELISM`.

## Sub-phase 8.1 — ETag conditional GET

```go
type CacheEntry struct {
    ETag         string
    LastFetched  time.Time
    Body         []byte
}

func (r *SparseRegistry) Versions(pkg string) ([]VersionEntry, error) {
    url := r.endpointFor(pkg)
    cached, _ := r.cache.Load(pkg)
    req, _ := http.NewRequest("GET", url, nil)
    if cached != nil {
        req.Header.Set("If-None-Match", `"`+cached.ETag+`"`)
    }
    resp, err := r.do(req)
    if err != nil { return nil, err }
    defer resp.Body.Close()
    switch resp.StatusCode {
    case 304:
        return parseJSONL(bytes.NewReader(cached.Body))
    case 200:
        body, _ := io.ReadAll(resp.Body)
        r.cache.Store(pkg, &CacheEntry{
            ETag: trimQuotes(resp.Header.Get("ETag")),
            LastFetched: time.Now(),
            Body: body,
        })
        return parseJSONL(bytes.NewReader(body))
    case 404:
        return nil, ErrPkgNotFound
    case 410:
        return nil, fmt.Errorf("%w: package gone (likely legal takedown)", ErrPkgGone)
    default:
        return nil, fmt.Errorf("%w: unexpected status %d", ErrIndexE001, resp.StatusCode)
    }
}
```

Cache layout on disk (canonical root `$MOCHI_HOME` documented in
[phase 0 §conventions](./phase-00-skeleton#files-changed); research note
08 §7):

```
$MOCHI_HOME/index/
  <bucket>/<scope>/<name>          # last 200 body
  <bucket>/<scope>/<name>.etag     # ETag string (trimmed quotes)
  <bucket>/<scope>/<name>.meta     # last-fetched timestamp, status
```

The reader rejects weak ETags (`W/"..."`); only strong ETags are honoured.

## Sub-phase 8.2 — Backoff with jitter

From research note 07 §4.4:

```go
func backoff(attempt int) time.Duration {
    base := 250 * time.Millisecond
    cap  := 30 * time.Second
    raw  := base << (attempt - 1)
    if raw > cap { raw = cap }
    jitter := time.Duration(rand.Int63n(int64(raw / 4)))  // +/- 25% jitter
    if rand.Intn(2) == 0 { return raw + jitter }
    return raw - jitter
}

func (r *SparseRegistry) do(req *http.Request) (*http.Response, error) {
    var lastErr error
    for attempt := 1; attempt <= 6; attempt++ {
        resp, err := r.client.Do(req)
        if err == nil && resp.StatusCode != 503 && resp.StatusCode != 429 {
            return resp, nil
        }
        lastErr = err
        if resp != nil && resp.Header.Get("Retry-After") != "" {
            time.Sleep(parseRetryAfter(resp.Header.Get("Retry-After")))
            continue
        }
        time.Sleep(backoff(attempt))
    }
    return nil, fmt.Errorf("%w: after 6 attempts: %v", ErrIndexE001, lastErr)
}
```

The 6-attempt cap is policy: at attempt 6 the total elapsed time approaches `250ms + 500ms + ... + 8s = ~16s` plus jitter, which is the budget MEP-57 allocates to a single fetch.

## Sub-phase 8.3 — JSONL parser

```go
func parseJSONL(r io.Reader) ([]VersionEntry, error) {
    var entries []VersionEntry
    scanner := bufio.NewScanner(r)
    scanner.Buffer(make([]byte, 0, 64*1024), 1024*1024)  // 1MB line limit
    line := 0
    for scanner.Scan() {
        line++
        var raw rawEntry
        if err := json.Unmarshal(scanner.Bytes(), &raw); err != nil {
            return nil, fmt.Errorf("%w: line %d: %v", ErrIndexE002, line, err)
        }
        entries = append(entries, raw.toEntry())
    }
    return entries, scanner.Err()
}

type rawEntry struct {
    V  string            `json:"v"`
    R  time.Time         `json:"r"`
    B3 string            `json:"b3"`
    S2 string            `json:"s2"`
    Y  bool              `json:"y,omitempty"`
    YR string            `json:"yr,omitempty"`
    C  []string          `json:"c,omitempty"`
    D  map[string]string `json:"d,omitempty"`
    T  []string          `json:"t,omitempty"`
    MP string            `json:"mp,omitempty"`
    ED string            `json:"ed,omitempty"`
    LK string            `json:"lk,omitempty"`
    PR struct{ Sig string `json:"sig"` } `json:"pr,omitempty"`
}
```

Field abbreviations match research note 07 §3. The parser is tolerant of unknown fields (forward-compat per §14, error code E003 reserved but not raised).

## Sub-phase 8.4 — Mock fixture harness

```go
// pkg/pkgregistry/sparse/testserver/server.go
type Mock struct {
    *httptest.Server
    Script map[string][]Response  // pkg -> sequence of responses
    Calls  []Call
}

type Response struct {
    Status   int
    Body     []byte
    Headers  map[string]string
    Delay    time.Duration
}

func NewMock(t *testing.T) *Mock {
    m := &Mock{Script: map[string][]Response{}}
    m.Server = httptest.NewTLSServer(http.HandlerFunc(m.handle))
    return m
}

func (m *Mock) Script200(pkg, body string)            { /* append */ }
func (m *Mock) Script503(pkg string, retryAfter string){ /* append */ }
func (m *Mock) Script429(pkg string, retryAfter string){ /* append */ }
func (m *Mock) Script304(pkg, etag string)            { /* append */ }
```

Each test loads scripted responses and drives the sparse client; the harness asserts the sequence of `Calls` matches expectations.

## Sub-phase 8.5 — Resilience cases

Fixture `tests/pkgsystem/sparse-index/resilience/`:

| Case | Script | Expected client behaviour |
|------|--------|---------------------------|
| `503-then-200` | 503, 200 | Succeeds on second attempt |
| `503-six-times` | 503 x6 | Fails with `M057_INDEX_E004` (fetch fail) |
| `429-retry-after` | 429 with `Retry-After: 1`, 200 | Waits 1s, succeeds |
| `partial-response` | 200 with truncated body | Fails with `M057_INDEX_E002` (parse), retried |
| `etag-mismatch` | 200 with stale ETag echo | Fails with `M057_INDEX_E007` if expected ETag was set |
| `flaky-network` | RST connection, then 200 | Retries via `net.Error.Temporary` check |

## Sub-phase 8.6 — `Retry-After` header

```go
func parseRetryAfter(h string) time.Duration {
    if secs, err := strconv.Atoi(h); err == nil {
        return time.Duration(secs) * time.Second
    }
    if t, err := time.Parse(http.TimeFormat, h); err == nil {
        return time.Until(t)
    }
    return 0
}
```

Both forms (seconds-int and HTTP-date) per RFC 7231.

## Sub-phase 8.7 — Registry failover

```go
type FailoverRegistry struct {
    Primary    Registry
    Alternates []Registry
}

func (r *FailoverRegistry) Versions(pkg string) ([]VersionEntry, error) {
    if entries, err := r.Primary.Versions(pkg); err == nil {
        return entries, nil
    } else if !isRetryable(err) {
        return nil, err
    }
    for _, alt := range r.Alternates {
        if entries, err := alt.Versions(pkg); err == nil {
            return entries, nil
        }
    }
    return nil, ErrAllRegistriesFailed
}
```

The order is: primary first, alternates in declared order. A `404` is not retryable (the package is genuinely absent); a `503/429` exhausting the retry budget is.

## Sub-phase 8.8 — `mochi config registry`

```
mochi config registry default URL                       # set default
mochi config registry alternate add NAME URL [--token=T] # add alternate
mochi config registry alternate remove NAME             # remove alternate
mochi config registry list                              # show all
```

Configuration lives at `$MOCHI_HOME/config/registries.toml` (canonical
layout: [phase 0 §conventions](./phase-00-skeleton#files-changed)):

```toml
[default]
url = "https://index.mochi.dev"

[[alternate]]
name = "corp"
url = "https://mirror.corp.example/mochi-registry"
token = "..."  # optional, for private mirrors
```

The CLI rewrites this file canonically.

## Sub-phase 8.9 — Forward-compat warnings

A new field in an index entry that the v1 client does not recognise is *accepted* with a warning logged (research note 07 §14). The warning is emitted via `Manifest.Warnings` and surfaced under `mochi pkg lock --verbose`:

```
warning: index entry @mochi/strings@0.5.0 has unknown field "futurefield"
  (this is forward-compat; resolution proceeds with the known fields)
```

This prevents the registry from being frozen by every new field requiring a client release.

## Files changed

| File | Purpose | Owner |
|------|---------|-------|
| `pkg/pkgregistry/sparse/sparse.go` | `SparseRegistry` | Owner |
| `pkg/pkgregistry/sparse/cache.go` | ETag + body cache | Owner |
| `pkg/pkgregistry/sparse/jsonl.go` | JSONL parser | Owner |
| `pkg/pkgregistry/sparse/backoff.go` | Backoff + jitter | Owner |
| `pkg/pkgregistry/sparse/failover.go` | `FailoverRegistry` (reused by Phase 11 mirror chain) | Owner |
| `pkg/pkgregistry/sparse/testserver/server.go` | Mock harness | Owner |
| `cmd/mochi/config.go` | `mochi pkg config registry ...` handler | Owner |
| `tests/pkgsystem/sparse-index/normal/*` | Happy path | Owner |
| `tests/pkgsystem/sparse-index/resilience/*` | 503/429/partial | Owner |
| `tests/pkgsystem/sparse-index/etag/*` | Cache hit/miss sequences | Owner |
| `tests/pkgsystem/sparse-index/failover/*` | Alternate registry | Owner |

## Error code surface

Phase 8 owns `M057_INDEX_E002` (sparse JSONL parse), `M057_INDEX_E003`
(forward-compat warning), `M057_INDEX_E004` (fetch failed after retries),
`M057_INDEX_E005` (rate-limit not respected), `M057_INDEX_E007` (failover
exhausted). `M057_INDEX_E006` (mirror divergence) is owned by Phase 11.
See the canonical [error registry](./errors).

## Test set

- `TestPhase8Versions` — happy path.
- `TestPhase8ETag` — conditional fetch.
- `TestPhase8Backoff` — backoff calculation.
- `TestPhase8Resilience` — scripted 503/429/partial.
- `TestPhase8RetryAfter` — both seconds and HTTP-date forms.
- `TestPhase8Failover` — alternates.
- `TestPhase8SolverParity` — Phase 5 fixtures via HTTPS.

## Open questions

- Whether to support `If-Modified-Since` as a secondary cache key; current plan: ETag is sufficient.
- Whether to negotiate `application/vnd.mochi.index+jsonl;v=1` content negotiation; deferred.
- Whether to expose the cache directory via `mochi pkg config index-cache <path>`; current plan: yes, defaults to `$MOCHI_HOME/index`.

## Cross-references

- Sparse index protocol: [research note 07](/docs/research/0057/registry-index).
- Rationale: [research note 02 §4](/docs/research/0057/design-philosophy).
- Solver consuming the registry interface: [research note 05 §8](/docs/research/0057/solver-design).
- Cargo's GA migration as prior art: [research note 03 §1](/docs/research/0057/prior-art-registries).
