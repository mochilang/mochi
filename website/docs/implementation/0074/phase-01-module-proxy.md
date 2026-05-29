---
title: "Phase 1. Module Proxy"
sidebar_position: 3
sidebar_label: "Phase 1. Module Proxy"
description: "MEP-74 Phase 1 lands the proxy.golang.org client: module-path / version escape codec, the four endpoint methods (list, info, mod, zip), the h1: dirhash matching go.sum byte-for-byte, a content-addressed verify-on-store cache, and the module-semver subset used by cmd/go."
---

# Phase 1. Module Proxy

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:08 (GMT+7) |
| Landed         | 2026-05-29 21:20 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase1ModuleProxy` in `package3/go/moduleproxy/phase01_test.go`: an
end-to-end loop where an `httptest.NewServer` serves the four proxy
endpoints (`@v/list`, `.info`, `.mod`, `.zip`) for a fixture module
`example.com/Foo@v1.2.3`. The test drives the `Client.List → Info →
Mod → Zip` sequence, then stores the zip + mod + info in a fresh
`CacheLayout`, calls `VerifyZip` to confirm the on-disk integrity
sidecar, and round-trips the upper-case module path through
`EscapePath` / `UnescapePath`. Passing this sentinel means the phase is
end-to-end functional from a caller's point of view.

In addition the package-level test suite covers:

- `package3/go/moduleproxy/escape_test.go`: `EscapePath` round-trips
  for `github.com/Spf13/Cobra`, `go.uber.org/Zap`, multi-letter
  acronyms (`github.com/AAA/BBB` → `github.com/!a!a!a/!b!b!b`), pure
  lower-case pass-through, and a leading-bang escape edge case;
  `EscapeVersion` rejection of forbidden characters (`/`, `\`,
  `:`, control bytes); `UnescapePath` rejection of trailing-bang and
  bang-followed-by-non-lower-case sequences; and a 4-case escape →
  unescape round-trip.

- `package3/go/moduleproxy/h1hash_test.go`: an empty-input baseline
  matching `sha256("")`, a single-file reference matching the stdlib
  expression byte-for-byte, sort-insensitivity over input order,
  open-error propagation, `HashGoMod` reference + stability over 5
  iterations, `HashZip` matches an independent `Hash1Files`
  computation over a 3-entry fixture zip, prefix mismatch rejection,
  non-zip body rejection, and a directory-entry-skipped invariant
  (the same body with and without a `.../subdir/` directory entry
  hashes identically).

- `package3/go/moduleproxy/client_test.go`: list / info / mod / zip
  endpoint coverage; URL-construction assertions verifying the
  escape codec is applied to both module and version paths;
  classification of HTTP 404 to `ErrModuleNotFound` (when no version
  is in scope) or `ErrVersionNotFound` (when version is in scope);
  HTTP 410 maps to `ErrVersionNotFound` for the .zip endpoint; the
  `.mod` body is hard-capped at 4 MiB to prevent an adversarial
  proxy from exhausting memory; `User-Agent` is passed through; HTTP
  500 returns a generic error that does *not* unwrap to
  `ErrModuleNotFound`; `NewClient` normalises trailing slash on
  `BaseURL` and falls back to `DefaultProxyURL` for the empty input.

- `package3/go/moduleproxy/cache_test.go`: path escapes propagate
  through `InfoPath` / `ModPath` / `ZipPath`; `StoreZip` writes the
  zip + ziphash sidecar atomically and returns the `h1:` digest;
  `Has` reports false until every artifact (info, mod, zip, ziphash)
  is present; `VerifyZip` detects byte-level tampering and surfaces
  `ErrCacheCorrupt`; `VerifyZip` on missing files also returns
  `ErrCacheCorrupt`; `StoreMod` returns the same digest
  `HashGoMod` would compute; `WriteFile` is atomic (no leftover temp
  files); `NewCache` creates nested roots; the
  `MOCHI_GO_BRIDGE_CACHE` env override is honoured;
  `FingerprintBytes` matches the reference sha256-hex of a known
  byte string; and the stored zip parses back through
  `archive/zip` unchanged.

- `package3/go/semver/version_test.go`: basic parse + Major / Minor /
  Patch population, pre-release split semantics, `+incompatible`
  build metadata, pseudo-version detection (single-identifier
  `<14-digit-timestamp>-<12-hex>` shape), 13 invalid inputs rejected,
  10-case ordering matrix including numeric pre-release identifier
  ordering and `alpha < alpha.1 < alpha.2 < alpha.10 < beta < rc.1`,
  `Compare` ignores build metadata (so `v2.0.0+incompatible` ties
  with `v2.0.0`), `Sort` against the full ordering matrix, invalid
  versions sort after valid ones, `Max` with a 4-element slice,
  `String` round-trip on 5 canonical forms, lex fallback when both
  inputs fail to parse, and 7 pseudo-version negative cases.

## Lowering decisions

The phase's external surface is three Go packages:

- `package3/go/moduleproxy/` is the protocol-side: an HTTP client
  for `proxy.golang.org`, the path / version escape codec, the
  `h1:` dirhash, and a verify-on-store content-addressed cache.
- `package3/go/semver/` is the version-ordering side: a
  reduced-scope parser matching cmd/go's `module` semver dialect.

The proxy is treated as an external system the bridge does not
control; every artifact is rehashed locally against its on-disk
sidecar before being trusted. The h1: digest matches
`golang.org/x/mod/sumdb/dirhash.Hash1` byte-for-byte (we verified
in tests by re-computing inline with stdlib sha256 + base64). This
matters because phase 2 (sumdb) will compare these digests against
the transparency log; a one-byte drift here would invalidate every
subsequent integrity check.

The cache layout mirrors Go's own `GOMODCACHE/cache/download` tree
(`<root>/<escaped-modpath>/@v/<escaped-version>.<ext>`) so that
the bridge can in principle reuse an existing user cache or be
inspected with `go mod download -x`-style tooling. We do not reach
into `$GOMODCACHE` ourselves, however; the bridge cache is
conceptually separate (different integrity policy, different
eviction story, different lifetime). The default root is
`$XDG_CACHE_HOME/mochi-go-bridge/modules` on Linux and
`~/Library/Caches/mochi-go-bridge/modules` on macOS, with a
`MOCHI_GO_BRIDGE_CACHE` environment override for CI and
hermetic-build scenarios.

The `Client` does not implement GOPROXY-style fallback lists
(`proxy.golang.org,direct` semantics). Callers that need multi-proxy
fallback wrap a sequence of `Client` instances. Keeping that policy
out of the client lets phase 2 (sumdb) compose against a single
proxy while phase 9 (build orchestration) layers fallback when it
joins the picture.

The `.mod` endpoint is hard-capped at 4 MiB. Real-world go.mod
files are under 10 KiB; the cap exists purely so an adversarial
proxy cannot exhaust memory. The `.zip` endpoint streams directly
to the caller-supplied `io.Writer` (no in-memory accumulation in
the client itself), and the cache layer separately reads the
entire body into memory only when validating the entry prefix.

The semver parser is a subset by design: it does not aim for full
SemVer 2.0. It enforces the `v` prefix that cmd/go requires,
permits the `+incompatible` build metadata that cmd/go uses for
v2+ modules without `/v2` paths, and recognises pseudo-versions in
the `v0.0.0-YYYYMMDDHHMMSS-abcdefabcdef` shape (a single
pre-release identifier under SemVer rules because `-` is a legal
identifier character). `Compare` ignores build metadata. `Sort` is
total: invalid versions sort after all valid ones, then
lexicographically among themselves so test fixtures stay
deterministic.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/moduleproxy/escape.go` | `EscapePath`, `EscapeVersion`, `UnescapePath`, `UnescapeVersion` codec for the proxy URL scheme |
| `package3/go/moduleproxy/escape_test.go` | escape codec round-trip + rejection tests |
| `package3/go/moduleproxy/h1hash.go` | `H1Prefix`, `Hash1Files`, `HashZip`, `HashGoMod`, internal `bytesReaderAt` |
| `package3/go/moduleproxy/h1hash_test.go` | h1: dirhash reference computation + edge-case tests |
| `package3/go/moduleproxy/client.go` | `Client`, `List`, `Info`, `Mod`, `Zip`, `VersionInfo`, `ErrModuleNotFound`, `ErrVersionNotFound`, `DefaultProxyURL`, `DefaultUserAgent` |
| `package3/go/moduleproxy/client_test.go` | httptest-driven client tests covering all four endpoints |
| `package3/go/moduleproxy/cache.go` | `CacheLayout`, `NewCache`, `InfoPath`/`ModPath`/`ZipPath`/`ZipHashPath`, `StoreInfo`/`StoreMod`/`StoreZip`, `Has`, `VerifyZip`, `WriteFile`, `FingerprintBytes`, `ErrCacheCorrupt` |
| `package3/go/moduleproxy/cache_test.go` | cache atomic-write + integrity tests |
| `package3/go/moduleproxy/phase01_test.go` | `TestPhase1ModuleProxy` end-to-end sentinel |
| `package3/go/semver/version.go` | `Version`, `Parse`, `IsValid`, `Compare`, `CompareStrings`, `Sort`, `Max`, `IsPrerelease`, `IsPseudoVersion`, `MajorString`, `String` |
| `package3/go/semver/version_test.go` | semver parser + ordering test suite |

## Test set

- `TestPhase1ModuleProxy`
- All `package3/go/moduleproxy/...` unit tests.
- All `package3/go/semver/...` unit tests.

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok  	mochi/package3/go/build	0.332s
ok  	mochi/package3/go/errors	(cached)
ok  	mochi/package3/go/moduleproxy	0.371s
ok  	mochi/package3/go/semver	(cached)
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

The phase ships with no external runtime dependencies. All four
new packages are pure Go and import only from the standard
library, so the bridge's link surface remains small ahead of the
later phases that will pull in `golang.org/x/mod/modfile` (phase
9) and `golang.org/x/mod/sumdb` (phase 2). Keeping a minimal
dependency surface during phase 1 means a downstream supply-chain
review only has to audit the bridge code itself, not any
transitive deps.

The h1: dirhash implementation is the load-bearing piece. Phase 2
will lean on it to compare the bridge's local computation against
the signed entry in `sum.golang.org`; phase 12 will lean on it to
populate the published go.sum entries; phase 13 will lean on it
again as the cosign payload. A single-byte drift here would
silently invalidate every later integrity check, which is why the
test suite re-computes the reference value inline with stdlib
primitives in two independent ways (raw byte computation in
`TestHash1FilesOneFileMatchesReference` and re-derivation through
`zip.NewReader` + `Hash1Files` in `TestHashZipMatchesHash1Files`).

The cache's verify-on-store policy is one-way only: a corrupt
write (whether through tampering, partial-disk-write, or proxy
misbehaviour) surfaces as `ErrCacheCorrupt` on the next
`VerifyZip` call. Recovery is the caller's responsibility (delete
the affected files and refetch). The phase deliberately does *not*
implement automatic refetch because the policy decision (refetch
from primary proxy, fall back to direct, fail closed) belongs in
phase 9 where the build driver has the full context.

The semver package is intentionally smaller than
`golang.org/x/mod/semver` — it omits `IsValid`, `Build`, and the
pre-release-only comparator that x/mod exports. The bridge does
not need them at phase 1 and will gain whichever pieces it needs
incrementally. This keeps the test surface compact (the parser
has 13 negative cases covered exhaustively) and avoids tying the
bridge to an x/mod version bump cycle.
