---
title: "Phase 1. Simple-index client"
sidebar_position: 3
sidebar_label: "Phase 1. Simple-index"
description: "MEP-71 Phase 1 lands the PEP 503 / 691 / 700 / 658 / 592 simple-index client, PEP 440 version + specifier algebra, and a streaming sha256+blake3 verifier."
---

# Phase 1. Simple-index client

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-71 §Phases](/docs/mep/mep-0071#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 22:42 (GMT+7) |
| Landed         | 2026-05-29 22:57 (GMT+7) |
| Tracking issue | (filled by automation) |
| Tracking PR    | (filled by automation) |
| Commit         | (filled by automation) |

## Gate

`TestPhase1SimpleIndex` in `package3/python/simple/phase01_test.go` with subtests:

- `end_to_end_html`. Spins up an `httptest.NewServer` returning a PEP 503 HTML index, fetches the project via `HTTPClient.FetchProject(ctx, name, FormatHTML)`, fetches the advertised file via `FetchFile`, and streams the body through `Verify` against the parsed hashes. Asserts the verifier reports no mismatch.
- `end_to_end_json`. Same shape using a PEP 691 JSON response (`application/vnd.pypi.simple.v1+json`). Asserts the parsed `Meta.APIVersion` round-trips and the streamed file body verifies.
- `normalise_invariants`. PEP 503 §"Normalised names" properties: idempotence of `Normalise`, output is lowercase, output contains no `_` or `.`.
- `verify_md5_rejected`. Even when the index advertises only md5, `Verify` must refuse rather than silently accept. Matches PEP 503's de facto guidance and the bridge's "no md5 ever" rule from MEP-71 §3.

The package-level test suite covers:

- `index_test.go`: PEP 503 normalisation across 12 inputs (lowercase, `.`/`_`/`-` collapsing, multi-run collapsing), `Validate` happy / sad paths, `FilesByFilename` map.
- `html_test.go`: minimal HTML index, relative-URL resolution against the base, PEP 592 `data-yanked` (with reason / empty reason / absent), PEP 503 `data-requires-python` with HTML-escaped value, PEP 658 `data-core-metadata` and the legacy `data-dist-info-metadata` alias, malformed fragment rejection (`#sha256` no `=`), empty digest rejection (`#sha256=`), multi-hash fragment (`#sha256=abc&blake3=def`), anchor without href skipped, uppercase hash key lowercased, basename extraction from a deeper URL path.
- `json_test.go`: PEP 691 minimal envelope, PEP 592 `yanked` as bool or string, PEP 691 `core-metadata` as bool or `{"sha256":"..."}` object, relative URL resolution, forward-compatible unknown-field tolerance at envelope and file level, missing name / filename / url errors, truncated JSON error, hash-key lowercasing, missing meta object yields empty `APIVersion`, numeric `yanked` rejected.
- `verify_test.go`: known sha256 + blake3 vectors, sha256 mismatch error, blake3 mismatch error, both-present-both-match success, one-of-two mismatch error, empty expected rejected, md5 rejected with "no supported hash algorithm" error, uppercase hex digest normalised before comparison, `HashAll` returns both algorithms over the same stream, empty-input edge case.
- `client_test.go`: `httptest.NewServer` fixtures cover HTML fetch, JSON fetch (asserts Accept header advertises `application/vnd.pypi.simple.v1+json`), JSON-hint negotiates down to HTML, HTTP error surfaces the status code, empty `BaseURL` rejected, name is PEP 503 normalised before insertion into the URL path (`Flask_SQLAlchemy` -> `/simple/flask-sqlalchemy/`), `FetchFile` streams, `FetchFile` surfaces HTTP errors, `ensureTrailingSlash` table, `IndexFormat.String` table, `NewHTTPClient` defaults.

The `semver/` package ships independently and is tested at:

- `version_test.go`: PEP 440 grammar acceptance over 12 inputs (epoch, release, pre `a`/`b`/`rc`, post, dev, local), rejection of 14 bad inputs (leading sign, embedded space, missing release, invalid pre kind, etc.), `String` round-trip, the canonical PEP 440 ascending chain `1.0.dev0 < 1.0a1.dev0 < 1.0a1 < 1.0a1.post1 < 1.0b1 < 1.0rc1 < 1.0 < 1.0.post1 < 1.0.post1.dev0 < 1.0.post2 < 1.1.dev0 < 1.1 < 1!1.0 < 2!1.0`, release-length zero-padding (`1.0` == `1.0.0`), local-version ordering, PEP 440 §"Local version" numeric-vs-alpha rule (`1.0+1` < `1.0+a`), `IsPreRelease` discrimination, `SortByCompare` stability.
- `specifier_test.go`: clause parser over 8 operators (`==`, `===`, `!=`, `<=`, `>=`, `<`, `>`, `~=`), order-sensitive `===`-before-`==` rule, rejection of unknown ops, multi-clause AND combination, `~=` "compatible release" semantics with PEP 440 right-zero-pad.

## Lowering decisions

The simple-index client is split into four self-contained sub-packages so phase 2 (uv resolver bridge) can mock them independently:

- `package3/python/semver/`. PEP 440 version + specifier algebra. No PyPI dependency; uses only `strconv` and `strings`. The version struct is `(Epoch int, Release []int, PreKind, PreNum, Post, Dev, Local)`. The comparator uses asymmetric infinity keys for pre / post / dev: an absent pre key sorts after any present pre key, but an absent pre key on a dev-only release (no post, no pre, dev present) sorts as -infinity to match `packaging.py._cmpkey`. This is the only non-obvious bit and it is the one that the canonical ascending-chain test pins down.
- `package3/python/simple/`. Index parser (HTML + JSON), HTTP client, stream verifier.

The HTML parser uses `golang.org/x/net/html` which is already a transitive Mochi dependency. The JSON parser uses `encoding/json` with `json.RawMessage` for the `yanked` and `core-metadata` fields so it can decide between bool / string / object after the initial decode. The decoder is deliberately permissive about unknown fields because PEP 691 §"Future extensions" reserves the right to add new fields and the bridge must not start refusing the index when PyPI adds, say, `data-attestation` in 2027.

The verifier picks all supported algorithms present in the `expected` map and streams the body through an `io.MultiWriter` of `hash.Hash` instances. SupportedHashAlgos is `{"blake3", "sha256"}` in that priority order. MD5 is rejected even when advertised: pip already refuses to install on MD5-only entries and the bridge follows suit.

The HTTP client is intentionally bare. `FetchProject` sets `Accept: application/vnd.pypi.simple.v1+json, application/vnd.pypi.simple.v1+html;q=0.5, text/html;q=0.1` when the caller hints `FormatJSON`, then dispatches to ParseHTML or ParseJSON based on the response Content-Type (substring match on `json`). This is the negotiation pattern `pip --index-url` uses. `FetchFile` is a transparent GET that returns the body stream so the bridge can `io.TeeReader` it into the cache while `Verify` drains it.

The client URL builder PEP 503-normalises the project name before constructing the request path. PyPI returns 301 redirects when handed an unnormalised name, but other simple indexes (devpi, gemfury, local mirror with a static-file serve) do not, so client-side normalisation is the safe default.

## Files changed

| File | Purpose |
|------|---------|
| `package3/python/semver/doc.go` | Package doc |
| `package3/python/semver/version.go` | `Version`, `Parse`, `String`, `Compare`, `IsPreRelease`, `SortByCompare` |
| `package3/python/semver/specifier.go` | `Operator`, `Clause`, `Specifier`, `ParseClause`, `ParseSpecifier`, `Match`, `Compatible` |
| `package3/python/semver/version_test.go` | PEP 440 parse + compare + sort tests |
| `package3/python/semver/specifier_test.go` | PEP 440 specifier tests including `~=` semantics |
| `package3/python/simple/doc.go` | Package doc |
| `package3/python/simple/index.go` | `Project`, `File`, `Meta`, `Normalise`, `Validate`, `FilesByFilename` |
| `package3/python/simple/html.go` | `ParseHTML` (PEP 503 + 592 + 658 + 700 data-attrs) |
| `package3/python/simple/json.go` | `ParseJSON` (PEP 691 envelope with permissive future fields) |
| `package3/python/simple/verify.go` | `Verify`, `HashAll`, `SupportedHashAlgos` |
| `package3/python/simple/client.go` | `Client` interface, `HTTPClient`, `IndexFormat`, `NewHTTPClient`, `FetchProject`, `FetchFile` |
| `package3/python/simple/index_test.go` | Index struct + Normalise + Validate tests |
| `package3/python/simple/html_test.go` | PEP 503 / 592 / 658 / 700 HTML fixtures |
| `package3/python/simple/json_test.go` | PEP 691 JSON fixtures |
| `package3/python/simple/verify_test.go` | sha256 + blake3 vectors |
| `package3/python/simple/client_test.go` | `httptest`-driven fetcher + negotiator tests |
| `package3/python/simple/phase01_test.go` | `TestPhase1SimpleIndex` sentinel with 4 subtests |

## Test set

- `TestPhase1SimpleIndex/end_to_end_html`
- `TestPhase1SimpleIndex/end_to_end_json`
- `TestPhase1SimpleIndex/normalise_invariants`
- `TestPhase1SimpleIndex/verify_md5_rejected`
- All `package3/python/semver/...` and `package3/python/simple/...` unit tests.

## Closeout notes

Phase 1 ships the entire "read the simple-index" surface in a single PR because the four files (parser HTML, parser JSON, client, verifier) are mutually self-referential through the `Project` / `File` struct and splitting them would have meant landing dead code in three of the four PRs. The total Go surface is ~700 lines plus ~600 lines of tests.

PEP 440 was implemented from scratch rather than wrapping the `version.Version` portions of `golang.org/x/mod/semver` because PEP 440 has six version segment classes (epoch, release, pre, post, dev, local) and `x/mod/semver` only models four. There is also no maintained pure-Go port of `packaging.py` we could vendor at the time of writing. The asymmetric-infinity rule for the pre key when dev-only is the only subtle bit; the canonical ascending-chain test (`1.0.dev0 < 1.0a1.dev0 < ...`) pins it down.

The verifier streams once. Callers that need the verified bytes wrap their reader in an `io.TeeReader` to capture them; the verifier drains the original stream and emits all bookkeeping on the captured digest after `io.Copy` finishes. This is how phase 8 (build orchestration) will tee the downloaded wheel into the content-addressed cache while verifying.

No CPython runtime is required for any test in this phase. The `httptest.NewServer` fixtures cover the index-and-fetch surface end-to-end without touching the network. CI cost is the four extra Go tests at sub-second total.
