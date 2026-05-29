---
title: "Phase 10. mochi.lock integration"
sidebar_position: 12
sidebar_label: "Phase 10. mochi.lock"
description: "MEP-74 Phase 10 lands the `[[go-package]]` lockfile schema, encoder, decoder, and drift checker as a self-contained `package3/go/lockfile/` module. The schema mirrors spec §3 (module, version, source, zip-blake3, zip-h1, sumdb-verified + tree-size + record-hash, api-surface-sha256, wrapper-sha256, capabilities-declared, dependencies, build-tags). Encode is byte-deterministic across permutations; Decode is line-oriented with forward-compat tolerance for unknown keys; CheckDrift returns the closed set of mismatch kinds (missing / stale / version / zip-hash / sumdb / api-surface / wrapper / capabilities) so `mochi pkg lock --check` can surface every drift class without ambiguity."
---

# Phase 10. mochi.lock integration

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 23:47 (GMT+7) |
| Landed         | 2026-05-29 23:55 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase10LockfileSentinel` in
`package3/go/lockfile/phase10_test.go` walks the cobra + pflag
fixture through the full encode -> decode -> drift-check cycle
and verifies:

- the encoded body covers every field MEP-74 spec §3 requires
  (module, version, source inline-table, zip-blake3, zip-h1,
  sumdb-verified, sumdb-tree-size, sumdb-record-hash,
  api-surface-sha256, wrapper-sha256, capabilities-declared,
  dependencies);
- the encoded body is byte-deterministic across 16 runs *and* under
  slice permutation;
- DecodeString round-trips the structure (including the integer
  `sumdb-tree-size` and the string-array fields);
- `CheckDrift(pkgs, pkgs)` returns no drifts;
- mutating each tracked field surfaces the matching drift kind
  (`zip-hash`, `sumdb`, `api-surface`, `wrapper`, `capabilities`);
- dropping a live package surfaces `stale`, dropping a locked
  package surfaces `missing`.

Plus 30 unit tests in `lockfile_test.go`:

- encode coverage / sort behaviour
  (`TestEncodeBasic`, `TestEncodeDeterministicSort`,
  `TestEncodeOmitsEmptyOptionals`, `TestEncodeEmptyReturnsEmpty`),
- per-Source-kind rendering
  (`TestSourceGitRendered`, `TestSourcePathRendered`),
- round-trip
  (`TestRoundTrip`),
- decode error paths
  (`TestDecodeRejectsMissingEq`, `TestDecodeRejectsBadSource`,
  `TestDecodeRejectsBadBool`, `TestDecodeRejectsBadInt`,
  `TestDecodeRejectsSourceMissingKind`),
- forward-compat tolerance
  (`TestDecodeToleratesUnknownKey`),
- empty arrays + helper coverage
  (`TestDecodeEmptyArraysOk`, `TestParseIntHandlesNegative`,
  `TestSplitTopLevelRespectsNesting`),
- every drift kind
  (`TestCheckDriftDetectsMissing`, `TestCheckDriftDetectsStale`,
  `TestCheckDriftDetectsVersion`, `TestCheckDriftDetectsZipDrift`,
  `TestCheckDriftDetectsApiSurface`,
  `TestCheckDriftDetectsWrapper`, `TestCheckDriftDetectsSumdb`,
  `TestCheckDriftDetectsCapabilities`),
- multi-set invariance, order-stability, and false-positive
  guards (`TestCheckDriftCapabilitiesOrderInsensitive`,
  `TestCheckDriftNoFalsePositives`,
  `TestCheckDriftDeterministicOrder`),
- diagnostic rendering (`TestDriftStringNoDetail`,
  `TestDriftStringWithDetail`).

## Lowering decisions

The lockfile package is layering-conservative: it imports no other
`package3/go/*` module and depends only on the Go stdlib
(`fmt`, `io`, `sort`, `strings`). Callers in the build pipeline
compose a `[]GoPackage` from their own state, hand it to `Encode`,
and read back the same shape via `Decode` / `DecodeString`. The
package is the single source of truth for the `[[go-package]]`
serialisation, so future MEP-57 lockfile-merger work can compose
`Encode(pkgs)` with the `[[rust-package]]` body that
`package3/rust/lockfile` produces without either side knowing about
the other.

**Source-kind enum mirrors the MEP-73 lockfile package.** Three
kinds (`module-proxy`, `git`, `path`) cover the spec §3 source
table. The `git` kind carries `url` + optional `rev`; the
`module-proxy` kind carries the proxy URL; the `path` kind carries
the relative directory. Inline-table rendering matches the spec
example byte-for-byte:

```toml
source = { kind = "module-proxy", proxy = "https://proxy.golang.org" }
source = { kind = "git", url = "git@corp.example.com:internal/foo.git", rev = "abcdef0" }
source = { kind = "path", path = "../fork/local" }
```

**Encode sorts by (module, version) for byte-stable output.** The
caller's slice order does not affect the rendered TOML; permuting
the input produces identical bytes. This is the same invariant the
MEP-73 lockfile holds; it makes `git diff mochi.lock` show only
the genuine changes rather than slice-permutation noise.

**Decode is line-oriented and tolerates unknown keys.** Unknown
keys are silently dropped (forward-compat: the bridge can add a
field in a future phase without breaking older readers); missing
required keys do *not* error (the spec allows partial entries
during in-progress lock work); structural errors (no `=`, bad
`source` inline-table, non-bool `sumdb-verified`, non-int
`sumdb-tree-size`) *do* error so a hand-edited mochi.lock with a
typo surfaces immediately rather than silently dropping the entry.

**Drift kinds are a closed set, sorted by (module, version, kind).**
Eight kinds total: `missing`, `stale`, `version`, `zip-hash`,
`sumdb`, `api-surface`, `wrapper`, `capabilities`. Each kind maps
to one MEP-74 spec §3 field cluster: `zip-hash` covers the
zip-blake3 + zip-h1 pair (a divergence in either flips the kind);
`sumdb` covers verified + tree-size + record-hash (any change is
suspicious); `capabilities` is multi-set-insensitive so a `["fs",
"net"]` -> `["net", "fs"]` reorder is not a drift.

**`sumdb-verified` always renders, even when `false`.** This is
load-bearing: the field is a security claim about a public module.
A `mochi.lock` that omits `sumdb-verified` could be silently down-
graded by a hand-edit; the spec requires the field be explicit. The
encoder unconditionally emits `sumdb-verified = true|false`.

**Empty optional fields are omitted, not rendered as empty.** The
spec allows a `[[go-package]]` entry to omit `dependencies`,
`build-tags`, `capabilities-declared` when none apply. Rendering
`dependencies = []` would be technically equivalent but bloats the
file and adds line-noise to `git diff`. Decode treats an explicit
`= []` as a `nil` slice (`TestDecodeEmptyArraysOk`).

**`CheckDrift` is the one place that compares lockfile state to
live state.** Phase 10 deliberately does not implement the *fetch*
side (that lives in the existing phases 1-2 module-proxy / sumdb
clients) or the *wrapper-hash* side (that lives in phase 6's
emitter). The caller stitches the pieces together: re-fetch each
module, recompute the hashes, build a fresh `[]GoPackage`, and pass
both slices to `CheckDrift`. The check is symmetric: missing
entries from the lockfile surface as `DriftMissing`; stale entries
not claimed by any live module surface as `DriftStale`.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/lockfile/lockfile.go` | `GoPackage`, `Source`, `Encode`, `Decode`, `DecodeString`, `Drift`, `DriftKind`, `CheckDrift`. |
| `package3/go/lockfile/lockfile_test.go` | 30 unit tests over encode / decode / drift behaviour. |
| `package3/go/lockfile/phase10_test.go` | `TestPhase10LockfileSentinel` end-to-end cobra+pflag fixture. |
| `website/docs/implementation/0074/phase-10-lockfile.md` | (this page) |

## Test set

- `TestPhase10LockfileSentinel` (6 sub-tests)
- 30 unit tests in `lockfile_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/lockfile/...
ok      mochi/package3/go/lockfile      0.5s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 10 lands the lockfile schema as a leaf module; the CLI
wiring (`mochi pkg lock` / `mochi pkg lock --check` extending
MEP-57 to walk `[go-dependencies]`) is reserved for phase 10.1
once the MEP-57 driver gains the language-tag dispatch hook
(today MEP-57 walks `[[rust-package]]` only; the same dispatch
pattern extends to `[[go-package]]`). The schema is fixed; the
CLI work will not require changes to this package beyond a small
adapter that translates manifest entries into a fresh `[]GoPackage`
for `CheckDrift`.

The `CheckDrift` output is the surface the LSP / IDE quick-fix
keys on: a `--check` failure prints one `<module>@<version>:
<kind> drift (<detail>)` line per drift, sorted, so downstream
tools can pattern-match on the kind without parsing free-form text.

Future phase 10.x reservations:

- **10.1** `mochi pkg lock` driver integration (read
  `[go-dependencies]` from `mochi.toml`, walk module proxy, write
  `[[go-package]]` entries).
- **10.2** `mochi pkg lock --sumdb-consistency` (Merkle consistency
  proof on top of the existing sumdb client).
- **10.3** Cross-MEP lockfile merger (interleave
  `[[go-package]]` with `[[rust-package]]` and other language-tag
  tables in a stable order).
