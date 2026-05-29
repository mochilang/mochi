---
title: "Phase 2. Sumdb"
sidebar_position: 4
sidebar_label: "Phase 2. Sumdb"
description: "MEP-74 Phase 2 lands the sum.golang.org transparency-log client: signed-note framing + Ed25519 verifier, verifier-key parser, /latest + /lookup + /tile HTTP endpoints, RFC-6962 Merkle tree + inclusion-proof verifier."
---

# Phase 2. Sumdb

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:21 (GMT+7) |
| Landed         | 2026-05-29 21:34 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase2Sumdb` in `package3/go/sumdb/phase02_test.go`: drives the full
consume loop against an `httptest` fake. The test generates a fresh
Ed25519 keypair, builds a 4-leaf RFC-6962 Merkle tree, signs a
tree-head note over it, serves `/lookup/<m>@<v>` and `/latest` from
the fake, then runs `Client.Latest → ParseNote → Verify →
ParseTreeHead`, `Client.Lookup → ParseLookup`, `ParseNote → Verify`
on the lookup's embedded tree note, and finally
`VerifyInclusion` of the target leaf against the verified root.
Passing this sentinel means a downstream caller can integrate
phase 2 end-to-end without any external network.

In addition the package-level test suite covers:

- `package3/go/sumdb/key_test.go`: parses the published
  `sum.golang.org+033de0ae+...` verifier key blob, asserts the
  4-byte key-hash matches `0x033de0ae` exactly, rejects 7 malformed
  blob shapes (empty, missing fields, bad base64, wrong hash field
  length, unsupported algorithm byte, empty pubkey body), rejects a
  blob whose embedded key-hash does not match the recomputed value,
  and round-trips a parsed key back to its on-wire form.

- `package3/go/sumdb/note_test.go`: `Sign → Parse → Verify`
  round-trip for a freshly-generated keypair, rejection of notes
  missing the blank-line separator, rejection of notes with no
  signature lines, rejection of notes whose body line begins with
  the U+2014 EM DASH signature prefix (which would alias as a
  signature), rejection on wrong-key verify, rejection on tampered
  body, `Sign` appends a trailing newline when needed, and
  rejection when no candidate keys are supplied to `Verify`.

- `package3/go/sumdb/lookup_test.go`: a happy-path 7-field response
  parses to the expected `LookupRecord`, rejection of empty body,
  rejection of missing blank-line separator, rejection of non-numeric
  log id, rejection of module mismatch between the zip-hash and
  mod-hash lines, rejection of version mismatch (e.g. zip hash claims
  v1.2.3 but mod hash claims v9.9.9), rejection when the mod-hash
  line is missing the `/go.mod` suffix on its version, and rejection
  of a zip-hash line whose hash field lacks the `h1:` prefix.

- `package3/go/sumdb/tree_test.go`: parsing a 3-line tree head body,
  rejection of 8 malformed bodies (empty, missing lines, non-numeric
  size, negative size, empty origin, bad base64, short hash), a
  known-vector check on `HashLeaf(nil)` against the literal
  `0x6e340b9c...` bytes, stability + asymmetry of `HashChildren`,
  `MerkleRoot` cross-check against a manual recursion for sizes
  1..16, `VerifyInclusion` across every leaf position in a 7-leaf
  tree (including the odd-boundary case at index 6), tampered-leaf
  rejection at every position, rejection on a too-short proof,
  rejection on out-of-range leaf index, and `HashFromBase64` error
  cases (bad base64, wrong length).

- `package3/go/sumdb/client_test.go`: httptest-driven
  `Latest`/`Lookup`/`Tile` happy paths, URL-construction
  assertions verifying the tile URL matches the `/tile/H/L/K` form,
  empty-arg rejection for `Lookup`, invalid-coord rejection for
  `Tile` (H<=0, negative level, negative index), HTTP 500
  returns an error, and `NewClient` normalises trailing slash +
  defaults to `DefaultBaseURL`.

## Lowering decisions

Phase 2 implements the *consume side* of the transparency log: given
a module@version, the bridge fetches the `/lookup/...` record,
verifies the embedded tree note against a baked-in verifier key,
parses the (id, zipHash, modHash, treeNote) tuple, and (when given a
tile fetcher) verifies an inclusion proof. The *publish side* —
appending new records to the bridge's own log — is deferred to
phase 12.

The verifier key is hard-coded as
`SumGolangOrgVerifierKey =
"sum.golang.org+033de0ae+Ac4zctda0e5eza+HJyk9SxEdh+s3Ux18htTTAD8OuAn8"`.
Embedding the key in source (rather than fetching it at boot from a
network endpoint) avoids a chicken-and-egg integrity problem: a
network-fetched key cannot itself be verified without already having
a trusted key on hand. The 4-byte key-hash prefix `0x033de0ae` is
verified by recomputing it during `ParseVerifierKey`, so even a
typo in the embedded constant is caught at parse time.

The signed-note framing uses U+2014 EM DASH + space as the
signature-line prefix (a literal three-byte UTF-8 sequence). The
parser explicitly rejects body lines that start with this prefix
because they would alias as signature lines and let an attacker
silently truncate the body. The signature payload is exactly
4 + 64 = 68 bytes (4-byte key-hash + 64-byte Ed25519 signature),
base64-StdEncoded.

The Merkle tree implementation follows RFC-6962 byte-for-byte:
leaf hash = SHA-256(0x00 || record), interior hash = SHA-256(0x01
|| left || right), MerkleRoot splits at the largest power of two
strictly less than the leaf count (matching x/mod/sumdb/tlog
exactly). This identity is verified in test by a manual recursion
against `merkleRootRange` for every tree size 1..16.

The inclusion-proof verifier handles the RFC-6962 odd-boundary case
(a node that is the last in its level and even-indexed) by promoting
without consuming a proof element. The test suite drives every leaf
position in a 7-leaf tree to exercise this path explicitly (index 6
in a 7-leaf tree being the canonical odd-boundary case where naïve
algorithms over-consume the proof).

`Client.Lookup` URL-escapes the `module@version` pair with
`url.PathEscape`. The literal `@` between module path and version is
left intact (the proxy expects it as a literal separator, not a
URL-escaped `%40`). The 16 MiB cap on `get` response bodies
protects against an adversarial log returning a hostile payload —
real tile bodies are a few KiB at most.

`Lookup` does *not* verify the tree note that ships inside the
response; that is the caller's responsibility. The split mirrors
the same caller-side verification pattern in
`golang.org/x/mod/sumdb`: the network client is purely a data
mover, and verification is composed in by the higher-level driver
(phase 9 in MEP-74 terms).

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/sumdb/key.go` | `VerifierKey`, `ParseVerifierKey`, `SumGolangOrgVerifierKey`, `AlgEd25519`, `computeKeyHash` |
| `package3/go/sumdb/key_test.go` | verifier-key parser + key-hash check tests |
| `package3/go/sumdb/note.go` | `Note`, `NoteSignature`, `ParseNote`, `Note.Verify`, `Signer`, `Signer.Sign`, `ErrNoteUnverified` |
| `package3/go/sumdb/note_test.go` | signed-note framing + sign/parse/verify round-trip tests |
| `package3/go/sumdb/lookup.go` | `LookupRecord`, `ParseLookup` |
| `package3/go/sumdb/lookup_test.go` | lookup-response parser + rejection tests |
| `package3/go/sumdb/tree.go` | `Hash`, `HashSize`, `TreeHead`, `ParseTreeHead`, `HashLeaf`, `HashChildren`, `MerkleRoot`, `VerifyInclusion`, `HashFromBase64` |
| `package3/go/sumdb/tree_test.go` | RFC-6962 Merkle tree + inclusion-proof verifier tests |
| `package3/go/sumdb/client.go` | `Client`, `NewClient`, `Latest`, `Lookup`, `Tile`, `DefaultBaseURL`, `DefaultUserAgent` |
| `package3/go/sumdb/client_test.go` | httptest-driven HTTP client tests |
| `package3/go/sumdb/phase02_test.go` | `TestPhase2Sumdb` end-to-end sentinel |

## Test set

- `TestPhase2Sumdb`
- All `package3/go/sumdb/...` unit tests (5 test files).

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok  	mochi/package3/go/build	(cached)
ok  	mochi/package3/go/errors	(cached)
ok  	mochi/package3/go/moduleproxy	(cached)
ok  	mochi/package3/go/semver	(cached)
ok  	mochi/package3/go/sumdb	0.518s
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

Phase 2 keeps the dependency surface minimal: the new package
imports only `crypto/ed25519`, `crypto/sha256`, `encoding/base64`,
`encoding/binary`, `net/http`, `strconv`, plus stdlib utilities.
No `golang.org/x/mod` import. This is a deliberate choice: x/mod's
`sumdb/note` and `sumdb/tlog` packages would be a fine reuse, but
inlining the 200 lines of framing + verifier needed keeps the
audit surface confined to the bridge's own code at this early
phase. Future phases that need the tile-fetcher abstraction
(`tlog.TileFetcher`) can either reach into x/mod or extend the
local tile primitives in this package.

The inclusion-proof verifier is the load-bearing piece for the
broader plan: phase 9 (build orchestration) will fold its
positive-or-negative result into the `mochi.lock` integrity
guarantee, and phase 13 (cosign) will reuse the underlying
sha256-prefix framing in the cosign payload. A subtle bug in the
boundary-case handling here would silently let through records
that look valid but were not actually committed to the public log;
the `TestVerifyInclusionAllPositions` test exhaustively drives
every leaf position in a 7-leaf tree (the smallest size that
exercises both balanced and unbalanced subtrees) to guarantee the
verifier doesn't over- or under-consume the proof.

The bake-in of the public sum.golang.org verifier key sets a
precedent: phase 12 (publish) and phase 13 (cosign) will likewise
embed their trust roots in source code rather than fetching them
at boot. Rotation of these constants becomes a code change with a
PR, which is the same level of review applied to every other
build-graph change.

The lookup-response parser intentionally rejects mismatched
modules / versions across the zip-hash and mod-hash lines. A real
sum.golang.org response will never have these mismatch (the
transparency log records are consistent by construction), but the
parser cannot trust an arbitrary HTTP body until verified; treating
the structural check as part of parse hardens us against an
attacker who controls the proxy and tries to swap the mod hash for
a different module's data while keeping the signed tree note
unchanged.
