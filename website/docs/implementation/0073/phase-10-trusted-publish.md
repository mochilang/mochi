---
sidebar_label: "Phase 10: Trusted publishing"
sidebar_position: 11
---

# MEP-73 Phase 10: Trusted publishing (Sigstore OIDC)

**Status:** LANDED (2026-05-30)
**Spec section:** [MEP-73 §3 Direction 2 — Publish flow](/docs/mep/mep-0073)
**Worktree:** `/Users/apple/mochi-mep73-p10`

## Gate

Land the publish-direction runtime: take a rendered crate (Phase 9
`library.Files`), pack it into a deterministic `.crate` tarball,
attach a Sigstore-keyless attestation bundle over the tarball's
SHA-256 digest, and upload to crates.io's `/api/v1/crates/new`
endpoint with a Bearer OIDC token per Cargo RFC #3724.

## Why it matters

Phase 9 produces a publishable crate as an in-memory `Files` map.
Phase 10 is the pipe between that map and crates.io. Without it,
`mochi pkg publish --to=crates.io` is a no-op. Trusted publishing
(no API tokens, no long-lived credentials) is the security posture
the rest of the ecosystem is moving toward (PyPI 2023, npm 2024,
crates.io 2025) and what MEP-73 promises.

Three things had to be true for the gate to pass:

1. The `.crate` tarball must be byte-stable across runs (matches the
   SOURCE_DATE_EPOCH=0 reproducibility model Phase 7 set up for
   `cargo build`).
2. The Sigstore bundle must carry the SHA-256 digest of the actual
   `.crate` bytes being uploaded (downstream verifiers replay this
   binding to detect tampering between sign and upload).
3. The upload body must match the exact wire format crates.io's
   `/api/v1/crates/new` decoder expects: a 4-byte little-endian length
   prefix for the JSON metadata, the metadata bytes (`{}` for trusted
   publishing since the manifest is inside the `.crate`), a 4-byte LE
   length prefix for the `.crate`, then the `.crate` bytes.

## What landed

### `package3/rust/publish/publish.go`

The publish driver and the `PublishRequest` / `PublishResult` /
`Transport` boundary types.

- `PublishRequest{CrateName, Version, Files, OIDCToken, RegistryURL,
  DryRun}` — the closed input shape.
- `PublishResult{CrateBytes, SignedBundle, UploadedURL, StatusCode}`
  — the outcome.
- `Transport` interface (one method `Do(url, headers, body)`) —
  Publish's only impure dependency. Real CLI passes a net/http
  adapter; tests pass `fakeTransport`.
- `PublishRequest.Validate()` — fails fast on empty crate name /
  version / OIDC token, missing `Cargo.toml`, missing `src/lib.rs`,
  and crate-name or version mismatch between `PublishRequest` and the
  rendered `Cargo.toml`.
- `Publish(req, transport)` — runs the pipeline: Validate, Tarball,
  SignBundle, then if not DryRun POST to
  `<registry>/api/v1/crates/new` with headers `Authorization: Bearer
  <token>`, `Content-Type: application/octet-stream`, `User-Agent:
  mochi-pkg-publish/1.0`, `X-Mochi-Sigstore-Bundle: <b64>`,
  `X-Mochi-Publish-Audience: crates.io`.
- `renderUploadBody(crate)` — packs the upload bytes in the wire
  format crates.io expects.
- `Audience = "crates.io"` and `DefaultRegistryURL = "https://crates.io"`
  constants.

### `package3/rust/publish/tarball.go`

The `.crate` tarball encoder + extractor.

- `Tarball(crateName, version, files)` produces a gzipped USTAR
  archive with every path prefixed `<crate>-<version>/`, mode `0644`,
  mtime epoch, alphabetically sorted entries, and a stripped gzip
  header (Name / ModTime cleared) so output is byte-stable.
- `ExtractTarball(crateBytes)` is the inverse, used by the test
  harness to verify the roundtrip survives gunzip + tar parsing.

### `package3/rust/publish/oidc.go`

OIDC claim handling.

- `OIDCClaims{Issuer, Subject, Audience, Expiry, IssuedAt,
  Repository, RepositoryOwner, JobWorkflowRef}` — the trusted-publisher
  claims crates.io's registry expects.
- `ParseOIDCToken(jwt)` parses the three-segment JWT (header / payload
  / signature), base64url-decoding the payload and JSON-unmarshalling
  the claims. Signature verification is intentionally deferred to the
  crates.io trust root — the bridge does not impersonate that trust
  boundary; it just passes the raw token through to the registry.
- `ValidateClaims(claims, now)` enforces audience == "crates.io",
  unexpired, non-empty issuer + subject. These are the structural
  checks that catch local mistakes before paying the network round
  trip.
- `EncodeUnverifiedJWT(claims)` is a test helper that emits an `alg=none`
  JWT for roundtrip coverage. Production code never produces JWTs;
  the CI's OIDC issuer does.

### `package3/rust/publish/sigstore.go`

The Sigstore-keyless attestation bundle.

- `SigstoreBundle{MediaType, PredicateType, Subject, IssuedAt,
  OIDCToken}` with `Subject{Name, Digest}`.
- `SignBundle(crate, version, crateBytes, token)` produces canonical
  JSON: keys sorted at every nesting level, no extra whitespace,
  `IssuedAt` pinned to Unix epoch for reproducibility.
- `MediaType = application/vnd.dev.sigstore.bundle.v0.3+json` — the
  Sigstore Bundle v0.3 media type the OpenSSF tooling expects.
- `PredicateType = https://cargo.crates.io/spec/Registry/v1` — the
  in-toto predicate the crates.io registry agreed on.
- `Subject.Name = "<crate>-<version>.crate"` and `Subject.Digest =
  {"sha256": "<hex>"}` over the actual `.crate` tarball bytes.
- `encodeBundleHeader(bundle)` returns the base64-url-no-pad encoding
  used in the `X-Mochi-Sigstore-Bundle` request header.
- `canonicalJSON` / `marshalSortedJSON` walk the JSON tree and emit
  sorted-key output so the bundle bytes are byte-stable across runs
  (the header is a hash-stable identifier of the upload).

In production the bundle also carries a Fulcio-issued certificate, a
DSSE envelope, and a Rekor transparency-log inclusion proof; those
are produced by the live Sigstore client at flow time and stitched
into the same JSON envelope. The deterministic pure-data portion is
what `SignBundle` owns.

### Tests

- `tarball_test.go` — 8 cases: rejects empty inputs, is gzip,
  roundtrip preserves entries, byte-stable across runs, paths prefixed
  with `<crate>-<version>/`, alphabetical entry order, gunzip error
  surfacing, large-body content preservation.
- `oidc_test.go` — 9 cases: encode + parse roundtrip, rejects
  malformed (empty, too-few segments, too-many segments, invalid
  base64, invalid JSON inside payload), accepts valid claims, rejects
  wrong audience, rejects expired token, rejects missing exp / iss /
  sub, handles additional `JobWorkflowRef` claim.
- `sigstore_test.go` — 8 cases: stable output across runs, carries
  SHA-256 of crate bytes, subject name format `<crate>-<version>.crate`,
  embeds OIDC token, rejects empty inputs, media type is
  `application/vnd.dev.sigstore.bundle.v0.3+json`, canonicalJSON sorts
  keys top-level and nested, `encodeBundleHeader` produces RawURL
  base64.
- `publish_test.go` — 14 cases with `fakeTransport`: Validate accepts
  valid request, rejects empty fields / name mismatch / version
  mismatch / missing manifest / missing lib.rs, dry-run skips
  transport, sends Bearer + Sigstore + audience headers, targets
  default URL, honors custom registry, fails on non-2xx, fails on
  transport error, fails on nil transport for non-dry-run, upload-body
  format matches crates.io wire, `PublishError.Error()` formatting,
  `registryURL()` default + trailing-slash trim.
- `phase10_test.go` (sentinel) with subtests:
  - `end_to_end`: full Render → Publish through `fakeTransport`
    asserts URL + Bearer + Sigstore header on the recorded call.
  - `dry_run_produces_bundle_without_upload`: DryRun=true produces a
    bundle but never calls the transport.
  - `tarball_byte_stable`: two `Tarball` calls produce identical
    bytes.
  - `oidc_validation_rejects_wrong_audience`.
  - `oidc_validation_rejects_expired`.
  - `sigstore_bundle_carries_crate_digest`: SHA-256 digest is present
    and Subject.Name is `<crate>-<version>.crate`.

## Target matrix

| Target           | Status   | Notes |
|------------------|----------|-------|
| Tarball encoder  | ✅       | Byte-stable USTAR + gzip, sorted entries, epoch mtime. |
| Tarball decoder  | ✅       | Round-trip parity in tests. |
| OIDC claim parser| ✅       | Three-segment JWT, base64url decode, JSON unmarshal. |
| OIDC validator   | ✅       | Audience + exp + iss + sub checks. |
| Sigstore bundle  | ✅       | v0.3 media type, in-toto predicate, SHA-256 subject. |
| Upload wire body | ✅       | u32-LE(meta-len) + meta + u32-LE(crate-len) + crate. |
| Bearer auth      | ✅       | `Authorization: Bearer <oidc-token>`. |
| DryRun mode      | ✅       | Skips the POST, retains the bundle for inspection. |
| Transport boundary | ✅     | Interface-typed; in-process fake covers all paths. |

## How this phase plugs in to the larger pipeline

```
  library.Render (Phase 9)               Phase 10 publish
                                       ┌────────────────────────────┐
  Files{                               │ PublishRequest             │
    "Cargo.toml": ...,    ───────────► │   CrateName, Version,      │
    "src/lib.rs": ...,                 │   Files, OIDCToken,        │
    ...                                │   RegistryURL, DryRun      │
  }                                    └──────────────┬─────────────┘
                                                      │
                                            publish.Publish(req)
                                                      │
                                      ┌───────────────┴───────────────┐
                                      ▼                               ▼
                                Tarball(...)                    SignBundle(...)
                                .crate bytes                    canonical-JSON
                                      │                         attestation
                                      └───────┬───────────────────────┘
                                              ▼
                                  Transport.Do(
                                    <registry>/api/v1/crates/new,
                                    {Authorization, Sigstore, ...},
                                    u32-LE-len + "{}" + u32-LE-len + .crate)
                                              │
                                              ▼
                                      PublishResult{
                                        CrateBytes, SignedBundle,
                                        UploadedURL, StatusCode}
```

The Transport boundary is what makes the CLI testable: production
wires a net/http-backed adapter, tests inject a `fakeTransport` that
records the URL / headers / body without making network calls.

## Timeline

| Time (GMT+7)        | Step |
|---------------------|------|
| 2026-05-29 23:55    | Worktree branch `mep/0073-phase-10` created off `origin/main`. |
| 2026-05-30 00:01    | `publish.go`, `tarball.go`, `oidc.go`, `sigstore.go` written. |
| 2026-05-30 00:03    | Per-package tests + Phase 10 sentinel written. |
| 2026-05-30 00:05    | `go test ./package3/rust/publish/...` and `./package3/rust/...` green. |
| 2026-05-30 00:06    | Tracking page + spec sync. |
