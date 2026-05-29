---
title: "Phase 15. Attestation verification"
sidebar_position: 17
sidebar_label: "Phase 15. Attestation verification"
description: "MEP-71 Phase 15 implementation tracking: install-time PEP 740 attestation verification + --require-attestations gate."
---

# Phase 15. Attestation verification

Land the install-time half of the trusted-publishing pipeline whose
publish half landed in [phase 11](/docs/implementation/0071/phase-11-trusted-publish):
fetch the PEP 740 `<wheel-url>.provenance` attestation bundle for every
resolved wheel, validate its Sigstore + SLSA fields, and gate the
install when `--require-attestations` is on.

## Status

`LANDED` (pending PR merge). Phase 15 ships the offline policy +
verifier surface that the wheel-install loop can dial in today; the
live sigstore-go crypto verifier, the live HTTPS fetch of
`<wheel-url>.provenance`, and the `mochi pkg install` flag wiring are
split into sub-phases 15.1 / 15.2 / 15.3 so the umbrella gate stays
deterministic.

## Gate

`go test ./package3/python/attest/... -count=1`

Covers:

- `ParseStatement` happy + six error paths (no `_type` / no
  `predicateType` / zero subjects / subject missing name / subject
  missing sha256 / non-JSON).
- `ParseBundle` happy + four error paths (no payload / no
  `payloadType` / no signatures / non-JSON).
- `(*Bundle).Statement()` payload-type guard, base64 round-trip,
  nil-safety.
- `(*Statement).BuilderID()` extraction (deep `runDetails.builder.id`)
  and nil-safety on every layer.
- Every `Policy.Verify` branch: missing bundle, unsupported
  `mediaType`, bad `payloadType`, statement parse failure, wrong
  `_type`, wrong `predicateType`, subject not found, digest mismatch,
  builder rejected, trusted-publisher rejection (with and without
  `IdentityExtractor`), publisher-extractor error.
- `Policy.Required` gate (returns error when not OK, returns nil error
  when not Required even with violations).
- `StaticFetcher` (empty -> `ErrNoAttestation`, non-empty -> round
  trip).
- `HTTPFetcher.AttestationURL` (`<url>.provenance` happy + non-`.whl`
  rejection).
- `Verifier.Verify` end-to-end with `StaticFetcher`: happy, missing +
  not-Required, missing + Required, bad-digest + Required,
  parse-error + Required, fetch-error + not-Required, fetch-error +
  Required, no-Fetcher error.
- `phase15_test.go` umbrella sentinel: trusted-publisher end-to-end,
  digest-mismatch flagged, Required gate fails the install.

## Files

```
package3/python/attest/
  doc.go               # 6-step verification pipeline overview
  statement.go         # in-toto Statement v1 + SLSA Provenance v1 shapes
  bundle.go            # Sigstore Bundle + DSSE envelope shapes
  report.go            # Reason codes + Violation + Report
  policy.go            # Policy + Verify() + WheelTarget + IdentityExtractor
  fetcher.go           # Fetcher interface + StaticFetcher + HTTPFetcher.AttestationURL
  verifier.go          # Verifier glues Fetcher + Policy
  statement_test.go    # 7 tests
  bundle_test.go       # 6 tests
  report_test.go       # 2 tests
  policy_test.go       # 17 tests
  fetcher_test.go      # 5 tests
  verifier_test.go     # 9 tests
  phase15_test.go      # 1 umbrella sentinel (4 sub-cases)
```

## Sub-phase decomposition

Sub-phases land separately so the umbrella stays offline and
deterministic.

### 15.1. Live sigstore-go crypto verifier

The crypto verifier (X.509 chain validation against Fulcio,
SCT verification, Rekor inclusion-proof check, DSSE signature check
against the certificate's public key) ships as a sigstore-go
shell-out. Today the bundle's `verificationMaterial` is parsed but not
cryptographically verified; a `crypto.Verifier` hook in the `Policy`
struct will surface this once 15.1 lands. The hook signature is
roughly `func(b *Bundle) error`; non-nil errors will flow through
`ReasonSignatureNotVerified`. Identity (via Fulcio SAN) will be lifted
out so the `IdentityExtractor` becomes the production
default rather than a test seam.

### 15.2. Live PyPI HTTP fetcher

Today `HTTPFetcher.AttestationURL` returns `<wheel-url>.provenance`
per PEP 740 but `HTTPFetcher.Fetch` is a stub returning
`ErrNoAttestation`. Sub-phase 15.2 wires `net/http` with:

- caching against the same content-addressed cache the wheel loop
  uses (so repeated installs are zero-RTT).
- retry-with-backoff on transient 5xx.
- 404 -> `ErrNoAttestation` (the wheel publisher has not opted in).

### 15.3. CLI verbs

The CLI surface:

- `--require-attestations` -> `Policy.Required = true`.
- repeated `--allowed-builder=<URI>` -> `Policy.AllowedBuilders`.
- repeated `--trusted-publisher=<identity>` -> `Policy.TrustedPublishers`.
- machine-readable JSON output of each `Report` for telemetry (the
  `Reason` constants exist precisely so the CLI can switch on cause
  without pattern-matching free-form messages).

## Skip count

Phase 15 is install-side and does not feed the wrapper-synthesiser
skip counter. The fixture-corpus golden counts are unchanged.

## Fixtures

Sub-phase 15.2 will add a fixture under `package3/python/attest/testdata/`
mirroring the publisher-emitted bundle for one of the 25-package
fixture corpus members so the live HTTP path can run offline against
a recorded transcript. Phase 15 itself stays offline.

## Cross-references

- [PEP 740 (attestation envelope)](https://peps.python.org/pep-0740/)
- [in-toto Statement v1](https://github.com/in-toto/attestation/tree/main/spec/v1)
- [SLSA Provenance v1](https://slsa.dev/spec/v1.0/provenance)
- [Sigstore Bundle v0.3](https://github.com/sigstore/protobuf-specs)
- [phase 11](/docs/implementation/0071/phase-11-trusted-publish) for
  the publish-side that produced the bundle this phase verifies.
- [MEP-71 spec § Trusted publishing](/docs/mep/mep-0071) for the
  normative design.
