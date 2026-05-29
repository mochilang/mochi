// Package attest implements MEP-71 Phase 15: install-time PEP 740
// attestation verification + the `--require-attestations` enforcement
// knob.
//
// At install time, for every wheel the resolver has decided to fetch,
// the bridge:
//
//  1. Fetches the attestation envelope from PyPI's
//     `<wheel-url>.provenance` companion URL via the Fetcher interface.
//  2. Parses the envelope as a Sigstore Bundle wrapping a DSSE-signed
//     in-toto Statement v1 carrying a SLSA Provenance v1 predicate.
//  3. Confirms the statement's subject sha256 matches the wheel's
//     sha256 the resolver committed to (this catches a swapped wheel
//     even before any crypto runs).
//  4. Confirms the statement's predicateType is the SLSA Provenance
//     v1 URI and the builder.id falls inside the policy's
//     AllowedBuilders set.
//  5. Confirms the OIDC identity an IdentityExtractor pulls off the
//     bundle's X.509 SAN extension matches the policy's
//     TrustedPublishers set.
//  6. When Policy.Required is true, a missing attestation, a parse
//     failure, or any structural violation aborts the install.
//
// The X.509 + Sigstore crypto path is intentionally behind interfaces
// so the umbrella gate stays offline. Sub-phase 15.1 wires the real
// `sigstore-go` (or equivalent) verifier behind the bundle's DSSE
// signatures + Rekor inclusion proof. Sub-phase 15.2 wires the real
// PyPI HTTP fetch (PEP 740 § "Fetching attestations"). Sub-phase 15.3
// adds the `mochi pkg install --require-attestations` CLI verb.
//
// Layout:
//
//   - statement.go: in-toto Statement v1 + Subject + Predicate shape
//     + the SLSA Provenance v1 + in-toto Statement v1 type URIs (mirrors
//     publish/attestation.go so install-side verification is decoupled
//     from publish-side construction).
//   - bundle.go: Sigstore Bundle envelope (the PyPI-emitted subset:
//     mediaType + dsseEnvelope + verificationMaterial) + ParseBundle +
//     Bundle.Statement decoder.
//   - policy.go: Policy + IdentityExtractor interface + verification
//     pipeline (`Policy.Verify(bundle, target) -> Report`).
//   - report.go: Report + Violation + Reason codes for the verifier
//     output the CLI surfaces.
//   - fetcher.go: Fetcher interface + StaticFetcher (test seam) +
//     HTTPFetcher (URL-construction only; real HTTP is sub-phase 15.2).
//   - verifier.go: Verifier glue (`Verifier.Verify(ctx, target) ->
//     Report`) the install-time orchestrator consumes.
package attest
