// Package publish implements MEP-71 Phase 11: trusted publishing to PyPI.
//
// The package surfaces the orchestrator that:
//
//   - Consumes the rendered sdist + wheel layouts the Phase 10 emit
//     produces (or a pre-built artifact pair on disk).
//   - Obtains an OIDC token from the CI environment via a pluggable
//     OIDCProvider (GitHub Actions, GitLab, Google Cloud Build,
//     ActiveState CircleCI are wired through small driver structs).
//   - Exchanges the OIDC token for a short-lived PyPI / TestPyPI API
//     token via the registry's `_/oidc/mint-token/` endpoint.
//   - Drives the actual upload via a pluggable Uploader (the default
//     shells to `uv publish --trusted-publishing always`; a recording
//     uploader is used in tests and behind --dry-run).
//   - Bundles a PEP 740 in-toto attestation envelope for every
//     artifact: the Mochi side computes the in-toto statement (subject
//     digests + builder identity); the actual Sigstore signing is
//     produced by uv at publish time (sub-phase 11.1 hooks the local
//     signing harness for offline dry runs).
//
// Layout:
//
//   - request.go: PublishRequest + PublishTarget + RegistryKind.
//   - oidc.go: OIDCProvider interface + GitHubActions / Generic drivers
//     + token-exchange helpers.
//   - attestation.go: PEP 740 in-toto subject + statement renderer.
//   - uploader.go: Uploader interface + uvUploader default +
//     RecordingUploader used in tests + dry-run mode.
//   - orchestrator.go: glues request -> OIDC -> attestation -> upload.
//   - result.go: PublishResult + per-artifact PublishedArtifact.
//
// See MEP-71 §6 "Sigstore-keyless OIDC trusted publishing" for the
// normative flow. Live Sigstore integration + real PyPI / TestPyPI
// requests are sub-phases 11.1 and 11.2; this umbrella phase covers
// the orchestration logic with mockable boundaries.
package publish
