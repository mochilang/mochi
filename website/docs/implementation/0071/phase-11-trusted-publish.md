---
title: MEP-71 Phase 11 (Trusted publish to PyPI)
sidebar_position: 12
sidebar_label: "Phase 11. Trusted publish to PyPI"
description: "MEP-71 Phase 11: mochi pkg publish --to=pypi orchestration with Sigstore-keyless OIDC trusted publishing + PEP 740 in-toto SLSA Provenance attestations."
---

# MEP-71 Phase 11. Trusted publish to PyPI

Status: **LANDED (pending merge)** as of 2026-05-30 00:36 (GMT+7). Implements the orchestration layer of `mochi pkg publish --to=pypi`: OIDC token minting, PEP 740 in-toto SLSA Provenance v1 attestation construction, and the per-target uploader driver. The actual Sigstore signing + live PyPI / TestPyPI HTTP calls are sub-phases 11.1 and 11.2.

## Gate

The umbrella sentinel `TestPhase11TrustedPublish` in `package3/python/publish/phase11_test.go` is green. The sentinel:

- Builds a `PublishRequest` with two artifact pairs (sdist + wheel) targeting TestPyPI.
- Runs the orchestrator in `DryRun=true` mode so the OIDC -> mint -> upload chain executes without touching the network (default uploader is `RecordingUploader`; the test also injects a `fakeUploader` that returns synthetic `PublishedArtifact` URLs).
- Asserts the orchestrator emits a PEP 740 in-toto v1 statement with `_type = "https://in-toto.io/Statement/v1"`, `predicateType = "https://slsa.dev/provenance/v1"`, sorted subjects covering every (sdist, wheel) pair with sha256 digests, and a `runDetails.builder.id` carrying the configured builder identity.
- Asserts every artifact in the result lands with the registry-shaped URL (`https://test.pypi.org/project/<name>/<ver>/`) and the AttestationURL field round-trips through the uploader.
- Asserts dry-run propagates an empty API token to the uploader (no minted credential leaves the orchestrator without an explicit non-dry-run pass).

Plus 38 unit tests (`go test ./package3/python/publish/... -count=1`) covering:

- `RegistryKind` String / URL / OIDCAudience for PyPI + TestPyPI + unknown fallback.
- `PublishRequest.Validate` rejecting unknown registry, empty targets, empty Distribution / Version / artifact path / sha256 / size, non-`.tar.gz` sdist, non-`.whl` wheel, and missing OIDCProvider when DryRun is false. Dry-run validates without an OIDC provider.
- `GitHubActionsProvider.Token`: missing env vars, happy-path token fetch from a mock httptest server, HTTP 403 propagation, and empty-token refusal.
- `StaticProvider`: returns the stored token; refuses empty TokenValue.
- `MintAPIToken`: rejects empty OIDC token, happy path against a mock httptest server (verifies the POST body carries `{"token": <oidc>}` and the response's expires field is RFC 3339 parsed), 401 propagation, and refusal when the response's `success` flag is false.
- `BuildStatement`: subjects are sorted by name; every subject carries a sha256 digest; subject filenames follow the PEP 491 / PEP 625 shape (`<dist>-<ver>.tar.gz` for sdists, `<dist>-<ver>-py3-none-any.whl` for wheels); builder id falls back to `AttestationBuilderID` when unset and is overridable; invocation id defaults to `"auto"`; `finishedOn` is in a recent UTC window when `BuildTime` is zero.
- `EncodeStatement` round-trips through `json.Marshal` deterministically (same input -> same bytes).
- `RecordingUploader.Upload` captures the call and returns the registry-shaped URL with `Skipped=true`.
- `UVUploader.Upload`: invokes `uv` with `publish --trusted-publishing always --publish-url <registry>/legacy/ <sdist> <wheel>` and `UV_PUBLISH_TOKEN=<minted>` in env; surfaces subprocess errors with the captured stderr; honours `Binary` override.
- `Orchestrator.Publish`: rejects invalid requests; dry-run skips OIDC and uses `RecordingUploader` by default; propagates OIDC errors; embeds every target in the attestation; surfaces uploader errors; propagates the configured Builder id into the attestation predicate.

## Files

- `package3/python/publish/doc.go` — package overview.
- `package3/python/publish/request.go` — `PublishRequest`, `PublishTarget`, `RegistryKind`, `Validate`.
- `package3/python/publish/oidc.go` — `OIDCProvider` interface, `GitHubActionsProvider`, `StaticProvider`, `MintAPIToken`, `MintedToken`.
- `package3/python/publish/attestation.go` — `Statement`, `Subject`, `BuildStatement`, `EncodeStatement` + constants for SLSA Provenance v1 predicate type, in-toto Statement v1 type uri, and the Mochi builder identity.
- `package3/python/publish/uploader.go` — `Uploader` interface, `UploadCall`, `UploadResult`, `UVUploader` (default), `RecordingUploader` (dry-run + tests).
- `package3/python/publish/orchestrator.go` — `Orchestrator.Publish` glue (validate -> attest -> mint -> upload -> result aggregation).
- `package3/python/publish/request_test.go` — request + registry validation (8 cases).
- `package3/python/publish/oidc_test.go` — OIDC provider + mint endpoint (10 cases).
- `package3/python/publish/attestation_test.go` — attestation statement shape + encoding determinism (9 cases).
- `package3/python/publish/uploader_test.go` — UVUploader + RecordingUploader (4 cases).
- `package3/python/publish/orchestrator_test.go` — orchestrator chain (7 cases).
- `package3/python/publish/phase11_test.go` — Phase 11 umbrella sentinel.

## Sub-phase decomposition

Phase 11 ships the orchestration layer with mockable subprocess + HTTP boundaries. The live integrations stay out of the umbrella gate so the test suite remains offline and deterministic:

| Sub-phase | Title | Status | Notes |
|-----------|-------|--------|-------|
| 11 | Publish orchestrator (OIDC + attestation + uploader + dry-run) | LANDED (pending merge) | This PR. |
| 11.1 | Sigstore live signing harness (`sigstore-python` shell-out + Rekor log fetch) | NOT STARTED | Wraps the in-toto statement in a Sigstore bundle. |
| 11.2 | Live PyPI / TestPyPI HTTP (mint endpoint contract test against Warehouse mock) | NOT STARTED | Replaces the test-only redirect transport with real `pypi.org` calls. |
| 11.3 | `mochi pkg publish --to=pypi [--testpypi] [--dry-run]` CLI verb | NOT STARTED | Wires the orchestrator behind the unified `mochi pkg` CLI. |

## Fixtures

Phase 11 is offline; the fixture corpus is not exercised. Sub-phase 11.2 will gate the live mint endpoint against a frozen `_/oidc/mint-token/` contract response taken from PyPI's Warehouse repo.

## Skip count

N/A. Phase 11 has no `SkipReport` surface; non-publishable requests are rejected by `PublishRequest.Validate()` before any external call.

## Cross-references

- [MEP-71 spec §6 "Sigstore-keyless OIDC trusted publishing"](/docs/mep/mep-0071) for the normative flow.
- [Phase 10](./phase-10-python-package-emit) for the sdist + wheel emit Phase 11 consumes.
- [Phase 15](./phase-15-attestation-verify) for the install-time PEP 740 attestation verifier that mirrors the publish-time signer.
- [Phase 18](./phase-18-abi2026) for the abi2026 transition that controls which wheel ABI tag Phase 11 publishes.
- [Trusted Publishing background (PyPI Warehouse)](https://docs.pypi.org/trusted-publishers/) and [PEP 740](https://peps.python.org/pep-0740/) for the attestation envelope spec.
