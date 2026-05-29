---
title: "Trusted publishing: Sigstore plus OIDC end-to-end"
description: "Sigstore plus OIDC publish flow walkthrough, supported OIDC providers, publisher registration, bundle format, Rekor transparency log, consumer-side verification, comparison with npm Trusted Publishing, Maven Central, PEP 740, Cargo RFC 3724, failure modes."
sidebar_position: 9
---

# 09. Trusted publishing: Sigstore + OIDC end-to-end

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/trusted-publishing`.

This note specifies the publish flow for MEP-57: Sigstore keyless signing bound to an OIDC identity. The "why no API tokens" rationale is in [02-design-philosophy](./02-design-philosophy) §5; per-ecosystem prior art is in [03-prior-art-registries](./03-prior-art-registries).

## 1. The trust chain

The publish flow's trust chain in three sentences:

1. The publisher's OIDC identity provider (GitHub Actions, GitLab CI, Buildkite, sigstore.dev browser flow) issues a short-lived token asserting "this build is happening on behalf of identity X for workflow Y".
2. Sigstore's Fulcio CA verifies the token against the provider's JWKS, then issues a short-lived X.509 certificate binding identity X to a one-time public key.
3. The publisher signs the SHA-256 of the tarball with the matching private key, attaches the signature + certificate + Rekor inclusion proof as the Sigstore bundle, and uploads bundle + tarball to the registry.

Verification reverses the chain: the registry (and any consumer running `mochi audit signatures`) checks the certificate's Fulcio chain, asserts the certificate's identity matches a registered publisher, verifies the signature against the tarball SHA-256, and verifies the Rekor inclusion proof against the public transparency log.

No long-lived secret exists at any point. A compromised CI run can only sign what its OIDC identity authorises, scoped to a single workflow run.

## 2. OIDC identity providers supported v1

- **GitHub Actions** (`https://token.actions.githubusercontent.com`). Subject format: `repo:owner/repo:ref:refs/tags/<tag>` or `repo:owner/repo:environment:<env>`. The subject is recorded in the published `[provenance]` block.
- **GitLab CI** (`https://gitlab.com`). Subject format: `project_path:owner/repo:ref_type:tag:ref:<tag>`.
- **Buildkite** (`https://agent.buildkite.com`). Subject format: `organization:org-slug:pipeline:pipeline-slug:build:NNN`.
- **CircleCI** (`https://oidc.circleci.com/org/<uuid>`). Subject format: `https://circleci.com/orgs/<uuid>/projects/<id>/jobs/...`.
- **Sigstore browser flow** (Google, Microsoft, GitHub) for laptop publishes.

The set is enumerable. A new CI provider requires a registry-side policy update; we treat this as a low-frequency operation.

## 3. Registering a publisher

Before a package can be published via Trusted Publishing, the package's owner registers an OIDC identity binding. This is a one-time operation per (package, identity) pair:

```
mochi publish register --package=@scope/name \
    --provider=github \
    --repo=mochilang/strings \
    --workflow=publish.yml \
    --environment=production
```

The CLI walks the user through the registry's UI flow; the registry stores the binding. Subsequent `mochi publish` operations from a matching CI run are accepted; non-matching are rejected with `M057_PUB_E001`.

For the laptop case, the user registers their personal OIDC identity:

```
mochi publish register --package=@scope/name \
    --provider=sigstore-browser \
    --email=jane@example.com
```

The registry binds the package to the Google / Microsoft / GitHub identity at email = jane@example.com. The browser flow prompts the user to log in to that provider on each publish.

## 4. The Sigstore bundle

A Sigstore bundle is a single JSON object (Sigstore Bundle v0.3 spec, GA 2024) containing:

- The signed claim (the SHA-256 of the artifact, framed as an in-toto statement v1).
- The signature (raw bytes).
- The signing certificate (PEM-encoded X.509 from Fulcio).
- The Rekor inclusion proof (Merkle audit path + log root signature).
- (Optional) the signing key's public component if not derived from the certificate.

The bundle is ~5KB for a typical signing. It is stored alongside the blob at the registry: `https://blobs.mochi.dev/<b3>.sigstore`. The index entry records the bundle URL.

Verification (Sigstore client library, cosign-compatible):

1. Parse the bundle.
2. Verify the certificate's chain against Fulcio's roots.
3. Verify the certificate's subject identity is one of the registered publishers for the package.
4. Verify the signature against the in-toto claim using the certificate's public key.
5. Verify the Rekor inclusion proof against the Rekor log root.
6. Verify the in-toto statement's subject SHA-256 matches the artifact SHA-256.

All five checks must pass. Failing any one fails the verification with the specific reason logged.

## 5. The Rekor transparency log

Rekor (Sigstore's transparency log, like Certificate Transparency for code signing) is a public Merkle log of every signing event. Properties:

- **Append-only**: entries cannot be modified.
- **Publicly verifiable**: any consumer can request an inclusion proof.
- **Witnessed**: independent witnesses periodically sign the log root, preventing the operator from forking the log.

For Mochi-57:

- Every publish writes a Rekor entry.
- Consumers verify the inclusion proof as part of bundle validation.
- A separate `mochi audit transparency` command can re-verify all locked artifacts against Rekor at audit time, catching any retroactive tampering attempts.

If Rekor is unavailable during publish, the publish fails (no offline signing). This is intentional: an unverifiable signature is not better than no signature.

## 6. Publish flow walkthrough

GitHub Actions workflow `.github/workflows/publish.yml`:

```yaml
name: Publish
on:
  push:
    tags: ['v*']

jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write   # required for OIDC
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: mochilang/setup-mochi@v1
        with:
          version: '0.7.0'
      - run: mochi build
      - run: mochi publish
```

What happens on tag push `v0.4.7`:

1. GitHub Actions starts the job. With `id-token: write`, the runner can request an OIDC token from `token.actions.githubusercontent.com`.
2. `mochi publish` reads `mochi.toml` and validates manifest, lockfile, tests, and the published tag matches the manifest version.
3. The CLI builds the tarball deterministically (zstd level 19, mtime=0, sorted tar). Computes BLAKE3 + SHA-256.
4. Requests an OIDC token from the GitHub Actions endpoint, claim audience `sigstore`.
5. Submits the token to Fulcio (`https://fulcio.sigstore.dev/api/v2/signingCert`) with a freshly generated ephemeral keypair. Fulcio verifies the token's claims (subject, audience, issuer), then issues a short-lived (10 min) X.509 certificate.
6. Signs the in-toto statement (subject = SHA-256 of tarball) with the ephemeral private key.
7. Submits the signature + certificate to Rekor (`https://rekor.sigstore.dev/api/v1/log/entries`). Rekor returns the entry's inclusion proof.
8. Assembles the Sigstore bundle.
9. POSTs the bundle + tarball to `https://index.mochi.dev/publish/<scope>/<name>/<version>`.
10. The registry verifies the bundle (steps 1-5 in §4 above) and that the OIDC subject matches a registered publisher binding for the package.
11. On success, the registry stores the blob and bundle, then appends the index entry.
12. `mochi publish` reports the index entry URL and Rekor entry index.

End-to-end time on GitHub Actions: ~30-60s, dominated by the CI orchestration not the signing itself (which is ~2s).

## 7. Verification on consumer side

`mochi audit signatures`:

1. Read the lockfile.
2. For each entry, fetch the Sigstore bundle at `<blob-url>.sigstore`.
3. Verify the bundle (steps 1-5 in §4 above).
4. Report counts: verified, unverified, mismatched.

Default `mochi fetch` does verification opportunistically:

- If the bundle is reachable, verify it inline and fail on mismatch.
- If the bundle is unreachable (offline), skip with a warning.
- If the package was published before Sigstore was required (legacy paths in v2 mirror operators), skip with a warning.

The lockfile records `sigstore_verified_count` and `sigstore_unverified` so `mochi lock --check` can fail in CI on unverified artifacts:

```toml
[provenance]
sigstore_verified_count = 28
sigstore_unverified     = []
```

A non-empty `sigstore_unverified` list fails `mochi lock --check --strict-sigstore`.

## 8. Comparison with other ecosystems

| Ecosystem    | Trusted Publishing GA | Mechanism                       | Mochi-57 borrows |
|--------------|----------------------|----------------------------------|------------------|
| npm          | April 2024           | Sigstore + GitHub/GitLab OIDC, provenance attestation | URL scheme, bundle layout |
| Maven Central| October 2024         | Sigstore + GitHub OIDC, namespace mapping | Namespace-binding to OIDC identity |
| PyPI         | November 2024 (PEP 740) | Sigstore + Trusted Publishers, attestation | Bundle storage alongside artifact |
| Cargo        | RFC #3724 accepted Aug 2024, impl 2025 | Sigstore + OIDC | Provenance binding to manifest |
| JSR          | March 2024 (GA)      | Sigstore + GitHub OIDC, every publish required | Mandatory-only publishing |
| GitHub artifacts | April 2024 (GA)  | Sigstore + GitHub OIDC, in-toto | Statement format |

MEP-57 ships all of these patterns from day one because Mochi has no legacy publish surface to maintain.

## 9. Bootstrap and the chicken-and-egg

The first Mochi packages cannot be published from CI because the CI publish workflow itself depends on Mochi. Bootstrap:

- Phase 0-12 packages: published by Mochi maintainers via the sigstore.dev browser flow, signing with Mochi maintainer GitHub identities.
- Phase 13+ packages: once the CI flow is hardened, every publish moves to CI-only.

This bootstrap is documented in the registry's onboarding docs; consumers see no difference (the bundle structure is identical).

## 10. Failure modes and responses

| Failure                                       | Response                                              |
|-----------------------------------------------|-------------------------------------------------------|
| OIDC token request fails                      | `M057_PUB_E002`: check `id-token: write` permission   |
| Fulcio rejects token                          | `M057_PUB_E003`: token claims mismatch or expired     |
| Rekor unreachable                             | `M057_PUB_E004`: cannot publish without transparency  |
| Bundle verification fails at registry         | `M057_PUB_E005`: publisher mismatch or invalid sig    |
| Publisher binding does not match              | `M057_PUB_E006`: register the workflow first          |
| Package version already exists                | `M057_PUB_E007`: bump and retry                       |
| Sigstore key rotation in progress             | `M057_PUB_E008`: retry after rotation window          |
| Consumer audit detects post-publish Rekor mismatch | `M057_AUDIT_E001`: forensic alert, do not install |

`M057_AUDIT_E001` is the canary for retroactive registry tampering. Mirrors that disagree with Rekor are surfaced via `mochi audit signatures` as `sigstore_unverified` with a `rekor_mismatch` reason.

## 11. Key rotation

Fulcio root certificate rotations are handled by the Sigstore project (TUF-managed). Consumers update their trust roots via:

- Bundling roots in the Mochi CLI release.
- Automatic update via `mochi audit roots` against `https://sigstore.dev/.well-known/sigstore`.
- Override via `MOCHI_SIGSTORE_ROOTS=<path>` for air-gapped operation.

Root rotation events have so far (2024-2026) been smooth; the TUF transition path is well-trodden.

## 12. Key-management risks

The ephemeral private key generated for each signing is held in memory for ~5 seconds. The key is not written to disk. If the runner is compromised during signing, the attacker can sign anything that runner has access to sign anyway. There is no long-lived key to exfiltrate.

A *certificate* exfiltration is similarly bounded: the certificate is valid for 10 minutes. An attacker stealing the cert can sign for 10 minutes, then the cert expires.

This 10-minute window is the residual risk; minimising CI step duration and hardening the runner are the mitigations. Compared with the historical "API token in CI env var for the lifetime of the project" model, this is a substantial improvement.

## 13. Reproducibility and provenance binding

The signed statement is an in-toto `Statement v1`:

```json
{
  "_type": "https://in-toto.io/Statement/v1",
  "subject": [{
    "name": "@scope/name@0.4.7.mochi.tar.zst",
    "digest": { "sha256": "<hex>" }
  }],
  "predicateType": "https://slsa.dev/provenance/v1",
  "predicate": {
    "buildDefinition": {
      "buildType": "https://mochi.dev/builds/v1",
      "externalParameters": { "source": "git+https://github.com/scope/name@<sha>" },
      "internalParameters": { "mochi_version": "0.7.0", "edition": "2026" }
    },
    "runDetails": {
      "builder": { "id": "https://github.com/actions/runner/v2" },
      "metadata": {
        "invocationId": "<github actions run id>",
        "startedOn": "2026-05-29T06:35:00Z"
      }
    }
  }
}
```

This is SLSA Build L3 provenance. The `subject` ties the statement to the specific artifact hash. The `predicate` records reproducible parameters; a second build with the same `externalParameters` should produce a byte-identical artifact.

## 14. Cross-references

- Rationale: [02-design-philosophy](./02-design-philosophy) §5.
- npm / PyPI / Maven Central / Cargo prior art: [03-prior-art-registries](./03-prior-art-registries) §1, §3, §4, §6.
- Manifest's `[provenance]` block: [04-manifest-format](./04-manifest-format) §7.
- Blob format and dual hash: [08-content-addressed-store](./08-content-addressed-store).
- Capability declarations enforced at publish: [10-capability-model](./10-capability-model) §5.
- Signing-related risks: [12-risks-and-alternatives](./12-risks-and-alternatives) §5.
