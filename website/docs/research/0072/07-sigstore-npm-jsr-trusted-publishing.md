---
title: "MEP-72 Note 07: Sigstore on npm and JSR"
sidebar_position: 8
sidebar_label: "07. Sigstore"
description: "The Sigstore-keyless OIDC Trusted Publishing infrastructure on both npm and JSR: the OIDC token exchange model, the npm Sigstore attestation format, the JSR Sigstore attestation format, the `actions/attest-build-provenance@v2` integration, the consumer-side verification path via `npm audit signatures`."
---

# 07. Sigstore on npm and JSR

This note describes the Sigstore-keyless OIDC Trusted Publishing flow on both npm and JSR, and how MEP-72 integrates with it. It is informative.

## 1. The OIDC token exchange model

Trusted Publishing on both npm and JSR uses the same overall pattern (modelled on PyPI's PEP 740, Cargo RFC #3724, and Maven Central):

1. A GitHub Actions workflow runs with `permissions: id-token: write`.
2. The workflow requests an OIDC token from the GitHub Actions OIDC provider (`https://token.actions.githubusercontent.com`). The token's claims include `iss`, `aud`, `repository`, `workflow`, `workflow_ref`, `event_name`, `ref`, `sha`.
3. The workflow submits the OIDC token to the registry's Trusted-Publishing endpoint as part of the `npm publish` (npm) or `deno publish` (JSR) call.
4. The registry verifies the OIDC token's signature against the GitHub OIDC public key, checks the token's claims against the registry-side pre-registered Trusted Publisher binding (which the publisher set up via the registry's UI), and mints a short-lived publish credential.
5. The registry signs the package artefact via Sigstore Fulcio (which mints a short-lived signing cert from the OIDC identity), records the signature in Sigstore's Rekor transparency log, and posts the attestation to the registry's metadata.

The trust boundary is the pre-registered Trusted Publisher binding (the registry's UI configuration that says "this GitHub repo's workflow at this ref is allowed to publish this package"). If a forked PR or a malicious workflow tries to publish, the OIDC token's claims do not match the binding and the registry refuses.

## 2. The npm attestation format

npm publishes Sigstore attestations as `<pkg>-<version>.sigstore` bundles alongside the tarball. The bundle is a Sigstore-bundle-protocol blob containing:

- The Fulcio signing certificate (with the OIDC-derived identity claim in the certificate's SAN).
- The signature over the tarball's SHA-512.
- The Rekor log entry's inclusion proof.

Consumer-side verification via `npm audit signatures`:

```bash
$ npm audit signatures
audited 142 packages in 3.2s
fully signed: 38 / 142
unsigned: 104 / 142
mismatch: 0 / 142
```

The mismatch count is the only one that fails the audit; un-signed packages (the majority in May 2026; trending toward `--provenance` adoption) emit a warning but do not fail.

## 3. The JSR attestation format

JSR's attestation lives in the registry's metadata document at `https://jsr.io/@<scope>/<pkg>/@<version>/meta.json`. The metadata includes:

- The Sigstore bundle base64-encoded.
- The OIDC-derived publisher identity claim.
- The tarball's BLAKE3 hash.

JSR-side verification is automatic: every `deno add jsr:@scope/pkg` validates the attestation against the public Sigstore root of trust, in the same way `npm audit signatures` validates npm attestations.

## 4. `actions/attest-build-provenance@v2`

The GitHub Actions Attestations API (October 2024 GA) provides a second-tier attestation for arbitrary build artefacts (not just registry-published packages). The MEP-52 Phase 18 workflow uses it for the browser bundle:

```yaml
- name: Attest browser bundle
  if: startsWith(github.ref, 'refs/tags/')
  uses: actions/attest-build-provenance@v2
  with:
    subject-path: dist/browser/dist/bundle/index.js
```

This records a SLSA-style attestation in GitHub's Attestations log for the bundle artefact, so consumers downloading the bundle from a release page (rather than from a package registry) can verify provenance.

## 5. The lockfile-side attestation record

MEP-72's `[[npm-package]]` and `[[jsr-package]]` lockfile entries record:

- `sigstore-attested = true | false`: whether an attestation was found.
- `sigstore-bundle-hash = "..."`: SHA-256 of the bundle.

`mochi pkg lock --check` re-fetches the bundle (over HTTPS, with caching) and verifies the hash matches the lockfile. A drift indicates the publisher re-published the version (rare; usually a sign of a key rotation or a re-attestation after a breaking change to the Sigstore root of trust).

## 6. The `--sigstore-required` gate

The strict mode (`mochi pkg lock --sigstore-required`) refuses to lock a version that lacks an attestation. This is the recommended mode for security-sensitive projects.

Default behaviour: warn and accept un-attested versions. Rationale: most npm packages published before April 2024 have no attestation; refusing them by default would make the bridge unusable for projects with large legacy dep graphs.

## 7. Cross-references

- [[06-npm-jsr-publish-flow]] — the publish flow details.
- [MEP-52 Phase 18 implementation tracking](/docs/implementation/0052/phase-18-trusted-publishing) — the workflow emit.
- [Sigstore project documentation](https://www.sigstore.dev/) — the upstream protocol.
