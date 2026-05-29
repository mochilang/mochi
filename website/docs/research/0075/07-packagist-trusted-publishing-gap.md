---
title: "07. Packagist trusted-publishing gap"
sidebar_position: 8
sidebar_label: "07. Packagist trust gap"
description: "Deep dive on the Packagist trusted-publishing gap: comparison with npm Trusted Publishing, PyPI OIDC, and crates.io RFC #3724; supply-chain risks the gap enables; Packagist's current mitigations; what a hypothetical OIDC-trusted Packagist would look like; the MEP-75 workaround; and the roadmap for when Packagist ships OIDC."
---

# 07. Packagist trusted-publishing gap

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note documents the supply-chain gap that distinguishes MEP-75 from MEP-73 (Rust) and MEP-74 (Go): Packagist does not support OIDC trusted publishing as of May 2026. This note analyses the risk, compares with ecosystems that have solved it, describes the best available mitigation, and outlines the roadmap.

## 1. What trusted publishing means

"Trusted publishing" (the term popularised by PyPI's PEP 740 implementation) refers to a model where:

1. A CI workflow (e.g., GitHub Actions) requests a short-lived OIDC token from the CI provider.
2. The token encodes the workflow identity: which repository, which branch/tag, which workflow file.
3. The package registry (PyPI, crates.io, npm) verifies the OIDC token against the expected identity.
4. The registry issues a short-lived upload credential bound to the token's identity.
5. The workflow uploads the package using the short-lived credential.

The critical property: no long-lived secret is stored. The CI environment's OIDC token is ephemeral (minutes lifetime); a compromised token cannot be used outside the originating workflow run. An attacker who steals the token from a CI log cannot replay it.

Four major package registries reached trusted publishing between April 2024 and Q4 2025:

| Registry | Standard | GA date |
|---------|---------|---------|
| npm (npmjs.org) | GitHub Actions OIDC | April 2024 |
| Maven Central (Sonatype) | Sigstore keyless | October 2024 |
| PyPI (python.org) | PEP 740 (GitHub/GitLab OIDC) | Late 2025 |
| crates.io (rust) | Cargo RFC #3724 (Sigstore keyless) | Rolling GA through 2026 |

Packagist has no equivalent as of May 2026. The Packagist roadmap (as discussed in the Packagist GitHub issues and in composer/composer discussions as of Q1 2026) lists OIDC trusted publishing as a future goal but has no committed GA date.

## 2. What the gap enables (risk analysis)

Without OIDC trusted publishing, Packagist publish requires a long-lived API token stored in CI secrets. Supply-chain risks:

**Token theft.** A developer stores `PACKAGIST_API_TOKEN` in a GitHub Actions secret. An attacker who gains access to the GitHub repository (via a compromised collaborator account, a malicious PR with CI access, or a GitHub token exfiltration) can read the secret and publish a malicious package version.

**Abandoned package takeover.** Packagist has a package transfer mechanism (maintainer inactivity, abandoned status). An API token from an abandoned maintainer account can be used to publish to the original package namespace until the transfer is processed. This is analogous to the npm "left-pad" and npm account takeover patterns (the npm Trusted Publishing system was partly motivated by this class of attack).

**Comparison with real incidents:**

- The **event-stream** incident (npm, 2018): a popular package was handed off to a malicious new maintainer who published a version with a backdoor. The long-lived npm token was the enabler.
- The **xz-utils** incident (GitHub, 2024): a social engineering campaign against a single maintainer. A long-lived token stored on the maintainer's machine was the trust anchor. OIDC trusted publishing, if it had existed and been required, would have required the attacker to also compromise the CI workflow (a harder target).
- **Packagist-specific risks**: Packagist's package claim/transfer process has been the subject of security research. The 2022 Packagist security audit (published by Packagist's maintainers) identified several potential token-abuse vectors.

The PHP ecosystem has not had a high-profile supply-chain incident at the Packagist level as of May 2026, but the structural risk is the same as npm pre-Trusted-Publishing.

## 3. Packagist's current mitigations

Packagist provides several mitigations that reduce (but do not eliminate) the token-theft risk:

**Two-factor authentication (2FA).** Packagist requires 2FA for maintainers of packages with more than a threshold download count. 2FA protects the account but not the API token itself (which is separate from the account login).

**API token scoping.** Packagist API tokens are scoped to specific packages (as of 2024). A token scoped to `my-vendor/my-package` cannot publish to `other-vendor/other-package`. This is a meaningful reduction in blast radius.

**Package takeover policy.** Packagist has a documented process for claiming abandoned packages. The process requires evidence of abandonment and a review period. This reduces (but does not eliminate) the risk of opportunistic takeovers.

**GitHub App webhook.** The GitHub App integration allows Packagist to crawl repositories in response to webhook events, reducing the need to store API tokens in CI secrets for ongoing releases. However, the initial registration still requires an API token.

None of these mitigations provide the same level of protection as OIDC trusted publishing, which eliminates long-lived secrets from the publish path entirely.

## 4. MEP-75's workaround stack

MEP-75 applies three layers of mitigation:

**Layer 1: GPG-signed git tags.** Every release tag is GPG-signed. A downstream consumer who verifies the GPG signature can confirm that the tag was created by the holder of the package author's signing key. A compromised Packagist API token does not allow the attacker to push a valid GPG-signed tag (they would need the private key).

Limitation: GPG signing is not enforced by Packagist. A compromised API token can trigger a crawl of any unsigned or differently-signed tag on the repository. GPG signing protects consumers who verify, not the Packagist discovery mechanism.

**Layer 2: Sigstore attestation.** The Sigstore `actions/attest-build-provenance@v1` action creates a transparency-log entry (Rekor) for the dist zip, bound to the CI workflow identity. A downstream consumer using `gh attestation verify` can confirm that the published artifact was produced by the expected GitHub Actions workflow.

Limitation: Packagist does not read or verify Sigstore attestations. The attestation is for independent downstream verification, not for Packagist itself.

**Layer 3: GitHub App integration.** Using the Packagist GitHub App eliminates the API token from the ongoing publish step. The App token is managed by GitHub; the user does not store it in CI secrets.

Limitation: the initial registration and any manual Update API ping still require an API token.

**Combined assessment:** the three layers together provide a credible supply-chain story for consumers who verify. They do not prevent a bad actor with an API token from pushing a tag and triggering a Packagist crawl. This is an acknowledged residual risk.

## 5. What OIDC trusted publishing would look like on Packagist

A hypothetical OIDC trusted publishing implementation for Packagist would work as follows (extrapolating from PyPI's PEP 740 and crates.io RFC #3724):

1. The package maintainer registers a "trusted publisher" on packagist.org: `repository: github.com/example/my-mochi-lib`, `workflow: release.yml`, `environment: production`.
2. In the CI workflow, the `mochi pkg publish --to=packagist` step requests an OIDC token from GitHub Actions (the `id-token: write` permission).
3. The tool presents the OIDC token to a Packagist OIDC endpoint (`https://packagist.org/api/oidc/exchange`).
4. Packagist verifies the token's `sub` claim against the registered trusted publisher configuration.
5. Packagist issues a short-lived upload credential (a JWT valid for ~10 minutes).
6. The tool uses the short-lived credential to trigger the Packagist crawl.
7. No long-lived secret is stored anywhere.

This model matches PyPI's PEP 740 almost exactly. The Packagist maintainers (Nils Adermann and Jordi Boggiano) have stated publicly that this is on the roadmap. The main blockers as of May 2026 appear to be engineering capacity and the need to coordinate with the `composer` CLI team.

MEP-75 will add a `mochi pkg publish --to=packagist --oidc` path once Packagist ships OIDC. The existing API-token path will be deprecated but not removed immediately.

## 6. Comparison table

| Feature | PyPI (PEP 740) | npm Trusted Publishing | crates.io (RFC #3724) | Packagist (May 2026) |
|---------|---------------|----------------------|----------------------|----------------------|
| OIDC trusted publishing | GA (late 2025) | GA (April 2024) | Rolling GA (2026) | Not available |
| Long-lived token required | No | No | No (fallback: yes) | Yes |
| Sigstore attestation | Yes (PEP 740) | Yes (provenance) | Yes (RFC #3724) | No (can add manually) |
| Transparency log | Rekor | Rekor | Rekor | None |
| GitHub App integration | No | No | No | Yes (partial mitigation) |
| Signed artifact | Yes (via Sigstore) | Yes (via Sigstore) | Yes (via Sigstore) | GPG tag only (user-managed) |

## 7. Roadmap

MEP-75's publish direction is designed to be forward-compatible with native Packagist OIDC:

- The `[php.publish]` table will gain `oidc-publisher = { repository = "...", workflow = "..." }` when Packagist ships OIDC.
- The `mochi pkg publish --to=packagist --oidc` flag will use the OIDC path when available.
- The `apiToken` field and `$PACKAGIST_API_TOKEN` environment variable will be deprecated (with a warning) once OIDC GA is reached.
- The GPG signing and Sigstore attestation steps will remain regardless of OIDC status; they are independently useful.

## Cross-references

- [[06-composer-publish-flow]] for the publish flow mechanics.
- [[02-design-philosophy]] §5 for the design rationale.
- [MEP-73 research/07](/docs/research/0073/07-sigstore-cargo-rfc3724) for crates.io's RFC #3724 implementation.
- [MEP-75 §Alternatives](/docs/mep/mep-0075#alternatives-considered) §8 for why MEP-75 does not block on Packagist OIDC.
