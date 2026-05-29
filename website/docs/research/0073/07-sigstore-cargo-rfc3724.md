---
title: "07. Sigstore and Cargo RFC #3724"
sidebar_position: 8
sidebar_label: "07. Trusted publishing"
description: "The OIDC token exchange flow, the Fulcio short-lived signing cert, the Rekor transparency log entry, the crates.io trusted-publishing endpoint, the verification path at install time, and the crates.io GA rollout timeline through 2026."
---

# 07. Sigstore and Cargo RFC #3724

This note covers the trusted-publishing flow MEP-73 uses for crates.io publishes. The flow matches MEP-57's broader Sigstore-keyless OIDC publishing principle.

## The Sigstore stack

Sigstore is a Linux Foundation project that ships three coordinated components:

- **Fulcio**: a certificate authority that issues short-lived (10-minute) signing certificates bound to an OIDC identity claim. Public-good instance at `fulcio.sigstore.dev`.
- **Rekor**: an append-only transparency log of signature events. Public-good instance at `rekor.sigstore.dev`.
- **Cosign**: a signing tool that orchestrates Fulcio + Rekor for container images and arbitrary blobs.

The keyless flow: the signer presents an OIDC token to Fulcio, Fulcio verifies the token, Fulcio issues a short-lived signing cert with the OIDC claim embedded as a SAN (Subject Alternative Name), the signer signs the artefact with the cert's private key, the signer submits the (signature, cert, artefact-digest) bundle to Rekor, Rekor returns a log entry index, the signer discards the private key. Verification: the verifier fetches the Rekor entry, validates the cert chain back to Fulcio's root, validates the OIDC claim matches the expected identity (e.g., "this crate must be signed by GitHub Actions for repo example/mochi-example on the main branch").

The cert lifetime (10 minutes) is the load-bearing security property: even if the private key leaks, it is useless 10 minutes after issuance. The OIDC binding ties signing authority to a specific CI identity, which is far harder to compromise than a long-lived API token sitting on a maintainer's laptop.

## Cargo RFC #3724

Cargo RFC #3724 (accepted Q4 2025, GA rolling through 2026) brings Sigstore-keyless trusted publishing to crates.io. The flow:

1. The publisher (a CI workflow with `permissions: id-token: write` on GitHub Actions, or equivalent on other CI) requests an OIDC token from its CI environment scoped to the audience `crates.io`.
2. The publisher sends the OIDC token plus the `.crate` tarball plus the crates.io metadata to `https://crates.io/api/v1/crates/new` with the header `Authorization: Sigstore <bundle>`.
3. crates.io's server-side handler:
   - Validates the OIDC token against the expected issuer (e.g., GitHub Actions' OIDC issuer URL).
   - Extracts the OIDC claim's `sub` field (e.g., `repo:example/mochi-example:ref:refs/tags/v0.1.0`).
   - Looks up the crate's trusted-publisher config (set by the crate owner via the crates.io web UI: "this crate may be published only by repo example/mochi-example on tags matching v*").
   - Asserts the OIDC claim matches the trusted-publisher config.
   - Requests a Fulcio cert bound to the OIDC claim.
   - Signs the tarball with the Fulcio-issued cert.
   - Writes a Rekor log entry recording the publish event.
   - Appends to the sparse index.
   - Returns 200 with the Rekor log entry index.

The publisher never sees the Fulcio cert directly; the signing happens server-side at crates.io, bound to the publisher's OIDC identity. This is the simpler "managed signing" variant of the Sigstore flow (PyPI PEP 740 uses the same model).

## OIDC issuer configuration

Trusted publishing on crates.io requires the crate owner to declare the OIDC issuer + claim shape on the crates.io web UI. Example configuration for a GitHub Actions workflow:

```
Issuer: https://token.actions.githubusercontent.com
Repository: example/mochi-example
Workflow: .github/workflows/release.yml
Environment: (optional) production
Tag pattern: v*
```

The OIDC `sub` claim must then match `repo:example/mochi-example:ref:refs/tags/v*` (the GitHub-Actions-issued claim shape). crates.io rejects publishes from any other OIDC identity.

The bridge generates this config in `.github/workflows/release.yml` automatically when the user runs `mochi pkg publish --to=crates.io --emit-ci`:

```yaml
name: Release
on:
  push:
    tags: ['v*']
jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: mochilang/setup-mochi@v1
      - run: mochi pkg publish --to=crates.io
```

## Verification at install time

When a downstream Rust user runs `cargo install mochi-example`, the verification flow:

1. cargo fetches the crate tarball from `https://static.crates.io/crates/mochi-example/mochi-example-0.1.0.crate`.
2. cargo fetches the Rekor entry index for the publish event from crates.io's index metadata.
3. cargo (if configured with `[install] verify = "sigstore"` in `~/.cargo/config.toml`) fetches the Rekor log entry, verifies the cert chain, asserts the OIDC claim matches the user's trust policy.
4. cargo extracts the tarball and proceeds with the install.

For Mochi-side verification (a downstream Mochi user running `mochi pkg add rust mochi-example`), the bridge performs the same verification at lock time:

```
$ mochi pkg add rust mochi-example@^0.1
[1/4] Resolving versions ... mochi-example@0.1.0
[2/4] Downloading .crate from https://static.crates.io/ ... 145 KB
[3/4] Verifying Sigstore bundle ...
        Rekor index: 12345678
        Fulcio cert SAN: repo:example/mochi-example:ref:refs/tags/v0.1.0
        Trust policy: any OIDC subject (default)
        Verification: OK
[4/4] Running rustdoc-JSON ingest ... 78 public items, 12 skipped
```

The user can set a stricter trust policy in `mochi.toml`:

```toml
[rust-dependencies.mochi-example]
version = "^0.1"
trust = { issuer = "https://token.actions.githubusercontent.com", subject = "repo:example/mochi-example:ref:refs/tags/v*" }
```

With this set, the verification step asserts the OIDC claim matches the declared trust policy. A publish from a different identity (e.g., a compromised maintainer token) would fail verification and the lock would error.

## GA rollout timeline

The crates.io trusted-publishing rollout per the Q4 2025 announcement:

| Date | Milestone |
|------|-----------|
| 2025-12 | Accepted as Cargo RFC #3724 |
| 2026-Q1 | Limited beta: opt-in via `[trusted-publishing]` in crates.io account settings; coexists with legacy `CARGO_REGISTRY_TOKEN`. |
| 2026-Q2 | Public beta: any crate owner can configure; legacy tokens still accepted. |
| 2026-Q3 | GA: trusted publishing is the recommended default; legacy tokens deprecated (still accepted, but with a deprecation warning). |
| 2026-Q4 | Legacy token publish requires explicit opt-in via `cargo publish --legacy-token` flag. |
| 2027-Q1 | Legacy tokens removed (target; subject to GA feedback). |

MEP-73 ships supporting only trusted publishing in the v1 release (expected 2026-Q3 alignment with crates.io GA). The `--allow-token-fallback` flag exists for the transition period; it is removed once crates.io's legacy path is removed.

## The sigstore-mock-fulcio harness

CI runs of `mochi pkg publish --to=crates.io --dry-run` use a local Sigstore mock to test the signing flow without touching the public Fulcio or Rekor. The mock is a Go service in `pkg/pkgsign/mock/` that:

- Accepts an OIDC-like token in any shape.
- Issues a deterministic Fulcio-shaped cert with the claim's `sub` embedded as a SAN.
- Records the signing event in an in-memory Rekor-shaped log.
- Returns a deterministic log entry index.

The mock's deterministic outputs make CI runs reproducible. The MEP-57 fixture for the broader OIDC flow (see [/docs/research/0057/09-trusted-publishing/](https://github.com/mochilang/mochi/blob/main/website/docs/research/0057/09-trusted-publishing.md)) reuses this mock.

## Cross-references

- [[06-cargo-publish-flow]] for what gets uploaded.
- [[02-design-philosophy]] §5 for why long-lived tokens are rejected.
- [MEP-57](/docs/mep/mep-0057) for the broader Sigstore-keyless principle.
- [Cargo RFC #3724](https://github.com/rust-lang/rfcs/pull/3724) for the normative protocol.
