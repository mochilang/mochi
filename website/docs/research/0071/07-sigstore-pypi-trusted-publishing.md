---
title: "07. Sigstore and PyPI Trusted Publishing"
sidebar_position: 8
sidebar_label: "07. Sigstore + Trusted Publishing"
description: "The OIDC token exchange, the Fulcio short-lived cert, the Rekor transparency log, PEP 740 attestations, the verification path at install time, the PyPI Trusted Publishing GA timeline (GitHub Actions 2023-Q2, GitLab CI / Google Cloud / ActiveState 2024)."
---

# 07. Sigstore and PyPI Trusted Publishing

This note covers the supply-chain story for the publish direction. The goal: every Mochi-published package on PyPI has provable provenance, no static long-lived secrets are involved at any point, and consumers can verify the signature at install time.

## The supply-chain pressure that drove this

PyPI has been a continuous target for supply-chain attacks. 2024-2025 incidents include:

- **The Top.gg colorama campaign (2024)**: typosquatted `colorama` packages with stealthy info-stealer payloads downloaded millions of times.
- **The fluentd-py wave (2024)**: a sequence of typosquats targeting infrastructure tooling.
- **The PyTorch dependency-confusion incident (Dec 2022, retrospectively documented)**: a transitive dep was hijacked via name-collision on the public index.

The common factor: unsigned, untraceable uploads. There was no cryptographic record of which CI run produced which file, no transparency log, and no way for downstream consumers to verify provenance.

PyPI's response was Trusted Publishing (GA 2023-Q2) plus PEP 740 attestations (GA 2024-Q4). Trusted Publishing eliminates static API tokens; PEP 740 attaches Sigstore-signed attestations to each release. The combination gives every release a verifiable build origin.

## Trusted Publishing: the OIDC dance

The Trusted Publishing flow replaces API tokens with short-lived credentials minted from an OIDC token. The flow:

```
┌──────────────────┐                ┌──────────────────┐                ┌──────────────────┐
│  CI Workflow     │                │  OIDC Issuer     │                │  PyPI            │
│  (GitHub Actions)│                │  (GitHub token   │                │                  │
│                  │                │   endpoint)      │                │                  │
└────────┬─────────┘                └────────┬─────────┘                └────────┬─────────┘
         │                                   │                                   │
         │ 1. request OIDC token             │                                   │
         │   (audience: pypi)                │                                   │
         ├──────────────────────────────────►│                                   │
         │                                   │                                   │
         │ 2. JWT (id, repo, sha, etc.)      │                                   │
         │◄──────────────────────────────────┤                                   │
         │                                   │                                   │
         │ 3. POST /_/oidc/mint-token        │                                   │
         │    Authorization: bearer <JWT>    │                                   │
         ├───────────────────────────────────────────────────────────────────────►
         │                                   │                                   │
         │ 4. Short-lived upload token       │                                   │
         │   (valid ~15 min)                 │                                   │
         │◄───────────────────────────────────────────────────────────────────────
         │                                   │                                   │
         │ 5. POST /legacy/ <wheel + token>  │                                   │
         ├───────────────────────────────────────────────────────────────────────►
         │                                   │                                   │
         │ 6. 200 OK                         │                                   │
         │◄───────────────────────────────────────────────────────────────────────
```

The OIDC JWT issued in step 2 claims (per the GitHub OIDC schema):

- `iss`: `https://token.actions.githubusercontent.com`
- `sub`: `repo:foo/mochi-pkg-foo:ref:refs/heads/main`
- `aud`: `pypi`
- `repository`: `foo/mochi-pkg-foo`
- `repository_id`: `12345`
- `repository_owner`: `foo`
- `workflow`: `release`
- `sha`: `abcd1234...`
- `actor`: `foouser`
- `runner_environment`: `github-hosted`
- `event_name`: `release`

PyPI's `_/oidc/mint-token` endpoint validates the JWT signature against the issuer's JWKS, then checks the claims against the package's configured Trusted Publishers. The package owner has previously registered (via the PyPI web UI):

- Issuer: `https://token.actions.githubusercontent.com`
- Repository: `foo/mochi-pkg-foo`
- Workflow filename: `release.yml`
- Environment (optional): `production`

If the JWT's claims match the registered Trusted Publisher, PyPI mints an upload token (valid ~15 minutes, scoped to the specific package + version being uploaded) and returns it.

Step 5's POST uses the upload token in the standard `Authorization: Basic` legacy upload protocol. The wheel is uploaded.

## The supported OIDC issuers

PyPI accepts Trusted Publishing tokens from:

- **GitHub Actions** (GA 2023-Q2): `https://token.actions.githubusercontent.com`
- **GitLab CI/CD** (GA 2024): `https://gitlab.com` and self-hosted GitLab instances
- **Google Cloud** (GA 2024): `https://accounts.google.com`
- **ActiveState** (GA 2024): `https://platform.activestate.com/api/v1/oauth/oidc`

The Mochi CLI auto-detects the OIDC issuer by checking environment variables (`GITHUB_ACTIONS=true`, `GITLAB_CI=true`, etc.) and uses the appropriate token endpoint. The user does not configure the issuer; the CI environment determines it.

When no supported issuer is detected, the publish flow refuses with a diagnostic pointing at the configuration page. There is no fallback to legacy tokens.

## Sigstore: the keyless signing flow

PEP 740 attestations are signed via Sigstore-keyless: the signing key is ephemeral, minted from the same OIDC token, and bound to a short-lived X.509 certificate from Fulcio (Sigstore's CA). The certificate's notBefore / notAfter window is ~10 minutes; after the signature is generated, the key material is discarded.

```
┌──────────────────┐                ┌──────────────────┐                ┌──────────────────┐
│  Mochi CI runner │                │  Fulcio (CA)     │                │  Rekor (transp.  │
│                  │                │                  │                │   log)           │
└────────┬─────────┘                └────────┬─────────┘                └────────┬─────────┘
         │                                   │                                   │
         │ 1. Generate ephemeral keypair     │                                   │
         │                                   │                                   │
         │ 2. POST /api/v2/signingCert       │                                   │
         │    OIDC JWT + CSR (pub key)       │                                   │
         ├──────────────────────────────────►│                                   │
         │                                   │                                   │
         │ 3. X.509 cert (10 min validity,   │                                   │
         │    SANs: repo + workflow)         │                                   │
         │◄──────────────────────────────────┤                                   │
         │                                   │                                   │
         │ 4. Sign each wheel + attestation  │                                   │
         │    bundle with ephemeral key      │                                   │
         │                                   │                                   │
         │ 5. POST /api/v1/log/entries       │                                   │
         │    signed entry (cert + sig)      │                                   │
         ├───────────────────────────────────────────────────────────────────────►
         │                                   │                                   │
         │ 6. Inclusion proof + log index    │                                   │
         │◄───────────────────────────────────────────────────────────────────────
         │                                   │                                   │
         │ 7. Bundle: cert + sig + proof +   │                                   │
         │    log index                      │                                   │
         │                                   │                                   │
```

The Rekor log entry is the public transparency record. Any third party can query Rekor (`https://rekor.sigstore.dev/api/v1/log/entries?logIndex=N`) and verify the entry exists. Sigstore's design relies on the log's append-only property: once an entry is logged, an attacker cannot retroactively forge a signature.

## PEP 740 attestations

PEP 740 defines the attestation format embedded in the wheel's dist-info:

```
mochi_pkg-1.2.3.dist-info/attestations.json
```

The file contains a list of in-toto attestation envelopes. Each envelope has:

- `payloadType`: `application/vnd.in-toto+json`
- `payload`: base64-encoded in-toto Statement
- `signatures`: list of `{ keyid, sig }`

The in-toto Statement has:

- `_type`: `https://in-toto.io/Statement/v1`
- `subject`: the artifacts (wheel filename + SHA256)
- `predicateType`: `https://docs.pypi.org/attestations/publish/v1`
- `predicate`: the build provenance (repo, commit, workflow, etc.)

The predicate matches the OIDC JWT claims that minted the certificate. Consumers can verify:

1. The certificate chains to Fulcio's root.
2. The certificate's SAN matches the repo/workflow that produced the wheel.
3. The Rekor inclusion proof is valid.
4. The signature over the wheel's SHA256 is valid.
5. The OIDC issuer matches the package's configured Trusted Publisher.

If all checks pass, the wheel is provably built by the registered CI workflow.

## Verification at install time

uv (and pip with `--require-attestations` planned 2025-Q3) verify attestations at install time:

```
$ uv pip install mochi-pkg-foo --require-attestations
Resolving mochi-pkg-foo>=1.0 ...
Downloading mochi_pkg_foo-1.2.3-py3-none-any.whl ...
Verifying attestation: cert valid, Rekor proof valid, signature valid.
Trusted publisher: GitHub Actions, repo foo/mochi-pkg-foo, workflow release.yml, sha abcd1234.
Installed mochi-pkg-foo 1.2.3
```

The Mochi-side equivalent runs the same verification when `mochi pkg lock` downloads the wheel. The verification result is recorded in mochi.lock as `attestation-provenance` so `mochi pkg lock --check` can re-verify.

When attestations are missing, the install proceeds with a warning. When attestations fail (cert expired in the Rekor log post-revocation, signature mismatch), the install aborts with a diagnostic.

## Cosign integration

The Sigstore-side tooling is `cosign` (the canonical CLI) and `sigstore-python` (the Python library). Mochi's publish path embeds the sigstore-python library; it does not shell out to the cosign CLI. Reasons:

- Sigstore-python is the reference implementation maintained by the Sigstore project and the PyPI authors.
- Embedding avoids a runtime dependency on a separate binary.
- The Python library exposes the in-toto envelope format directly, which is what PEP 740 expects.

## The PyPI Trusted Publishing timeline

| Date | Event |
|------|-------|
| 2023-Q2 | Trusted Publishing GA for GitHub Actions. |
| 2023-Q4 | Initial PEP 740 draft circulated. |
| 2024-Q1 | Trusted Publishing expanded to GitLab CI/CD. |
| 2024-Q2 | Trusted Publishing expanded to Google Cloud and ActiveState. |
| 2024-Q4 | PEP 740 GA: PyPI accepts and serves attestations. |
| 2025-Q3 | uv enables `--require-attestations` for default verification. (Planned.) |
| 2026-Q1 | Pip enables attestation verification by default. (Planned per the typing community SC notes.) |

MEP-71 ships with attestation generation enabled by default from day one. The verification side runs when consuming any PyPI package that ships attestations.

## Edge cases and refusals

| Case | Behaviour |
|------|-----------|
| Self-hosted GitHub Enterprise runners | Supported via custom OIDC issuer URL configuration. |
| Self-hosted runners with no OIDC issuer | Publish refused with `M071_PUBLISH_E001_NoOIDC`. |
| Fulcio cert expired before Rekor log entry | Sign retries once; if still failing, abort. |
| Rekor down | Sign retries with exponential backoff up to 3 attempts; if still failing, abort. |
| OIDC token replay (same token reused) | PyPI rejects with `409 Conflict`. The Mochi CLI surfaces a diagnostic suggesting to re-run the workflow. |
| PyPI registered Trusted Publisher mismatch | Mint-token endpoint returns `403`; Mochi surfaces the configured publisher vs the JWT claims. |
| Private PyPI mirror without OIDC | Publish to private index is a separate code path that Mochi-71 does not enable; the public PyPI path is OIDC-only. |

## Cross-references

- [[02-design-philosophy]] §5 for why this is the only path.
- [[06-pypi-publish-flow]] for the upload protocol context.
- [[12-risks-and-alternatives]] §R7 for the OIDC-issuer-downtime risk.
- [PEP 740](https://peps.python.org/pep-0740/) for attestations.
- [PyPI Trusted Publishing docs](https://docs.pypi.org/trusted-publishers/).
- [Sigstore docs](https://docs.sigstore.dev/).
- [in-toto attestation spec](https://github.com/in-toto/attestation).
