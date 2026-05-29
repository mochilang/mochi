---
title: "07. OIDC trusted publishing"
sidebar_position: 8
sidebar_label: "07. OIDC trusted publishing"
description: "The Hex.pm trusted publishing flow (launched 2024), the JWT claim requirements from GitHub Actions, the id-token: write permission, HEX_API_KEY fallback prohibition, and the mock-hex harness for integration tests."
---

# 07. OIDC trusted publishing

## Background

Hex.pm launched Hex Trusted Publishing in 2024. The mechanism follows the pattern pioneered by PyPI (PEP 740), RubyGems.org (2023), npm Provenance (2023), and crates.io (RFC #3724, 2025): a CI job requests a short-lived OIDC token from the CI provider's OIDC endpoint, presents it to the package registry, and the registry exchanges it for a short-lived publishing credential. No long-lived API key is stored in CI secrets.

## GitHub Actions flow

```yaml
jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write      # required for OIDC token
      contents: read
    steps:
      - uses: actions/checkout@v4
      - run: mochi pkg publish --to=hex.pm
```

The `id-token: write` permission allows the job to call the GitHub Actions OIDC endpoint (`ACTIONS_ID_TOKEN_REQUEST_URL`) with the request token (`ACTIONS_ID_TOKEN_REQUEST_TOKEN`). The response is a signed JWT with the following claims relevant to Hex.pm:

| Claim | Example value | Meaning |
|-------|--------------|---------|
| `iss` | `https://token.actions.githubusercontent.com` | OIDC issuer |
| `sub` | `repo:owner/repo:ref:refs/heads/main` | Subject |
| `repository` | `owner/repo` | GitHub repository |
| `ref` | `refs/heads/main` or `refs/tags/v1.0.0` | Branch or tag |
| `workflow` | `.github/workflows/publish.yml` | Workflow file path |
| `actor` | `tamnd` | GitHub username |

Hex.pm validates the JWT against the OIDC issuer's public keys (fetched from `https://token.actions.githubusercontent.com/.well-known/openid-configuration`), checks the `sub` claim against the package's trusted publisher configuration (set up once in the Hex.pm UI or API), and issues a short-lived Hex.pm API token scoped to publishing exactly one package version.

## Trusted publisher configuration

Before the first publish, the user registers the trusted publisher on Hex.pm. This requires:

1. A Hex.pm account with ownership of the package name.
2. Calling `POST https://hex.pm/api/packages/<name>/releases/trusted_publishers` with the GitHub Actions OIDC configuration:
   ```json
   {
     "repository": "owner/repo",
     "workflow": ".github/workflows/publish.yml",
     "environment": null
   }
   ```

The `mochi pkg publish --to=hex.pm --setup-oidc` subcommand automates this one-time setup step, prompting for Hex.pm credentials locally and writing the trusted publisher record to the registry.

## Token exchange sequence

```
GitHub Actions job
  -> OIDC_REQUEST_URL?audience=hex.pm
  <- signed JWT

Bridge (package3/erlang/publish/)
  -> POST https://hex.pm/api/auth/oidc
     { "jwt": "<jwt>" }
  <- { "api_key": "<short-lived-key>", "expires_at": "<iso8601>" }

Bridge
  -> POST https://hex.pm/api/packages/<name>/releases
     Authorization: <short-lived-key>
     Body: <tarball bytes>
  <- { "url": "...", "checksum": "..." }
```

The short-lived key has a TTL of 15 minutes and is scoped to publishing one version of one package. It is never written to disk.

## HEX_API_KEY prohibition

The `HEX_API_KEY` environment variable is not checked by the bridge. If set, it is ignored. The rationale is identical to MEP-73's rejection of `CARGO_REGISTRY_TOKEN` and MEP-76's rejection of `GEM_HOST_API_KEY`: long-lived registry tokens are the primary supply-chain attack vector for package ecosystem compromise. The bridge was designed after those incidents and does not provide the unsafe path.

Users who need to publish from a non-OIDC environment (e.g., a local workstation) have two options:
1. Configure their CI to run the publish step (the recommended path, keeps secrets out of local environments).
2. Use `mochi pkg publish --dry-run` to produce the tarball and upload it manually via `rebar3 hex publish` with a local Hex.pm user API key (the user's responsibility, not the bridge's).

## mock-hex harness

Integration tests for the publish flow use a `mock-hex` harness (`package3/erlang/testutil/mock_hex.go`) that implements the relevant Hex.pm API endpoints locally:

- `POST /api/auth/oidc`: accepts any JWT and returns a mock API key.
- `POST /api/packages/:name/releases`: records the upload and returns a mock response with a computed checksum.

The `--dry-run` flag also exercises the mock-hex harness in CI, ensuring the signing flow is tested without contacting the real registry.

## Other CI providers

| Provider | OIDC endpoint variable | Notes |
|----------|----------------------|-------|
| GitHub Actions | `ACTIONS_ID_TOKEN_REQUEST_URL` | Primary supported path |
| GitLab CI | `CI_JOB_ID_TOKEN_REQUEST_URL` | Supported; different JWT claims |
| Buildkite | Not yet supported | Planned in MEP-66 N.2 |
| CircleCI | `CIRCLE_OIDC_TOKEN` | Pre-generated token; supported with validation |

## Cross-references

- [[06-hex-publish-flow]] for the tarball construction that precedes the OIDC exchange.
- [[12-risks-and-alternatives]] §R5 for the OIDC API stability risk.
- [MEP-66 §9](/docs/mep/mep-0066#9-publish-flow-direction-2) for the normative publish flow.
