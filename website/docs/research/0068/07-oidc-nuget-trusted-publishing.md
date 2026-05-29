---
title: "07. OIDC and NuGet trusted publishing"
sidebar_position: 8
sidebar_label: "07. OIDC and NuGet trusted publishing"
description: "The GitHub Actions OIDC token, the nuget.org trusted publisher endpoint (GA November 2024), the token exchange flow, GitLab CI and Azure Pipelines paths, the dry-run mock server harness, and the --allow-apikey-fallback transition flag."
---

# 07. OIDC and NuGet trusted publishing

NuGet trusted publishing (also called "keyless publishing" or "API key-free publishing") is the mechanism by which a CI pipeline can publish to nuget.org without storing a long-lived API key as a secret. It uses OpenID Connect (OIDC) identity tokens issued by the CI provider, verified by nuget.org against a registered publisher configuration.

## Background: why trusted publishing matters

Traditional NuGet publishing requires a long-lived API key (`NUGET_API_KEY`):

```sh
dotnet nuget push MyPackage.1.0.0.nupkg --api-key $NUGET_API_KEY --source https://api.nuget.org/v3/index.json
```

These keys are 72-character random tokens that grant push access to the package owner's account for all packages (or a scoped subset). They are typically stored as CI secrets, rotated infrequently, and revoked only after a compromise is detected. The 2023-2024 NuGet typosquatting incident (where attackers registered lookalike package names and injected malicious DLLs) involved stolen or guessed API keys in several cases. The npm event-stream supply-chain incident (2018) and the PyPI reflected-string flood (March 2025) both trace to compromised long-lived tokens.

NuGet trusted publishing eliminates the API key entirely:

1. The CI pipeline requests a short-lived OIDC token from the CI provider (GitHub Actions, GitLab CI, Azure Pipelines).
2. The token is presented to nuget.org, which verifies it against the package's registered trusted-publisher configuration.
3. nuget.org exchanges the OIDC token with the CI provider's JWKS endpoint to validate the signature.
4. If the token's claims (`repository`, `workflow`, `environment`, `ref`) match the registered configuration, nuget.org accepts the push and records the publish in the Rekor transparency log (via Sigstore Fulcio).
5. The short-lived token (TTL typically 5-10 minutes) cannot be used again or stolen for future pushes.

## GitHub Actions OIDC token

In a GitHub Actions workflow, `id-token: write` grants the job access to the OIDC provider:

```yaml
jobs:
  publish:
    runs-on: ubuntu-latest
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - run: mochi pkg publish --to=nuget.org
```

The `mochi pkg publish` command requests an OIDC token from the GitHub Actions token endpoint (`ACTIONS_ID_TOKEN_REQUEST_URL` + `ACTIONS_ID_TOKEN_REQUEST_TOKEN`). The token JWT contains claims:

```json
{
  "iss": "https://token.actions.githubusercontent.com",
  "aud": "api://AzureADTokenExchange",
  "sub": "repo:MyOrg/MyRepo:ref:refs/heads/main",
  "repository": "MyOrg/MyRepo",
  "repository_owner": "MyOrg",
  "workflow": "release.yml",
  "job_workflow_ref": "MyOrg/MyRepo/.github/workflows/release.yml@refs/heads/main",
  "ref": "refs/heads/main",
  "sha": "abc123...",
  "exp": 1748520000,
  "iat": 1748519700
}
```

The `sub` claim is the primary identity. nuget.org's trusted publisher configuration stores the expected `repository`, `workflow`, and optionally `environment` values. The token is used once for one push; expired tokens (> 10 minutes old) are rejected.

## nuget.org trusted publisher registration

A one-time setup is required on nuget.org (or via the NuGet CLI for automation):

1. Log in to nuget.org, navigate to the package's "Manage package" page.
2. Under "Trusted Publishers", click "Add GitHub Actions publisher".
3. Enter the repository owner, repository name, workflow file name, and (optionally) the environment name and branch/tag constraint.

After registration, any push from the registered workflow and repository is accepted without an API key.

For new packages (first publish), nuget.org allows trusted publishing with `--new-package-owner` claim in the OIDC token, which automatically creates the package entry under the authenticated user's account.

## Token exchange flow (bridge implementation)

`package3/dotnet/nuget/oidc.go` implements:

```
1. Detect CI environment:
   - GitHub Actions: read ACTIONS_ID_TOKEN_REQUEST_URL + ACTIONS_ID_TOKEN_REQUEST_TOKEN
   - GitLab CI: read CI_JOB_JWT_V2 (OIDC v2 token, available in GitLab 14.7+)
   - Azure Pipelines: invoke `az account get-access-token --resource api://AzureADTokenExchange`

2. Request OIDC token from provider.

3. POST to nuget.org TrustedPackagePublish endpoint:
   PUT https://api.nuget.org/v3/trustedpublish/<package-id>/<version>
   Headers:
     X-NuGet-ApiKey: <OIDC-token>  (reuses the existing API key header for OIDC tokens)
     Content-Type: multipart/form-data
   Body: <.nupkg bytes>

4. On HTTP 201 Created: record Rekor log entry from X-NuGet-Rekor-Log-Entry header.
5. On HTTP 403 Forbidden: open trusted-publisher setup URL in browser and exit with error.
6. On HTTP 409 Conflict: the package version already exists; exit with error.
```

## GitLab CI path

GitLab CI provides OIDC v2 tokens via the `id_tokens` keyword:

```yaml
publish:
  id_tokens:
    NUGET_OIDC_TOKEN:
      aud: api://AzureADTokenExchange
  script:
    - mochi pkg publish --to=nuget.org
```

The `NUGET_OIDC_TOKEN` environment variable contains a JWT whose `sub` claim is `project_path:MyGroup/MyProject:ref_type:branch:ref:main`. nuget.org validates it against the registered GitLab publisher configuration.

## Azure Pipelines path

Azure Pipelines provides OIDC tokens via the `GetAccessToken` API when the service connection has `id_token` granted:

```yaml
- task: DotNetCoreCLI@2
  inputs:
    command: custom
    custom: pkg
    arguments: publish --to=nuget.org
  env:
    AZURE_CLIENT_ID: $(azureClientId)
    AZURE_TENANT_ID: $(azureTenantId)
```

The bridge detects Azure Pipelines via `TF_BUILD=True` and calls `az account get-access-token`.

## Dry-run mock server harness

`mochi pkg publish --to=nuget.org --dry-run` starts an in-process mock nuget server:

```go
// package3/dotnet/nuget/mockserver_test.go
func TestDryRunPublish(t *testing.T) {
    srv := nuget.NewMockServer(t)
    srv.ExpectTrustedPublish("MyPackage", "1.0.0")
    err := nuget.Publish(ctx, nupkgBytes, nuget.PublishOptions{
        Endpoint: srv.URL(),
        OIDCToken: "mock-token-for-testing",
        DryRun: false,
    })
    require.NoError(t, err)
    srv.AssertExpectations(t)
}
```

The mock server validates the `.nupkg` structure, the presence of required metadata, and returns a synthetic Rekor log URL. The mock server is also used in the MEP-68 CI gate to test the publish flow without real nuget.org credentials.

## The `--allow-apikey-fallback` flag

During the nuget.org trusted publishing rollout period, some users may not have completed the trusted publisher setup on nuget.org. The bridge provides:

```sh
mochi pkg publish --to=nuget.org --allow-apikey-fallback
```

When this flag is set:
1. The bridge tries the OIDC flow first.
2. If the OIDC flow is unavailable (no CI environment, no OIDC token endpoint) or returns HTTP 403 (trusted publisher not configured), it falls back to the `NUGET_API_KEY` environment variable.
3. If neither is available, it exits with an error describing both setup paths.

The flag is:
- Off by default.
- Emits a `WARNING: --allow-apikey-fallback is a security downgrade; configure trusted publishing to remove this warning` message on use.
- Scheduled for removal once nuget.org's trusted publishing GA enforcement timeline is established.

## Comparison with MEP-73 (Cargo RFC #3724)

| Dimension | MEP-68 (NuGet trusted publishing) | MEP-73 (Cargo RFC #3724) |
|-----------|-----------------------------------|--------------------------|
| GA date | November 2024 | Q4 2025 (rolling GA through 2026) |
| Signing backend | Sigstore Fulcio + Rekor (via nuget.org's internal Sigstore integration) | Sigstore Fulcio + Rekor (via crates.io's Cargo RFC #3724 integration) |
| OIDC providers | GitHub Actions, GitLab CI, Azure Pipelines | GitHub Actions, GitLab CI, Buildkite, CircleCI |
| Token exchange | nuget.org TrustedPackagePublish endpoint | crates.io trusted-publishing endpoint |
| Legacy fallback | `--allow-apikey-fallback` (explicit, default off) | `--allow-token-fallback` (explicit, default off) |

Both MEPs implement the same Sigstore-keyless OIDC principle. MEP-68's implementation is about one year ahead of MEP-73's in terms of registry GA support.
