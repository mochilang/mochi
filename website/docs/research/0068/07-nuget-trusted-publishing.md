---
title: "07. NuGet trusted publishing"
sidebar_position: 8
sidebar_label: "07. Trusted publishing"
description: "The NuGet trusted publishing OIDC flow (GA March 2024), the GitHub Actions id-token: write permission model, the nuget.org publisher configuration UI, the verification path at install time, comparison to PyPI PEP 740 and Cargo RFC #3724, and the sigstore-mock harness."
---

# 07. NuGet trusted publishing

This note covers the trusted-publishing flow MEP-68 uses for nuget.org publishes. NuGet trusted publishing reached general availability in March 2024, making it the earliest major package ecosystem to ship keyless OIDC publishing.

## The NuGet trusted publishing model

NuGet trusted publishing (announced March 2024 at the .NET Blog, documented at `https://learn.microsoft.com/nuget/nuget-org/publish-a-package#trusted-publisher`) allows packages to be published using short-lived OIDC identity tokens from CI environments without storing a long-lived NuGet API key.

The flow:

1. The package owner (one-time setup) configures a trusted publisher on the nuget.org web UI:
   - Selects the CI environment: GitHub Actions, Azure Pipelines, or Google Cloud Build.
   - For GitHub Actions: specifies the repository (`example/mymochi`), the workflow file (`.github/workflows/release.yml`), and optionally an environment name or tag pattern.

2. The CI workflow (on each release) requests an OIDC token from GitHub Actions scoped to `nuget.org`:
   ```yaml
   permissions:
     id-token: write
     contents: read
   steps:
     - run: mochi pkg publish --to=nuget.org
   ```

3. `mochi pkg publish` requests the OIDC token from the GitHub Actions OIDC endpoint (`https://token.actions.githubusercontent.com`) with audience `nuget.org`.

4. The CLI sends the OIDC token in the `Authorization: Bearer <token>` header alongside the `.nupkg` upload:
   ```
   PUT https://www.nuget.org/api/v2/package
   Authorization: Bearer <oidc-token>
   X-NuGet-Protocol-Version: 4.1.0
   ```

5. nuget.org's server validates the OIDC token:
   - Verifies the token signature against the OIDC provider's (GitHub Actions') public keys.
   - Extracts the `sub` claim (e.g., `repo:example/mymochi:ref:refs/tags/v1.0.0`).
   - Looks up the package's trusted publisher configuration.
   - Asserts the `sub` claim matches the configured repository + workflow + tag pattern.

6. On success, nuget.org stores the `.nupkg` and appends the version to the NuGet v3 sparse index.

## Comparison to other ecosystem trusted publishing

| Ecosystem | GA date | OIDC provider support | Signing mechanism |
|-----------|---------|----------------------|-------------------|
| NuGet (nuget.org) | March 2024 | GitHub Actions, Azure Pipelines, Google Cloud Build | OIDC token validation; package signatures via NuGet repository signing |
| npm (npmjs.com) | April 2024 | GitHub Actions, other CI | Sigstore-keyless (Fulcio + Rekor) |
| Maven Central | October 2024 | GitHub Actions | Sigstore-keyless |
| PyPI (PEP 740) | Late 2025 | GitHub Actions, GitLab CI, Google Cloud, others | Sigstore-keyless |
| crates.io (RFC #3724) | Q4 2025 (rolling GA 2026) | GitHub Actions, GitLab CI, Buildkite, CircleCI | Sigstore-keyless |

NuGet's trusted publishing model differs from the Sigstore-keyless approach used by npm, Maven Central, PyPI, and Cargo: NuGet validates the OIDC token directly at nuget.org's server rather than going through Fulcio certificate issuance. The effect is equivalent (the OIDC identity is bound to the package version; no long-lived token is used), but the implementation is simpler (no Rekor transparency log for NuGet publishes).

For MEP-68's nuget.org publish path, the implementation is:

1. Obtain the GitHub Actions OIDC token via the `ACTIONS_ID_TOKEN_REQUEST_TOKEN` + `ACTIONS_ID_TOKEN_REQUEST_URL` environment variables (the standard GitHub Actions OIDC mechanism).
2. Exchange the request token for an ID token via `GET <ACTIONS_ID_TOKEN_REQUEST_URL>&audience=nuget.org`.
3. Pass the ID token to `mochi pkg publish`'s HTTP client.

## OIDC issuer configuration on nuget.org

A first-time publish from a new repository requires a one-time configuration on nuget.org. The bridge detects a 403 response and emits configuration guidance:

```
$ mochi pkg publish --to=nuget.org
[1-4/6] ... OK
[5/6] Obtaining GitHub Actions OIDC token ... OK
[6/6] Uploading to nuget.org ...
ERROR: 403 Forbidden
  nuget.org message: "No trusted publisher found for package 'MyMochiLib' from workflow
                      'repo:example/mymochi:ref:refs/tags/v1.0.0'"
  Resolution: configure a trusted publisher at https://www.nuget.org/packages/MyMochiLib/manage
              or for a new package at https://www.nuget.org/account/manage-trusted-publishers
  See: https://learn.microsoft.com/nuget/nuget-org/publish-a-package#trusted-publisher
```

The bridge can generate a CI workflow template automatically:

```
$ mochi pkg publish --to=nuget.org --emit-ci
Generated .github/workflows/release.yml
  Trusted publisher config needed on nuget.org:
    Repository: example/mymochi
    Workflow:   .github/workflows/release.yml
    Tag pattern: v*
```

The generated workflow:

```yaml
name: Release to NuGet
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
      - uses: actions/setup-dotnet@v4
        with:
          dotnet-version: '8.x'
      - run: mochi pkg publish --to=nuget.org
```

## Token fallback for the transition period

Some organisations may not have configured nuget.org trusted publishing. The bridge provides an `--allow-token-fallback` flag for this case:

```
$ mochi pkg publish --to=nuget.org --allow-token-fallback
WARNING: using legacy NuGet API key (NUGET_API_KEY environment variable)
         NuGet trusted publishing is preferred. Configure at https://www.nuget.org/packages/<id>/manage
[6/6] Uploading with legacy API key ... OK
```

The fallback reads from the `NUGET_API_KEY` environment variable. It emits a deprecation warning on every use. The flag and the fallback path are removed in MEP-68 v2, after trusted publishing adoption is sufficiently widespread.

## Verification at install time

When a downstream .NET user runs `dotnet add package MyMochiLib`, no trusted-publishing verification is currently performed by the standard .NET SDK (the NuGet client does not yet verify the OIDC claim at install time). Verification is optional and requires the nuget.org package signing infrastructure.

For Mochi-side verification (a downstream Mochi user running `mochi pkg add dotnet MyMochiLib`), the bridge does verify the NuGet repository signature if present:

```
$ mochi pkg add dotnet MyMochiLib@^1.0
[1/4] Resolving versions ... MyMochiLib@1.0.0
[2/4] Downloading .nupkg from https://api.nuget.org/ ... 145 KB
[3/4] Verifying NuGet repository signature ...
        Signer: nuget.org repository signing certificate
        SHA-512: abc123...
        Signature: valid
[4/4] Running mochi-dotnet-meta ... 45 public types, 8 skipped
```

NuGet packages published via trusted publishing carry a `repository-signed` nuspec attribute that the bridge checks:

```xml
<metadata>
  ...
  <repository type="git" url="https://github.com/example/mymochi" commit="abc123" />
</metadata>
```

The bridge records the repository URL and commit SHA in `mochi.lock` alongside `nupkg-sha512`:

```toml
[[dotnet-package]]
id = "MyMochiLib"
version = "1.0.0"
source = { kind = "registry", registry = "https://api.nuget.org/v3/index.json" }
nupkg-sha512 = "..."
repository-url = "https://github.com/example/mymochi"
repository-commit = "abc123"
```

## The nuget.org mock harness

CI runs of `mochi pkg publish --to=nuget.org --dry-run` use a local nuget.org mock to test the publish flow without touching the public nuget.org API. The mock is a Go service in `pkg/pkgpub/mock/nugetmock/` that:

- Accepts an OIDC-shaped token.
- Validates the token against a deterministic mock issuer.
- Accepts the `.nupkg` upload.
- Returns a 200 with a deterministic confirmation message.
- Stores the uploaded package in an in-memory store for test assertions.

The mock is activated by setting `MOCHI_NUGET_REGISTRY=http://localhost:<port>` in the test environment.

## Cross-references

- [[06-nuget-publish-flow]] for what gets uploaded.
- [[02-design-philosophy]] §5 for why long-lived API keys are rejected.
- [MEP-57](/docs/mep/mep-0057) for the broader Sigstore-keyless principle.
- [MEP-73 §07](/docs/research/0073/07-sigstore-cargo-rfc3724) for the Cargo RFC #3724 analogue.
- [NuGet trusted publishing docs](https://learn.microsoft.com/nuget/nuget-org/publish-a-package#trusted-publisher) for the normative protocol.
