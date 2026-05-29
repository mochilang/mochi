---
title: "07. OIDC trusted publishing"
sidebar_position: 8
sidebar_label: "07. Trusted publishing"
description: "RubyGems.org OIDC trusted publishing (GA 2023), the GitHub Actions token exchange, gem signing with Sigstore, the rubygems.org trusted publisher configuration, and how MEP-76 integrates it."
---

# 07. OIDC trusted publishing

This note covers the trusted-publishing flow MEP-76 uses for rubygems.org publishes. The flow mirrors the broader Sigstore-keyless OIDC publishing principle established in MEP-57 and applied to PyPI (MEP-57), npm (MEP-57), crates.io (MEP-73), and pkg.go.dev (MEP-74).

## Timeline

| Date | Milestone |
|------|-----------|
| 2022-11 | RubyGems.org blog post announces trusted publishing support in beta. |
| 2023-02 | GA: any gem owner can configure a trusted publisher via the rubygems.org web UI. |
| 2024 | Sigstore gem attestation (`gem attest`) added alongside the existing OIDC token-exchange flow. |
| 2026-05 | Estimated 40%+ of new rubygems.org pushes use trusted publishing (based on rubygems.org blog post data). |

The implementation follows OpenID Connect (OIDC) using GitHub Actions' `id-token: write` permission as the initial supported issuer, with GitLab CI and CircleCI added in 2024.

## The token exchange flow

```
1. CI workflow requests OIDC token (audience: rubygems.org)
   via $ACTIONS_ID_TOKEN_REQUEST_URL + $ACTIONS_ID_TOKEN_REQUEST_TOKEN

2. mochi pkg publish --to=rubygems.org exchanges the OIDC JWT
   POST https://rubygems.org/api/v1/trusted_publishing/tokens
   Body: { "jwt": "<oidc-jwt>" }
   Response: { "api_key": "<short-lived-key>", "expires_at": "..." }

3. The short-lived key is used as the auth header for gem push:
   gem push mochi-example-0.1.0.gem
   GEM_HOST_API_KEY=<short-lived-key>

4. rubygems.org records the provenance attestation linking the gem
   to the specific GitHub Actions run (repo + workflow + ref).
```

The short-lived key is scoped to `gems/push` for the specific gem name only. It expires after 15 minutes. The bridge never writes this key to disk; it is held only in the process environment for the duration of the push.

## GitHub Actions integration

The bridge generates `.github/workflows/release.yml` when the user runs `mochi pkg publish --to=rubygems.org --emit-ci`:

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
      - uses: ruby/setup-ruby@v1
        with:
          ruby-version: '3.3'
          bundler-cache: true
      - uses: mochilang/setup-mochi@v1
      - run: mochi pkg publish --to=rubygems.org
```

The `id-token: write` permission grants the workflow the ability to request an OIDC JWT from GitHub's token endpoint. Without it, `$ACTIONS_ID_TOKEN_REQUEST_URL` is unset and the bridge falls back to an error (not to an API key).

## Configuring a trusted publisher on rubygems.org

### Web UI path

1. Navigate to `https://rubygems.org/gems/<gem-name>/trusted_publishers`.
2. Click "Create Trusted Publisher".
3. Fill in the form:

| Field | Example |
|-------|---------|
| GitHub owner (org or user) | `mochilang` |
| Repository name | `mochi-example` |
| Workflow filename | `release.yml` |
| Environment name (optional) | `production` |

4. Save. The trusted publisher is active immediately.

### API path

```sh
curl -X POST https://rubygems.org/api/v1/trusted_publishers/github \
  -H "Authorization: <your-rubygems-api-key>" \
  -H "Content-Type: application/json" \
  -d '{
    "rubygem_name": "mochi-example",
    "owner": "mochilang",
    "repository": "mochi-example",
    "workflow_filename": "release.yml",
    "environment": ""
  }'
```

The API key used here is only needed once to register the trusted publisher configuration. After that, the CI workflow no longer needs a stored API key.

## Sigstore gem attestation

RubyGems.org added Sigstore-based attestation in 2024 via the `gem attest` command (part of the `rubygems-attestation` gem). This attaches a Sigstore bundle to the published gem, providing a cryptographic proof linking the gem binary to the GitHub Actions run that built it.

The bridge can optionally run `gem attest` after the push. Default is off (the OIDC token-exchange itself is sufficient for provenance traceability). Opt in via:

```toml
[ruby.publish]
attest = true
```

When `attest = true`:

```
[1/5] Build gem ...              mochi-example-0.1.0.gem
[2/5] Obtain OIDC token ...      OK (GitHub Actions)
[3/5] Exchange for short-lived key ... OK (expires in 15m)
[4/5] Push gem ...               OK
[5/5] Attest gem (Sigstore) ...  OK
       Rekor index: 98765432
       Bundle: mochi-example-0.1.0.gem.sigstore
```

The Sigstore bundle is stored at `<gem-name>-<version>.gem.sigstore` in the same directory as the `.gem` file, and is uploaded alongside the gem binary.

## Local testing

`--dry-run` runs `gem build` and validates the gemspec but skips the push. The OIDC exchange is also skipped (no network calls):

```sh
mochi pkg publish --to=rubygems.org --dry-run
```

For offline testing against a local registry (geminabox or Gemstash):

```sh
mochi pkg publish \
  --to=rubygems.org \
  --mock-registry=http://localhost:9292
```

The `--mock-registry` flag also accepts a mock OIDC endpoint via `--mock-oidc-url=http://localhost:9293` for testing the full token-exchange flow without hitting `rubygems.org`.

## Why no API key path

Storing a long-lived rubygems.org API key as a CI secret creates a supply-chain attack surface: a compromised secret can publish any version of any gem the owner controls. The OIDC token-exchange model bounds the blast radius to the specific gem name registered with the trusted publisher, for the duration of the short-lived key (15 minutes), and only from the registered repo + workflow.

This is the same rationale applied in MEP-73 §5 (crates.io) and MEP-57 §publish (PyPI / npm). The principle across all MEPs: no long-lived credentials in CI secrets.

The `--allow-key-fallback` flag exists for emergency use during CI bootstrapping (when the trusted publisher configuration has not yet been created on rubygems.org):

```sh
GEM_HOST_API_KEY=<key> mochi pkg publish --to=rubygems.org --allow-key-fallback
```

This flag is explicitly marked as deprecated in the CLI help text and will be removed once trusted publishing is the only supported path.

## Cross-references

- [[06-rubygems-publish-flow]] for the `.gem` format and upload protocol.
- [[09-bundler-lockfile]] for version provenance recorded in the lockfile.
- [MEP-57](/docs/mep/mep-0057) for the broader Sigstore-keyless principle.
- [MEP-73 §7](/docs/research/0073/07-sigstore-cargo-rfc3724) for the parallel crates.io trusted-publishing flow.
- [RubyGems.org trusted publishing docs](https://guides.rubygems.org/trusted-publishing/) for the upstream configuration guide.
