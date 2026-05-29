---
title: "06. Composer publish flow"
sidebar_position: 7
sidebar_label: "06. Composer publish flow"
description: "The Packagist publish flow in detail: `composer.json` schema, VCS-based Packagist discovery, git tag plus webhook vs API token ping, dist zip vs source, why Packagist has no OIDC trusted publishing, GPG-signed tags, Sigstore attestation workaround, and the GitHub App integration as a partial mitigation."
---

# 06. Composer publish flow

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note details the mechanics of publishing a Mochi package to Packagist as a Composer library: the `composer.json` schema, the git tag flow, the API mechanisms, and the security story.

## 1. Packagist's discovery model

Packagist does not accept uploads. Unlike crates.io (where `cargo publish` sends the .crate tarball to the registry), Packagist discovers packages by crawling VCS repositories:

1. The author submits their package URL to packagist.org (`https://packagist.org/packages/submit`).
2. Packagist clones the git repository and reads the root-level `composer.json`.
3. Packagist scans the repository's tags for semver-compatible tag names (`v1.0.0`, `1.0.0`).
4. For each tag, Packagist records the `composer.json` at that tag's commit as the package metadata.
5. The `dist.url` in the Packagist response points to the VCS host's archive URL for that tag (a GitHub tarball, a GitLab archive, etc.).

There is no "Packagist blob store". Packagist is a metadata registry and a discovery service; the actual package source comes from the VCS host.

## 2. The `composer.json` schema

The `TargetPhpLibrary` emitter writes a `composer.json` at the package root. Key fields:

```json
{
    "name": "<vendor>/<package>",
    "description": "Short description of the package.",
    "type": "library",
    "license": "MIT",
    "authors": [
        {"name": "Author Name", "email": "author@example.com"}
    ],
    "keywords": ["mochi", "php"],
    "homepage": "https://github.com/example/my-mochi-lib",
    "require": {
        "php": "^8.4",
        "mochi/runtime": "^<version>",
        "ext-mbstring": "*",
        "ext-gmp": "*"
    },
    "autoload": {
        "psr-4": {
            "<psr4-namespace>": "src/"
        }
    },
    "minimum-stability": "stable"
}
```

The `mochi/runtime` dependency is Mochi's PHP runtime library (the same `composer.json` that MEP-55's `transpiler3/php/runtime/composer.json` ships). The emitted library always depends on `mochi/runtime` because the lowered PHP uses the runtime helpers for sum types, records, and other Mochi constructs.

## 3. Git tag and publish flow

The `mochi pkg publish --to=packagist` flow:

1. **Validate `composer.json`**: parse and validate the emitted `composer.json` against the Composer schema.
2. **Run `php -l` on all files**: syntax check every emitted PHP file.
3. **Run PHPStan and Psalm** (if available on PATH): static analysis as a quality gate.
4. **Determine the version**: read from `mochi.toml [package] version`. Must be a semver string like `1.2.3` (Composer strips the `v` prefix; both `v1.2.3` and `1.2.3` are accepted).
5. **Create a GPG-signed git tag**:
   ```
   git tag -s -u <gpg-key-id> v<version> -m "Release v<version>"
   ```
   The `-s` flag signs the tag. The `--gpg-sign` flag is equivalent. If `$GPG_KEY_ID` is set in the environment, it is used; otherwise the default signing key is used.
6. **Push the tag**:
   ```
   git push origin v<version>
   ```
7. **Produce a dist zip**: zip the `src/`, `composer.json`, `README.md`, `LICENSE` tree.
8. **Attach a Sigstore attestation**: in a CI environment with `ACTIONS_ID_TOKEN_REQUEST_URL` set (GitHub Actions), invoke `actions/attest-build-provenance@v1` to create a Sigstore keyless OIDC attestation on the dist zip SHA-256.
9. **Ping the Packagist Update API** (if not using GitHub App):
   ```
   POST https://packagist.org/api/update-package?username=<user>&apiToken=<token>
   Content-Type: application/json
   {"repository": {"url": "https://github.com/example/my-mochi-lib"}}
   ```
   This triggers an immediate Packagist crawl of the repository. Packagist will discover the new tag and record the new version.
10. **Verify Packagist indexed the tag**: wait up to 60 seconds for `GET https://packagist.org/packages/<vendor>/<package>.json` to reflect the new version. The `--no-verify` flag skips this wait.

## 4. GitHub App integration as a partial mitigation

Packagist offers a GitHub App (`packagist/packagist-mirror`) that, when installed on a repository, receives webhook payloads for push and tag events. When the App is installed:

- No manual `POST` to the Update API is needed after a tag push.
- The GitHub App token is managed by GitHub (not stored in CI secrets as a long-lived user token).
- Tag creation automatically triggers a Packagist crawl within ~30 seconds.

The GitHub App does not eliminate the API token entirely: the initial package registration at `packagist.org/packages/submit` still requires a Packagist account. But for ongoing releases, the App reduces the secret surface to the one-time registration step.

MEP-75's publish flow detects the GitHub App by checking whether the repository has the `packagist` GitHub App webhook registered (via the GitHub REST API) and skips the manual Update API ping if the App is installed.

## 5. GPG-signed tags

PHP's package ecosystem has a GPG-signing tradition predating Sigstore. Several major packages (Symfony, Doctrine, PHPUnit) sign their release tags and distribute the signer's public key in `CREDITS` or their website.

The MEP-75 bridge signs git tags with GPG via `git tag -s` when a signing key is available. The signing key is resolved from:

1. `$GPG_KEY_ID` environment variable.
2. `[php.publish] gpg-key-id = "..."` in `mochi.toml`.
3. The default signing key configured in `~/.gnupg/gpg.conf`.

If no signing key is available and GPG signing is not explicitly disabled, the bridge emits a warning and proceeds without a signature (unsigned tags are still valid for Packagist; signed tags are a best-practice, not a requirement).

## 6. Sigstore attestation

The Sigstore `actions/attest-build-provenance@v1` action (GitHub Actions, GA 2024) creates a keyless OIDC attestation on any artifact. The attestation:

- Is tied to the GitHub Actions workflow identity (the `sub` claim: `repo:<owner>/<repo>:ref:refs/tags/v<version>:workflow:<workflow-name>`).
- Creates a Sigstore bundle (DSSE envelope + Fulcio certificate + Rekor log entry).
- Stores the bundle in GitHub's attestation store and optionally as a release asset.

The MEP-75 bridge attaches the Sigstore attestation on the dist zip SHA-256. This provides:

- A transparency-log entry (Rekor) for the published artifact.
- A CI-identity-bound signature (the workflow that built and published the package is recorded).
- Interoperability with `gh attestation verify` (GitHub's CLI tool for verifying Sigstore attestations).

This is not a Packagist-native feature; Packagist does not read or verify Sigstore attestations. The attestation is a supply-chain story for downstream consumers who verify the artifact independently of Packagist.

## 7. The API token (and why it persists)

Even with the GitHub App integration, Packagist requires an API token for the initial package registration and for the fallback Update API ping. The token is obtained from `packagist.org/profile/` and is stored as a CI secret (e.g., `PACKAGIST_API_TOKEN`).

The bridge reads the token from `$PACKAGIST_API_TOKEN` in the environment. It is never written to `mochi.toml` or `mochi.lock`. The token is used only in step 9 of the publish flow.

This is the known supply-chain gap: a compromised `$PACKAGIST_API_TOKEN` can trigger a Packagist crawl that publishes a malicious tag. The GPG-signed tag and Sigstore attestation mitigations reduce the impact (a downstream user who verifies the GPG signature or Sigstore attestation will detect a tag from a different key/workflow), but they do not prevent a bad actor from pushing a tag and triggering the crawl.

See [[07-packagist-trusted-publishing-gap]] for the full analysis.

## 8. Dist zip vs source download

Packagist serves two download modes for each package version:

- **dist**: a pre-packaged zip from the VCS host (GitHub tarball URL, GitLab archive URL). Fast to download; the content may differ from what `composer install --prefer-source` downloads.
- **source**: a fresh git clone of the repository at the tagged commit. Slower; always matches the VCS state.

MEP-75's consumer path always uses **dist** (consistent with Composer's default). The bridge records `dist-sha256` in the lockfile. The `--prefer-source` flag is not supported in v1.

## Cross-references

- [[07-packagist-trusted-publishing-gap]] for the OIDC gap analysis.
- [[09-psr-autoloading]] for the PSR-4 layout the `composer.json` references.
- [[01-language-surface]] for the `mochi pkg publish` CLI.
- [MEP-75 §10](/docs/mep/mep-0075#10-packagist-publish-flow) for the normative publish flow.
- [MEP-55 Phase 18](/docs/mep/mep-0055) for the GPG + Sigstore trust chain MEP-75 reuses.
