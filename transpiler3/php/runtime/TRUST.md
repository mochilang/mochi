# mochi/runtime release trust root

This document explains how consumers verify a published `mochi/runtime`
Composer package. The trust chain is layered: three independent
mechanisms are exposed so a consumer can pick whichever fits their
threat model.

## 1. GPG-signed Git tags (primary)

Every release tag is signed with the Mochi release-signing key. Verify
with:

```
git tag -v v<version>
```

The release-signing key fingerprint is published in this file (see
"Release key fingerprint" below) and in the GitHub `verified-signatures`
API. The key is rotated on the cadence documented in
`docs/security/release-signing.md` of the main `mochilang/mochi`
repository.

This is the **primary** trust root: if `git tag -v` fails or reports
"no signature", do not install the release.

## 2. GitHub Actions OIDC attestation (secondary)

The release workflow at `.github/workflows/transpiler3-php-publish.yml`
uses `actions/attest-build-provenance@v1` (GA April 2024) to attach a
Sigstore-backed provenance statement to the release tarball. Verify
with:

```
gh attestation verify mochi-runtime-v<version>.tar.gz \
  --repo mochilang/mochi
```

The attestation carries: repository URL, commit SHA, workflow file,
runner identity, build invocation, and the SHA-256 of the staged
tarball. Consumers who do not run `git` can rely on this mechanism
alone.

## 3. php-signify Ed25519 (optional tertiary)

For consumers who want signature verification independent of GitHub,
the release workflow can optionally emit a `php-signify` Ed25519
signature alongside the tarball. `php-signify` is Drupal's port of
OpenBSD `signify(1)` to PHP. Verify with:

```
signify -V -m mochi-runtime-v<version>.tar.gz \
  -p mochi-php-release.pub
```

The `php-signify` public key is published at the same location as the
GPG key. This route is optional in v1; promoting it to default-on
requires a wider PHP-community signature standard, which has not yet
emerged.

## What Packagist provides

Packagist (the canonical PHP package registry) does **not** as of Q1
2026 ship a Trusted Publishing flow comparable to PyPI's or npm's.
Packagist auto-discovers tags via the GitHub webhook and serves
content-addressed tarballs from the repository's tag SHAs. The chain
of custody from `git tag` to `composer require` therefore relies on
GitHub's web hosting plus the three mechanisms above. When Packagist's
"Composer Verification" experimental program reaches GA (target window
v1.5), this document will be updated to promote it to the primary
trust root and demote the others to secondary.

## composer audit

Every release is published after a clean `composer audit` against the
FriendsOfPHP advisory database. The audit runs against the **extracted
release tarball** (not the in-tree source), so an out-of-band
`composer.lock` or `vendor/` directory in the working tree cannot mask
a vulnerable artifact. The publish workflow re-runs the audit in an
independent `verify-gate` job after publish, which re-stages the
tarball from a fresh checkout and audits the result, so a leaked
credential in the publish job alone cannot mask a vulnerable release.

## Release key fingerprint

The release-signing key fingerprint is published in the main
[`SECURITY.md`](../../../SECURITY.md) and updated on every rotation.
The current fingerprint is intentionally not duplicated here so there
is a single source of truth.

## Reporting a release-signature issue

If you observe a release that fails `git tag -v` or
`gh attestation verify`, do **not** install it. Open an issue at
<https://github.com/mochilang/mochi/issues> tagged `release-signing`.
The maintainers will rotate the key and re-issue the release.
