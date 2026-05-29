---
title: "07. Sonatype trusted publishing + GPG signing"
sidebar_position: 8
sidebar_label: "07. Trusted publishing"
description: "The OIDC short-lived token flow for Central Portal, GPG key management, the Maven Central signing requirement history, in-memory signing (no keyring on disk), and Sigstore as a future alternative to GPG for JVM artifacts."
---

# 07. Sonatype trusted publishing + GPG signing

Publishing to Maven Central requires artifact signing. This note covers the two signing paths the bridge supports, the security model behind each, and the future direction toward Sigstore-based signing.

## Why Maven Central requires signatures

Maven Central's GPG signature requirement (enforced since 2012) serves two purposes:

1. **Authenticity**: a consumer can verify that the artifact was signed by the registered key for that namespace, detecting supply-chain substitution attacks.
2. **Integrity**: the `.asc` file covers the JAR's content; tampering with the JAR invalidates the signature.

The requirement predates modern package-registry signing approaches (PyPI attestations, crates.io Sigstore, npm provenance). Sonatype is evaluating Sigstore-based signing as an alternative but has not announced a GA timeline as of May 2026.

## GPG signing: the current requirement

Maven Central requires:

1. A GPG/PGP key pair (RSA 4096-bit recommended, or Ed25519).
2. The public key published to a public keyserver (keys.openpgp.org or Ubuntu keyserver).
3. The key fingerprint registered in the Sonatype Central Portal namespace settings.
4. Each artifact signed with `gpg --detach-sign --armor`, producing a `.asc` file.

The signature must be a detached armored PGP signature (PGP message type `PGP SIGNATURE`). The bridge produces these using `golang.org/x/crypto/openpgp` (for RSA-2048 and RSA-4096 keys) or the `filippo.io/age` / `golang.org/x/crypto/ed25519` path for Ed25519 keys, matching the PGP armoring format exactly.

## Mode 1: Environment-variable key (current stable)

The user sets:

```bash
MOCHI_MAVEN_SIGNING_KEY="-----BEGIN PGP PRIVATE KEY BLOCK-----
...base64...
-----END PGP PRIVATE KEY BLOCK-----"
MOCHI_MAVEN_SIGNING_KEY_PASSPHRASE="my passphrase"
```

The bridge:

1. Reads the ASCII-armored key from `MOCHI_MAVEN_SIGNING_KEY`.
2. Decrypts the private key using `MOCHI_MAVEN_SIGNING_KEY_PASSPHRASE` (an unencrypted key is also accepted but warned against).
3. Signs each artifact file in-process.
4. Discards the private key bytes from memory after signing.
5. Never writes the private key to disk.

The key material exists in the process address space only for the duration of signing. The bridge zeroes the key byte slice after use via a `runtime.SetFinalizer` + explicit zero on the key struct.

In GitHub Actions, the recommended setup is to store the ASCII-armored key as a repository secret (`MAVEN_SIGNING_KEY`) and pass it via the environment:

```yaml
- name: Publish to Maven Central
  env:
    MOCHI_MAVEN_SIGNING_KEY: ${{ secrets.MAVEN_SIGNING_KEY }}
    MOCHI_MAVEN_SIGNING_KEY_PASSPHRASE: ${{ secrets.MAVEN_SIGNING_KEY_PASSPHRASE }}
    MOCHI_MAVEN_USERNAME: ${{ secrets.MAVEN_USERNAME }}
    MOCHI_MAVEN_PASSWORD: ${{ secrets.MAVEN_PASSWORD }}
  run: mochi pkg publish --to=maven-central
```

## Mode 2: Sonatype Central Portal OIDC (beta, May 2026)

Sonatype's beta trusted-publishing flow eliminates the need for a long-lived signing key in CI:

1. The CI job requests an OIDC token from the identity provider (GitHub Actions: `id-token: write` permission).
2. The bridge exchanges the OIDC token for a Central Portal session token via `POST https://central.sonatype.com/api/v1/auth/oidc`.
3. The session token grants signing authority for the namespace associated with the OIDC subject claim.
4. The bridge uploads the unsigned artifacts; the Central Portal server signs them server-side.
5. The session token expires after 15 minutes.

Sonatype maps OIDC subject claims to namespace ownership:

| OIDC subject claim | Namespace |
|-------------------|-----------|
| `repo:octocat/myapp:ref:refs/heads/main` | `io.github.octocat` |
| Custom claim | Configured in namespace settings |

**Detection logic:** The bridge uses Mode 2 when `MOCHI_MAVEN_SIGNING_KEY` is absent and `ACTIONS_ID_TOKEN_REQUEST_URL` is present (GitHub Actions) or `GITLAB_CI` + `CI_JOB_JWT_V2` is set (GitLab CI). Outside CI, Mode 1 is required.

**Beta caveat:** Mode 2 is in beta as of May 2026. The OIDC endpoint URL may change before GA. The bridge prints a beta warning when using Mode 2 and falls back to Mode 1 if the OIDC exchange fails.

## Key rotation and expiry

Maven Central recommends rotating signing keys every 2-3 years. The bridge has no automated rotation; the user must:

1. Generate a new key pair.
2. Upload the new public key to the keyserver.
3. Update `MOCHI_MAVEN_SIGNING_KEY` in CI secrets.
4. Update the fingerprint in Central Portal namespace settings.

Keys do not expire by default in OpenPGP unless an expiry date is set. The bridge warns if the key's expiry date is within 90 days.

## Verifying signatures as a consumer

A consumer who wants to verify a Maven Central artifact:

```bash
gpg --recv-keys <fingerprint>
gpg --verify mylib-1.0.0.jar.asc mylib-1.0.0.jar
```

Alternatively, the Maven Dependency Plugin and Gradle's dependency verification feature (`gradle/verification-metadata.xml`) can verify signatures automatically.

## Future: Sigstore for JVM artifacts

Sigstore provides keyless artifact signing using ephemeral OIDC-bound certificates (Fulcio) with a transparency log (Rekor), identical in concept to how crates.io's RFC #3724 (MEP-73 §7) works. The Maven ecosystem is exploring Sigstore adoption:

- **Cosign** supports JAR signing and verification.
- **Maven Artifact Plugin** (Apache) has experimental Sigstore support (`sigstore:sign`).
- **Gradle** has a PR for Sigstore-based dependency verification.

None of these paths are stable on Maven Central as of May 2026. When Maven Central formally adopts Sigstore (estimated 2026-2027), the bridge will add a Mode 3 that uses the same keyless OIDC flow as MEP-73's Cargo trusted publishing, eliminating the GPG key management requirement entirely.

The bridge's publish code is structured to make adding Mode 3 a matter of wiring a new signing backend; the bundle assembly, POM generation, and upload logic are signing-backend-agnostic.

## Cross-references

- [[06-maven-central-publish]] for the bundle assembly and upload protocol.
- [MEP-73 research note 07](/docs/research/0073/07-sigstore-cargo-rfc3724.md) for the analogous Sigstore story on crates.io.
- [MEP-70 §5.3](/docs/mep/mep-0070#53-kotlinpublish) for the `[kotlin.publish] signing-key-id` field.
