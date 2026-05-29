---
title: "07. Sigstore Maven attestations"
sidebar_position: 8
sidebar_label: "07. Sigstore Maven attestations"
description: "sigstore-maven-plugin (GA September 2024), how Sigstore attestations attach to Maven artifacts, Fulcio certificate chains, Rekor log inclusion, downstream consumer verification via mvn sigstore:verify, comparison with PyPI PEP 740 and crates.io RFC 3724, the OIDC auth gap in the Sonatype Central Portal, and the roadmap."
---

# 07. Sigstore Maven attestations

This note covers the supply-chain signing story for Mochi packages published to Maven Central, and honestly describes the gap between Maven's current state and the keyless trusted publishing that PyPI and crates.io now provide.

## 1. sigstore-maven-plugin

The `sigstore-maven-plugin` (Maven plugin group: `dev.sigstore`, artifact: `sigstore-maven-plugin`) reached GA in September 2024. It wraps the Sigstore Java SDK (`dev.sigstore:sigstore-java`, v1.0+) and provides Maven lifecycle goals:

- `sigstore:sign`: signs the artifact during the `verify` lifecycle phase (after packaging).
- `sigstore:verify`: verifies a Sigstore bundle (`.att` file) for an artifact.

The plugin's `sign` goal:

1. Obtains an OIDC identity token from the ambient environment (GitHub Actions `ACTIONS_ID_TOKEN_REQUEST_*` variables, Google Workload Identity, or interactive OIDC browser flow for local development).
2. Sends the public key to Sigstore's Fulcio certificate authority. Fulcio issues a short-lived X.509 certificate (valid 10 minutes) binding the public key to the OIDC identity (e.g., `https://github.com/actions/runner_id=12345`).
3. Signs the artifact (SHA-256 digest) with the private key.
4. Submits the signature, the certificate, and the artifact hash to Rekor (Sigstore's transparency log). Rekor returns a signed transparency log entry (a "tlog entry") with a UUID.
5. Writes a Sigstore bundle (`.att` file, JSON format per the Sigstore bundle spec) containing the certificate chain, the signature, and the Rekor log entry.

The bundle file is attached to the Maven artifact alongside the JAR:

```
guava-32.1.2-jre.jar
guava-32.1.2-jre.jar.att   ← Sigstore bundle
guava-32.1.2-jre.jar.sha1
guava-32.1.2-jre.jar.sha256
```

## 2. The Sigstore bundle format (.att)

A `.att` file is a JSON document following the [Sigstore bundle spec](https://github.com/sigstore/protobuf-specs/blob/main/protos/sigstore_bundle.proto):

```json
{
  "mediaType": "application/vnd.dev.sigstore.bundle+json;version=0.3",
  "verificationMaterial": {
    "certificate": {
      "rawBytes": "<base64-encoded DER certificate>"
    },
    "tlogEntries": [
      {
        "logIndex": "12345678",
        "logId": {"keyId": "<hex key ID of the Rekor instance>"},
        "kindVersion": {"kind": "hashedrekord", "version": "0.0.1"},
        "integratedTime": "1716988800",
        "inclusionPromise": {"signedEntryTimestamp": "<base64 SET>"},
        "inclusionProof": {
          "checkpoint": "...",
          "hashes": ["..."],
          "logIndex": "12345678",
          "treeSize": "50000000",
          "rootHash": "..."
        },
        "canonicalizedBody": "<base64-encoded JSON>"
      }
    ]
  },
  "messageSignature": {
    "messageDigest": {
      "algorithm": "SHA2_256",
      "digest": "<base64-encoded SHA-256 of the JAR>"
    },
    "signature": "<base64-encoded signature>"
  }
}
```

## 3. Downstream verification

A downstream Maven user can verify the Sigstore attestation:

```xml
<!-- pom.xml -->
<plugin>
  <groupId>dev.sigstore</groupId>
  <artifactId>sigstore-maven-plugin</artifactId>
  <version>1.2.0</version>
  <executions>
    <execution>
      <goals><goal>verify</goal></goals>
    </execution>
  </executions>
  <configuration>
    <!-- Require the artifact to have been signed by a GitHub Actions workflow
         from the publisher's repository -->
    <certificateIdentity>
      <issuer>https://token.actions.githubusercontent.com</issuer>
      <subjectRegExp>https://github.com/example/my-library/.github/workflows/publish.yml@refs/tags/.*</subjectRegExp>
    </certificateIdentity>
  </configuration>
</plugin>
```

`mvn sigstore:verify` validates:

1. The Rekor log entry is present and the inclusion proof verifies against the Rekor checkpoint.
2. The signing certificate was issued by Sigstore's Fulcio (verified against the Sigstore TUF root of trust).
3. The certificate's Subject Alternative Name (SAN) matches the declared `certificateIdentity` (the GitHub Actions workflow URL in the example).
4. The signature over the artifact's SHA-256 digest is valid under the certificate's public key.
5. The certificate was valid at the time of signing (not expired; Fulcio certs are 10-minute short-lived).

## 4. The OIDC auth gap

This is the honest part of the story.

**What PyPI PEP 740 and crates.io RFC 3724 do**: the CI OIDC token (GitHub Actions `id-token: write`) is exchanged with the registry's Sigstore Fulcio CA for a short-lived signing certificate. The artifact is signed with the ephemeral private key. The private key is discarded after signing. There is no long-lived private key anywhere in the chain.

**What Sonatype Central Portal does (as of May 2026)**: the CI OIDC token authenticates the publisher to Sonatype (proves "this upload comes from a GitHub Actions workflow in the declared repository"). But Sonatype does not itself act as a Sigstore Fulcio CA. The artifact signing (via `sigstore-maven-plugin`) is a separate step that does exchange the OIDC token with Sigstore's Fulcio. So:

- The upload is authenticated by OIDC (no long-lived Sonatype token needed).
- The artifact signing is Sigstore keyless (no long-lived GPG key needed).
- But these are two separate operations. The Central Portal does not verify or enforce that the Sigstore attestation was produced by the same OIDC identity that performed the upload.

In contrast, on crates.io (RFC 3724), the registry is the Sigstore relying party: crates.io verifies the Sigstore attestation as part of the upload acceptance, and only accepts uploads whose attestation's OIDC identity matches the trusted publishing configuration.

This is a meaningful gap. MEP-67 ships what is available and documents what is missing. The Maven ecosystem is actively working on closer Sigstore integration (tracking issue in the Sonatype Central Portal roadmap as of 2026 Q1), and the bridge's publish flow will be updated when it lands.

## 5. Comparison with other ecosystems

| Registry | OIDC auth | Keyless signing | Registry-verified attestation | GA date |
|----------|-----------|-----------------|-------------------------------|---------|
| PyPI PEP 740 | Yes | Yes (Sigstore) | Yes (Warehouse verifies) | Late 2025 |
| crates.io RFC 3724 | Yes | Yes (Sigstore) | Yes (crates.io verifies) | Q4 2025 rolling |
| npm (provenance) | Yes | Yes (Sigstore) | Yes (registry verifies) | April 2023 |
| Maven Central (Central Portal) | Yes (late 2025) | Yes (sigstore-maven-plugin) | No (separate, unverified) | Sept 2024 (plugin) / late 2025 (OIDC auth) |
| RubyGems.org | Yes | No (gem signing experimental) | No | 2023 (OIDC publish) |

## 6. MEP-67 publish CI workflow

The recommended GitHub Actions workflow for publishing a Mochi package to Maven Central:

```yaml
name: Publish to Maven Central
on:
  push:
    tags: ['v*']

permissions:
  id-token: write   # Required for OIDC token
  contents: read

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-java@v4
        with:
          java-version: '21'
          distribution: 'temurin'
      - name: Install Mochi
        run: curl -sSf https://mochi-lang.org/install.sh | sh
      - name: Publish to Maven Central
        run: mochi pkg publish --to=maven-central
        env:
          # Sonatype Central Portal OIDC auth
          # No explicit token needed; the bridge uses ACTIONS_ID_TOKEN_REQUEST_URL
          # and ACTIONS_ID_TOKEN_REQUEST_TOKEN automatically
          SONATYPE_NAMESPACE: io.mochi
```

The OIDC token exchange, Sigstore attestation, and upload are all handled by `mochi pkg publish --to=maven-central`. The user does not need to configure the `sigstore-maven-plugin` in a POM (the bridge generates the POM and includes the plugin configuration automatically).

## 7. Roadmap

When Sonatype Central Portal integrates Sigstore keyless signing natively:

1. The bridge will exchange the OIDC token with the Central Portal's Fulcio endpoint (rather than Sigstore's public Fulcio).
2. The Central Portal will verify the attestation and reject uploads whose attestation OIDC identity does not match the namespace's trusted publishing configuration.
3. The separate `sigstore-maven-plugin` invocation will be replaced by a single Central Portal API call that handles both upload and signing.
4. The `mochi.lock` will record the Central Portal's signed tlog entry URL alongside the JAR SHA-256.

This roadmap is speculative (based on Sonatype's public roadmap communications as of 2026 Q1) and subject to change.
