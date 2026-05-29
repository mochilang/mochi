---
title: "07. Sigstore attestation"
sidebar_position: 8
sidebar_label: "07. Sigstore attestation"
description: "in-toto v1.0 predicate schema, Sigstore Bundle 0.3 format, canonical JSON, OIDC token exchange, and base64-RawURL encoding."
---

# 07. Sigstore attestation

## in-toto v1.0 predicate

MEP-67 uses the in-toto v1.0 Statement schema with a Maven-specific predicate type:

```
https://maven.apache.org/spec/MavenCentralPublish/v1
```

The predicate binds the Maven coordinate (`groupId:artifactId:version`) to the JAR SHA-256 digest and the CI OIDC token. The `Subject` field contains the artifact filename and its `sha256` digest.

## Sigstore Bundle 0.3

The bundle media type is `application/vnd.dev.sigstore.bundle.v0.3+json`. The bundle JSON contains the in-toto statement, the OIDC-derived signing certificate chain from Fulcio, and the Rekor transparency log inclusion proof.

## Canonical JSON

`marshalSortedJSON` produces canonical JSON by sorting object keys alphabetically at every level. This ensures the attestation bytes are stable across Go versions and JSON library implementations, which matters for Rekor log verification.

## OIDC token exchange

The CI OIDC token (from `ACTIONS_ID_TOKEN_REQUEST_URL` / `ACTIONS_ID_TOKEN_REQUEST_TOKEN` in GitHub Actions) is exchanged with Fulcio for an ephemeral signing certificate. Fulcio logs the exchange in the Certificate Transparency log. MEP-67 passes the raw OIDC token to `SignBundle`; the caller is responsible for obtaining the token from the CI environment.

## EncodeBundleHeader

`EncodeBundleHeader` base64-RawURL-encodes the bundle bytes for inclusion in the Maven Central upload request header `X-Sigstore-Attestation`. This follows the Central Portal's attestation upload protocol documented in the Sonatype Central Portal API reference (May 2025 revision).
