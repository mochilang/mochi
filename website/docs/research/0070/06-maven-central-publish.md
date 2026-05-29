---
title: "06. Maven Central publish flow"
sidebar_position: 7
sidebar_label: "06. Maven Central publish"
description: "The Sonatype Central Portal REST API, the deployment bundle format (JARs + POM + .asc signatures + checksums), the namespace verification requirements, the status polling protocol, and the legacy OSSRH migration."
---

# 06. Maven Central publish flow

When the user runs `mochi pkg publish --to=maven-central`, the bridge assembles a deployment bundle and POSTs it to Sonatype's Central Portal API. This note documents every step.

## Central Portal overview

Sonatype Central Portal (central.sonatype.com) replaced the legacy OSSRH (Nexus-based staging) in February 2024. The publishing API has three endpoints:

| Endpoint | Method | Purpose |
|----------|--------|---------|
| `/api/v1/publisher/upload` | POST | Upload a deployment bundle ZIP. Returns a `deploymentId`. |
| `/api/v1/publisher/status?id=<deploymentId>` | GET | Poll deployment status. |
| `/api/v1/publisher/published?namespace=<ns>&name=<artifact>&version=<v>` | GET | Check if a version is already published. |

Authentication is via a Base64-encoded `username:password` Bearer token header, where `username` and `password` are Portal-generated user tokens (not Sonatype account credentials). The bridge reads `MOCHI_MAVEN_USERNAME` and `MOCHI_MAVEN_PASSWORD` from the environment.

## Deployment bundle format

The bundle is a ZIP file with the following structure:

```
mylib-1.0.0-bundle.zip
  com/example/mylib/1.0.0/
    mylib-1.0.0.jar           (compiled classes)
    mylib-1.0.0.jar.asc       (GPG signature)
    mylib-1.0.0.jar.sha1
    mylib-1.0.0.jar.md5
    mylib-1.0.0.jar.sha256
    mylib-1.0.0-sources.jar
    mylib-1.0.0-sources.jar.asc
    mylib-1.0.0-sources.jar.sha1
    mylib-1.0.0-sources.jar.md5
    mylib-1.0.0-sources.jar.sha256
    mylib-1.0.0-javadoc.jar
    mylib-1.0.0-javadoc.jar.asc
    mylib-1.0.0-javadoc.jar.sha1
    mylib-1.0.0-javadoc.jar.md5
    mylib-1.0.0-javadoc.jar.sha256
    mylib-1.0.0.pom
    mylib-1.0.0.pom.asc
    mylib-1.0.0.pom.sha1
    mylib-1.0.0.pom.md5
    mylib-1.0.0.pom.sha256
```

Maven Central requires all four files per artifact (classes, sources, javadoc, POM), GPG signatures for each, and SHA-1/MD5/SHA-256 checksums for each. Missing any of these causes the deployment to fail with a `VALIDATION_FAILED` status.

## POM requirements

The `pom.xml` must include:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project>
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.example</groupId>
  <artifactId>mylib</artifactId>
  <version>1.0.0</version>
  <name>My Mochi Library</name>
  <description>A library published from Mochi</description>
  <url>https://github.com/example/mylib</url>
  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
    </license>
  </licenses>
  <developers>
    <developer>
      <id>alice</id>
      <name>Alice</name>
      <email>alice@example.com</email>
    </developer>
  </developers>
  <scm>
    <connection>scm:git:https://github.com/example/mylib.git</connection>
    <developerConnection>scm:git:https://github.com/example/mylib.git</developerConnection>
    <url>https://github.com/example/mylib</url>
  </scm>
</project>
```

The bridge constructs the POM from `[kotlin.publish]` in `mochi.toml`. All required fields (`groupId`, `artifactId`, `version`, `name`, `description`, `url`, `licenses`, `developers`, `scm`) are validated before the bundle is assembled; missing fields produce an error with the field name and the `mochi.toml` key to add.

## GPG signing

Maven Central requires GPG/PGP signatures (`.asc` files) for every artifact. The bridge supports two signing modes:

**Mode 1: Environment-variable signing key**

```
MOCHI_MAVEN_SIGNING_KEY=<ASCII-armored GPG private key>
MOCHI_MAVEN_SIGNING_KEY_PASSPHRASE=<passphrase>
```

The bridge imports the key into an in-process Go GPG implementation (`golang.org/x/crypto/openpgp`) and signs each artifact without writing the key to disk. The key fingerprint is validated against the `[kotlin.publish] signing-key-id` value if set.

**Mode 2: Sonatype Central Portal OIDC (beta, May 2026)**

Sonatype's beta OIDC trusted-publishing flow: the CI environment provides an OIDC token (GitHub Actions: `ACTIONS_ID_TOKEN_REQUEST_URL` / `ACTIONS_ID_TOKEN_REQUEST_TOKEN`), the bridge exchanges it for a Central Portal session token that carries signing authority, and the server signs on behalf of the user. The bridge detects this mode when `MOCHI_MAVEN_SIGNING_KEY` is absent and `ACTIONS_ID_TOKEN_REQUEST_URL` is present.

When neither mode is available, the bridge exits with an error and an explanation of both options.

## Status polling

After upload, the bridge polls the status endpoint with exponential backoff:

```
POST /api/v1/publisher/upload       → { "deploymentId": "abc123" }
GET  /api/v1/publisher/status?id=abc123 → { "deploymentState": "PENDING" }
GET  /api/v1/publisher/status?id=abc123 → { "deploymentState": "VALIDATING" }
GET  /api/v1/publisher/status?id=abc123 → { "deploymentState": "PUBLISHING" }
GET  /api/v1/publisher/status?id=abc123 → { "deploymentState": "PUBLISHED" }
```

Polling interval: 10 s, 20 s, 40 s, 80 s, 160 s (capped). Total timeout: 15 minutes. `VALIDATION_FAILED` and `FAILED` states exit immediately with the error message from the API response.

After a `PUBLISHED` status, the artifact is available on Maven Central within 30 minutes (search index delay) and immediately via direct coordinates (`repo1.maven.org/maven2/com/example/mylib/1.0.0/`).

## Namespace verification

Maven Central requires namespace ownership verification before any artifact can be published under a `groupId`. This is a one-time manual step that the bridge does not automate (it requires DNS TXT record or GitHub repository verification via the Central Portal web UI). The bridge checks at publish time whether the namespace is already verified by attempting a metadata HEAD request; if not, it prints the verification instructions and exits.

Namespace ownership rules:
- `com.example` requires a DNS TXT record `_sonatype_central_verification=<token>` at `example.com`.
- `io.github.username` is automatically approved if the GitHub OIDC token matches the username.
- Reverse-domain convention: `com.example.subpackage` inherits from `com.example` namespace.

## Sources JAR

The sources JAR contains the Mochi source files for the package, not the generated Kotlin/JVM bytecode source. The directory structure inside the sources JAR mirrors the package structure:

```
mylib-1.0.0-sources.jar
  META-INF/
    MANIFEST.MF
  com/example/mylib/
    main.mochi
    util.mochi
    types.mochi
```

Maven tooling and IDE plugins (IntelliJ, Android Studio) understand sources JARs and attach them for source navigation. Attaching `.mochi` source files is not ideal for Java/Kotlin IDEs, but it provides attribution. A future improvement could include a second `*-kotlin-sources.jar` with the generated Kotlin wrapper source.

## Javadoc JAR

The Javadoc JAR is required by Maven Central even if the library is not primarily Java. The bridge generates a minimal Javadoc from the KDoc comments in the Mochi source:

1. Extract doc-comments from the Mochi AST.
2. Format them as Javadoc HTML for each `extern fn` / `extern type` declaration.
3. Package the HTML in the standard Javadoc directory layout.

If no doc-comments are present in the Mochi source, the bridge generates a minimal Javadoc with the function signatures and a "Generated by Mochi" note. An empty Javadoc JAR (with only `META-INF/MANIFEST.MF`) also satisfies Maven Central's requirement.

## Idempotency and version conflict

The bridge calls `GET /api/v1/publisher/published` before uploading to check if the version already exists. If the version is published, the bridge exits with an error (`version X.Y.Z is already published on Maven Central; increment the version to publish again`). Maven Central does not allow overwriting a published version.

## Cross-references

- [[07-sonatype-trusted-publishing]] for the OIDC signing flow.
- [[11-kmp-android]] for the Android AAR publish variant.
- [MEP-70 §7](/docs/mep/mep-0070#7-cli-surface) for the CLI `publish` subcommand.
