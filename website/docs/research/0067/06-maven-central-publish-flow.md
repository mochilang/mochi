---
title: "06. Maven Central publish flow"
sidebar_position: 7
sidebar_label: "06. Maven Central publish flow"
description: "Maven coordinate system, POM structure, Maven repo protocol directory layout, SHA-1 and SHA-256 checksums, Maven Central's four-artifact requirement (jar, sources, javadoc, pom), Sonatype Central Portal Publisher API, namespace verification, GPG signing vs Sigstore attestations, versioning policy, release vs staging workflow."
---

# 06. Maven Central publish flow

This note covers the Maven Central publishing pipeline: what artifacts are required, how they are structured, and how MEP-67 orchestrates the upload.

## 1. Maven coordinate system

A Maven coordinate is a triple:

```
groupId : artifactId : version
```

- `groupId`: a reversed domain name or organisation identifier (e.g., `com.google.guava`, `org.apache.commons`, `io.mochi`).
- `artifactId`: the project name within the group (e.g., `guava`, `commons-lang3`, `my-library`).
- `version`: a version string in Maven's version comparison scheme (e.g., `32.1.2-jre`, `3.14.0`, `1.0.0`).

Maven Central's repository layout maps coordinates to a directory path:

```
https://repo1.maven.org/maven2/{groupId-slashed}/{artifactId}/{version}/
```

where `groupId-slashed` replaces `.` with `/`. For `com.google.guava:guava:32.1.2-jre`:

```
https://repo1.maven.org/maven2/com/google/guava/guava/32.1.2-jre/
├── guava-32.1.2-jre.jar
├── guava-32.1.2-jre.jar.sha1
├── guava-32.1.2-jre.jar.sha256
├── guava-32.1.2-jre.jar.md5   (legacy, not checked by bridge)
├── guava-32.1.2-jre.pom
├── guava-32.1.2-jre.pom.sha1
├── guava-32.1.2-jre.pom.sha256
├── guava-32.1.2-jre-sources.jar
├── guava-32.1.2-jre-sources.jar.sha1
├── guava-32.1.2-jre-sources.jar.sha256
├── guava-32.1.2-jre-javadoc.jar
├── guava-32.1.2-jre-javadoc.jar.sha1
└── guava-32.1.2-jre-javadoc.jar.sha256
```

The bridge fetches the main JAR and POM. Sources and Javadoc JARs are fetched only when the user runs `mochi pkg source java "..."` (a future command).

## 2. POM structure

The POM (Project Object Model) is an XML file that describes the artifact's metadata and dependencies. The bridge reads the POM to resolve transitive dependencies:

```xml
<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>com.google.guava</groupId>
  <artifactId>guava</artifactId>
  <version>32.1.2-jre</version>
  <packaging>jar</packaging>
  <dependencies>
    <dependency>
      <groupId>com.google.guava</groupId>
      <artifactId>failureaccess</artifactId>
      <version>1.0.1</version>
      <scope>compile</scope>
    </dependency>
    <dependency>
      <groupId>com.google.code.findbugs</groupId>
      <artifactId>jsr305</artifactId>
      <version>3.0.2</version>
      <scope>compile</scope>
      <optional>true</optional>
    </dependency>
  </dependencies>
</project>
```

The bridge parses POMs in Go using a standard XML parser. It follows `<parent>` references (for multi-module Maven projects where the parent POM inherits `<dependencyManagement>` settings), resolves property references (`${project.version}`, `${guava.version}`), and applies scope filtering (only `compile` and `runtime` scope deps are included in the classpath; `test` and `provided` are excluded).

## 3. SHA-1 and SHA-256 verification

Maven Central publishes checksum files alongside every artifact:

- `.sha1`: the SHA-1 hash of the artifact (hex-encoded, 40 characters).
- `.sha256`: the SHA-256 hash (hex-encoded, 64 characters). Available for artifacts built with Maven 3.9+ (which introduced `maven.checksums.sha256=true` by default).
- `.md5`: MD5 (legacy, not verified by the bridge due to collision vulnerabilities).

The bridge verification flow for a JAR fetch:

1. Fetch `<artifact>.jar` and `<artifact>.jar.sha1` concurrently.
2. Compute SHA-1 of the downloaded bytes.
3. Compare against the fetched `.sha1` file.
4. If `.sha256` is available (HTTP 200 on the `.sha256` URL), fetch it and verify as well.
5. If either check fails, reject the JAR and emit an error.

SHA-1 is retained as the primary check (Maven Central's canonical integrity mechanism and universally available) and SHA-256 as an additional check where available. The `mochi.lock` records both (`jar-sha1`, `jar-sha256`).

## 4. Transitive dependency resolution

The bridge implements a simplified Maven dependency resolution algorithm:

1. Start with the root `[java-dependencies]` list.
2. For each root dependency, fetch its POM and extract `<dependencies>` with `<scope>` of `compile` or `runtime`.
3. Recursively process each transitive dependency.
4. Maintain a version map: the first time a `groupId:artifactId` pair is seen, its version is locked. Subsequent occurrences of the same `groupId:artifactId` at a different version are resolved to the already-locked version (nearest-wins rule, matching Maven's default conflict resolution).
5. Apply exclusions declared in `[java-dependencies]` entries: if `groupId:artifactId` appears in the `exclusions` list, it and its transitive deps are omitted.

This is a faithful implementation of Maven's dependency mediation algorithm. It does not implement the full Maven version range syntax (`[1.0, 2.0)`) or the full `<dependencyManagement>` import resolution of multi-module Maven projects; these are deferred to a future phase.

## 5. Content-addressed JAR cache

Fetched JARs are stored in `~/.cache/mochi/java-deps/<sha256-hex>/`:

```
~/.cache/mochi/java-deps/
├── 3c679bd3c4a1e9cc70c92f0d.../
│   ├── jar         (the JAR file)
│   ├── pom         (the POM file)
│   ├── meta.json   (groupId, artifactId, version, sha1, sha256, fetch-time)
│   └── reflect.json (cached ReflectTool output)
```

The cache key is the JAR's SHA-256 (computed from the downloaded bytes). This means two artifacts with the same content (e.g., a JAR that was republished under a different version with no changes) share a single cache entry. The cache is content-addressed and append-only; entries are never mutated in place.

`mochi pkg cache --list-java` shows cache entries. `mochi pkg cache --prune-java` removes entries not referenced by any `mochi.lock` in the user's project tree.

## 6. Maven Central's four-artifact requirement

Maven Central requires all four artifacts for a published release:

| Artifact | Filename pattern | Required | Contents |
|----------|-----------------|----------|---------|
| Main JAR | `<artifactId>-<version>.jar` | Yes | Compiled class files |
| Sources JAR | `<artifactId>-<version>-sources.jar` | Yes | Java source files |
| Javadoc JAR | `<artifactId>-<version>-javadoc.jar` | Yes | Generated HTML Javadoc |
| POM | `<artifactId>-<version>.pom` | Yes | Project metadata and dependencies |

Historically, the Central Portal also required a GPG signature (`.asc`) for each artifact. As of 2024, GPG signatures are optional when Sigstore attestations are present. MEP-67's `TargetJavaLibrary` produces all four artifacts plus Sigstore `.att` bundles.

## 7. Sonatype Central Portal Publisher API

The Sonatype Central Portal (https://central.sonatype.com) replaced OSSRH (Nexus Repository Manager) as Maven Central's ingestion gateway. OSSRH was retired in February 2024.

The publish workflow:

### Step 1: Authentication

For CI environments (GitHub Actions):

```
POST https://central.sonatype.com/api/v1/auth/token
Content-Type: application/json

{
  "oidcToken": "<GitHub Actions OIDC JWT>",
  "provider": "github_actions"
}
```

Returns a short-lived upload token (valid for 1 hour).

For non-CI environments (human developers):

```
POST https://central.sonatype.com/api/v1/auth/token
Content-Type: application/json

{
  "username": "<sonatype-username>",
  "password": "<sonatype-password-or-api-token>"
}
```

### Step 2: Upload

The upload bundle is a ZIP file containing:

```
<artifactId>-<version>/
├── <artifactId>-<version>.jar
├── <artifactId>-<version>.jar.sha1
├── <artifactId>-<version>.jar.sha256
├── <artifactId>-<version>.jar.asc           (if GPG signing enabled)
├── <artifactId>-<version>.jar.att            (Sigstore attestation bundle)
├── <artifactId>-<version>-sources.jar
├── <artifactId>-<version>-sources.jar.sha1
├── <artifactId>-<version>-sources.jar.sha256
├── <artifactId>-<version>-sources.jar.att
├── <artifactId>-<version>-javadoc.jar
├── <artifactId>-<version>-javadoc.jar.sha1
├── <artifactId>-<version>-javadoc.jar.sha256
├── <artifactId>-<version>-javadoc.jar.att
├── <artifactId>-<version>.pom
├── <artifactId>-<version>.pom.sha1
├── <artifactId>-<version>.pom.sha256
└── <artifactId>-<version>.pom.att
```

```
POST https://central.sonatype.com/api/v1/publisher/upload
Authorization: Bearer <upload-token>
Content-Type: multipart/form-data

bundle=<ZIP bytes>
publishingType=AUTOMATIC  (or USER_MANAGED for staged releases)
```

### Step 3: Poll status

```
GET https://central.sonatype.com/api/v1/publisher/status?id=<deployment-id>
Authorization: Bearer <upload-token>
```

Returns a JSON object with `deploymentState`: `PENDING`, `VALIDATING`, `PUBLISHING`, `PUBLISHED`, or `FAILED`. The bridge polls every 5 seconds until the state is terminal.

### Step 4: Handle result

On `PUBLISHED`: print the Maven Central artifact URL (`https://central.sonatype.com/artifact/<groupId>/<artifactId>/<version>`) and the publication timestamp.

On `FAILED`: print the `errors` array from the status response and exit non-zero.

## 8. Namespace verification

Before a namespace (groupId) can be used to publish to Maven Central, the publisher must verify ownership:

- For `com.*`, `org.*`, `io.*`, `net.*`: prove ownership of the corresponding domain name (e.g., `com.example` requires controlling `example.com`) by either adding a TXT DNS record or placing a verification file at a URL on the domain.
- For GitHub-hosted projects: `io.github.<username>` and `com.github.<username>` are automatically verifiable by authorising the Sonatype Central Portal GitHub App.

MEP-67 does not automate namespace verification; it must be completed out-of-band before the first publish. The bridge checks at publish time whether the declared `group-id` is a verified namespace and emits a clear error if not.

## 9. Versioning policy

Maven Central does not accept:

- `SNAPSHOT` versions (ending in `-SNAPSHOT`). The bridge validates this at `mochi pkg publish` time.
- Re-uploads of an existing version. Maven Central is immutable; once published, a version cannot be changed. The bridge checks whether the version already exists on Central before uploading.
- Non-release versions (pre-release qualifiers like `-alpha`, `-beta`, `-RC1` are accepted by Maven Central but generate a warning from the bridge, recommending the user opt in explicitly via `[java.publish] allow-prerelease = true`).

## 10. The generated POM

The `TargetJavaLibrary` path generates a POM from `mochi.toml` metadata. A minimal generated POM:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                             https://maven.apache.org/xsd/maven-4.0.0.xsd">
  <modelVersion>4.0.0</modelVersion>

  <groupId>io.mochi</groupId>
  <artifactId>my-library</artifactId>
  <version>1.0.0</version>
  <packaging>jar</packaging>

  <name>my-library</name>
  <description>My Mochi library published to Maven Central</description>
  <url>https://github.com/example/my-library</url>

  <licenses>
    <license>
      <name>Apache License, Version 2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>

  <developers>
    <developer>
      <name>Jane Doe</name>
      <email>jane@example.com</email>
      <organization>Example Corp</organization>
    </developer>
  </developers>

  <scm>
    <connection>scm:git:https://github.com/example/my-library.git</connection>
    <developerConnection>scm:git:ssh://github.com/example/my-library.git</developerConnection>
    <url>https://github.com/example/my-library</url>
    <tag>v1.0.0</tag>
  </scm>

  <dependencies>
    <!-- Runtime dependency on the Mochi native library shim -->
    <dependency>
      <groupId>io.mochi</groupId>
      <artifactId>mochi-runtime</artifactId>
      <version>0.7.0</version>
      <scope>runtime</scope>
    </dependency>
  </dependencies>
</project>
```

The `<dependencies>` section lists only the `mochi-runtime` shim JAR (which wraps the native library loading). The Mochi source package's transitive Mochi dependencies are compiled into the native library and do not appear as Maven dependencies.
