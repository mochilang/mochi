---
title: "06. Maven Central publish flow"
sidebar_position: 7
sidebar_label: "06. Maven Central publish flow"
description: "Sonatype Central Portal API, deployment bundle ZIP structure, upload and poll cycle, and the dry-run validation path."
---

# 06. Maven Central publish flow

## Sonatype Central Portal

Since March 2024, the Sonatype Central Portal (`central.sonatype.com`) is the GA publish path for Maven Central. The legacy OSSRH Nexus endpoint still exists for existing publishers but is no longer the recommended path for new artifacts. MEP-67 targets the Central Portal exclusively.

## Bundle ZIP structure

A valid Central Portal bundle ZIP contains:

```
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.jar
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.jar.sha1
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.jar.md5
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.pom
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.pom.sha1
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.pom.md5
# optional:
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.jar.asc
{groupId-path}/{artifactId}/{version}/{artifactId}-{version}.pom.asc
```

`BuildBundle` assembles this ZIP from the wrapper JAR and rendered POM.

## Upload and poll

`Client.Upload` POSTs the bundle as a `multipart/form-data` body to `https://central.sonatype.com/api/v1/publisher/upload`. The response contains a deployment ID. `Client.PollUntilPublished` polls `https://central.sonatype.com/api/v1/publisher/status?id={id}` until the state reaches `PUBLISHED` or `FAILED`.

## Dry-run validation

`DryRun` validates the bundle ZIP without uploading: it checks that required files are present, that SHA-1 and MD5 hashes match the artifact bytes, and that the POM is well-formed XML. The dry-run gate runs in CI before the actual upload step.

## POM rendering

`RenderPOM` produces a minimal Maven POM with `<groupId>`, `<artifactId>`, `<version>`, `<name>`, `<description>`, and `<dependencies>`. The wrapper artifact's group ID is derived by prepending `dev.mochi.java-bridge.` to the upstream group ID, ensuring it lives in a Mochi-controlled Maven coordinate namespace.
