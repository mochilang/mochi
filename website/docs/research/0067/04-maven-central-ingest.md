---
title: "04. Maven Central ingest"
sidebar_position: 5
sidebar_label: "04. Maven Central ingest"
description: "Maven Central HTTP API, SHA-1 checksum verification, content-addressed JAR cache, and the reflection tool embedded-JAR approach."
---

# 04. Maven Central ingest

## Maven Central HTTP API

Maven Central exposes artifacts at a stable URL pattern:

```
https://repo1.maven.org/maven2/{groupId path}/{artifactId}/{version}/{artifactId}-{version}.jar
https://repo1.maven.org/maven2/{groupId path}/{artifactId}/{version}/{artifactId}-{version}.jar.sha1
```

The `groupId` uses `.` as a separator in the coordinate but `/` in the URL path (`com.google.guava` becomes `com/google/guava`). MEP-67's `maven.Client` handles this translation.

## SHA-1 checksum verification

Maven Central publishes a `.jar.sha1` file alongside each JAR. The `maven.Client` fetches both, verifies the SHA-1, and rejects the JAR if the checksum does not match. SHA-256 is not published by Maven Central natively (it is an extension offered by some mirrors). The bridge computes SHA-256 locally after SHA-1 verification and records both in `mochi.lock`.

## Content-addressed JAR cache

Verified JARs are stored in `~/.cache/mochi/java-deps/<sha256>`. The atomic write pattern (write to `<sha256>.tmp`, then rename) prevents corrupt entries if the process is interrupted. A cache hit skips the network fetch entirely.

## Reflection tool

The reflection tool is a small Java program packaged as an executable JAR (embedded in the Go binary via `go:embed`). When invoked as `java -jar mochi-reflect.jar <jar-path>`, it loads the upstream JAR, enumerates public classes, reflects all public methods and fields, and emits a JSON surface document. The tool runs as a one-shot subprocess: no JVM is kept resident between reflect calls. The surface JSON SHA-256 is recorded in `mochi.lock` as `surface-sha256`.
