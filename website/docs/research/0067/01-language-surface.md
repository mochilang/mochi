---
title: "01. Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The import java surface form, mochi.toml java-dependencies table, CLI surface, and per-import alias resolution."
---

# 01. Language surface

## Import syntax

```mochi
import java "com.google.guava:guava@33.0.0-jre" as guava
import java "io.grpc:grpc-core@1.62.0" as grpc
```

The path format is `groupId:artifactId@version`. The version is required; an import without a pinned version is rejected with diagnostic P070 (invalid Java import path). An import without `as <alias>` is rejected with diagnostic P071 (missing alias).

## mochi.toml

Java dependencies declared in the manifest:

```toml
[java-dependencies]
"com.google.guava:guava" = "33.0.0-jre"
"io.grpc:grpc-core" = "1.62.0"
```

Each entry maps a `groupId:artifactId` key to a version string. The bridge validates the coordinate format at manifest parse time.

## CLI surface

```
mochi pkg add java "com.google.guava:guava@33.0.0-jre"
mochi pkg publish --to=maven-central
mochi pkg lock --check
```

`mochi pkg add java` resolves the coordinate, fetches the JAR, runs reflection, synthesises the wrapper, and writes the `[[java-package]]` lock entry.

`mochi pkg publish --to=maven-central` assembles the Sonatype Central Portal bundle and uploads it.

## Alias resolution

The alias following `as` becomes the Mochi namespace for all bridged classes. Within a Mochi source file, `guava.Optional` resolves to the bridged `com.google.common.base.Optional` class. Sub-namespacing uses dot notation matching the Java package hierarchy under the artifact's root package.
