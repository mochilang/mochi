---
title: "09. mochi.lock integration"
sidebar_position: 10
sidebar_label: "09. mochi.lock integration"
description: "The [[java-package]] TOML table schema, four-hash verification, --check mode drift detection, and dependency chain encoding."
---

# 09. mochi.lock integration

## Schema

Each resolved Java import adds a `[[java-package]]` entry to `mochi.lock`:

```toml
[[java-package]]
group = "com.google.guava"
artifact = "guava"
version = "33.0.0-jre"
source = { kind = "maven" }
jar-sha256 = "..."
jar-sha1 = "..."
surface-sha256 = "..."
wrapper-sha256 = "..."
dependencies = ["com.google.code.findbugs:jsr305@3.0.2"]
```

The `source.kind` field accepts `"maven"`, `"git"`, and `"path"`. Only `"maven"` is supported in the initial 16 phases.

## Four-hash verification

`mochi pkg lock --check` compares the four hashes in the lock file against freshly computed values:

- `jar-sha256`: SHA-256 of the downloaded JAR bytes.
- `jar-sha1`: SHA-1 of the downloaded JAR bytes (also verified against Maven Central's `.sha1` file).
- `surface-sha256`: SHA-256 of the JSON surface document produced by the reflection tool.
- `wrapper-sha256`: SHA-256 of the synthesised Java wrapper source.

If any hash drifts, `Check` returns a `CheckError` listing the artifact, field, expected value, and actual value. The check exits non-zero, blocking CI.

## Deterministic encoding

`Encode` sorts `[[java-package]]` entries by `group + artifact` before serialising to TOML. This makes the lock file byte-stable across runs where the resolution order differs.

## Dependency chain

The `dependencies` array encodes the transitive JAR dependencies discovered by inspecting the POM. Dependencies are listed in `groupId:artifactId@version` notation. The lock file records only the direct declared dependencies of each artifact; transitive closure is stored by including entries for each transitive dependency in the `[[java-package]]` list.
