---
title: "Phase 01: Maven Central metadata client"
sidebar_position: 3
sidebar_label: "Phase 01: Maven client"
description: "Maven Central metadata index client: POM fetch, version resolution, transitive dependency graph, and Gradle Module Metadata (GMM) variant selection."
---

# Phase 01: Maven Central metadata client

**Status:** Planned

## Deliverables

1. `package3/kotlin/maven/client.go` — HTTP client for Maven Central metadata.
   - `FetchPOM(coord Coordinate) (*POM, error)` — fetch and parse the POM XML.
   - `FetchModuleMetadata(coord Coordinate) (*GradleModule, error)` — fetch and parse the `.module` file (Gradle Module Metadata).
   - `ResolveVersionRange(groupID, artifactID, versionRange string) (string, error)` — resolve a version range to a concrete version using the Maven metadata XML (`maven-metadata.xml`).
   - `FetchMavenMetadata(groupID, artifactID string) (*MavenMetadata, error)` — fetch `maven-metadata.xml` listing all available versions.
2. `package3/kotlin/maven/pom.go` — POM XML struct and parser (using `encoding/xml`).
3. `package3/kotlin/maven/graph.go` — `ResolveTransitive(rootCoords []Coordinate) ([]Coordinate, error)` — BFS transitive dependency resolution, handling `<exclusions>`, `<scope>`, and dependency management sections.
4. `package3/kotlin/maven/gmm.go` — Gradle Module Metadata parser and JVM variant selector.
5. `package3/kotlin/maven/registry.go` — registry configuration: Maven Central base URL, JitPack, Google Maven, and custom URL support.

## Maven Central metadata protocol

Maven Central serves metadata at:
- `https://repo1.maven.org/maven2/{group/path}/{artifact}/{version}/{artifact}-{version}.pom`
- `https://repo1.maven.org/maven2/{group/path}/{artifact}/maven-metadata.xml`
- `https://repo1.maven.org/maven2/{group/path}/{artifact}/{version}/{artifact}-{version}.module`

Version range resolution reads `maven-metadata.xml` which lists all available versions and the `<release>` and `<latest>` tags.

## Gate

Resolve `kotlinx-coroutines-core@1.7.3` and its 11 transitive dependencies from Maven Central. Validate:

1. Correct transitive graph (11 specific artifacts).
2. All POMs parsed without error.
3. No excluded dependencies included.
4. `scope = "test"` and `scope = "provided"` dependencies excluded from the compile graph.
5. JVM variant correctly selected from the `kotlinx-coroutines-core-1.7.3.module` GMM file.
