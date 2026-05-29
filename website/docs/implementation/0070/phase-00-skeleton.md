---
title: "Phase 00: Skeleton"
sidebar_position: 2
sidebar_label: "Phase 00: Skeleton"
description: "Create the package3/kotlin/ Go module skeleton with error types, semver parser, and Maven coordinate parser."
---

# Phase 00: Skeleton

**Status:** Planned

## Deliverables

1. `package3/kotlin/go.mod` — new Go module `github.com/mochilang/mochi/package3/kotlin`.
2. `package3/kotlin/errors/errors.go` — bridge-specific error types: `ErrUnsupportedMetadataVersion`, `ErrArtifactNotFound`, `ErrNativeImageBuildFailed`, `ErrCapabilityViolation`, `ErrGraalVMNotFound`, `ErrLockMismatch`.
3. `package3/kotlin/semver/` — Maven version range parser: PVP/Semver/Maven range syntax (`[1.0,2.0)`, `1.7.+`, `LATEST`).
4. `package3/kotlin/maven/coord.go` — Maven coordinate parser: `ParseCoordinate("org.example:mylib@1.0")` → `{GroupID, ArtifactID, Version, Classifier}`.
5. `package3/kotlin/README.md` — component overview.

## Gate

```
go test ./...
```

All packages compile cleanly. The semver parser handles the following inputs:

| Input | Result |
|-------|--------|
| `"1.7.3"` | Exact version 1.7.3 |
| `"[1.0,2.0)"` | `>=1.0 <2.0` |
| `"[1.0,)"` | `>=1.0` |
| `"(,2.0]"` | `<=2.0` |
| `"1.7.+"` | `>=1.7.0 <1.8.0` |
| `"LATEST"` | Latest available (resolve at fetch time) |
| `"RELEASE"` | Latest release (non-SNAPSHOT) |

The Maven coordinate parser accepts:

| Input | Result |
|-------|--------|
| `"org.example:mylib"` | `{GroupID: "org.example", ArtifactID: "mylib"}` |
| `"org.example:mylib@1.0.0"` | `{..., Version: "1.0.0"}` |
| `"org.example:mylib@1.0.0@jdk8"` | `{..., Version: "1.0.0", Classifier: "jdk8"}` |
| `"invalid"` | `ErrInvalidCoordinate` |
