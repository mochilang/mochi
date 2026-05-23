---
title: MEP-49 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 49. Mochi-to-Swift transpiler"
description: "Per-phase implementation tracking for MEP-49 (Mochi-to-Swift transpiler for iOS, macOS, Linux, Windows). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-49 implementation tracking

Per-phase tracking for [MEP-49 Mochi-to-Swift transpiler](/docs/mep/mep-0049). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main`.

A phase is LANDED only when its gate is green on every target listed for it in MEP-49 §Phases. Missing targets become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title                                            | Status      | Commit |
|-------|--------------------------------------------------|-------------|--------|
| 1     | Hello world                                      | NOT STARTED | n/a    |
| 2     | Scalars                                          | NOT STARTED | n/a    |
| 3.1   | Lists                                            | NOT STARTED | n/a    |
| 3.2   | Maps                                             | NOT STARTED | n/a    |
| 3.3   | Sets                                             | NOT STARTED | n/a    |
| 3.4   | List of records                                  | NOT STARTED | n/a    |
| 4     | Records                                          | NOT STARTED | n/a    |
| 5     | Sum types and pattern matching                   | NOT STARTED | n/a    |
| 6     | Closures and higher-order functions              | NOT STARTED | n/a    |
| 7     | Query DSL                                        | NOT STARTED | n/a    |
| 8     | Datalog                                          | NOT STARTED | n/a    |
| 9     | Agents (actor + AsyncStream)                     | NOT STARTED | n/a    |
| 10    | Streams (AsyncSequence)                          | NOT STARTED | n/a    |
| 11    | async colouring, typed throws                    | NOT STARTED | n/a    |
| 12    | FFI (module maps, @_silgen_name)                 | NOT STARTED | n/a    |
| 13    | LLM (FoundationModels on Apple)                  | NOT STARTED | n/a    |
| 14    | fetch (URLSession)                               | NOT STARTED | n/a    |
| 15    | iOS app bundle (.ipa via xcodebuild)             | NOT STARTED | n/a    |
| 16    | Reproducible build                               | NOT STARTED | n/a    |
| 17    | Static Linux SDK single binary                   | NOT STARTED | n/a    |
| 18    | App Store / Mac App Store validation             | NOT STARTED | n/a    |

Per-phase tracking pages will be added as phases open.
