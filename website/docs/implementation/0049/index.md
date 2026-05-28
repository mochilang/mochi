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

Per-phase tracking pages:

- [Phase 1. Hello world](phase-01-hello.md)
- [Phase 2. Scalars](phase-02-scalars.md)
- [Phase 3.1. Lists](phase-03-lists.md)
- [Phase 3.2. Maps](phase-03-maps.md)
- [Phase 3.3. Sets](phase-03-sets.md)
- [Phase 3.4. List of records](phase-03-list-of-records.md)
- [Phase 4. Records](phase-04-records.md)
- [Phase 5. Sum types and pattern matching](phase-05-sums.md)
- [Phase 6. Closures and higher-order functions](phase-06-closures.md)
- [Phase 7. Query DSL](phase-07-query.md)
- [Phase 8. Datalog](phase-08-datalog.md)
- [Phase 9. Agents (actor + AsyncStream)](phase-09-agents.md)
- [Phase 10. Streams (AsyncSequence)](phase-10-streams.md)
- [Phase 11. Async colouring and typed throws](phase-11-async.md)
- [Phase 12. FFI (module maps, @_silgen_name)](phase-12-ffi.md)
- [Phase 13. LLM (FoundationModels on Apple)](phase-13-llm.md)
- [Phase 14. fetch (URLSession)](phase-14-fetch.md)
- [Phase 15. iOS app bundle (.ipa via xcodebuild)](phase-15-ios.md)
- [Phase 16. Reproducible build](phase-16-repro.md)
- [Phase 17. Static Linux SDK single binary](phase-17-static-linux.md)
- [Phase 18. App Store / Mac App Store validation](phase-18-appstore.md)
