---
title: MEP-56 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 56. Mochi-to-Ruby transpiler"
description: "Per-phase implementation tracking for MEP-56 (Mochi-to-Ruby transpiler). Status + commit columns get filled in along the way as sub-PRs land."
---

# MEP-56 implementation tracking

Per-phase tracking for [MEP-56 Mochi-to-Ruby transpiler](/docs/mep/mep-0056). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR #22510, the in-branch commit on `mep/0056-ruby`).

A phase is LANDED only when its gate is green on every Ruby runtime listed for it in MEP-56 §6. Missing runtimes become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: rtree / lower / emit / build / runtime gem | LANDED | d5559fc885 | [phase-00](/docs/implementation/0056/phase-00-skeleton) |
| 1 | Hello world | LANDED | d5559fc885 | [phase-01](/docs/implementation/0056/phase-01-hello) |
| 2 | Scalars and arithmetic | LANDED | d5559fc885 | [phase-02](/docs/implementation/0056/phase-02-scalars) |
| 2.5 | Control flow | LANDED | 4738082cde | [phase-025](/docs/implementation/0056/phase-025-control-flow) |
| 3 | Lists | LANDED | 4738082cde | [phase-03](/docs/implementation/0056/phase-03-lists) |
| 3.5 | Maps | LANDED | bdc3ca1fc7 | [phase-035](/docs/implementation/0056/phase-035-maps) |
| 4 | Records (Data.define) | LANDED | 4738082cde | [phase-04](/docs/implementation/0056/phase-04-records) |
| 5 | User functions | LANDED | 4738082cde | [phase-05](/docs/implementation/0056/phase-05-functions) |
| 6 | Sum types and pattern matching | LANDED | bdc3ca1fc7 | [phase-06](/docs/implementation/0056/phase-06-sums) |
| 7 | Closures and higher-order functions | LANDED | a25a09e775 | [phase-07](/docs/implementation/0056/phase-07-closures) |
| 8 | Query DSL (from / where / select / order / skip / take) | LANDED | 271fe13799 | [phase-08](/docs/implementation/0056/phase-08-query) |
| 9 | Datalog (compile-time semi-naive eval) | LANDED | 2ab17282e6 | [phase-09](/docs/implementation/0056/phase-09-datalog) |
| 10 | Channels (Thread::SizedQueue) | LANDED | 00bf88ebb5 | [phase-10](/docs/implementation/0056/phase-10-channels) |
| 11 | Agents (Ruby classes) | LANDED | 9494a06464 | [phase-11](/docs/implementation/0056/phase-11-agents) |
| 12 | async / await (Thread + .value) | LANDED | c6317bc4be | [phase-12](/docs/implementation/0056/phase-12-async) |
| 13 | Streams (Mochi::Runtime::Stream) | LANDED | d0041ea451 | [phase-13](/docs/implementation/0056/phase-13-streams) |
| 14 | Deep string ops (index, contains, substring, reverse, upper, lower, split, join, str) | LANDED | 07eb070db3 | [phase-14](/docs/implementation/0056/phase-14-strings) |
| 15 | List aggregates and HOF (min, max, sum, in, map, filter, reduce) | LANDED | 0732df08e0 | [phase-15](/docs/implementation/0056/phase-15-list-hof) |
| 16 | Sets and ordered maps | LANDED | a39456b067 | [phase-16](/docs/implementation/0056/phase-16-sets-omaps) |
| 17 | Math (abs, floor, ceil) and map helpers (len, keys, values) | LANDED | d4ab0240e7 | [phase-17](/docs/implementation/0056/phase-17-math-map-helpers) |
| 18 | File I/O, JSON, CSV, HTTP | LANDED | 3890f4bd77 | [phase-18](/docs/implementation/0056/phase-18-io-json-csv-http) |
| 19 | try / catch / panic (Mochi::Runtime::Panic) | LANDED | a1b3d4bcdc | [phase-19](/docs/implementation/0056/phase-19-try-catch) |
| 20 | Agent spawn (`spawn AgentType()`) | LANDED | 2157007a50 | [phase-20](/docs/implementation/0056/phase-20-spawn) |
| 21 | Stream subscribe_limit + saveCSV | LANDED | 7a8e62a176 | [phase-21](/docs/implementation/0056/phase-21-stream-limit-savecsv) |
| 22 | TargetRubyGem | LANDED | 59c6cb5f0a | [phase-22](/docs/implementation/0056/phase-22-gem) |
| 23 | TargetRubyBundle | LANDED | 15293215af | [phase-23](/docs/implementation/0056/phase-23-bundle) |
| 24 | TargetIRubyKernel | LANDED | 72522b6311 | [phase-24](/docs/implementation/0056/phase-24-iruby-kernel) |
| 25 | TargetTebako | LANDED | 89202f961a | [phase-25](/docs/implementation/0056/phase-25-tebako) |
| 26 | TargetTruffleNative | LANDED | 64bccce8c5 | [phase-26](/docs/implementation/0056/phase-26-truffle-native) |
| 27 | TargetMRuby | LANDED | 64bccce8c5 | [phase-27](/docs/implementation/0056/phase-27-mruby) |
| 28 | Audit gap closure (driver errors, edge cases, emitted-syntax checks) | LANDED | d43705737a | [phase-28](/docs/implementation/0056/phase-28-audit) |

## Runtime matrix

A phase is fully LANDED only after its gate passes on every required runtime. Phases not yet exercised on a runtime become N.1, N.2, ... sub-phases.

| Runtime | Phases 0-21 (language) | Phases 22-28 (packaging + audit) |
|---------|------------------------|-----------------------------------|
| CRuby 3.2 LTS | NOT EXERCISED IN CI | NOT EXERCISED IN CI |
| CRuby 3.4 | NOT EXERCISED IN CI | NOT EXERCISED IN CI |
| CRuby 4.0 (Homebrew) | LANDED (local) | LANDED (local) |
| JRuby 10 | NOT STARTED | NOT STARTED |
| TruffleRuby 33 | NOT STARTED | NOT STARTED |
| mruby 4 | NOT STARTED (subset) | NOT STARTED (subset) |

CRuby 3.4 / 3.2 / JRuby / TruffleRuby / mruby coverage will land as sub-phases (e.g., 0.1 JRuby 10 on phase 0, 0.2 TruffleRuby 33 on phase 0) once a Ruby CI matrix workflow is added.
