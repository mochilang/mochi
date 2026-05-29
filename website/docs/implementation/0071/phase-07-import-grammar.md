---
title: "MEP-71 Phase 7: import grammar"
sidebar_label: "Phase 7. Import grammar"
sidebar_position: 8
description: "Phase 7 of MEP-71. Parses the body of `import python \"<spec>\"` into a typed Spec covering the five MEP-71 §3 spec shapes."
---

# Phase 7: import grammar

Phase 7 introduces `package3/python/importspec`, the parser that consumes the body of the MEP-71 surface form

```
import python "<spec>" as <alias>
```

and produces a typed `Spec` value. The parser does not consume the alias (the MEP-51 grammar already handles that): it is fed only the double-quoted string body. The returned Spec is what Phase 8 (build orchestration) and Phase 9 (lockfile integration) consume.

## Gate

`go test ./package3/python/importspec/...` is green. The gate covers:

- The `Parse` entry point: rejects empty input, leading or trailing whitespace, an empty qualifier after `@`, and any invalid distribution name.
- The five MEP-71 §3 spec shapes, each round-tripping through `Spec.String()`:
  - **Bare** (`requests`) sets `Source=SourceRegistry` and leaves `Specifier` empty.
  - **Semver-pinned** (`requests@>=2.0,<3.0`) parses the PEP 440 specifier into `Specifier.Clauses` via `semver.ParseSpecifier`.
  - **Non-default index** (`torch@torch+https://download.pytorch.org/whl/cu121`) sets `Source=SourceIndex` and `IndexURL` to the URL after the `+`.
  - **VCS** (`mypkg@git+https://github.com/user/repo#commit`) sets `Source=SourceGit`, `GitURL`, and the optional `GitRev` from the fragment.
  - **Local path** (`mypkg@path+../sibling`) sets `Source=SourcePath` and `LocalPath`.
- PEP 503 name normalisation: `Foo.Bar_baz` → `foo-bar-baz`; `RawName` preserves the original spelling for emit and error display, `Name` carries the normalised form for resolution.
- PEP 508 / PEP 426 name validation: rejects empty, leading or trailing separators, internal whitespace, and characters outside `[A-Za-z0-9._-]`.
- Index / local-version disambiguation: a `+` followed by a recognised URL scheme (`http`, `https`, `ssh`, `git`, `file`, `ftp`) is an index URL; a `+` followed by alphanumerics is a PEP 440 local-version segment and stays inside the specifier.
- Git form edge cases: empty URL, empty URL before fragment, empty fragment after `#`, and non-URL targets are all rejected with a Spec-quoted error message.
- Path form edge cases: empty path, `.`, `/`, and paths that collapse to either via `path.Clean` are rejected.
- Index form edge cases: empty index identifier (`@+<url>`) is rejected.
- `Source.String()` renders `registry` / `index` / `git` / `path` / `unknown` for the four valid kinds plus the out-of-range fallback.
- `isURLLike` admits exactly the six schemes the bridge supports and rejects unknown schemes, local-version segments, and the empty string.
- Sentinel `TestPhase7ImportGrammar` walks all five spec shapes end-to-end and asserts the decomposed Spec matches.

## Files

- `package3/python/importspec/doc.go` — package overview enumerating the five spec shapes and pointing at MEP-71 §3 / §5.
- `package3/python/importspec/spec.go` — `Source` enum, `Spec` struct, `Parse`, `Spec.String`, and the `validateName` / `normaliseName` / `isURLLike` helpers.
- `package3/python/importspec/spec_test.go`, `phase07_test.go` — ~40 tests covering the gate above.

## Fixtures

The unit tests cover the spec shapes directly with synthetic strings drawn from the MEP-71 §3 examples. Corpus-wide validation against the 25-package fixture set lands with Phase 8 (build orchestration), where Spec values flow into the wheel-install pipeline and the resolved Specifier drives the uv invocation from Phase 2.

## Skip count

Phase 7 does not introduce its own refusal cases. Any rejection it surfaces (invalid name, malformed version, bad URL) is a hard parse error visible at the call site rather than a Phase-4 / Phase-5 SkipReport. Per-fixture SkipReport counts remain identical to Phase 5's.

## Notes

- The package preserves `RawName` for emit and error display, so the alias and error messages in the importing file show the user's spelling rather than the PEP 503-normalised form. The `Name` field is what the lockfile and resolver key against.
- `Spec.String` is the inverse of `Parse` and is exact for git, path, index, and bare forms. The semver-pinned form round-trips through `semver.Specifier.String()`, which inserts a space after every comma; this is the only intentional whitespace drift.
- Phase 8 will turn `SourceIndex` into a non-default `[[tool.uv.index]]` entry, `SourceGit` into a `git+` VCS URL passed to uv, and `SourcePath` into an editable install rooted at the manifest dir.

## Timestamps

- 2026-05-30 00:00 (GMT+7): Phase 7 started.
- 2026-05-30 00:03 (GMT+7): Phase 7 LANDED.
