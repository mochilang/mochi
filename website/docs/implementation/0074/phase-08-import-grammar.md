---
title: "Phase 8. Import-go grammar extension"
sidebar_position: 10
sidebar_label: "Phase 8. Grammar"
description: "MEP-74 Phase 8 extends the main Mochi parser to validate `import go \"<module>@<semver>\" as <alias>`. A version-pinned form requires a FQDN-style module path (first segment must contain a dot), a non-empty whitespace-free version, and an explicit alias; stdlib FFI imports without an `@` pin continue to parse unchanged. Two new diagnostic codes (P067/P068) land along with the GoImportRef helper that downstream phases (9 build orchestration, 10 lockfile, 11 publish) consume to split the path into module + version."
---

# Phase 8. Import-go grammar extension

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 23:25 (GMT+7) |
| Landed         | 2026-05-29 23:34 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase8GoImport` in `parser/phase08_go_import_test.go`
exercises the full lex -> parse -> normalize -> validate pipeline
over a mixed source covering every shape phase 8's grammar admits:

- Version-pinned FQDN imports with aliases
  (`github.com/spf13/cobra@v1.8.0 as cobra`).
- Non-trivial path / version shapes (`gopkg.in/yaml.v3@v3.0.1`,
  major-version `/v2`, pseudo-versions).
- Stdlib FFI imports without an `@` pin (`fmt`, `net/http`), with
  and without an alias.

It also asserts the structural errors land as positioned
diagnostics with the MEP-74-allocated codes:

- **P067** (`go import path %q with `@` pin is not in
  `<module>@<semver>` form`) for non-FQDN module paths, empty
  versions, consecutive dots in the path, etc.
- **P068** (`go import %q with `@` pin requires `as <alias>`)
  when the version-pinned form omits the `as` clause.

Plus golden fixtures end-to-end:

- `tests/parser/valid/import_go.mochi` -> `import_go.golden`:
  parser produces the expected AST for the happy path corpus
  (cobra, yaml.v3, x/tools, stdlib fmt, stdlib net/http).
- `tests/parser/errors/import_go_bad_path.mochi` -> `.err`:
  rendered P067 diagnostic for `foo/bar@v1.0.0` (no dot in first
  segment).
- `tests/parser/errors/import_go_missing_alias.mochi` -> `.err`:
  rendered P068 diagnostic for pinned import without `as`.
- `tests/parser/errors/import_go_empty_version.mochi` -> `.err`:
  rendered P067 diagnostic for `github.com/spf13/cobra@` (empty
  version).

In addition, `parser/go_import_test.go` covers:

- `TestGoImportRef`: 23-case matrix over (happy paths × structural
  errors). Validates the splitter rejects empty modules, empty
  versions, missing dots, empty segments, segment-leading
  underscores/dots/hyphens, consecutive dots, `+` in the path,
  whitespace in the version, and the empty string.
- `TestHasGoSemverPin`: 5 cases distinguishing pinned from stdlib
  forms (the precheck `validateGoImport` uses to decide whether
  to apply the strict validator).
- `TestIsGoModulePath`: 17 cases over module-path validity rules
  (FQDN-style first segment, segment shape, accepted punctuation).

## Lowering decisions

Phase 8's grammar pattern is *additive*: the existing
`import <lang> "<path>" [as <alias>] [auto] [! effects]` ABNF from
parser/ast.go:751 already admits MEP-74's surface form. What
phase 8 lands is the **semantic validator** that turns a malformed
`<module>@<semver>` into a positioned parse-time diagnostic
rather than a "module not found" failure deep inside phase 9's
build orchestration.

The **FQDN-first-segment rule** (the first path segment must
contain a dot) is the discriminator between stdlib FFI imports
(no version required) and proxy.golang.org imports (version
required). The strict subset:

- `fmt`, `net/http` -- first segment is `fmt` or `net`; no dot;
  classified as stdlib; no pin required; alias optional.
- `github.com/spf13/cobra` -- first segment is `github.com`; has
  a dot; classified as proxy.golang.org module; pin required;
  alias required.

This mirrors how Go's own `cmd/go` distinguishes stdlib from
external modules: the stdlib path set is the closed set returned
by `go list -m std`; everything else must be a module with a
version in `go.sum`. Phase 8 doesn't materialise the stdlib set
(no `go list` invocation at parse time); the FQDN rule is the
cheap structural proxy that catches the typo class while leaving
the precise stdlib boundary to phase 9.

The **alias-required-when-pinned** rule (P068) is MEP-74's
contract: the extern fn emitter (phase 7) namespaces every wrapped
function under `<alias>.<Name>`, and the build orchestrator (phase
9) uses the alias as the Go package name suffix
(`mochi_go_<flat>_<alias>`). Without a user-chosen alias, two
imports of the same module under different versions would collide
in the extern namespace. Stdlib imports don't have this problem
because they don't produce a generated extern file -- they reuse
the existing MEP-54 phase 10 FFI surface.

The **module-path character set** is the strict subset of
golang.org/ref/mod (ASCII letters, digits, `.`, `_`, `-`, `~`,
with a segment-leading alnum constraint). The Go reference adds
`+` and a few other punctuation marks, but the public corpus on
proxy.golang.org (as of April 2026's 1.5-million-module snapshot)
does not exercise those, so the strict subset turns typos like
`github.com/foo+bar` into a parse-time error rather than a "404
not found" deep in the build.

The **version-character set** is "anything non-whitespace and
non-empty". This admits semver tags (`v1.2.3`), pre-release tags
(`v1.2.3-rc.1`), build metadata (`v1.2.3+build.5`), pseudo-
versions (`v0.0.0-20260520150000-abcdef012345`), and the rare
git rev shape (`abcdef0`). Stricter parsing belongs in
`package3/go/semver/` (phase 1's helper); phase 8's job is just
to catch whitespace-in-version typos.

## Files changed

| File | Purpose |
|------|---------|
| `parser/go_import.go` | `GoImportRef`, `HasGoSemverPin`, `isGoModulePath`, `isGoModuleSegment`, `isGoModuleStartByte`. |
| `parser/go_import_test.go` | 3 test funcs covering 23 splitter cases, 5 pin-check cases, 17 module-path cases. |
| `parser/phase08_go_import_test.go` | `TestPhase8GoImport` end-to-end sentinel (7 sub-tests). |
| `parser/normalize.go` | `errInvalidGoImportPath` (P067), `errGoImportMissingAlias` (P068), `validateGoImport`, wire into `normalizeStatement`. |
| `tests/parser/valid/import_go.mochi` + `.golden` | Happy-path AST fixture. |
| `tests/parser/errors/import_go_bad_path.{mochi,err}` | P067 rendered diagnostic. |
| `tests/parser/errors/import_go_missing_alias.{mochi,err}` | P068 rendered diagnostic. |
| `tests/parser/errors/import_go_empty_version.{mochi,err}` | P067 rendered diagnostic for empty version. |
| `website/docs/implementation/0074/phase-08-import-grammar.md` | (this page) |

## Test set

- `TestPhase8GoImport` (7 sub-tests)
- `TestGoImportRef` (23 cases)
- `TestHasGoSemverPin` (5 cases)
- `TestIsGoModulePath` (17 cases)
- `TestParser_ValidPrograms/import_go` (golden round-trip)
- `TestParser_SyntaxErrors/import_go_bad_path`
- `TestParser_SyntaxErrors/import_go_missing_alias`
- `TestParser_SyntaxErrors/import_go_empty_version`

Local run on darwin-arm64:

```
$ go test ./parser/...
ok      mochi/parser    3.205s
$ go vet ./parser/...
(no output)
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 8 is complete -- there are no deferred sub-phases for the
grammar itself; downstream consumers (phase 9 build orchestration,
phase 10 lockfile) extend behaviour but the grammar surface is
final.

The two diagnostic codes (P067, P068) are reserved in MEP-2's
parser-error registry; the next available slot is P069. The codes
are stable and downstream tooling (LSP, IDE) keys on them for
quick-fix surface.

`GoImportRef` is exported because phase 9, 10, and 11 all consume
it: phase 9's build orchestrator resolves the import to a module
proxy URL; phase 10's lockfile records `(module, version, h1:)`
keyed on the parsed pair; phase 11's `TargetGoLibrary` writes the
matching `go.mod` `require` line.

`HasGoSemverPin` is a deliberately cheap precheck so the
normalisation pass can dispatch the strict-vs-stdlib path without
re-parsing. It also leaves room for a future phase 8.x where the
parser materialises an `ImportGoRef` AST node carrying the split
fields; for now keeping the split in a string helper means phase
8 lands without a backwards-compatibility break for existing
`*ImportStmt` consumers.
