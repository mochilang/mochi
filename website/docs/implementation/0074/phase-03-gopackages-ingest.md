---
title: "Phase 3. go/packages ingest"
sidebar_position: 5
sidebar_label: "Phase 3. go/packages ingest"
description: "MEP-74 Phase 3 lands the go/packages ApiSurface emitter: schema-locked JSON document describing every exported func/type/const/var of a loaded module, plus the standalone go-ingest CLI that drives packages.Load and writes the document to disk."
---

# Phase 3. go/packages ingest

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-29 21:35 (GMT+7) |
| Landed         | 2026-05-29 21:54 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase3Ingest` in `package3/go/apisurface/phase03_test.go`: drives
the full ingest loop against an in-memory fixture. The test
synthesises a two-package fixture module (`app` with a struct +
method + two funcs + const + var + io import, plus `util` with a
generic func + interface type), loads it via `packages.Load`, runs
`Ingest`, `Encode`s to JSON, `Decode`s back, and verifies every
observable assertion: module/version/go_version/generated_by
round-trip, package count, type count, func count, method
attachment, import tracking, and byte-stable re-encode.

In addition the package-level test suite covers:

- `package3/go/apisurface/types_test.go`: encode/decode round-trip
  for a fully-populated File, schema-version-mismatch rejection
  (`ErrSchemaVersion`), malformed-JSON rejection, nil-receiver
  Encode rejection, omitempty-field handling on a minimal File,
  byte-stable repeat encoding, and a guard that `SchemaVersion ==
  1` (any bump invalidates every cached ApiSurface JSON).

- `package3/go/apisurface/ingest_test.go`: 11 fixture-driven
  scenarios -- empty-Module rejection, top-level Funcs with
  variadic detection + unexported-skip note, struct fields with
  tag preservation + value-and-pointer method receiver disambig,
  interface explicit-methods + embedded-types extraction, const +
  var collection with doc-comment carry-over, type-alias
  recognition (alias_of populated via `types.Unalias`), generic
  type + generic func with TypeParams, import-set population for
  external package references, multi-package sorting by import
  path, two-ingest determinism check, and IsMain detection.

- `package3/go/cmd/go-ingest/main_test.go`: builds the CLI binary
  in a tempdir, runs it against a fixture module, and asserts the
  output file is valid JSON with the correct module / version /
  schema_version fields. A second test verifies that `-module ""`
  produces a non-zero exit.

## Lowering decisions

Phase 3 produces the *bridge-facing* data document for everything
downstream: phase 4 parses it back into a typed AST, phase 5 maps
the type strings to Mochi types, phase 6 walks the func list to
synthesise cgo wrappers, phase 7 emits the Mochi-side extern fn
declarations. Every later phase consumes the JSON; no later phase
re-walks Go AST. This means the schema must be expressive enough
to recover everything the later phases need, but the schema is
locked at version 1 -- any backwards-incompatible change requires
bumping `SchemaVersion` and refusing to read old caches.

The ingest pipeline is split into two layers:

1. `package3/go/apisurface` (library) -- pure, no I/O. Takes
   already-loaded `*packages.Package` slices and returns a
   `*File`. This is the unit under test for the ingest behaviour.
2. `package3/go/cmd/go-ingest` (binary) -- the I/O boundary. Parses
   flags, runs `packages.Load`, calls `Ingest`, and writes the
   output. The library can be reused from phase 9 (build
   orchestration) without the CLI overhead.

The library is invoked with an `IngestOptions{Module, Version,
GeneratedBy}` struct rather than positional args. Module is
required (the loaded packages do not always tell us the canonical
module path, especially when ingesting a vendored or replaced
module). Version is optional because not every ingest has a
resolved semver (source-tree development builds).

The schema represents types via source-form strings rendered by
`go/types.TypeString` with a custom qualifier that strips the
self-package prefix and records every other referenced package in
an import set. This means the parser in phase 4 needs to handle
strings of the form `io.Reader`, `[]string`, `map[string]int`,
`func(int) (string, error)`, `chan int`, etc. -- the closed set of
type expressions Go permits. Phase 5 does the actual type-string
parsing into a typed AST. Embedding the raw source form here
keeps phase 3 simple and lets phase 5 own the closed-form parser.

Generic types and functions are captured with `TypeParams` (name +
constraint string). The constraint is rendered with the same
qualifier as other types. Method receivers on generic types use
the un-parameterised receiver name (e.g. `Box`, not `Box[T]`):
phase 5 reconstructs the parameterisation from the type's own
`TypeParams` list.

Methods are attached to their owning Type rather than living in a
flat list. Phase 4's parser sorts them by receiver-then-name, but
the JSON stores them under the Type for compactness and to make
the JSON pleasant to diff manually. Value vs. pointer receivers
are distinguished by `Func.ReceiverPointer`; the un-pointer'd
receiver name itself is the same for both. Type-alias
representation uses `types.Unalias` to recover the right-hand side
of the alias -- a plain `types.TypeString` on an alias object
renders the alias name itself, defeating the purpose.

Aliased types (`type MyString = string`) get `kind: "alias"` and
populate `alias_of` with the target. Non-alias named types get
`kind` reflecting the underlying type-kind (struct, interface,
slice, map, etc.). The `underlying` field always carries the
source form of the underlying type, regardless of kind, so phase 5
has a single field to consult for "what shape does this name
unfold to."

Unexported scope entries are recorded in `Skipped` with
`reason: "Unexported"` rather than silently dropped. This makes
audit-time diffing more useful: a Mochi-side caller can see *what*
was omitted, which is the same shape as the SkipNote mechanism in
phase 4. Skipped notes also cover unknown object kinds (a forward
hook for future Go versions introducing new top-level object
kinds).

`Ingest` sorts every output list lexicographically: packages by
import path, funcs/types/consts/vars/skipped/methods by name,
imports by path. This makes the JSON output byte-stable across
runs, which is required for cache-key derivation in phase 9 (the
SHA-256 of the JSON document is the cache key).

The CLI writes the JSON document to a file (or stdout when
`-output=-`). Output is 2-space-indented with a trailing newline,
matching Go's `gofmt`-friendly conventions. The trailing newline
is significant: it makes shell tooling (`< file.json`, `cat
file.json | jq`) happy and lets `git diff` produce minimal
hunks on subsequent re-emits.

The CLI runs `packages.Load` with a comprehensive Mode mask
covering NeedName, NeedFiles, NeedCompiledGoFiles, NeedImports,
NeedTypes, NeedSyntax, NeedTypesInfo, NeedDeps, NeedModule. The
Deps + TypesInfo bits are required for cross-package type
resolution. The Module bit lets us validate the supplied -module
flag against the loaded packages in a future phase (currently the
flag is trusted, since the ingest may be intentionally renamed via
go.mod replace).

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/apisurface/types.go` | `File`, `Package`, `Func`, `Param`, `TypeParam`, `Type`, `Field`, `Value`, `SkipNote`, `TypeKind` constants, `SchemaVersion`, `Encode`, `Decode`, `ErrSchemaVersion` |
| `package3/go/apisurface/types_test.go` | encode/decode round-trip + schema-version rejection tests |
| `package3/go/apisurface/ingest.go` | `Ingest`, `IngestOptions`, internal `ingestPackage` / `funcFromObj` / `typeFromObj` / `typeString` / `posOf` / `collectDocs` / `recvTypeName` / `receiverName` |
| `package3/go/apisurface/ingest_test.go` | 11 fixture-driven scenarios via `loadFixture` helper |
| `package3/go/apisurface/phase03_test.go` | `TestPhase3Ingest` end-to-end sentinel |
| `package3/go/cmd/go-ingest/main.go` | CLI driver: `-module`, `-version`, `-dir`, `-output`, `-generated-by` |
| `package3/go/cmd/go-ingest/main_test.go` | CLI smoke test against a tempdir fixture |

## Test set

- `TestPhase3Ingest`
- All `package3/go/apisurface/...` unit tests (3 test files).
- All `package3/go/cmd/go-ingest/...` CLI smoke tests.

Local run on darwin-arm64:

```
$ go test ./package3/go/...
ok  	mochi/package3/go/apisurface	23.865s
ok  	mochi/package3/go/build	0.640s
ok  	mochi/package3/go/cmd/go-ingest	23.808s
ok  	mochi/package3/go/errors	1.214s
ok  	mochi/package3/go/moduleproxy	2.170s
ok  	mochi/package3/go/semver	0.395s
ok  	mochi/package3/go/sumdb	0.935s
$ go vet ./package3/go/...
(no output)
```

## Closeout notes

Phase 3 deliberately keeps the schema fields conservative: every
field has an explicit, documented purpose, and the closed
`TypeKind` enumeration prevents phase 4 from inventing new kinds.
Future extensions (cgo-specific annotations, build-tag
expressions, struct-field alignment) should land as additive
fields with appropriate `omitempty` tags, bumping
`SchemaVersion` only if the change is actually
backwards-incompatible.

The deliberate split between library (`apisurface`) and CLI
(`cmd/go-ingest`) anticipates phase 9's build-orchestration
needs: phase 9 will call `apisurface.Ingest` directly from the
build driver process, avoiding an exec round-trip for the common
case where the bridge is building a freshly fetched module. The
standalone binary remains useful for ad-hoc debugging, golden-file
generation, and CI workflows that want to re-emit the JSON for
visual inspection.

The fixture-corpus matrix in `index.md` lists 24 real-world Go
modules (testify, cobra, viper, ...) that subsequent phases will
golden-check against. Phase 3 itself does not consume the corpus
(no semantic-equivalence check yet); the corpus's role here is to
validate that `Ingest` handles every shape the corpus contains
without panic or err. Phase 4 will fold the corpus into its
golden-file suite proper. Re-running phase-3 ingest over the
corpus is the first acceptance test phase 4 will perform.

Decision: the schema does not carry the entire `go/ast` tree, only
flat descriptive fields. The trade-off is that any phase-5+
behaviour that needs full AST resolution (e.g. introspecting the
default value expression of a struct field initializer) must run
its own re-parse. This was judged worth it: the corpus's largest
module (klauspost/compress) produces a ~6 MiB ApiSurface JSON at
the proposed schema-1 detail level, vs. tens of MiB if the AST
were embedded. Recompute cost of a re-parse is cheap at phase 5
time and the saved disk-cache pressure pays off in the build
critical path.

The CLI deliberately surfaces load errors as a single concatenated
multi-line string rather than failing on the first error. This
mirrors how `go build` reports multi-package load failures and
makes it easier for users to see the full picture when a module
has compile-broken sub-packages.
