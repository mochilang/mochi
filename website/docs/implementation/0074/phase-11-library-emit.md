---
title: "Phase 11. TargetGoLibrary emit"
sidebar_position: 13
sidebar_label: "Phase 11. TargetGoLibrary"
description: "MEP-74 Phase 11 lands the publish-direction emit layer: a self-contained `package3/go/library/` module that lowers a Mochi package's public surface (const, var, type, func) into a publishable Go module (go.mod, doc.go, <pkg>.go, README, LICENSE, optional _cgo_export.h). Emit is byte-deterministic across slice permutations, gofmt-clean against `gofmt -l`, passes `go vet ./...`, builds with `go build ./...`, and the emitted module imports cleanly into a separate consumer module via a `replace` directive."
---

# Phase 11. TargetGoLibrary emit

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-30 00:00 (GMT+7) |
| Landed         | 2026-05-30 00:30 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase11TargetGoLibrarySentinel` in
`package3/go/library/phase11_test.go` walks a representative
calculator package (Version const, Greeting var, Stats struct,
Operand type definition, Add / Sub / Mul / Record funcs) through
the full Emit -> materialise -> gofmt -> go vet -> go build ->
consumer-run cycle and asserts:

- `gofmt -l .` is silent against the materialised module (the
  emitter produces gofmt-clean Go source as written);
- `go vet ./...` reports no issues;
- `go build ./...` succeeds, so every emitted identifier resolves
  and every body type-checks;
- a separate consumer module that imports the emitted library via
  a `replace example.com/mochi/calc => <emit-dir>` directive builds
  and runs;
- the consumer's output matches the expected calculation result
  string byte-for-byte (`v=v0.1.0 g=hello from calc sum=5 diff=6
  prod=42 stats=5`), proving the wire-level contract;
- doc.go carries the `// Package calc provides ...` first-sentence
  godoc form;
- LICENSE and README artefacts are emitted when `Meta.LicenseText`
  / `Meta.Description` are set.

Plus 33 unit tests in `library_test.go`:

- baseline shape (`TestEmitBaseline`, `TestEmitResultNamesSorted`),
- validation paths (`TestEmitRejectsMissingModulePath`,
  `TestEmitRejectsBadModulePath`, `TestEmitRejectsMissingPackageName`,
  `TestEmitRejectsBadPackageName`, `TestEmitRejectsMissingVersion`,
  `TestEmitRejectsUnexportedFunc`, `TestEmitRejectsUnknownTypeKind`,
  `TestEmitRejectsAliasMissingTarget`,
  `TestEmitRejectsConstMissingValue`),
- determinism (`TestEmitDeterministicAcrossPermutations`,
  `TestEmitDeterministicAcrossRuns`),
- item grouping (`TestSortedItemsGroupsByKind`),
- go.mod rendering (`TestRenderGoModWithDeps`,
  `TestRenderGoModOmitsEmptyRequire`,
  `TestEmitGoVersionOverride`,
  `TestEmitDependencyPropagatesToGoMod`),
- doc.go rendering (`TestRenderDocGoSynthesisesStub`,
  `TestRenderDocGoIncludesRepoAndVersion`),
- per-item rendering (`TestRenderSourceStructWithFields`,
  `TestRenderSourceInterfaceMethods`, `TestRenderSourceAlias`,
  `TestRenderSourceConstAndVar`, `TestRenderSourceFuncEmptyBody`,
  `TestRenderSourceFuncMultiResult`, `TestRenderFuncBodyIndents`),
- cgo extern path (`TestRenderSourceExternEmitsCgoImport`,
  `TestRenderCHeaderDeclares`, `TestRenderCHeaderSkipsNonPrimitive`,
  `TestRenderCHeaderOmittedWhenDisabled`),
- README / LICENSE pass-through (`TestEmitReadmeScaffold`,
  `TestEmitReadmeVerbatim`, `TestEmitLicensePassthrough`),
- identifier helpers (`TestPascalCase`, `TestIsGoIdentifier`,
  `TestIsModulePath`).

## Lowering decisions

The library package is layering-conservative: it imports no other
`package3/go/*` module and depends only on the Go stdlib (`errors`,
`fmt`, `sort`, `strings`, `unicode`). Callers in the build pipeline
compose a `PublicAPI` from their own IR state and hand it to
`Emit`. The package is the single source of truth for the
publish-side Go module shape, so future phase 12 (`mochi pkg
publish --to=go+git+...`) can call `library.Emit` directly and
then drive `git tag` / `git push` from the materialised files.

**Items are grouped by kind, then sorted by name.** The closed
kind order is const, var, type, func. Within each kind, items
sort alphabetically. The grouping is the rendered file layout;
permuting `api.Items` produces identical bytes. This is the same
permutation-stability invariant the MEP-73 rust-library package
holds, and it makes `git diff` show only genuine API changes
rather than slice-order noise.

**Validation is upfront and total.** `Emit` calls `validateAPI`
before rendering anything: an invalid input produces a wrapped
`ErrLibrary` rather than a partially-rendered file set. Required
fields (`ModulePath`, `PackageName`, `Version`) are checked;
identifier validity is enforced (Go keyword names rejected, ASCII
start-letter rule applied); the closed `ItemType.Kind` set
(`struct`, `alias`, `definition`, `interface`) is enforced;
const items must have a value (Go does not allow `const X int`
without an initialiser).

**doc.go carries the first-sentence godoc form.** When the caller
supplies `Meta.DocComment`, the first sentence MUST begin with
`Package <name> ...` to satisfy `go doc`'s summary rule. When the
caller omits the doc comment, `Emit` synthesises a stub
`Package <name> is generated by Mochi MEP-74.` so the package
always has a discoverable summary. The repository URL (from
`Meta.Repository`) and the published version are appended as
secondary lines in the comment block for traceability.

**Extern items emit `//export` and pull in `import "C"`.** Per
MEP-74 spec §3 Direction 2, the library target uses
`-buildmode=c-archive` when shipping to non-Go consumers. The
`ItemFunc.Extern` flag flips the emitter to render an `//export
<Name>` directive immediately above the function declaration; if
any function is extern, the file gets an `import "C"` header.
Non-extern functions render as plain Go funcs. The dual mode
(some extern, some not) is supported in a single rendered file
so callers can incrementally add c-archive exports without
splitting the module layout.

**The `_cgo_export.h` header is the cbindgen analogue for non-Go
non-cgo consumers.** When `PublicAPI.CHeader` is true, `Emit`
renders a plain C header declaring every Extern `ItemFunc` with
primitive-only signatures. The header has an include guard
(`MOCHI_<PKG>_H`), `#include <stdint.h>` + `<stdbool.h>`, an
`extern "C"` block for C++ consumers, and one `extern <retT>
<Name>(<paramT> <paramN>, ...);` line per eligible function.
Non-primitive parameter or return types disqualify the function
from the header with an explanatory `/* skipped X: non-primitive
... */` comment, so callers can see what was deferred without
parsing free-form text. The header is the c-archive contract Mochi
ships to C / C++ / Rust ffi consumers.

**README / LICENSE pass-through prefers verbatim over scaffold.**
When `Meta.Readme` is non-empty, the emitter writes it verbatim
(callers own the entire README content). When `Meta.Readme` is
empty but `Meta.Description` is set, the emitter synthesises a
minimal scaffold with `# <pkg>`, the description, an installation
block (`go get <module>@<version>`), and optional License /
Authors sections. The Authors list is sorted alphabetically for
permutation-stable output. `LICENSE` is verbatim only: if the
caller does not supply `Meta.LicenseText`, no LICENSE artefact is
emitted.

**Go version defaults to 1.21 but is configurable.** The go.mod
`go` directive defaults to `1.21` so the embedded toolchain
accepts generics (Go 1.18+) and the `runtime/cgo.Handle` API (Go
1.17+). Callers can override via `PublicAPI.GoVersion = "1.22"`
or later when they depend on `range over int` or other newer
language features.

**Dependencies render as a sorted `require` block.** Empty
`Dependencies` omits the block entirely (per Go convention).
Non-empty maps sort by module path and render one `require` entry
per line; the encoding mirrors `go mod tidy` output so a follow-up
`go mod tidy` would be a no-op against the emitted module.

**`PascalCase` is exported.** Mochi identifiers are snake_case
(`n_iter`, `default_name`); Go requires PascalCase for exported
names. `library.PascalCase` is the canonical translation helper
the MEP-54 driver calls when populating `PublicAPI.Items`; it is
exported (not internal) so other callers (for example the future
MEP-57 polyglot manifest validator) can use the same mapping
without duplicating the logic.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/library/library.go` | `PublicAPI`, `PackageMeta`, `Item` sealed interface, `ItemFunc` / `ItemType` / `ItemConst` / `ItemVar`, `EmitResult`, `Emit`, `validateAPI`, `sortedItems`, `PascalCase`, identifier helpers. |
| `package3/go/library/render.go` | `renderGoMod`, `renderDocGo`, `renderSource`, `renderItem`, `renderConst` / `renderVar` / `renderType` / `renderFunc`, `renderReadmeScaffold`, `renderCHeader`, `writeGodoc`, `cTypeOf`. |
| `package3/go/library/library_test.go` | 33 unit tests over emit / render / validation / determinism. |
| `package3/go/library/phase11_test.go` | `TestPhase11TargetGoLibrarySentinel` end-to-end materialise + gofmt + go vet + go build + consumer-run. |
| `website/docs/implementation/0074/phase-11-library-emit.md` | (this page) |

## Test set

- `TestPhase11TargetGoLibrarySentinel` (6 sub-tests)
- 33 unit tests in `library_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/library/...
ok      mochi/package3/go/library      30.9s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/library       (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

The 30-second wall time is dominated by the sentinel's consumer
`go run` step (cold gomodcache + go build); the unit-test side
runs in well under a second.

## Closeout notes

Phase 11 lands `library.Emit` as a leaf module: the MEP-54 build
driver wiring (turning a Mochi package IR into a `PublicAPI` and
calling `library.Emit` from `Driver.Build` when target is
`TargetGoLibrary`) is reserved for phase 11.1 once the MEP-54
driver gains a target-tag dispatch hook for library mode. The
emit surface itself is fixed; the driver work will not require
changes to this package beyond a small adapter that walks the
Mochi IR and populates `PublicAPI.Items`.

The `_cgo_export.h` header is the contract phase 12 (`mochi pkg
publish --to=go+git+...`) and phase 14 (goroutine bridge) both
consume: phase 12 includes the header in the published git-tagged
artefact for non-Go downstream consumers; phase 14 will extend
the header with handle-pool entry points (`mochi_handle_alloc`,
`mochi_handle_free`) for channel and callback handles.

Future phase 11.x reservations:

- **11.1** MEP-54 `Driver.Build` adapter that pipes
  `TargetGoLibrary` through `library.Emit` (gated on the target-
  tag dispatch hook).
- **11.2** `go.sum` synthesis (the spec calls for transitive-dep
  pinning; today `go mod tidy` on the emitted module produces
  the sum file but the emitter does not pre-compute it).
- **11.3** Generics emission (Mochi `fun foo<T>(x: T) -> T` ->
  Go `func Foo[T any](x T) T`) once phase 15 monomorphisation
  decides whether to ship the generic form or the monomorphised
  instantiation in the published library.
- **11.4** Mochi `extern fn` mirror (a Mochi package that re-
  exports a third-party Go module's API as Mochi-level externs
  needs the published library to forward to the underlying calls
  rather than re-implementing them).
