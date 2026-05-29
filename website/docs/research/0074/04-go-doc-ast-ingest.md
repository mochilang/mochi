---
title: "04. go/packages + go/types ingest"
sidebar_position: 5
sidebar_label: "04. go/packages ingest"
description: "The `go/packages.Load` flow, the `types.Type` discriminator tree, why no nightly toolchain is needed (unlike MEP-73's rustdoc-JSON), the Go-side helper binary shape, the ApiSurface JSON schema."
---

# 04. `go/packages` + `go/types` ingest

This note details the ingest pipeline: how the bridge turns a Go module .zip in the content-addressed cache into a structured ApiSurface JSON document the typemap can consume.

## The two-layer Go stdlib API

The Go stdlib (and its supplement `golang.org/x/tools`) ships two complementary packages the bridge uses:

- **`go/types`** (in-stdlib since Go 1.5, 2015): the type representation. Every Go type has a `types.Type` value. The discriminator tree:
  - `*types.Basic` (int, int32, int64, float64, string, bool, ...)
  - `*types.Pointer` (*T)
  - `*types.Slice` ([]T)
  - `*types.Array` ([N]T)
  - `*types.Map` (map[K]V)
  - `*types.Chan` (chan T, <-chan T, chan<- T)
  - `*types.Struct` (struct { ... }; query fields via `NumFields()` and `Field(i)`)
  - `*types.Interface` (interface { ... }; query method set via `NumMethods()` and `Method(i)`)
  - `*types.Signature` (func types; query params and results via `Params()` and `Results()`)
  - `*types.Named` (named types via `type` declarations; the wrapper around any of the above)
  - `*types.TypeParam` (Go 1.18+ generics)
  - `*types.Union` (interface constraint unions in generics)
  - `*types.Tuple` (multi-return signatures)

- **`golang.org/x/tools/go/packages`** (separate Go module, stable since 2018): loads a Go program into a typed form. `packages.Load(cfg, patterns...)` returns `[]*packages.Package` where each entry carries:
  - `Name` (package name, e.g., `cobra`)
  - `PkgPath` (canonical-import-path, e.g., `github.com/spf13/cobra`)
  - `GoFiles` (list of source files)
  - `Imports` (transitive dep map)
  - `Types` (a `*types.Package` with the full export scope)
  - `TypesInfo` (a `*types.Info` with token-to-type mappings)

The combination is exactly what the bridge needs: `packages.Load` gives the dep graph, `types.Package` gives the surface, and `TypesInfo` gives the source-token to type mapping for diagnostics.

## Why no nightly toolchain

Unlike MEP-73's rustdoc-JSON ingest (which requires `cargo +nightly rustdoc --output-format=json -Z unstable-options`), the Go ingest path runs on stable Go from day one:

- `go/types` has been stable in the Go stdlib since Go 1.5 (August 2015).
- `golang.org/x/tools/go/packages` has documented backwards-compatibility since 2018.
- The Go team's stability commitment for these APIs is as strong as for `net/http` or `encoding/json`.

The bridge's helper binary builds against stable Go and can be cross-compiled to every host the Mochi toolchain runs on.

This is the single largest risk-reduction relative to MEP-73. The Rust bridge has to ship a nightly rustup-toolchain-install command in its onboarding; the Go bridge does not.

## Helper binary shape

The bridge ships a Go-toolchain-resident helper at `package3/go/cmd/go-ingest` (~300 LOC):

```go
package main

import (
    "encoding/json"
    "go/types"
    "os"

    "golang.org/x/tools/go/packages"
)

type ApiSurface struct {
    Module       string       `json:"module"`
    Version      string       `json:"version"`
    GoVersion    string       `json:"go_version"`
    Items        []ApiItem    `json:"items"`
    Skipped      []SkipReport `json:"skipped"`
}

type ApiItem struct {
    Kind   string         `json:"kind"` // "func" | "type" | "const" | "var" | "method"
    Name   string         `json:"name"`
    Type   ApiTypeRef     `json:"type"`
    Doc    string         `json:"doc,omitempty"`
    Methods []ApiItem     `json:"methods,omitempty"` // for type items
    TypeParams []ApiTypeParam `json:"type_params,omitempty"` // for generic items
}

type ApiTypeRef struct {
    Kind     string       `json:"kind"` // "basic" | "pointer" | "slice" | "map" | "chan" | "struct" | "interface" | "func" | "named" | "type_param" | "error"
    Basic    string       `json:"basic,omitempty"`     // "int64", "float64", "string", ...
    Elem     *ApiTypeRef  `json:"elem,omitempty"`      // slice / pointer / chan element
    Key      *ApiTypeRef  `json:"key,omitempty"`       // map key
    Value    *ApiTypeRef  `json:"value,omitempty"`     // map value
    Fields   []ApiField   `json:"fields,omitempty"`    // struct fields
    Methods  []ApiItem    `json:"methods,omitempty"`   // interface methods
    Params   []ApiTypeRef `json:"params,omitempty"`    // func params
    Results  []ApiTypeRef `json:"results,omitempty"`   // func results
    Variadic bool         `json:"variadic,omitempty"`  // func variadic
    Named    string       `json:"named,omitempty"`     // canonical-path "."-joined name
    Dir      string       `json:"dir,omitempty"`       // chan direction "send", "recv", "both"
    TypeParam string      `json:"type_param,omitempty"` // type-param name
}

type ApiField struct {
    Name string     `json:"name"`
    Type ApiTypeRef `json:"type"`
    Tag  string     `json:"tag,omitempty"`
    Anonymous bool  `json:"anonymous,omitempty"`
}

type ApiTypeParam struct {
    Name       string     `json:"name"`
    Constraint ApiTypeRef `json:"constraint"`
}

type SkipReport struct {
    Item   string `json:"item"`
    Reason string `json:"reason"`
}

func main() {
    cfg := &packages.Config{
        Mode: packages.NeedName | packages.NeedTypes | packages.NeedDeps |
              packages.NeedTypesInfo | packages.NeedSyntax | packages.NeedFiles,
        Dir:  os.Args[1],
    }
    pkgs, err := packages.Load(cfg, "./...")
    if err != nil { panic(err) }
    surface := walk(pkgs)
    json.NewEncoder(os.Stdout).Encode(surface)
}
```

(Full implementation lives in `package3/go/cmd/go-ingest/main.go`; the snippet above shows the data shape.)

## ApiSurface JSON schema

The emitted JSON document is the contract between the helper binary and the bridge's main Go binary. The schema is the closed shape above. A schema version (`api_surface_schema_version`) field at the top level records the schema epoch; bridge upgrades that change the schema bump the version.

The `Type.Kind` discriminator is the closed set the typemap pass consumes. Items with `Kind` outside the set are not emitted; they appear in the `Skipped` array with a reason string.

## Walk order and visibility rules

The helper walks every `*packages.Package` and emits an `ApiItem` for every exported identifier in the package's `Scope()` (computed via `pkg.Types.Scope()` and iterated with `Names()`). Visibility rules:

- An identifier is exported iff its first rune is uppercase (Go's universal rule).
- An identifier inside an `internal/` directory subtree is invisible to consumers outside the module tree. The helper checks the import path against `golang.org/x/mod/module.MatchPath(<...>, "internal")` and silently skips.
- Identifiers with a `// Deprecated:` comment in the godoc are still emitted, but the bridge marks them with a `deprecated = true` field that the typemap honours (it emits a warning in the `extern fn` declaration).
- Identifiers behind build tags are emitted if the build-tag set the manifest declares includes the tag. The helper is invoked with the user's tag set via `cfg.BuildFlags`.

## Generic items

Go 1.18+ generic items (functions and types with `[T constraint]` type parameters) are emitted with the `type_params` array populated. The typemap pass refuses generic items by default; the user opts in via `[go.monomorphise]`. When an entry like `{ item = "encoding/json.Unmarshal", T = "MyStruct" }` is declared, the bridge synthesises a non-generic wrapper that instantiates the source at the declared `T`.

## Method sets and receiver kinds

A Go type can have value-receiver methods and pointer-receiver methods. The combined method set:

- For value type `T`: methods with receiver `T` (not methods with receiver `*T`; this matters for interface satisfaction).
- For pointer type `*T`: methods with receiver `T` AND methods with receiver `*T`.

The helper emits both receiver kinds with a `receiver_kind: "value"` or `receiver_kind: "pointer"` field on each method item. The wrapper synthesiser uses this to pick the right call shape on the cgo side (passing by value vs. by pointer).

## Embedded fields and promoted methods

A Go struct can embed another struct or interface (`type Foo struct { Bar; ... }`), which promotes the embedded type's methods and exported fields to the embedding type's surface. The helper resolves promotions and emits the promoted items in the embedding type's `methods` array, with a `promoted_from = "<embedded-type-path>"` field for traceability.

## Test files

`*_test.go` files are NOT included in the ApiSurface. Test files are conditionally compiled and would inflate the surface with helper utilities. The helper invokes `packages.Load` with the default `Tests = false` to exclude them.

## Cgo files

A source file declaring `import "C"` is a cgo file. The helper emits cgo-file items but marks them with a `requires_cgo = true` field. The bridge's `[go.capabilities] cgo` check refuses the import unless the user opts in.

## Performance

The helper takes ~50-500ms per module on warm filesystem cache (measured against the 24-module fixture corpus on darwin-arm64 with Go 1.23, May 2026). For a Mochi program with 30 imported modules, the total ingest time is ~5-15 seconds at `mochi pkg lock`. The result is cached in `~/.cache/mochi/go-deps/api-surface/<api-surface-sha256>/`; subsequent locks against the same module versions skip the ingest step.

## Why not consume the `go doc -json` output

Go 1.22 added a `go doc -json` flag that emits a JSON-formatted godoc summary. The bridge does not use it because:

- `go doc` operates on the rendered documentation, not the full type information.
- `go doc -json` does not expose `types.Type` discriminators; the bridge would have to parse natural-language type strings.
- `go doc -json`'s schema is not as stable as `go/packages` (added 2024 vs. 2018).

The `go/packages` path is the right level of abstraction.

## Cross-references

- [[02-design-philosophy]] §2 for why `go/packages` over alternatives.
- [[05-type-mapping]] for what the typemap pass does with the ApiSurface.
- [[10-interface-and-method-set]] for the interface and method-set encoding details.
- [MEP-74 §1](/docs/mep/mep-0074#1-pipeline-overview) for where ingest sits in the pipeline.
