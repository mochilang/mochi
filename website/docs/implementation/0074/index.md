---
title: MEP-74 implementation tracking
sidebar_position: 1
sidebar_label: "MEP 74. Mochi+Go package manager"
description: "Per-phase implementation tracking for MEP-74 (Mochi+Go package manager). Status + commit columns capture how each phase landed on main, plus the per-target coverage matrix for host stable Go, linux-amd64+arm64, windows-amd64, wasm-wasip1, wasm-js, and tinygo embedded."
---

# MEP-74 implementation tracking

Per-phase tracking for [MEP-74 Mochi+Go package manager](/docs/mep/mep-0074). Status values: `NOT STARTED`, `IN PROGRESS`, `BLOCKED`, `LANDED`, `DEFERRED`. Commit is the merge commit short SHA on `main` (or, for the umbrella PR, the in-branch commit on `mep/0074-go-package`).

A phase is LANDED only when its gate is green for every applicable target (consume direction + publish direction). Missing surfaces become N.1, N.2, ... sub-phases per the umbrella-phase coverage rule.

## Phase status

| Phase | Title | Status | Commit | Tracking page |
|-------|-------|--------|--------|---------------|
| 0 | Skeleton: package3/go/ layout + helper-binary plumbing | LANDED | (pending) | [phase-00](/docs/implementation/0074/phase-00-skeleton) |
| 1 | Module-proxy client (`proxy.golang.org` info/mod/zip reader, h1:-hash verify) | LANDED | (pending) | [phase-01](/docs/implementation/0074/phase-01-module-proxy) |
| 2 | sum.golang.org transparency-log client (signed-tree-head + tile fetch + inclusion proof) | LANDED | (pending) | [phase-02](/docs/implementation/0074/phase-02-sumdb) |
| 3 | go/packages ingest helper (`package3/go/cmd/go-ingest`) | LANDED | (pending) | [phase-03](/docs/implementation/0074/phase-03-gopackages-ingest) |
| 4 | ApiSurface JSON schema + bridge-side parser | LANDED | (pending) | [phase-04](/docs/implementation/0074/phase-04-apisurface) |
| 5 | Closed type-mapping table (scalars / strings / `[]byte` / `[]T` / `map[K]V` / structs / interfaces / `chan T` / `func` / `error` / generics) | LANDED | (pending) | [phase-05](/docs/implementation/0074/phase-05-typemap) |
| 6 | Cgo wrapper synthesiser (`//export` directives + `c-archive` + handle pool) | NOT STARTED | — | [phase-06](/docs/implementation/0074/phase-06-wrapper) |
| 7 | Mochi-side extern fn emitter + alias shim file generation | NOT STARTED | — | [phase-07](/docs/implementation/0074/phase-07-extern-emit) |
| 8 | `import go "<module>@<semver>" as <alias>` grammar + parser | NOT STARTED | — | [phase-08](/docs/implementation/0074/phase-08-import-grammar) |
| 9 | Build orchestration: workspace synth + `go build -buildmode=c-archive` + artifact link | NOT STARTED | — | [phase-09](/docs/implementation/0074/phase-09-build) |
| 10 | mochi.lock `[[go-package]]` integration + `--check` mode | NOT STARTED | — | [phase-10](/docs/implementation/0074/phase-10-lockfile) |
| 11 | `TargetGoLibrary` emit (`go.mod` + exported package + `_cgo_export.h`) | NOT STARTED | — | [phase-11](/docs/implementation/0074/phase-11-go-library-emit) |
| 12 | Git-tag publish flow (`mochi pkg publish --to=git-tag`) + canonical-import-path gate | NOT STARTED | — | [phase-12](/docs/implementation/0074/phase-12-git-tag-publish) |
| 13 | Cosign-on-sibling-tag opt-in (`mochi pkg publish --cosign-sign`) | NOT STARTED | — | [phase-13](/docs/implementation/0074/phase-13-cosign) |
| 14 | Goroutine bridge (cgo handle pool + channel-as-handle + callback-as-handle) | NOT STARTED | — | [phase-14](/docs/implementation/0074/phase-14-goroutine-bridge) |
| 15 | Monomorphisation (`[go.monomorphise]` manifest + per-instantiation wrapper) | NOT STARTED | — | [phase-15](/docs/implementation/0074/phase-15-monomorphise) |
| 16 | TinyGo embedded subset (`profile = "embedded"` + `//go:linkname` wrapper) | NOT STARTED | — | [phase-16](/docs/implementation/0074/phase-16-tinygo-embedded) |
| 17 | Vanity-import resolver + wasm-wasip1 / wasm-js publish gate (wazero host integration) | NOT STARTED | — | [phase-17](/docs/implementation/0074/phase-17-vanity-and-wasm) |

## Target coverage matrix

Each phase's LANDED gate must be green for every applicable target. `n/a` cells mark targets where the phase does not apply (for example, publish-only phases on consume-only targets, or cgo-dependent phases on wasm targets).

| Phase | host stable go 1.23 (darwin-arm64) | linux-amd64 / linux-arm64 | windows-amd64 | wasm-wasip1 / wasm-js | tinygo embedded |
|-------|--------|--------|--------|--------|--------|
| 0. skeleton | LANDED | n/a | n/a | n/a | n/a |
| 1. module-proxy client | LANDED | n/a | n/a | n/a | n/a |
| 2. sum.golang.org client | LANDED | n/a | n/a | n/a | n/a |
| 3. go/packages ingest | LANDED | n/a | n/a | n/a | n/a |
| 4. ApiSurface JSON | LANDED | n/a | n/a | n/a | n/a |
| 5. type-mapping table | LANDED | required | required | required | required |
| 6. cgo wrapper synthesiser | NOT STARTED | required | required | n/a (cgo off on wasm) | required (no_cgo subset) |
| 7. extern emitter | NOT STARTED | required | required | required | required |
| 8. import-go grammar (semver) | NOT STARTED | required | required | required | required |
| 9. build orchestration | NOT STARTED | required | required | required (no cgo) | required |
| 10. mochi.lock integration | NOT STARTED | required | required | required | required |
| 11. TargetGoLibrary emit | NOT STARTED | required | required | required | required |
| 12. git-tag publish | NOT STARTED | n/a (publish is host-only) | n/a | n/a | n/a |
| 13. cosign on tag.sig | NOT STARTED | n/a | n/a | n/a | n/a |
| 14. goroutine bridge | NOT STARTED | required | required | n/a (no goroutines on wasm-js without scheduler shim) | required |
| 15. monomorphisation | NOT STARTED | required | required | required | required |
| 16. TinyGo embedded subset | NOT STARTED | n/a | n/a | required (wasm-js via tinygo) | required |
| 17. vanity-import + WASI publish | NOT STARTED | required | required | required | n/a |

Cell legend: `required` means the phase's gate runs against this target; `n/a` means the phase's behaviour is intentionally not exercised on this target (architectural reason in parentheses).

## Per-phase fields

Each phase tracking page documents (or will document, once the phase begins):

- **Gate**: the test or check that must pass for the phase to be LANDED on every applicable target column.
- **Files to touch**: the bridge-side files (Go) and emit-side files (Go template) the phase introduces or modifies.
- **Fixtures**: which of the 24-module fixture corpus the phase validates against.
- **Skip count**: the expected SkipReport count per fixture module (golden numbers).
- **Sub-phase decomposition** (if needed): N.1, N.2, ... entries when an upstream constraint forces splitting.

## Fixture corpus

The 24-module fixture corpus (April 2026 top-25-most-imported-on-pkg.go.dev minus the deprecated `github.com/pkg/errors` standalone usage):

testify (stretchr/testify), cobra (spf13/cobra), viper (spf13/viper), logrus (sirupsen/logrus), uuid (google/uuid), mux (gorilla/mux), pkg/errors, go-cmp (google/go-cmp), protobuf (google.golang.org/protobuf), json-iterator (json-iterator/go), go-spew (davecgh/go-spew), golang/mock, xxhash/v2 (cespare/xxhash), go-isatty (mattn/go-isatty), klauspost/compress, prometheus/client_golang, zap (uber-go/zap), yaml.v3 (gopkg.in/yaml.v3), pflag (spf13/pflag), gin (gin-gonic/gin), echo/v4 (labstack/echo), fasthttp (valyala/fasthttp), sqlx (jmoiron/sqlx), color (fatih/color).

Each phase that touches the type-mapping, wrapper, or runtime layer asserts golden counts against this corpus. The corpus is regenerated quarterly to track module API drift.

## Implementation location

The bridge lives at `package3/go/` in the repo root:

```
package3/go/
  README.md               # pointer to MEP-74 spec
  cmd/
    go-ingest/            # go/packages ApiSurface emitter (phase 3)
  moduleproxy/            # proxy.golang.org client (phase 1)
  sumdb/                  # sum.golang.org transparency client (phase 2)
  apisurface/             # ApiSurface JSON parser (phase 4)
  typemap/                # closed type table (phase 5)
  wrapper/                # cgo wrapper synthesiser (phase 6)
  emit/                   # Mochi extern fn emitter (phase 7)
  build/                  # workspace + go build orchestration (phase 9)
  publish/                # git-tag publish + cosign (phases 12-13)
  goroutine/              # cgo handle pool + bridge runtime (phase 14)
  tinygo/                 # TinyGo embedded subset (phase 16)
  vanity/                 # vanity-import redirect resolver (phase 17)
```

The `package3/go/` location is shared with the broader MEP-57 polyglot package work (where `package3/` is the v3 package-system tree). It sits next to `package3/rust/` (MEP-73) and follows the same internal structure.

## Status snapshot

As of 2026-05-29: phases 0-5 LANDED on `main`, phases 6-17 NOT STARTED.

## Cross-references

- [MEP-74 spec](/docs/mep/mep-0074) for the normative design.
- [MEP-74 research bundle](/docs/research/0074/) for the 12-note deep-research collection.
- [MEP-54 implementation tracking](/docs/implementation/0054) for the underlying Go transpiler that MEP-74 extends.
- [MEP-57 implementation tracking](/docs/implementation/0057) for the polyglot package system MEP-74 builds on.
- [MEP-73 implementation tracking](/docs/implementation/0073) for the sister Rust-bridge phase plan.
