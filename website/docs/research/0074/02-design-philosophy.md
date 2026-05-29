---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why a bidirectional bridge, why go/packages over alternatives, why a synthesised cgo wrapper package over direct cgo emission or gomobile-style codegen, why no async runtime singleton is needed for Go (the runtime ships in the c-archive), why sum.golang.org cross-check is mandatory by default, why the type-mapping table is closed not open."
---

# 02. Design philosophy

This note frames the six load-bearing design decisions in MEP-74 alongside the alternatives that were considered and rejected. Each section follows the same structure: the decision, the alternatives, the trade-offs. The note parallels the MEP-73 §02 design philosophy note; readers familiar with MEP-73 will find the structure unchanged and only the Go-specific reasoning different.

## 1. Why bidirectional

MEP-54 phase 10 already shipped a one-way Go consume path (a Mochi program can name a Go package and link against it via the host Go toolchain). MEP-74 could have stopped at "fix the consume path: add semver pinning, capability declaration, and lockfile pinning" and deferred Mochi-as-Go-publisher to a future MEP. Or could have shipped the publish direction without touching the consume path.

Shipping both directions in one MEP is the right scope because:

- **Symmetric distribution.** A library author writes Mochi, depends on Go modules, publishes to a publicly-resolvable git repo that the module proxy picks up. A library consumer either writes Mochi (uses MEP-74's `import go`) or writes Go (uses `go get` against the published canonical-import-path). A unidirectional bridge would leave one side of the symmetry broken.

- **Shared sum.golang.org integration.** The consume path verifies imported modules against sum.golang.org's checksum DB. The publish path optionally adds a cosign signature over the published git tag. Both interact with the Go supply-chain surface; implementing one without the other would leave a notch in the design.

- **Shared capability surface.** The `[go.capabilities]` table that audits which capabilities the imported dep graph requires has its mirror in MEP-57's `[capabilities]` table that audits which capabilities the Mochi-published library requires. The same monotonicity rule applies; the audit pass is shared.

- **Parity with MEP-73.** The Rust bridge ships bidirectional. Shipping a unidirectional Go bridge would leave the Go interop story behind the Rust interop story for no defensible reason.

The trade-off is that MEP-74 is a larger spec than a strict single-direction extension would be. The compensating gain (parity with MEP-73, single capability surface, single lockfile section, single ingest pipeline) is large.

## 2. Why `go/packages` + `go/types`

The bridge needs a machine-readable description of every Go module's public surface. Four candidate sources existed:

- **`go/packages.Load`** (under `golang.org/x/tools/go/packages`, which the Go team officially maintains as the canonical "load a Go program into a typed form" entry point). Returns `[]*packages.Package` where each `*packages.Package` carries a `*types.Package` exposing every exported identifier with its full `types.Type`. Stable since 2018, used by gopls, staticcheck, golangci-lint, and effectively every Go static-analysis tool. Ships in the official Go module `golang.org/x/tools`.

- **`go/parser` + manual type inference.** One level lower in the Go stdlib: returns untyped ASTs. The bridge would have to re-implement `go/types` to recover the type information. Wasteful (go/types is the right tool already).

- **`pkg.go.dev` HTML scraping.** The website renders the public API as HTML; the bridge could parse the HTML. Rejected: HTML is rendered for human reading, not normative. Items behind feature flags, build tags, or platform-conditional code paths are conditional in `go/packages` but flat in HTML.

- **`gomobile bind`'s `.aar` / `.framework` schema.** gomobile generates Java / Objective-C bindings for a tiny subset of Go. The schema is conservative (no channels, no generics, no interfaces with complex method signatures, no goroutine-spawning callbacks). Insufficient for full bridging.

`go/packages` wins on every axis: stable, machine-readable, in-stdlib, with full type information. The only cost is that the bridge needs a Go-toolchain-resident helper binary (`package3/go/cmd/go-ingest`) to call `packages.Load`; the binary emits a JSON document that the main Mochi binary parses. The helper is ~300 LOC.

This is materially less risky than MEP-73's rustdoc-JSON ingest, which is nightly-only and behind `-Z unstable-options` as of May 2026. The Go bridge can run on stable Go from day one.

## 3. Why synthesised cgo wrapper package

Given an ingested ApiSurface, the bridge has three routes to making Go items callable from Mochi:

- **Direct cgo in the user's main**: the MEP-54 emit pass generates Go source that imports the source module directly. This is what MEP-54 phase 10 does today for unversioned imports. The cost: every additional Go feature the bridge wants to expose (generics, channels, callbacks, interfaces with non-trivial method sets) has to be taught to the `aotir` IR, which violates the target-agnostic invariant.

- **Synthesised cgo wrapper Go package**: the bridge generates a sibling Go package that imports the source module and exposes a flat `//export` surface wrapping each translatable public item. The Mochi side calls into the wrapper via a known, stable, generics-free, interface-free C ABI. The wrapper is built via `go build -buildmode=c-archive`.

- **gomobile-bind-style codegen**: the user authors an interface description file (`.gobind`); gomobile generates the bindings on both sides. Same boilerplate violation MEP-73 §02-3 rejects.

The synthesised wrapper path is the only one that delivers the "no boilerplate" promise without breaking the `aotir` IR's target-agnosticism. The wrapper-package build is added to the user's `go build`; warm-cache wrapper compile is ~1-3 seconds per module, cold-cache ~5-15 seconds (Go's compile speed is materially better than Rust's). The wrappers are cached in `~/.cache/mochi/go-deps/wrappers/<wrapper-sha256>/` and shared across workspaces.

The wrapper-vs-direct trade-off mirrors how gopy works (gopy generates Python bindings for Go by synthesising a wrapper) and how PyO3 / napi-rs work in Rust. The bridge does the synthesis from the Go side at lock time, not by Go generate directives at compile time, so the user does not edit Go source at all.

## 4. Why no async runtime singleton is needed

This is the load-bearing difference from MEP-73. MEP-73's bridge synthesises a tokio runtime singleton because Rust's `async fn` requires a host executor and Mochi does not provide one. Go does not have this problem:

- Go's `goroutine` is preemptible green-threading inside the Go runtime. The runtime scheduler ships in every Go binary including c-archives.
- A cgo call from Mochi into a Go function `f` runs `f` on a Go runtime thread (cgo callbacks acquire a per-call goroutine).
- If `f` spawns its own goroutines (`go some_func()`), the Go runtime schedules them; the cgo call returns when `f` returns; the spawned goroutines continue running in the background until the c-archive's Go runtime is shut down (which only happens at process exit).
- If `f` blocks on a channel receive (`<-ch`), the Go runtime parks the goroutine and the cgo call blocks until the receive completes.

The bridge has zero runtime-singleton code. The cost: every cgo call carries the goroutine scheduler's overhead (a per-call goroutine acquire), which is ~200ns on darwin-arm64 (May 2026 benchmark). Programs that hot-loop across the cgo boundary pay this per iteration. The mitigation is the batched-variant wrapper (§[[09-abi-stability]] §3).

The bridge does need a small piece of cgo-handle plumbing to bridge Mochi's `stream<T>` to Go's `chan T` and Mochi callback values to Go func parameters. This plumbing is a `sync.Map` of active handles plus per-channel `_send` / `_recv` / `_close` exported functions. ~150 LOC total. No runtime executor singleton.

## 5. Why sum.golang.org cross-check is mandatory by default

The Go ecosystem ships an unusually strong supply-chain story by 2026 standards:

- The module proxy at `proxy.golang.org` caches every public module .zip and serves them on `go get`.
- The checksum database at `sum.golang.org` is a Merkle-tree transparency log: every public module version is appended, the log's signed tree head is anchored in the public timeline, and clients can fetch a consistency proof on demand.
- Every `go get` since Go 1.13 (September 2019) cross-checks the module .zip against sum.golang.org by default.

A 2026 Mochi-to-Go bridge that does not cross-check `[[go-package]]` lockfile hashes against sum.golang.org is shipping a weaker supply-chain story than `go get` itself. The cross-check is one HTTP GET per dep at lock time; the cost is negligible; the safety gain is substantial.

The alternative would be to make the cross-check opt-in, e.g., via a `sumdb-verify = true` flag. Rejected: the Go-ecosystem default is opt-out for private modules only (`GONOSUMCHECK`), and Mochi should match. The bridge ships with `[go.private] sumdb-skip = [...]` as the opt-out path, mirroring `GONOSUMCHECK` semantics.

This is the second-strongest supply-chain stance in any Mochi MEP (after MEP-73's Sigstore-keyless-only stance). It is strictly stronger than what the Rust bridge can offer because the Rust ecosystem does not have a comparable transparency log (Sigstore Rekor is the closest analogue, and it is opt-in per-publisher; sum.golang.org is mandatory for every public module).

## 6. Why a closed type-mapping table

Same argument as MEP-73 §02-6, transferred to Go:

- **Predictable user surface.** The Mochi user can read the table and predict whether a given Go item translates.
- **Refusal is information.** The `SkipReport` tells the user precisely why an item was skipped.
- **Generic explosion containment.** Go 1.18+ generics are bounded by the explicit `[go.monomorphise]` declaration.
- **Auditability.** The table fits in a single source file (~250 LOC of Go).

Go's type system is slightly richer than Rust's at the closed-table level because Go has first-class channels (`chan T`), first-class function types (`func(...) (..., error)`), and structural interfaces (`type I interface { M() }`). The bridge handles all three via the cgo-handle pattern: channels become opaque handles with `_send` / `_recv` / `_close` cgo exports, function values become opaque handles with a `_call` cgo export, and interfaces become opaque handles with per-method cgo exports.

The cost: the closed table refuses many items the user might want. The mitigation is the `extern fn ... from go "..." custom` override path: the user can always escape the table by taking responsibility for the FFI boundary.

## 7. Why git-tag publish over a central registry

The Go ecosystem famously has no central package registry. Module publication is a git operation: tag a commit with a semver-compatible tag and push the tag to the canonical-import-path repo. The module proxy picks up the new tag on the next `go get`.

This is materially different from Rust's crates.io / Python's PyPI / Node's npm / Maven Central. The bridge embraces the difference rather than trying to abstract it away:

- **`mochi pkg publish --to=go+git+<repo-url>@<tag>`** is a thin wrapper around `git tag <tag> && git push origin <tag>`. The Mochi side adds: build via `TargetGoLibrary`, validate `go vet` / `gofmt` / `go build`, optionally cosign-sign the tag.

- **No upload endpoint.** The bridge does not POST a tarball to a registry. The git remote (GitHub, GitLab, Gitea, self-hosted) is the upload target.

- **Module proxy is implicit.** Once the tag exists on the public remote, `proxy.golang.org` fetches it on the next request and submits it to sum.golang.org. The bridge does not coordinate with the proxy directly (though it can ping the proxy to warm the cache).

- **Authentication is git authentication.** SSH key or HTTPS Personal Access Token (PAT). No `GO_PROXY_TOKEN`. No long-lived API token.

The cost: the publish target has to support publicly-resolvable git URLs. A private corporate repo at `corp.example.com/internal/foo` is publishable, but only consumers with credentials to that repo can `go get` it. This matches Go's normal behaviour.

The benefit: no central-registry single-point-of-failure for Mochi-to-Go publish. The git remote landscape is more diverse and more resilient than any single registry.

## Cross-references

- [[01-language-surface]] for the user-visible surface.
- [[03-prior-art-bridges]] for the gopy / gomobile / gobind comparison the wrapper-vs-direct decision draws on.
- [[04-go-doc-ast-ingest]] for the `go/packages` schema choice.
- [[05-type-mapping]] for the closed-table contents.
- [[07-sigstore-go-checksumdb]] for the sum.golang.org cross-check and the optional cosign signing.
- [[08-goroutine-bridge]] for the no-runtime-singleton story.
- [MEP-74](/docs/mep/mep-0074) for the normative spec.
- [MEP-73](/docs/mep/mep-0073) for the sister Rust bridge whose design philosophy this note mirrors.
