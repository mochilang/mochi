---
title: "12. Risks and alternatives"
sidebar_position: 13
sidebar_label: "12. Risks and alternatives"
description: "The risk register (cgo cost, Go GC interaction, generic explosion, module proxy compromise, sum.golang.org single-key trust, TinyGo subset size) and the rejected alternatives (parse Go source directly, pkg.go.dev HTML scraping, gomobile bind subset, plugin build mode, protobuf bridge, scheduler reimplementation, purego-only path, dotnet-go annotation surface)."
---

# 12. Risks and alternatives

This note is the consolidated risk register plus the rejected-alternatives ledger. It parallels MEP-73's §12, with the entries adapted to Go's reality. Risks (R) and alternatives (A) are numbered for cross-reference from other notes and the MEP-74 spec.

## Risks

### R1. The cgo cost per call is real and visible in benchmarks

Each cgo crossing costs ~200ns on darwin-arm64 (Go 1.23, May 2026). For workloads that hot-loop across the boundary (per-element transformations, per-message dispatch in a server), this is the dominant cost.

**Mitigation**: the batched-variant wrapper amortises across N elements; the bridge documents the cost per `extern fn` in the emitted shim file's comments. For functions whose Mochi caller is in a `for` body, the bridge automatically offers a `<fn>_batched` variant.

**Residual risk**: the user has to know to call the batched variant. The bridge could add a static-analysis warning when an `extern fn` is called inside a `for` body without the batched variant, but the warning is best-effort.

### R2. Go's GC and cgo interact non-trivially

A pointer the Go side passes to the C side via `//export` is NOT GC-managed on the C side. If the Mochi side holds the pointer past the cgo call return without an explicit pin, Go's GC may move or reclaim the underlying object. The classic failure mode: a cgo call returns a `*C.char`; the Mochi side stores it; the underlying Go string is GC'd; the Mochi side later reads garbage.

**Mitigation**: the wrapper synthesiser inserts `runtime.KeepAlive(obj)` at the end of every `//export` function for every parameter that escapes via pointer. For values where the Mochi side needs to hold the pointer past the call, the wrapper uses `cgo.NewHandle` to pin the Go object until the Mochi side calls `_free`.

**Residual risk**: a user hand-writing a `custom` `extern fn` could violate the contract. The bridge's documentation warns about this; the `unsafe` capability declaration is the audit trail.

### R3. Generic monomorphisation explosion

A Go module exposing a generic function over many possible types, paired with a user who lists many monomorphisations, produces a wrapper with O(N×M) `//export` symbols. The combinatorial explosion is real.

**Mitigation**: the `[go.monomorphise]` table is REQUIRED to enumerate; the bridge does not auto-monomorphise. The lockfile records the instantiation list; a manifest change is the user's explicit opt-in to the explosion.

### R4. The Go module proxy can serve a different .zip than the upstream git repo

If proxy.golang.org is compromised (or if a corporate proxy is compromised), the proxy can serve a malicious .zip while the upstream git repo holds the legitimate source. The sum.golang.org cross-check is the primary mitigation; if the proxy's .zip differs from the checksum-DB-recorded `h1:`, the lock fails.

**Mitigation**: every public module lock cross-checks against sum.golang.org. The `[go.private] sumdb-skip` opt-out is the only way to bypass; the lockfile records `sumdb-verified = false` for audit.

**Residual risk**: a successful attack on both the proxy AND sum.golang.org is conceivable but requires compromising two separately-keyed services. The trust model is the same as `go get`'s default.

### R5. sum.golang.org operates a single signing key

The Go checksum DB's tree head is signed by one Go-team-controlled Ed25519 key. If that key is compromised, the cross-check provides no protection. The bridge's hard-coded pubkey is the same one the Go toolchain ships.

**Mitigation**: the trust assumption is identical to `go get`'s default. Mochi inherits the Go-ecosystem's trust model; if that model is broken, the Go ecosystem at large has bigger problems than the bridge.

**Future direction**: a sub-phase could integrate with a second log (e.g., Sigstore Rekor recording Go module hashes; an industry working group is exploring this in 2026-Q1) for defence-in-depth.

### R6. TinyGo embedded subset is small

Only ~8-15% of pkg.go.dev's top 1,000 modules compile under TinyGo. Most modules pulling in reflect, cgo, or net/http with TLS fail.

**Mitigation**: the embedded gate (phase 16) checks TinyGo compatibility at lock time; non-compatible modules are rejected with a clear diagnostic.

**Residual risk**: users targeting embedded face a constrained module choice. The mitigation is to document the TinyGo-compatible subset prominently in the user-facing docs.

### R7. Build-tag-conditional code paths produce different surfaces

A module that conditions important code on `//go:build` tags produces a different ApiSurface under different tag sets. The lockfile pins the tag set at lock time.

**Mitigation**: changing `[go.build-tags]` requires re-running `mochi pkg lock`; the `--check` mode catches drift. The lockfile's `build-tags` field is the audit trail.

### R8. Vanity import paths require an HTTP redirect

Modules at vanity paths (`go.uber.org/zap` redirecting to `github.com/uber-go/zap`) require the bridge to honour the `<meta name="go-import">` redirect in the canonical URL's HTML response.

**Mitigation**: the phase 17 vanity-import resolver implements the redirect logic per the Go modules spec.

**Residual risk**: a vanity-host that goes down breaks lock until the redirect is resolved. The bridge caches the redirect resolution in `~/.cache/mochi/go-deps/vanity-redirects/` for resilience.

### R9. Cross-platform `_cgo_export.h` generation

The cgo-generated header depends on the host C compiler's pointer width and integer sizes. A wrapper built on darwin-arm64 may produce a slightly different header than the same wrapper on linux-amd64.

**Mitigation**: the bridge writes the header to `<workdir>/go_wrap/<module>/<goarch>-<goos>.h` and the build driver picks the right one. The build matrix in the implementation tracking page validates each (target, host) pair.

### R10. Go's `internal/` package visibility rule

Imports of `<module>/internal/*` from outside the module tree are compile errors. The bridge respects this rule: an item whose qualified name traverses an `internal/` boundary is silently skipped.

**Mitigation**: the typemap pass treats `internal` items as invisible. No SkipReport is emitted (the item is invisible by Go's own rules; surfacing it in the report would be noise).

**Residual risk**: a user who depends on an `internal` item must fork the module or hand-write a custom `extern fn` (which then violates the source module's encapsulation, but at the user's explicit choice).

### R11. Mochi-as-library cgo symbol collision

Multiple Mochi libraries compiled to c-archives loaded into the same process collide on the `mochi_go_<module>_<fn>` symbol prefix.

**Mitigation**: the bridge prefixes every exported symbol with the publishing module's path-hash (first 8 hex chars of SHA-256 of canonical-import-path). Collisions only arise across compatible majors of the same library.

### R12. Go's `replace` directive does not survive publish

When a published Go module is consumed by another, the consumer's `go.mod` does NOT inherit the producer's `replace` directives. A Mochi package using `[go-dependencies] = { path = "../local-fork" }` produces a published module that downstream consumers cannot resolve.

**Mitigation**: `mochi pkg publish` rejects publishing a module that has unresolved `replace` directives in `[go-dependencies]`; all replaces must point to a real upstream version before publish.

### R13. CI image dependency

The MEP-54 gates already require the Go toolchain. MEP-74 adds:

- Optional TinyGo for phase 16.
- Optional `cosign` for phase 13.
- The `go-ingest` helper binary, which is built from Go source and is part of the bridge.

The CI image is bigger.

**Mitigation**: the standard `mochilang/ci-go` image bundles the full toolset. Standalone users install via `go install` plus `brew install cosign tinygo` / `apt-get install cosign tinygo`.

### R14. Go runtime version drift between wrapper and source module

The wrapper's `go.mod` pins `go <version>`. The source module's `go.mod` may pin a different version. The Go toolchain picks the higher of the two for the build. If the wrapper's required version is older than the source's, the build fails.

**Mitigation**: the bridge synthesises the wrapper's `go.mod` with `go <max(wrapper-floor, source-floors)>`. The lockfile records the resolved floor.

### R15. Wasm-target absence of cgo

wasm-wasip1 and wasm-js targets do not support cgo. The bridge's wasm path uses wazero (host-side wasm runtime) instead of cgo for cross-boundary calls.

**Mitigation**: the wasm path is gated separately (phase 17); modules with `import "C"` are rejected for wasm targets.

## Rejected alternatives

### A1. Parse Go source directly with `go/parser`

Rejected: `go/parser` returns an untyped AST. The bridge needs `types.Type` to translate accurately. `go/packages` is exactly the right level (it runs `go/parser` plus `go/types` plus dep resolution). There is no win from going one level lower.

### A2. Use pkg.go.dev's HTML rendering as the API source

Rejected: HTML is rendered for human reading, not normative. Items behind feature flags or platform-conditional code are flat in HTML but conditional in `go/packages`.

### A3. Use `gomobile bind`'s subset as the bridge surface

Rejected: gomobile bind supports a tiny subset of Go (no channels, no callbacks with non-builtin types, no generics). The MEP-74 bridge needs the full surface.

### A4. Generate cgo source with hand-written `//export` directives (gobind-style)

Rejected: that IS what MEP-74 does, just with the type-mapping decisions made by the bridge rather than by the user. gobind's user-written interface file is the boilerplate violation MEP-74 avoids.

### A5. Use Go's `plugin` build mode

Rejected: `plugin` is linux-only, has hard limitations on cross-package symbol visibility, and has known dlopen-time crashes on Go version drift. `c-archive` is portable across darwin, linux, windows, freebsd.

### A6. Protobuf or flatbuffers as cross-boundary serialisation instead of cgo

Rejected: introduces a runtime dependency the user did not opt into, adds 100ns+ per call for the marshal step, and would still need cgo for the actual call dispatch. The cgo path is the shortest.

### A7. Translate Go's goroutine scheduler into Mochi's host runtime

Rejected: Go's goroutine scheduler is deeply tied to the Go runtime (GC interaction, preemption via segmented stacks, channel multiplexing in `runtime/chan.go`). Reimplementing in Mochi would be a 10,000+ LOC project lagging the Go team's optimisations. The bridge sidesteps the impedance by letting the Go runtime's scheduler run inside the c-archive.

### A8. Treat the Go module proxy's `info`, `mod`, `zip` triple as the bridge surface (skip `go/packages`)

Rejected: the triple is enough for fetch + verify but does not parse the Go source. The bridge still needs `go/packages` for type-aware translation.

### A9. Long-lived `GOPROXY` API tokens for private-module fetch

Acknowledged but not rejected: private modules legitimately need authentication. The bridge supports `GOPROXY` env-var passthrough and `.netrc`-style git-credential helpers. There is no long-lived token to "publish" because publishing is a git push to a user-controlled remote.

### A10. Sigstore-keyless mandatory for publish (mirror MEP-73's stance)

Rejected for v1: the Go ecosystem has not converged on a canonical signing format. The gosum-cosign workflow draft of 2026-Q1 is the front-runner but has not landed in any production Go tooling. MEP-74 ships `--cosign-sign` as opt-in and re-evaluates the default when the Go team publishes a canonical signing spec.

### A11. `golang.org/x/mod`-driven re-implementation vs in-bridge HTTP client

Accepted partially: the bridge uses `golang.org/x/mod/module` and `golang.org/x/mod/semver` for module-path validation and semver comparison (pure functions with no I/O). The bridge re-implements the HTTP client for the proxy because the bridge needs to interleave BLAKE3-256 hashing with the .zip download, which is awkward with the upstream client.

### A12. `purego`-only consume path (no cgo)

Rejected for v1: `purego` is a recent project (first commit 2022, GA 2024) that requires the source module ship a `.so`/`.dylib`. Most pkg.go.dev modules are pure Go and do not ship binaries. The cgo path covers more modules.

A future MEP-74 v2 could add purego as an alternative consume path on platforms where cgo is unavailable (wasm-js without wazero). See R15 for the platform-specific reasoning.

### A13. Hand-author the type-mapping table per-module

Rejected: this is the cxx-style violation MEP-73 already rejected. The closed table is a one-time investment that covers every module.

### A14. wazero-only wasm-target instead of cgo+wazero hybrid

Rejected: the host-binary targets (darwin / linux / windows native) materially outperform wazero for cross-boundary calls. wazero is the right tool for wasm hosts; cgo is the right tool for native hosts. The bridge picks per-target.

### A15. dotnet-go-style `[GoExport]` annotation surface

Rejected: explicit annotations are the boilerplate violation MEP-74 was designed to avoid. The bridge derives the export set from `go/packages`.

### A16. Reuse MEP-54 phase 10's `import go "..."` exactly without semver extension

Rejected: phase 10 admits any package name and trusts the host toolchain. MEP-74 adds the semver pin, lockfile, capability declaration, and sumdb verification. Removing these would leave the same gap MEP-74 was authored to close.

### A17. Split into MEP-74 consume + MEP-75 publish

Rejected: the consume and publish directions share the manifest tables, the lockfile sections, the capability surface, and the sum.golang.org integration. Splitting would force two redundant lockfile-section migrations and would push the Mochi-as-Go-publisher story behind both the MEP-73 Rust publish and the MEP-74 consume by an arbitrary amount of time.

### A18. Use `go.work` workspace files instead of synthesised `go.mod` per build

Rejected: `go.work` is the user's workspace, not the bridge's. The bridge owns the build's workspace; mixing manifest authorities is the same anti-pattern MEP-73 §11 rejected for Cargo. The bridge does respect a user-provided `go.work` for path-source modules during development (mirroring how `replace` works in `go.mod`), but the canonical workspace is the bridge's.

### A19. Make TinyGo the default compiler (drop standard Go)

Rejected: TinyGo's subset is too restrictive for the majority of pkg.go.dev modules. The standard Go toolchain remains the default; TinyGo is opt-in via the embedded profile.

### A20. Skip cosign signing entirely (don't even offer the opt-in)

Rejected: even though the Go ecosystem has no canonical signing format, the bridge ships the opt-in so users who want defence-in-depth can have it. The opt-in is non-default; users who don't enable it pay no cost.

## Cross-references

- [[02-design-philosophy]] for the design decisions these alternatives evaluate.
- [[06-go-module-publish-flow]] for the publish-side details.
- [[07-sigstore-go-checksumdb]] for the supply-chain story.
- [[08-goroutine-bridge]] for the cgo-cost discussion.
- [[09-abi-stability]] for the c-archive vs alternatives.
- [[11-tinygo-embedded-wasm]] for the TinyGo subset.
- [MEP-74](/docs/mep/mep-0074) for the normative spec.
- [MEP-73 §12](/docs/research/0073/12-risks-and-alternatives) for the sister Rust-bridge risk register.
