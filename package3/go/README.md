# package3/go

Bidirectional Go module bridge for Mochi. Implementation lives under this directory once phases land per [MEP-74 implementation tracking](../../website/docs/implementation/0074/index.md).

## Status

Stub. No code yet. The phase-00 skeleton (`go.mod` for the bridge, helper-binary plumbing, package layout) is the first implementation deliverable.

## What this is

The MEP-74 bridge sits between MEP-54 (Mochi-to-Go transpiler) and MEP-57 (Mochi source-level package system). Two directions:

- **Consume**: `import go "<module>@<semver>" as <alias>` in Mochi source. The bridge ingests `go/packages.Load` via the `package3/go/cmd/go-ingest` helper, lowers via a closed type table, synthesises a cgo wrapper package with `//export` directives, builds it as a `c-archive`, and exposes Go items as Mochi `extern fn` declarations.
- **Publish**: `mochi pkg publish --to=git-tag`. The bridge lowers the Mochi package via a new `TargetGoLibrary` emit (canonical-import-path-respecting Go package with `go.mod`), commits the generated tree to the user's git remote, tags with `v<semver>` so `proxy.golang.org` picks it up asynchronously, and optionally signs the sibling `v<semver>.sig` tag with Sigstore keyless cosign.

## Planned layout

```
package3/go/
  cmd/
    go-ingest/    # go/packages ApiSurface emitter (phase 3)
  moduleproxy/    # proxy.golang.org client (phase 1)
  sumdb/          # sum.golang.org transparency client (phase 2)
  apisurface/     # ApiSurface JSON parser (phase 4)
  typemap/        # closed type-mapping table + SkipReport (phase 5)
  wrapper/        # cgo wrapper synthesiser (phase 6)
  emit/           # Mochi extern-fn emitter (phase 7)
  build/          # workspace + go build orchestration (phase 9)
  publish/        # git-tag publish + cosign (phases 12-13)
  goroutine/      # cgo handle pool + bridge runtime (phase 14)
  tinygo/         # TinyGo embedded subset (phase 16)
  vanity/         # vanity-import redirect resolver (phase 17)
```

## How MEP-74 differs from MEP-73

Three architectural simplifications versus the Rust bridge:

- **Stable ingest, no nightly toolchain.** `go/packages` + `go/types` are in-stdlib and stable since 2018. The Rust bridge depends on nightly-only rustdoc-JSON.
- **No async runtime singleton.** Go's runtime ships the goroutine scheduler inside every c-archive. No equivalent of MEP-73's tokio `OnceLock<Runtime>`.
- **Built-in transparency log.** `sum.golang.org` provides a Merkle-tree checksum DB out of the box, mandatorily cross-checked at lock time. The Rust bridge layers Sigstore on top because crates.io has no equivalent.

Two architectural complications versus the Rust bridge:

- **Publish is git-tag, not registry-upload.** No central registry to publish to. `proxy.golang.org` discovers modules by polling git remotes for `v<semver>` tags. Publishing is a `git push` + `git tag` + (optional) cosign-sign sequence rather than an upload-token API call.
- **Cgo cost per call.** ~200ns per crossing on darwin-arm64. The bridge offers a batched-variant wrapper for hot-loop call sites; the Rust bridge has no equivalent because `extern "C"` calls cost ~5ns.

## References

- [MEP-74 spec](../../website/docs/mep/mep-0074.md) for the normative design.
- [MEP-74 research bundle](../../website/docs/research/0074/index.md) for the 12-note deep-research collection.
- [MEP-74 implementation tracking](../../website/docs/implementation/0074/index.md) for the 18-phase rollout.
