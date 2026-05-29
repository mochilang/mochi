# package3/rust

Bidirectional Rust crate bridge for Mochi. Implementation lives under this directory once phases land per [MEP-73 implementation tracking](../../website/docs/implementation/0073/index.md).

## Status

Stub. No code yet. The phase-00 skeleton (cargo workspace plumbing, package layout) is the first implementation deliverable.

## What this is

The MEP-73 bridge sits between MEP-53 (Mochi-to-Rust transpiler) and MEP-57 (Mochi source-level package system). Two directions:

- **Consume**: `import rust "<crate>@<semver>" as <alias>` in Mochi source. The bridge ingests `cargo +nightly rustdoc --output-format=json`, lowers via a closed type table, synthesises an `extern "C"` wrapper crate, and exposes Rust items as Mochi `extern fn` declarations.
- **Publish**: `mochi pkg publish --to=crates.io`. The bridge lowers the Mochi package via a new `TargetRustLibrary` (rlib + cdylib), packages into a `.crate` tarball, and uploads through Cargo RFC #3724 Sigstore-keyless trusted publishing.

## Planned layout

```
package3/rust/
  rustdoc/      # rustdoc-types JSON parser (phase 2)
  typemap/      # closed type-mapping table + SkipReport (phase 3)
  wrapper/      # extern-C wrapper synthesiser (phase 4)
  emit/         # Mochi extern-fn emitter (phase 5)
  build/        # workspace + cargo orchestration (phase 7)
  publish/      # crates.io publish + Sigstore-keyless (phase 10)
  embedded/     # no_std subset (phase 13)
```

## References

- [MEP-73 spec](../../website/docs/mep/mep-0073.md) for the normative design.
- [MEP-73 research bundle](../../website/docs/research/0073/index.md) for the 12-note deep-research collection.
- [MEP-73 implementation tracking](../../website/docs/implementation/0073/index.md) for the 14-phase rollout.
