---
title: "MEP-72 Note 06: npm and JSR publish flow"
sidebar_position: 7
sidebar_label: "06. publish flow"
description: "The dual-registry publish flow: the npm registry's `npm publish --provenance --access=public` (Trusted Publishing GA April 2024), the JSR registry's `deno publish --token-source=github-actions` (Trusted Publishing GA mid-2024), the dual-publish-from-one-workflow-run pattern that MEP-52 Phase 18 already emits, the per-registry metadata requirements, the publish-side gate against verdaccio-mock + JSR-mock."
---

# 06. npm and JSR publish flow

This note describes the end-to-end publish flow for Mochi-emitted library packages. It is informative; the normative reference is [MEP-72 §Specification §5](/docs/mep/mep-0072) plus the MEP-52 Phase 18 workflow emit at `transpiler3/typescript/build/workflow.go`.

## 1. The npm path

When `mochi pkg publish --to=npm` runs (in CI, against a tag-triggered workflow), the sequence is:

1. **Build**: `mochi build --target=npm-library -o dist/npm` produces a `dist/npm/` directory with:
   - `package.json` (npm manifest with the `exports` map, `dependencies`, `devDependencies`, `peerDependencies` harvested from `mochi.toml`)
   - `dist/index.js` (the compiled JS entry; produced by `tsc` against the Mochi-emitted `.ts` source)
   - `dist/index.d.ts` (TypeScript declarations; produced by `tsc --declaration --emitDeclarationOnly`)
   - `src/` (the Mochi-emitted TypeScript source, kept for JSR consumption and for downstream debugging)
   - `README.md`, `LICENSE` (harvested from the Mochi package root or from `mochi.toml` `[ts.publish]`)
2. **Validate**: `tsc --noEmit` against `dist/npm/dist/index.d.ts` produces zero diagnostics.
3. **Dry-run**: `npm pack --dry-run` (in `dist/npm/`) prints the tarball contents and verifies the manifest is well-formed.
4. **Publish via CI**: the MEP-52 Phase 18 emitted workflow at `.github/workflows/release.yml` runs `npm publish --provenance --access=public`:
   - The `--provenance` flag triggers the Sigstore signing path. The CI workflow's `id-token: write` permission lets `npm publish` acquire a GitHub OIDC token.
   - `npm publish` submits the OIDC token to npm's Trusted-Publishing endpoint. The npm side mints a short-lived publish credential, signs the tarball via Sigstore Fulcio, and records the attestation in the registry.
   - The `--access=public` flag is required for scoped packages (`@scope/pkg`); defaults to `restricted` otherwise.
5. **Verify**: post-publish, `npm view <pkg>@<version> dist.tarball` returns the URL; `npm audit signatures` against a synthetic downstream consumer verifies the Sigstore attestation against the public Sigstore root of trust.

## 2. The JSR path

When `mochi pkg publish --to=jsr` runs:

1. **Build**: `mochi build --target=jsr-library -o dist/jsr` produces a `dist/jsr/` directory with:
   - `jsr.json` (JSR manifest with `name = "@scope/pkg"`, `version`, `exports = "./src/index.ts"`, `publish.include = ["src/**/*.ts", "README.md", "LICENSE"]`)
   - `src/` (the Mochi-emitted TypeScript source; JSR transpiles server-side, so no `dist/` tree is published)
   - `README.md`, `LICENSE`
2. **Validate**: `deno check src/index.ts` against the Mochi-emitted source produces zero diagnostics.
3. **Dry-run**: `deno publish --dry-run --allow-dirty` validates the manifest and walks the include list.
4. **Publish via CI**: the same workflow's `deno publish --token-source=github-actions` step:
   - Submits the OIDC token to JSR's Trusted-Publishing endpoint. JSR mints a short-lived publish credential, signs the manifest via the JSR signing infrastructure, posts the attestation to the JSR registry.
5. **Verify**: post-publish, `https://jsr.io/@scope/pkg` returns the published page; the Sigstore attestation is recorded in the JSR registry's metadata.

## 3. Dual publish from one workflow run

The MEP-52 Phase 18 workflow runs both flows in the same `release` job:

```yaml
jobs:
  release:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
      - uses: denoland/setup-deno@v2
      - name: Build npm library
        run: mochi build --target=npm-library -o dist/npm
      - name: Publish to npm
        if: startsWith(github.ref, 'refs/tags/')
        working-directory: dist/npm
        run: npm publish --provenance --access=public
      - name: Build JSR library
        run: mochi build --target=jsr-library -o dist/jsr
      - name: Publish to JSR
        if: startsWith(github.ref, 'refs/tags/')
        working-directory: dist/jsr
        run: deno publish --token-source=github-actions
```

Both publishes run in the same job, with the same OIDC token, on the same tag. The two registries' attestation chains are independent (npm Sigstore is npm's Fulcio cert; JSR Sigstore is JSR's). A downstream consumer verifying both attestations gets defence-in-depth against either registry's compromise.

## 4. Per-registry metadata requirements

npm requires:

- `name` (must be unique in the npm namespace; scoped names like `@mochilang/foo` allow re-use of unscoped names)
- `version` (semver)
- `description`
- `license` (SPDX or `SEE LICENSE IN <file>`)
- `repository` (RECOMMENDED for provenance attestation)
- At least one of `bin`, `main`, `exports`

JSR requires:

- `name` (must be `@<scope>/<pkg>`)
- `version` (semver)
- `exports` (string path to entry or table of conditions)
- `license` (SPDX)
- The `publish.include` allowlist

MEP-72 harvests all of the above from `mochi.toml` `[ts.publish]` plus `[package]`. Missing required fields produce a build-time error.

## 5. Publish-side gate

The MEP-52 Phase 18 test set includes `TestPhase18ProvenanceDryRun` (already shipping); MEP-72 adds:

- `TestPhase72NpmLibraryPack`: `npm pack --dry-run` succeeds against the emitted `dist/npm/` directory.
- `TestPhase72JsrLibraryDryRun`: `deno publish --dry-run --allow-dirty` succeeds against `dist/jsr/`.
- `TestPhase72DualPublishYAML`: the emitted workflow has both `npm publish` and `deno publish` steps with the right flags.
- `TestPhase72LibraryMetadataHarvest`: the emitted `package.json` and `jsr.json` carry the metadata harvested from `mochi.toml` `[ts.publish]`.
- `TestPhase72NoLongLivedTokens`: the emitted workflow has zero references to `NPM_TOKEN` or `JSR_TOKEN`.

These gates run against the verdaccio-mock + JSR-mock harness from MEP-52 Phase 18.

## 6. Cross-references

- [MEP-72 §Specification §5](/docs/mep/mep-0072) — the normative CLI surface.
- [[07-sigstore-npm-jsr-trusted-publishing]] — the Sigstore detail.
- [MEP-52 Phase 18 implementation tracking](/docs/implementation/0052/phase-18-trusted-publishing) — the workflow emit.
