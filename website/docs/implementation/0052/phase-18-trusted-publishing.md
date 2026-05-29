---
title: "Phase 18. npm Trusted Publishing"
sidebar_position: 19
sidebar_label: "Phase 18. Trusted Publishing"
description: "MEP-52 Phase 18, npm publish --provenance via Sigstore + GitHub OIDC (GA April 2024); npm audit signatures consumer verification; JSR publish via OIDC; no long-lived API tokens stored in CI."
---

# Phase 18. npm Trusted Publishing

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 18](/docs/mep/mep-0052#phase-plan) |
| Status         | NOT STARTED |
| Started        | n/a |
| Landed         | n/a |
| Tracking issue | n/a |
| Tracking PR    | n/a |

## Gate

`TestPhase18Provenance`: `npm publish --dry-run --provenance` against a verdaccio local registry, with the GitHub OIDC token exchanged for a Sigstore signing credential, produces a valid provenance statement. The statement is verified locally via `npm audit signatures` (Sigstore-backed verifier). Secondary gates: `deno publish --dry-run` against the local JSR mirror also uses OIDC (no long-lived token in CI); no `NPM_TOKEN` or `JSR_TOKEN` secret is referenced anywhere in the workflow file other than the `secrets.NPM_TOKEN` fallback for the legacy path (which Phase 18 removes); the published tarball's SHA256 matches the Phase 16 reproducible-build SHA256 (provenance attests reproducibility).

## Goal-alignment audit

Phase 18 is the supply-chain endpoint. Phases 15-17 produce the artefacts; Phase 18 publishes them under a verifiable identity. Without Trusted Publishing, the only attestation an npm consumer has is "someone with the project's npm token uploaded this", and the token might have been stolen from CI logs, a developer's laptop, or a 1Password vault. With `npm publish --provenance` (GA April 2024), the registry attests "this tarball was built by GitHub Actions, in this repository, by this workflow, at this commit SHA, and the build matches the provenance statement signed by Sigstore". Consumers verify via `npm audit signatures`. This is the gate for "Mochi-emitted npm packages carry the same supply-chain attestations as TypeScript-the-language itself" (`typescript@5.6` ships with provenance since 5.3).

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 18.0 | GitHub Actions workflow: `id-token: write` permission; OIDC token exchange | NOT STARTED | n/a |
| 18.1 | `npm publish --provenance` invocation with no `NPM_TOKEN`; OIDC-only credential path | NOT STARTED | n/a |
| 18.2 | Provenance statement: schema, fields (repo URL, commit SHA, workflow file, runner, tarball SHA256) | NOT STARTED | n/a |
| 18.3 | Consumer verification: `npm audit signatures` runs at install in the gate harness | NOT STARTED | n/a |
| 18.4 | JSR Trusted Publishing: `deno publish` with OIDC token; no `JSR_TOKEN` | NOT STARTED | n/a |
| 18.5 | Release workflow: tag-triggered; runs Phase 15-17 builds; uploads provenance attestations | NOT STARTED | n/a |

## Sub-phase 18.0, OIDC workflow

### Decisions made (18.0)

**Workflow permission block** (required for OIDC):

```yaml
permissions:
  contents: read
  id-token: write
  attestations: write
```

`id-token: write` is the new permission since GitHub introduced OIDC tokens in 2022. `attestations: write` is required since GitHub Attestations (October 2024) for the build provenance API surface.

**OIDC token exchange**: GitHub Actions issues a short-lived JWT signed by `https://token.actions.githubusercontent.com`. npm registry's Trusted Publishing endpoint exchanges this JWT for a publishing credential valid for one publish operation. No long-lived secret persists.

**Workflow trigger**: tag push (`v*.*.*`). Tag triggers the release workflow; PR triggers run the same code with `--dry-run` only.

## Sub-phase 18.1, npm publish --provenance

### Decisions made (18.1)

**Command** (run in the release workflow):

```bash
npm publish --provenance --access=public
```

**No `NPM_TOKEN`**: the workflow does not reference `${{ secrets.NPM_TOKEN }}`. The OIDC token is read by npm CLI directly from the GitHub Actions environment (`ACTIONS_ID_TOKEN_REQUEST_URL`, `ACTIONS_ID_TOKEN_REQUEST_TOKEN`).

**`--access=public`**: required for the `@mochi/*` scope (scoped packages default to private). The user's own scope (`@<user>/<pkg>`) inherits the user's npm scope configuration.

**Dry-run gate path** (Phase 18 gate, runs on every PR):

```bash
npm publish --dry-run --provenance --access=public
```

The dry-run still exercises the OIDC exchange and Sigstore signing; only the final registry-side commit is skipped.

## Sub-phase 18.2, Provenance statement shape

### Decisions made (18.2)

**Statement fields** (per npm Sigstore documentation):

```json
{
  "_type": "https://in-toto.io/Statement/v0.1",
  "subject": [{
    "name": "pkg:npm/%40mochi/hello@0.0.1",
    "digest": { "sha256": "abc...123" }
  }],
  "predicateType": "https://slsa.dev/provenance/v0.2",
  "predicate": {
    "builder": { "id": "https://github.com/actions/runner" },
    "buildType": "https://github.com/npm/cli/gha@v2",
    "invocation": {
      "configSource": {
        "uri": "git+https://github.com/mochilang/mochi-hello@refs/tags/v0.0.1",
        "digest": { "sha1": "abc...def" },
        "entryPoint": ".github/workflows/release.yml"
      }
    }
  }
}
```

**Sigstore signature**: signed by a Sigstore Fulcio-issued certificate; the certificate's Subject Alternative Name carries `https://github.com/mochilang/mochi-hello/.github/workflows/release.yml@refs/tags/v0.0.1`. Anyone with the Sigstore root can verify the statement was signed by this exact workflow on this exact commit.

**Public Sigstore transparency log**: every signature is logged to `rekor.sigstore.dev`. Tampering after the fact is detectable.

## Sub-phase 18.3, Consumer verification

### Decisions made (18.3)

**`npm audit signatures`**: runs against an installed `node_modules/` and verifies the Sigstore signature on every package that ships provenance.

**Gate harness**:

```bash
mkdir /tmp/audit-test && cd /tmp/audit-test
npm init -y
npm install <tarball>
npm audit signatures
# Expected output: "X packages have verified registry signatures"
```

The Phase 18 gate runs this against the Phase 15 tarball (built with `--provenance --dry-run` against the local registry). Any unverified signature fails the gate.

**Why the gate is meaningful**: provenance the publisher generates without anyone verifying it on the consumer side is theatre. The gate proves the round-trip: build with provenance, publish, install, verify, all succeed.

## Sub-phase 18.4, JSR Trusted Publishing

### Decisions made (18.4)

**`deno publish` with OIDC**: JSR (`jsr.io`) accepts the same GitHub Actions OIDC token. No `JSR_TOKEN` in CI.

```bash
deno publish --token-source=github-actions
```

The `--token-source` flag (Deno 2.0+) tells `deno publish` to fetch the OIDC token from the Actions environment and exchange it with JSR's Trusted Publishing endpoint.

**JSR scope claim**: same SAN-based verification as npm. JSR's transparency log mirrors Sigstore's pattern.

**Dry-run**: `deno publish --dry-run --token-source=github-actions`. Verified end-to-end against a local JSR mirror in the Phase 18 gate.

## Sub-phase 18.5, Release workflow

### Decisions made (18.5)

**`.github/workflows/release.yml`** (the workflow Phase 18 emits as part of the project scaffold):

```yaml
name: release
on:
  push:
    tags: ["v*.*.*"]

permissions:
  contents: read
  id-token: write
  attestations: write

jobs:
  release:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "22.11.0"
          registry-url: "https://registry.npmjs.org"
      - uses: denoland/setup-deno@v2
        with:
          deno-version: "2.0.0"
      - run: mochi build --target=npm-package
      - run: npm publish --provenance --access=public
      - run: mochi build --target=deno-jsr
      - run: deno publish --token-source=github-actions
      - run: mochi build --target=browser-bundle
      - uses: actions/upload-artifact@v4
        with:
          name: browser-bundle
          path: dist/bundle/
```

**One workflow, three publish paths**: npm, JSR, browser-bundle artefact. The artefact is uploaded as a GitHub release asset for users who want to download the standalone ESM file directly.

**Provenance for the artefact**: `actions/upload-artifact@v4` plus `actions/attest-build-provenance@v2` (the latter generates a SLSA provenance attestation that GitHub stores in its Attestations service). The bundle is verifiable via `gh attestation verify`.

## Files (planned)

| File | Purpose |
|------|---------|
| `transpiler3/typescript/build/workflow.go` | Emit `.github/workflows/release.yml` as part of project scaffold |
| `transpiler3/typescript/build/publish.go` | `npm publish` and `deno publish` driver (used only in `--target=npm-package` real publish, not the gate) |
| `transpiler3/typescript/build/phase18_test.go` | `TestPhase18Provenance`, `TestPhase18Verification`, `TestPhase18JSR` |
| `tests/transpiler3/typescript/fixtures/phase18-provenance/` | Phase 18 fixtures (one canonical hello fixture exercises the full pipeline) |

## Test set

- `TestPhase18Provenance`, runs `npm publish --dry-run --provenance` against a local verdaccio; parses the produced provenance statement; validates the schema.
- `TestPhase18Verification`, runs `npm audit signatures` against the installed dry-run tarball; expects all-verified.
- `TestPhase18JSR`, runs `deno publish --dry-run --token-source=github-actions` against the local JSR mirror.
- `TestPhase18NoLongLivedTokens`, greps the emitted `.github/workflows/release.yml` for `NPM_TOKEN`, `JSR_TOKEN`, `secrets\.`; expects none (only `id-token: write`).
- `TestPhase18ProvenanceReproducible`, the `subject[].digest.sha256` field in the provenance statement matches the Phase 16 reproducible-build SHA256 for the same tag.

## Deferred work

- npm Trusted Publishing for arbitrary user scopes (`@<user>/<pkg>`). Phase 18 ships the `@mochi/` scope; user-scope publishing requires the user to configure their own scope-to-Trusted-Publishing binding on npmjs.org. Documented; no transpiler change needed.
- GitLab CI as an alternative OIDC issuer. GitHub Actions is the Phase 18 target; GitLab support lands in Phase 18.5 once GitLab's OIDC-to-npm integration GAs.
- Self-hosted runners with OIDC. GitHub-hosted runners are the Phase 18 target. Self-hosted runners can issue OIDC tokens but require additional Subject Alternative Name configuration on the npm side; documented as a deployment caveat.
- Sigstore key rotation. Phase 18 trusts Sigstore's published root; rotation is Sigstore's responsibility, not Mochi's. Documented.
- SLSA Level 4 (hermetic builds). Phase 18 reaches SLSA Level 3 (Trusted Publishing + provenance + reproducible build). Level 4 (hermetic, fully-isolated build environment) is an open question for v2.
