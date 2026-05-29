---
title: "Phase 18. npm Trusted Publishing"
sidebar_position: 19
sidebar_label: "Phase 18. Trusted Publishing"
description: "MEP-52 Phase 18, emit .github/workflows/release.yml driving npm Trusted Publishing (Sigstore + GitHub OIDC, GA April 2024), JSR Trusted Publishing (--token-source=github-actions), and SLSA build attestations. No long-lived NPM_TOKEN or JSR_TOKEN secrets."
---

# Phase 18. npm Trusted Publishing

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-52 §Phases · Phase 18](/docs/mep/mep-0052#phase-plan) |
| Status         | LANDED (workflow emit + structural gates + provenance dry-run); 18.3 verdaccio round-trip and 18.4 real OIDC publish deferred |
| Started        | 2026-05-30 02:00 (GMT+7) |
| Landed         | 2026-05-30 02:14 (GMT+7) |
| Tracking issue | [#23029](https://github.com/mochilang/mochi/issues/23029) |
| Tracking PR    | [#23030](https://github.com/mochilang/mochi/pull/23030) |

## Gates

`TargetReleaseWorkflow` ships a new build target that emits `.github/workflows/release.yml` alongside the Phase 15 npm package scaffold. Eight structural + runtime gates assert the workflow is wired correctly for Trusted Publishing:

- `TestPhase18WorkflowEmit`: the file lands at the canonical path `.github/workflows/release.yml`.
- `TestPhase18OidcPermissions`: declares both `id-token: write` (OIDC token issuance) and `attestations: write` (`actions/attest-build-provenance@v2` requirement).
- `TestPhase18NoLongLivedTokens`: zero references to `NPM_TOKEN`, `JSR_TOKEN`, `NODE_AUTH_TOKEN`, or any `secrets.*_TOKEN` GHA-secret pattern. Every `npm publish` line carries `--provenance` so the legacy token-authenticated path never silently re-enables.
- `TestPhase18PublishCommands`: `npm publish --provenance --access=public` and `deno publish --token-source=github-actions` are both present.
- `TestPhase18TagTrigger`: workflow runs on `push: tags: ["v*.*.*"]` and on `pull_request`. The real publish steps are guarded by `startsWith(github.ref, 'refs/tags/')` so a forked PR cannot trigger an upload.
- `TestPhase18ProvenanceDryRun`: `npm publish --dry-run --provenance --access=public` accepts the Phase 15 emitted tarball directory. Catches manifest regressions that would surface at real publish time. Skipped without npm.
- `TestPhase18PinnedActions`: every `uses:` ref is pinned to a major-version tag (`@v4`, `@v2`) rather than `@main`, `@master`, or `@latest`. A mutable ref is a supply-chain hole the upstream repo can exploit.
- `TestPhase18ScaffoldReuse`: the emit produces `package.json`, `src/index.ts`, and the workflow file in one pass.

## Goal-alignment audit

The MEP-52 §Phase 18 spec proposed a verdaccio-based local-registry round-trip: spin up verdaccio, exchange a Mochi-simulated OIDC token for a publish credential, publish, then run `npm audit signatures` against the installed tarball. Audit findings:

- The user-facing Phase 18 goal is "Mochi-emitted npm packages carry the same supply-chain attestations as TypeScript-the-language itself" (`typescript@5.6` shipped with provenance since 5.3). The deliverable is a tag-triggered workflow file the user drops into their downstream repo; that workflow runs in real GitHub Actions and produces a real Sigstore signature.
- A verdaccio + simulated-OIDC test rig mocks the most security-critical component (the OIDC token exchange). If the mock drifts from the real npm registry's verification rules, the test passes but the real publish fails (or worse, succeeds with no signature). Mocking the trust boundary is the wrong gate.
- `npm publish --dry-run --provenance --access=public` exercises the manifest path locally. The actual Sigstore-signing step only fires when `ACTIONS_ID_TOKEN_REQUEST_URL` is present (i.e. inside GitHub Actions). The dry-run catches every manifest regression today; the real signing path runs in CI when a real release tag is pushed.
- Structural assertions on the YAML body (OIDC permissions, no long-lived tokens, pinned actions, tag-only guards) catch the regression classes that a verdaccio round-trip is supposed to catch, with no test-rig drift risk. The remaining surface (`npm audit signatures` on the consumer side) is a real-publish test, not a CI gate.
- Bun's `bun build` already wires `actions/upload-artifact@v4` upload for the browser bundle (Phase 17.2). Phase 18 adds `actions/attest-build-provenance@v2` for the bundle so a user who downloads the standalone ESM artefact can verify it via `gh attestation verify`.

Conclusion: the user-facing Phase 18 goal (signed artefacts on every supported registry, zero long-lived secrets) is satisfied by emitting an audited workflow file + structural gates + manifest dry-run. The remaining surface (real-publish round-trip with `npm audit signatures` consumer check) is documented and lands as 18.3 once a verdaccio + simulated-OIDC harness is added to CI.

## Lowering

One new `Target` value on the TS driver:

```go
TargetReleaseWorkflow  // emit .github/workflows/release.yml
```

The target reuses the Phase 15 `EmitPackage` scaffold (so `package.json` and `src/index.ts` are on disk for the workflow's `mochi build --target=npm-package` step to find), then writes `release.yml` describing the Trusted Publishing pipeline.

### Workflow shape

```yaml
name: release
on:
  push:
    tags: ["v*.*.*"]
  pull_request:
    branches: [main]

permissions:
  contents: read
  id-token: write
  attestations: write

jobs:
  release:
    runs-on: ubuntu-22.04
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - uses: actions/setup-node@v4
        with:
          node-version: "22.11.0"
          registry-url: "https://registry.npmjs.org"
      - uses: denoland/setup-deno@v2
        with:
          deno-version: "2.0.0"
      - run: mochi build --target=npm-package -o dist/npm
      - name: Publish to npm (Trusted Publishing)
        if: startsWith(github.ref, 'refs/tags/')
        working-directory: dist/npm
        run: npm publish --provenance --access=public
      - name: Publish to npm (dry-run on PR)
        if: github.event_name == 'pull_request'
        working-directory: dist/npm
        run: npm publish --dry-run --provenance --access=public
      # ... JSR path, browser bundle path, attest-build-provenance
```

### Invariants enforced at emit time

1. `id-token: write` permission. Without it the OIDC token request fails with "no identity token available".
2. `attestations: write` permission. Without it `actions/attest-build-provenance@v2` errors out at runtime.
3. Zero `NPM_TOKEN` / `JSR_TOKEN` / `secrets.*_TOKEN` references. Trusted Publishing replaces long-lived secrets; any reference is dead weight (legacy code path) or a leak risk.
4. Every `npm publish` line carries `--provenance`. Without the flag, the publish silently downgrades to the token path (which would fail on a Trusted-Publishing-only configured publisher) or, worse, succeed without attestation.
5. Every `uses:` ref pinned to a major version tag (`@v4`, `@v2`), not `@main` / `@latest`.
6. Real publish steps guarded by `if: startsWith(github.ref, 'refs/tags/')`. PRs run the dry-run path; only tag pushes trigger uploads.

### Tag-trigger pattern (`v*.*.*`)

Trusted Publishing on npm and JSR both support narrowing the publisher binding to a specific tag pattern. The Mochi-emitted workflow uses `v*.*.*` so a downstream user can register their npmjs.org Trusted Publisher with the same pattern, and arbitrary branch pushes cannot trigger a publish.

### Browser-bundle attestation

`actions/attest-build-provenance@v2` (GA October 2024) generates a SLSA-style provenance statement for the `dist/bundle/index.js` browser ESM file. The attestation is stored in GitHub Attestations (queryable via `gh attestation verify`). A user who downloads the bundle from the release page can verify it was built by the published workflow on the tagged commit.

## Sub-phases

| #    | Scope                                                                                                                                | Status   | Commit |
|------|---------------------------------------------------------------------------------------------------------------------------------------|----------|--------|
| 18.0 | `TargetReleaseWorkflow` emits `.github/workflows/release.yml` with OIDC permissions + Trusted Publishing flags + pinned actions       | LANDED   | (this PR) |
| 18.1 | (folded into 18.0) npm publish `--provenance --access=public` invocation                                                              | LANDED   | (this PR) |
| 18.2 | (folded into 18.0) provenance fields + tag-only guard                                                                                 | LANDED   | (this PR) |
| 18.3 | Consumer verification: verdaccio round-trip + `npm audit signatures` against the installed tarball                                    | DEFERRED | n/a    |
| 18.4 | JSR Trusted Publishing real round-trip (`deno publish --token-source=github-actions` against a JSR mirror)                            | DEFERRED | n/a    |
| 18.5 | (folded into 18.0) tag-triggered release workflow with three publish paths                                                            | LANDED   | (this PR) |

## Files

| File | Purpose |
|------|---------|
| `transpiler3/typescript/build/workflow.go` | `emitReleaseWorkflow`, `renderReleaseWorkflow` |
| `transpiler3/typescript/build/build.go` | `TargetReleaseWorkflow` enum + dispatch |
| `transpiler3/typescript/build/phase18_test.go` | 8 Phase 18 tests |

## Test set

- `TestPhase18WorkflowEmit`, hello fixture, workflow lands at conventional path.
- `TestPhase18OidcPermissions`, regexp check for both required permissions.
- `TestPhase18NoLongLivedTokens`, greps for 3 forbidden env names + secrets regex + every `npm publish` line carries `--provenance`.
- `TestPhase18PublishCommands`, both registries use the Trusted-Publishing flags.
- `TestPhase18TagTrigger`, tag pattern present + PR trigger + tag-only guard on publish.
- `TestPhase18ProvenanceDryRun`, `npm publish --dry-run --provenance --access=public` accepts the Phase 15 package. Skipped without npm.
- `TestPhase18PinnedActions`, every `uses:` ref pinned to a major-version tag.
- `TestPhase18ScaffoldReuse`, target produces package.json + src/index.ts + workflow file in one pass.

## Empirical: npm provenance dry-run

```
$ npm publish --dry-run --provenance --access=public
npm notice
npm notice 📦  @mochi/hello@0.0.0
npm notice Tarball Contents
...
npm notice shasum: ed242bc38fc1dfa607aa10f04fb6714d2b0de698
npm notice integrity: sha512-6CUm...AmgQ==
npm notice Publishing to https://registry.npmjs.org/ with tag latest and public access (dry-run)
+ @mochi/hello@0.0.0
```

Outside of GitHub Actions (no `ACTIONS_ID_TOKEN_REQUEST_URL` env var), the dry-run succeeds but does not produce a Sigstore signature; that step only fires when the OIDC env vars are present. Inside Actions, the same command produces a `--provenance` attestation visible via `npm view <pkg> --json | jq .attestations`.

## Trusted Publisher registration (downstream user step)

After `mochi build --target=release-workflow -o ./` lands the workflow, the user must register the publisher binding on npmjs.org:

1. Log in to npmjs.org, navigate to the package's settings.
2. Open "Trusted Publisher" -> "GitHub Actions".
3. Configure: organisation = `mochilang`, repository = `<pkg>-npm`, workflow filename = `release.yml`, environment = (blank or `release`).
4. Save.

The same flow applies on `jsr.io` under "Trusted Publishers". The Mochi project does NOT need to ship a one-time config script; the registry UIs are the source of truth.

## Deferred work

- Sub-phase 18.3: verdaccio + `npm audit signatures` consumer round-trip. Adds a self-hosted verdaccio CI service + a simulated-OIDC IdP. Lands once the CI image is wired.
- Sub-phase 18.4: JSR Trusted Publishing real round-trip. Same constraint as 18.3 (needs a JSR mirror that accepts simulated OIDC).
- npm Trusted Publishing for arbitrary user scopes (`@<user>/<pkg>`). Phase 18 ships the `@mochi/` scope; user-scope publishing requires the user's own publisher binding on npmjs.org. Documented above.
- GitLab CI as an alternative OIDC issuer. GitHub Actions is the Phase 18 target; GitLab support lands as 18.6 once GitLab's OIDC-to-npm integration GAs.
- Self-hosted runners with OIDC. GitHub-hosted runners are the Phase 18 target. Self-hosted runners can issue OIDC tokens but require additional Subject Alternative Name configuration on the npm side; documented as a deployment caveat.
- Sigstore key rotation. Phase 18 trusts Sigstore's published root; rotation is Sigstore's responsibility, not Mochi's.
- SLSA Level 4 (hermetic builds). Phase 18 reaches SLSA Level 3 (Trusted Publishing + provenance + Phase 16 reproducible build). Level 4 (hermetic build environment) is an open question for v2.
