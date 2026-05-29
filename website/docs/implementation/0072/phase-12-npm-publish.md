---
title: "Phase 12. npm Trusted-Publishing publish"
sidebar_position: 14
sidebar_label: "Phase 12. npm publish"
description: "MEP-72 Phase 12: npm Trusted-Publishing publish flow. Wires the TargetNpmLibrary emit into MEP-52 Phase 18's emitted GitHub Actions workflow (`npm publish --provenance --access=public` under OIDC token exchange)."
---

# Phase 12. npm Trusted-Publishing publish

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase12NpmPublish` in `package3/typescript/publish/phase12_test.go`: subtests `emit_workflow`, `oidc_token_exchange_recorded`, `attestation_generated`, `consumer_verification`, `npm_audit_signatures`. The first asserts that `mochi pkg publish --target=npm-library` produces a GitHub Actions workflow at `.github/workflows/mochi-publish-npm.yml` that runs `npm publish --provenance --access=public` under the `id-token: write` permission. The second replays a recorded OIDC token-exchange flow (using a fixture-mode npm registry) and asserts the token is exchanged correctly. The third runs the publish flow end-to-end against the fixture registry and asserts the resulting `_attestations` entry on the packument carries a valid Sigstore signature. The fourth runs `npm install @mochi/example` from a downstream consumer fixture and asserts the install verifies the attestation. The fifth runs `npm audit signatures` against the same install and asserts the audit passes.

## Lowering decisions

The phase emits a GitHub Actions workflow (already shipping in MEP-52 Phase 18 under the `TargetReleaseWorkflow`):

```yaml
name: Publish to npm

on:
  push:
    tags: ['v*']

permissions:
  contents: read
  id-token: write

jobs:
  publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '22'
          registry-url: 'https://registry.npmjs.org'
      - name: Build Mochi package
        run: mochi build --target=npm-library
      - name: Publish to npm with provenance
        working-directory: ./target/ts/npm-library
        run: npm publish --provenance --access=public
      - name: Attest build provenance
        uses: actions/attest-build-provenance@v2
        with:
          subject-path: 'target/ts/npm-library/dist/**'
```

The `id-token: write` permission lets `actions/attest-build-provenance` exchange the GitHub Actions OIDC token for a Sigstore certificate via Fulcio. `npm publish --provenance` flag triggers npm's server-side attestation recording.

The workflow has NO `NPM_TOKEN` secret (matches MEP-52 §A11 + MEP-73 §A9 + MEP-74 §A9): authentication is OIDC token exchange only. The user pre-configures the package's npm registry settings to enable Trusted Publishing for the specific GitHub repository (one-time UI flow at https://www.npmjs.com/settings/<scope>/packages/<pkg>/access).

The workflow uploads only `target/ts/npm-library/` (the Phase 10 emit output). The bridge's reference Go skeleton does NOT shell out to `npm publish` from inside Mochi; the publish is GitHub-Actions-driven, the bridge only emits the workflow.

The phase's gate runs against a fixture-mode npm registry (the `npm-registry-fake` test server) so the test suite is hermetic and does not require a real npm publish.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/publish/npm.go` | `EmitNpmPublishWorkflow`, `EmitProvenanceStep` |
| `package3/typescript/publish/oidc.go` | OIDC token-exchange helper (used in fixture mode) |
| `package3/typescript/publish/phase12_test.go` | `TestPhase12NpmPublish` sentinel |
| `package3/typescript/publish/testdata/npm-registry-fake/` | fixture-mode registry server |
| `.github/workflows/templates/mochi-publish-npm.yml` | the canonical workflow template emitted by Phase 18 |

## Test set

5 subtests as listed in the Gate section.

## Cross-references

- [Research note 06 npm publish flow](/docs/research/0072/06-npm-jsr-publish-flow) — the publish-side design.
- [Research note 07 §1 npm Trusted Publishing](/docs/research/0072/07-sigstore-npm-jsr-trusted-publishing) — the OIDC token-exchange flow.
- [MEP-52 phase 18](/docs/implementation/0052/phase-18-trusted-publishing) — the underlying workflow emitter.
