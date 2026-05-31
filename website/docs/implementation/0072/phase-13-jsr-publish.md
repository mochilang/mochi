---
title: "Phase 13. JSR Trusted-Publishing publish"
sidebar_position: 15
sidebar_label: "Phase 13. JSR publish"
description: "MEP-72 Phase 13: JSR Trusted-Publishing publish flow. Wires the TargetJsrLibrary emit into MEP-52 Phase 18's emitted GitHub Actions workflow (`deno publish --token-source=github-actions` under OIDC token exchange)."
---

# Phase 13. JSR Trusted-Publishing publish

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase13JsrPublish` in `package3/typescript/publish/phase13_test.go`: subtests `emit_workflow`, `oidc_token_exchange_recorded`, `attestation_generated`, `consumer_verification`, `deno_check_after_publish`. The first asserts that `mochi pkg publish --target=jsr-library` produces a GitHub Actions workflow at `.github/workflows/mochi-publish-jsr.yml` that runs `deno publish --token-source=github-actions` under the `id-token: write` permission. The second replays a recorded OIDC token-exchange flow (using a fixture-mode JSR registry) and asserts the token is exchanged correctly. The third runs the publish flow end-to-end against the fixture registry and asserts the resulting attestation matches the recorded Sigstore certificate chain. The fourth runs `deno add jsr:@mochi/example` from a downstream consumer fixture and asserts the install pulls the attested version. The fifth runs `deno check` against the consumed package and asserts no type errors.

## Lowering decisions

The phase emits a GitHub Actions workflow:

```yaml
name: Publish to JSR

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
      - uses: denoland/setup-deno@v2
        with:
          deno-version: v2.x
      - name: Build Mochi package
        run: mochi build --target=jsr-library
      - name: Publish to JSR
        working-directory: ./target/ts/jsr-library
        run: deno publish --token-source=github-actions
```

`deno publish --token-source=github-actions` reads the `ACTIONS_ID_TOKEN_REQUEST_URL` + `ACTIONS_ID_TOKEN_REQUEST_TOKEN` env vars (auto-provided by GitHub Actions when `id-token: write` is set), exchanges the OIDC token with JSR's server, and uploads the package with the resulting attestation.

The workflow has NO `JSR_TOKEN` or `DENO_TOKEN` secret. The publish is GitHub-Actions-driven; the bridge only emits the workflow.

The phase's gate runs against a fixture-mode JSR registry (the `jsr-registry-fake` test server) so the test suite is hermetic.

JSR's server-side transpile produces the `.d.ts` artifacts; the bridge does NOT pre-compute them. This is the structural simplification from Research Note 06 §3.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/publish/jsr.go` | `EmitJsrPublishWorkflow` |
| `package3/typescript/publish/phase13_test.go` | `TestPhase13JsrPublish` sentinel |
| `package3/typescript/publish/testdata/jsr-registry-fake/` | fixture-mode registry server |
| `.github/workflows/templates/mochi-publish-jsr.yml` | the canonical workflow template emitted by Phase 18 |

## Test set

5 subtests as listed in the Gate section.

## Cross-references

- [Research note 07 §2 JSR Trusted Publishing](/docs/research/0072/07-sigstore-npm-jsr-trusted-publishing#2-jsr-trusted-publishing) — the OIDC token-exchange flow.
- [Research note 06 §3 JSR publish flow](/docs/research/0072/06-npm-jsr-publish-flow#3-jsr-publish-flow) — the source-not-dist invariant.
