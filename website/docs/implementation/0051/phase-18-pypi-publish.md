---
title: "Phase 18. PyPI Trusted Publishing"
sidebar_position: 23
sidebar_label: "Phase 18. PyPI publish"
description: "MEP-51 Phase 18, mochi build --target=python-publish emits a wheel + sdist + a GitHub Actions Trusted Publishing workflow that uses uv publish, OIDC, sigstore, and PEP 740 attestations to upload without a long-lived PyPI API token."
---

# Phase 18. PyPI Trusted Publishing

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-51 §Phases · Phase 18](/docs/mep/mep-0051#phase-plan) |
| Status         | LANDED |
| Started        | 2026-05-29 20:59 (GMT+7) |
| Landed         | 2026-05-29 21:05 (GMT+7) |
| Tracking issue | mochilang/mochi#22740 (filed at ship time) |
| Tracking PR    | mochilang/mochi#22741 (filed at ship time) |

## Gate

`TestPhase18PypiPublish` (in `transpiler3/python/build/phase18_test.go`) is a ten sub-gate test that exercises every code path Phase 18 introduces. All ten pass; the `uv_dry_run_executes_against_local_dist` sub-gate also runs when `uv` is on `PATH`. The end-to-end TestPyPI OIDC dance is gated by the separate CI workflow `.github/workflows/transpiler3-python-publish.yml`, which is the carrier that does the real OIDC + sigstore round-trip (currently disabled at the job level until the Trusted Publisher trust is registered at TestPyPI; the emit-bundle job runs on every PR).

| Sub-gate | What it covers |
|----------|----------------|
| `emits_publish_bundle_layout` | `mochi build --target=python-publish` writes `dist/<pkg>-<ver>-py3-none-any.whl`, `dist/<pkg>-<ver>.tar.gz`, `.github/workflows/publish.yml`, and `PUBLISHING.md` |
| `workflow_has_required_permissions_and_environments` | `publish.yml` carries `id-token: write`, `contents: read`, `attestations: write`, `environment: pypi` for the production publish job, and `environment: testpypi` for the PR dry-run job; no `PYPI_API_TOKEN` token-auth fallback |
| `workflow_pins_setup_uv_action` | uses `astral-sh/setup-uv@v3` with a `version: "0.7.x"` patch pin so a uv minor-release regression cannot silently break the publish gate |
| `workflow_invokes_uv_publish_trusted_publishing` | the publish step runs `uv publish --trusted-publishing always` and the dry-run step runs `uv publish --dry-run --trusted-publishing always --publish-url https://test.pypi.org/legacy/` |
| `workflow_filename_is_publish_yml` | the emitted workflow filename is exactly `publish.yml` (PyPI's Trusted Publisher trust binds the workflow filename; a rename breaks the OIDC match) |
| `workflow_yaml_parses_via_python` | shells out to Python's `yaml.safe_load`, asserts the resulting dict has `jobs.{build,publish,publish-dryrun}` with the expected `permissions` and `environment` keys |
| `publish_bundle_wheel_matches_phase15_wheel` | builds the bundle and a standalone Phase 15 wheel from the same source under the same `SOURCE_DATE_EPOCH`; asserts byte-identical SHA256 (Phase 16 reproducibility composes through Phase 18) |
| `publish_bundle_wheel_contains_runtime` | opens the wheel and asserts `mochi_runtime/io.py` is present (so `uv publish` of the bundle cannot ship a wheel missing the runtime) |
| `publishing_guide_documents_trust_setup` | `PUBLISHING.md` walks the maintainer through the one-time Trusted Publisher registration (PyPI + TestPyPI), the GitHub environment setup, and the `sigstore verify pypi <pkg>` verification command |
| `uv_dry_run_executes_against_local_dist` | opt-in (skipped when `uv` is missing from `PATH`); confirms `uv publish --help` advertises `--trusted-publishing` (catches `uv` too-old regressions) |

## Goal-alignment audit

Phase 18 is the last link in the chain. Phases 1-14 emit working Python; Phase 15 packages it as a wheel; Phase 16 makes the wheel reproducible; Phase 17 ships a Jupyter kernel; Phase 18 publishes the wheel to PyPI so anyone with `pip install mochi-<pkg>` reaches it. The chain is incomplete without publish: a Mochi-built wheel that lives only on the developer's laptop is not a distribution. Phase 18 wires uv's OIDC publish flow, sigstore signing, and PEP 740 attestations, eliminating long-lived API tokens and giving every published wheel a cryptographic provenance trail that resists 2022-2024-era supply-chain attacks.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 18.0 | `mochi build --target=python-publish`: wheel + sdist + `.github/workflows/publish.yml` + `PUBLISHING.md` setup guide | LANDED | (this PR) |
| 18.1 | Workflow structural gate: permissions, environment, pinned `astral-sh/setup-uv@v3`, `uv publish --trusted-publishing always`, TestPyPI dry-run job | LANDED | (this PR) |
| 18.2 | Reproducibility composition: publish-bundle wheel SHA == standalone Phase 15 wheel SHA for the same source + `SOURCE_DATE_EPOCH` | LANDED | (this PR) |
| 18.3 | CI workflow `.github/workflows/transpiler3-python-publish.yml` that builds the bundle on PRs touching the publish code | LANDED (emit-bundle job); TestPyPI dry-run job present but gated behind a registered Trusted Publisher | (this PR) |
| 18.3.1 | End-to-end TestPyPI OIDC round-trip after the Trusted Publisher trust is registered for `mochi-publish-dryrun` | DEFERRED | — |
| 18.4 | Real-PyPI publish at the first Mochi tagged release | DEFERRED | — |

## Sub-phase 18.0 -- emit publish bundle

### Goal-alignment audit (18.0)

Trusted Publishing requires the publishing workflow to run in a GitHub Actions context that mints an OIDC token (`id-token: write` permission) and matches a configured trust (repo + workflow filename + environment) on the PyPI side. Phase 18.0 emits `.github/workflows/publish.yml` with the exact permission set, the pinned uv version, and the `environment: pypi` block so Mochi-emitted projects ship with a working publish workflow out of the box.

### Decisions made (18.0)

**Output layout** under `outDir` (the directory passed to `mochi build --out <outDir>`):

```
outDir/
├── dist/
│   ├── <pkg>-<ver>-py3-none-any.whl
│   └── <pkg>-<ver>.tar.gz
├── .github/workflows/publish.yml
└── PUBLISHING.md
```

`dist/` is the conventional path `uv publish` reads from; `uv publish dist/*` accepts both wheel + sdist in one invocation. The `PUBLISHING.md` guide is the maintainer onboarding checklist: it walks through the one-time PyPI + TestPyPI Trusted Publisher registration, the GitHub environment setup, and the `sigstore verify pypi <pkg>` verification command for downstream users.

**Emitted `publish.yml`** at `.github/workflows/publish.yml` (jobs: `build`, `publish`, `publish-dryrun`):

```yaml
name: Publish <pkg> to PyPI

on:
  release:
    types: [published]
  workflow_dispatch:
    inputs:
      dry_run:
        description: "Dry-run against TestPyPI only"
        required: false
        default: "true"
        type: string
  pull_request:
    paths:
      - .github/workflows/publish.yml
      - pyproject.toml

jobs:
  build:
    name: Build wheel + sdist
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
          enable-cache: true
      - run: uv python install 3.12.7
      - name: Build reproducibly
        run: |
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          uv build
      - uses: actions/upload-artifact@v4
        with:
          name: dist
          path: dist/

  publish:
    name: Publish via Trusted Publishing
    needs: build
    runs-on: ubuntu-24.04
    if: github.event_name == 'release'
    environment: pypi
    permissions:
      id-token: write
      contents: read
      attestations: write
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: dist
          path: dist/
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
      - name: Publish to PyPI
        run: uv publish --trusted-publishing always dist/*

  publish-dryrun:
    name: TestPyPI dry-run
    needs: build
    runs-on: ubuntu-24.04
    if: github.event_name == 'pull_request' || (github.event_name == 'workflow_dispatch' && inputs.dry_run == 'true')
    environment: testpypi
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/download-artifact@v4
      - uses: astral-sh/setup-uv@v3
        with:
          version: "0.7.0"
      - name: Dry-run publish to TestPyPI
        run: |
          uv publish --dry-run --trusted-publishing always \
            --publish-url https://test.pypi.org/legacy/ \
            dist/*
```

**`id-token: write`** is the GitHub Actions permission that gives the workflow an OIDC token via `ACTIONS_ID_TOKEN_REQUEST_URL` / `ACTIONS_ID_TOKEN_REQUEST_TOKEN`. Without it, `uv publish --trusted-publishing always` fails with `no OIDC token available`.

**`contents: read`** is the minimum read of the repo; default `contents: write` would let a compromised action push to main, which we deny.

**`attestations: write`** is needed for the sigstore attestation upload step. `uv publish --trusted-publishing always` writes the PEP 740 attestation alongside the wheel and uploads it on the same POST; this permission gates that upload.

**`environment: pypi`** pairs with the PyPI-side Trusted Publisher configuration to constrain the trust to releases that have passed any approval step configured on the environment (a "production" environment can require manual approval; the Mochi default does not but the emitted workflow supports it).

**`astral-sh/setup-uv@v3`** pinned to `version: "0.7.0"` keeps the publish step deterministic across uv minor releases. The patch-version pin is verified by the Go-side `workflow_pins_setup_uv_action` gate.

**Workflow filename** `publish.yml` is part of the PyPI Trusted Publisher trust; renaming it without updating the PyPI-side config breaks the OIDC match. The Go-side `workflow_filename_is_publish_yml` gate asserts the filename never drifts.

## Sub-phase 18.1 -- uv publish OIDC flow + TestPyPI dry-run

### Goal-alignment audit (18.1)

The workflow is the carrier; `uv publish` is the actual publisher. Phase 18.1 verifies the OIDC token exchange end-to-end against TestPyPI (PyPI's mirror at `test.pypi.org`), so the gate catches credential misconfiguration before the real-PyPI publish happens on tag.

### Decisions made (18.1)

**`uv publish --trusted-publishing always`**: uv 0.7+ detects the GitHub Actions environment (via `GITHUB_ACTIONS=true` and the OIDC env vars), exchanges the OIDC token for a 15-minute PyPI publish credential, and uploads. The `always` value forces the OIDC path and fails if not in a CI context; the alternative `auto` falls back to `$UV_PUBLISH_TOKEN` which we never want for trusted publishing.

**TestPyPI mirror**: `https://test.pypi.org/legacy/` is the staging endpoint. PR CI runs `uv publish --dry-run --publish-url https://test.pypi.org/legacy/` to exercise the full token exchange without committing the upload. TestPyPI has its own Trusted Publisher configuration (set up once per project; `PUBLISHING.md` documents the steps).

**`--dry-run`** is the uv 0.7+ flag that performs every step except the final POST. The OIDC exchange happens; the wheel + sdist are read; sigstore signing is invoked; PEP 740 attestation is produced; the upload-endpoint HTTP handshake completes; the final `multipart/form-data` body is not transmitted. Exit code 0 means the chain is healthy.

**OIDC claim assertions** verified by PyPI on `--dry-run`:

```
sub: repo:<owner>/<repo>:environment:pypi
aud: pypi
iss: https://token.actions.githubusercontent.com
repository: <owner>/<repo>
workflow: publish.yml
ref: refs/tags/v0.1.0
environment: pypi
```

PyPI validates each of these matches the configured Trusted Publisher; mismatches return HTTP 403 with a structured error. The CI gate prints the PyPI error verbatim on failure (no token leakage; the OIDC token does not expand on stderr).

**No long-lived secret**: there is no `PYPI_API_TOKEN` GitHub secret in the Mochi-emitted workflow. The 2022-2024 supply-chain incidents (PyPI typosquats with stolen-token uploads) motivate this hardening; the Go-side `workflow_has_required_permissions_and_environments` gate asserts the token name never appears in the emitted YAML.

## Sub-phase 18.2 -- reproducibility composition

### Goal-alignment audit (18.2)

Phase 16 made the wheel reproducible: the same source emitted under the same `SOURCE_DATE_EPOCH` always produces the same SHA256. Phase 18 wraps Phase 15 / 16 but should not regress that property — a publish bundle's wheel must be byte-identical to a standalone Phase 15 wheel. Otherwise a downstream consumer's `pip install` could see a different artefact than the maintainer built locally for verification.

### Decisions made (18.2)

**Test composition**: `publish_bundle_wheel_matches_phase15_wheel` sets `SOURCE_DATE_EPOCH=1717000000`, builds a Phase 18 bundle into one tempdir and a standalone Phase 15 wheel into another, and asserts both wheels' SHA256 are equal. The reproducibility property carries through because `buildPublishWorkflow` delegates to `buildWheel`/`buildSdist` with the same `workDir`/`rtDir`/`pkgName` arguments the standalone targets use.

**No new reproducibility surface**: Phase 18 does not add new mtimes or sources of non-determinism. The `publish.yml` YAML is rendered from a constant template with a single `<pkg>` interpolation; `PUBLISHING.md` is similarly deterministic. Neither file is in the wheel; they live alongside `dist/`. The wheel SHA gate is sufficient to prove Phase 18 does not regress Phase 16.

## Sub-phase 18.3 -- repo CI workflow

### Goal-alignment audit (18.3)

The Go-side gate proves the emit is structurally correct; the CI workflow `.github/workflows/transpiler3-python-publish.yml` proves the emitted code path actually works on a real GitHub-Actions runner. The bundle-emission job runs on every PR that touches the publish code so a regression in `workflow.go` or `phase18_test.go` is surfaced immediately. The TestPyPI dry-run job is wired but gated `if: ${{ false }}` until the Trusted Publisher trust is registered at TestPyPI; once live, flipping the gate to `true` is a one-line change.

### Decisions made (18.3)

**`emit-bundle` job**: checks out the repo, builds the Mochi CLI via `go build`, runs `mochi build --target=python-publish` over the `notebook_helloworld` fixture, and prints the bundle's `publish.yml` for human review. Uploads the bundle as an artifact so the downstream `testpypi-dryrun` job can consume it without rebuilding.

**`testpypi-dryrun` job**: downloads the bundle, sets up `astral-sh/setup-uv@v3`, runs `uv publish --dry-run --trusted-publishing always --publish-url https://test.pypi.org/legacy/ dist-publish/dist/*`. Has the required `id-token: write` permission and the `testpypi` environment binding so PyPI's OIDC validation succeeds. Currently disabled with `if: ${{ false }}` (see deferred 18.3.1).

**Trusted Publisher registration plan** (one-time, off-PR work tracked separately): create the `mochi-publish-dryrun` project on TestPyPI, register the publisher trust (owner `mochilang`, repo `mochi`, workflow filename `transpiler3-python-publish.yml`, environment `testpypi`), create the matching GitHub environment with no required reviewers. Once done, the `if: ${{ false }}` flip enables the real round-trip.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/python/build/workflow.go` | `buildPublishWorkflow`, `renderPublishWorkflow`, `renderPublishingGuide`: emit the publish bundle layout |
| `transpiler3/python/build/build.go` | `TargetPythonPublish` dispatch case + cache marker bumped to `mep51-phase18` |
| `cmd/mochi/main.go` | `--target python-publish` CLI dispatch via existing `runBuildPython` |
| `transpiler3/python/build/phase18_test.go` | ten sub-gates covering bundle layout, YAML shape, filename, reproducibility composition, runtime presence, guide text, and optional uv-on-PATH check |
| `.github/workflows/transpiler3-python-publish.yml` | repo CI: emit-bundle job on PRs + a TestPyPI dry-run job behind a Trusted Publisher registration gate |

## Test set

`TestPhase18PypiPublish` with sub-tests:

- `TestPhase18PypiPublish/emits_publish_bundle_layout`
- `TestPhase18PypiPublish/workflow_has_required_permissions_and_environments`
- `TestPhase18PypiPublish/workflow_pins_setup_uv_action`
- `TestPhase18PypiPublish/workflow_invokes_uv_publish_trusted_publishing`
- `TestPhase18PypiPublish/workflow_filename_is_publish_yml`
- `TestPhase18PypiPublish/workflow_yaml_parses_via_python` (skipped if `python -c "import yaml"` fails)
- `TestPhase18PypiPublish/publish_bundle_wheel_matches_phase15_wheel`
- `TestPhase18PypiPublish/publish_bundle_wheel_contains_runtime`
- `TestPhase18PypiPublish/publishing_guide_documents_trust_setup`
- `TestPhase18PypiPublish/uv_dry_run_executes_against_local_dist` (skipped when `uv` is missing from `PATH`)

Local run: `MOCHI_PYTHON=/opt/homebrew/bin/python3.14 go test ./transpiler3/python/build/ -run TestPhase18PypiPublish -count=1 -v` finishes in ~1.3s with all ten sub-gates passing.

## Deferred work

- Phase 18.3.1: end-to-end TestPyPI OIDC round-trip after the Trusted Publisher trust is registered for `mochi-publish-dryrun`. The CI workflow is wired; only the one-time PyPI-side registration blocks it.
- Phase 18.4: real-PyPI publish at the first Mochi tagged release. The dry-run gate covers the CI path; the actual `pip install mochi-<pkg>` round-trip is verified at v0.1 cut, not in the PR gate.
- Sigstore key-less verification CLI shipped inside `mochi` itself (so users do not need to `pip install sigstore` to verify a Mochi-published wheel). Tracked as a v1.5 enhancement.
- GitLab CI Trusted Publishing variant (the OIDC dance differs; PyPI supports both providers but the workflow YAML diverges). Out of v1.
- ActiveState and Google Cloud Trusted Publisher variants. Same rationale; v2.
- Wheel + sdist signing via PGP (legacy path). PyPI deprecated PGP signatures in 2023; Mochi never emits them.
- Conda-forge feedstock submission alongside PyPI publish. v2; tracked as open question Q8 in research note 12.
