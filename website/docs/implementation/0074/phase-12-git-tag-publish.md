---
title: "Phase 12. Git-tag publish flow"
sidebar_position: 14
sidebar_label: "Phase 12. Git-tag publish"
description: "MEP-74 Phase 12 lands the `mochi pkg publish --to=go+git+<repo-url>@<tag>` driver as a self-contained `package3/go/publish/` module. The driver consumes a `library.EmitResult`, validates the canonical-import-path invariant (`go.mod` module directive matches the configured `ModulePath`), materialises the rendered files into a fresh git working tree, runs `git init` / `add` / `commit` / `tag` / `push` via an abstract Runner, and returns the resulting commit SHA + tag for the CLI confirmation line. A DryRun flag stops short of the push step (still tags locally). The sentinel exercises the full flow against a local bare-repo fixture and then clones the remote to confirm the publish is consumable."
---

# Phase 12. Git-tag publish flow

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-30 00:30 (GMT+7) |
| Landed         | 2026-05-30 00:50 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase12GitTagPublishSentinel` in
`package3/go/publish/phase12_test.go` walks a representative calculator package (Version const + Add func) through the full flow and asserts:

- `library.Emit` + `Publish.Validate` + `Publish.Run` together produce a real git working tree whose committed contents match the rendered Files map byte-for-byte;
- the configured tag and the publish commit both land in a local bare-repo remote (`file://<tmp>/remote.git`);
- a downstream `git clone` of the remote + `git checkout <tag>` resolves the tag and exposes every expected file (`go.mod`, `doc.go`, `calc.go`, `LICENSE`);
- `go build ./...` against the cloned tree succeeds, proving the publish does not corrupt the underlying library buildability;
- DryRun mode leaves the local tree intact (commit + tag both present locally) but the remote bare-repo stays empty for that version;
- a canonical-import-path mismatch (`go.mod` says `example.com/x`, request says `example.com/wrong`) is blocked before any git side effect occurs.

Plus 17 unit tests in `publish_test.go`:

- validation (`TestValidateAccepts`,
  `TestValidateRejectsEmptyFiles`,
  `TestValidateRejectsMissingGoMod`,
  `TestValidateRejectsBadTag`,
  `TestValidateRejectsMissingRemoteURL`,
  `TestValidateRejectsMissingAuthor`,
  `TestValidateRejectsCanonicalImportPathMismatch`),
- canonical-import-path parser
  (`TestCanonicalImportPathHappy`,
  `TestCanonicalImportPathSkipsLeadingComments`,
  `TestCanonicalImportPathRejectsMissingDirective`,
  `TestCanonicalImportPathRejectsEmpty`),
- happy-path command sequence
  (`TestPublishHappyPath`),
- dry-run skips push (`TestPublishDryRunSkipsPush`),
- custom branch / commit message overrides
  (`TestPublishUsesCustomBranch`,
  `TestPublishUsesCustomCommitMessage`),
- file materialisation (`TestPublishMaterialisesFiles`),
- early-exit on validation failure
  (`TestPublishValidateFailureSkipsAllRunner`),
- runner failure surfaces (`TestPublishRunnerErrorSurfaces`),
- auto-generated workspace temp dir
  (`TestPublishGeneratesTempWorkspaceWhenEmpty`).

## Lowering decisions

The publish package is layering-conservative: it imports `package3/go/library` (for the `EmitResult` input shape) and `package3/go/semver` (for the tag validator), and otherwise depends only on the Go stdlib. The publish surface splits into a pure shaping layer (`PublishRequest.Validate`, `CanonicalImportPath`) and an impure runner (`Publish`) so callers fully validate the request before touching git.

**The canonical-import-path gate is upfront and total.** `PublishRequest.Validate` parses the supplied go.mod, extracts the `module <path>` directive, and asserts it equals `PublishRequest.ModulePath`. A mismatch surfaces a wrapped `ErrPublish` before any git command runs. This is the MEP-74 spec §3 invariant that prevents a user from publishing a Mochi package under a vanity import path different from the one consumers will write in their `import` statements; a mismatch would cause every downstream `go get` to fail at the `go/packages.Load` step.

**The Runner is the command-execution boundary.** `Publish` accepts a `Runner` interface (`Run(dir, name, args...)`, `Output(dir, name, args...)`) rather than calling `os/exec` directly. The production runner is `NewExecRunner()`, which shells out via `exec.Command` and scrubs `GIT_DIR` / `GIT_WORK_TREE` / `GIT_INDEX_FILE` / `GIT_OBJECT_DIRECTORY` from the env (so a nested git context in the parent process cannot leak into the publish workspace). The unit-test runner `RecordingRunner` captures every call in order, returns a synthetic SHA for `git rev-parse HEAD`, and supports an `ErrAt` index that fails the Nth call (so every error path is reachable without a real failure mode).

**The local commit + tag always happens; the push is gated by DryRun.** The publish flow always runs `git init` / `config` / `remote add` / `add` / `commit` / `tag` locally; only the two `git push` invocations are skipped under DryRun. This mirrors `npm publish --dry-run` and `cargo publish --dry-run` semantics: the local working tree is fully valid after a dry-run, so the user can inspect it before retrying with DryRun off. The MEP-74 spec §289 explicitly calls for this: `mochi pkg publish --to=go+git+<repo-url>@<tag> [--dry-run]`.

**Default branch defaults to `main`, configurable per request.** The publish working tree initialises on `main` (via `git init --initial-branch=main`), the publish commit lands on `main`, and the push pushes to `origin main`. Callers that publish to a repo with a non-`main` default branch (e.g. legacy `master`, organisation-specific `release/`) override via `PublishRequest.DefaultBranch`. Phase 17's vanity-import resolver may populate this from the resolved `<meta name="go-import">` redirect's repo metadata.

**Commit message defaults to `publish <tag>` but is overridable.** When the caller does not supply `CommitMessage`, the publish commit subject is `publish <tag>` (matching the cargo-publish and gem-push convention). Override via `PublishRequest.CommitMessage` for callers that need a richer subject (e.g. `release: v0.2.0 publish (CI #1234)` for an automated CI flow).

**Annotated tags, not lightweight tags.** `Publish` uses `git tag -a <tag> -m "release <tag>"`. Annotated tags carry a tagger identity, a timestamp, and a message, so `go get` (which fetches the tag via the module proxy protocol) records a stable revision identifier. Lightweight tags would also work but lack the audit trail.

**GPG signing is explicitly disabled.** `Publish` runs `git config commit.gpgsign false` and `git config tag.gpgsign false` in the publish workspace (not globally). This is load-bearing: a CI runner that inherits a globally-enabled `commit.gpgsign = true` from the parent env would otherwise hang waiting for a passphrase. Phase 13 (cosign signing) is the bridge's preferred signing path; users who need GPG-signed publish commits will get that as a deferred sub-phase.

**Workspace cleanup is the caller's responsibility.** `Publish` does not delete the working tree on success or failure. The CLI layer (deferred to phase 12.1) will own the temp-dir lifecycle: print the workspace path on success for the user to optionally inspect, then either `rm -rf` it or leave it on disk based on a `--keep-workspace` flag. This matches the `cargo publish` and `goreleaser` conventions of leaving the build output for post-mortem inspection.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/publish/publish.go` | `PublishRequest`, `Author`, `PublishResult`, `ErrPublish`, `Validate`, `CanonicalImportPath`, `Publish`, `materialiseFiles`. |
| `package3/go/publish/runner.go` | `Runner` interface, `NewExecRunner`, `execRunner`, `sanitisedEnv`. |
| `package3/go/publish/env.go` | `getenv` indirection for unit-test stubbing. |
| `package3/go/publish/publish_test.go` | 17 unit tests + `RecordingRunner` helper. |
| `package3/go/publish/phase12_test.go` | `TestPhase12GitTagPublishSentinel` end-to-end against a local bare-repo fixture. |
| `website/docs/implementation/0074/phase-12-git-tag-publish.md` | (this page) |

## Test set

- `TestPhase12GitTagPublishSentinel` (3 sub-tests)
- 17 unit tests in `publish_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/publish/...
ok      mochi/package3/go/publish       1.3s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/library       (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/publish       1.315s
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 12 lands the publish driver as a leaf module. The CLI wiring (`mochi pkg publish --to=go+git+<repo-url>@<tag>`) and the workspace-lifecycle policy are reserved for phase 12.1 once the MEP-57 CLI driver gains a publish-target dispatch hook.

Future phase 12.x reservations:

- **12.1** CLI wiring + workspace-lifecycle (keep-workspace flag, prompt on push failure, retry).
- **12.2** Module-proxy warm-up: after a successful push, issue a `GET <proxy>/<module>/@v/<tag>.info` to prime the public proxy cache so downstream consumers can `go get <module>@<tag>` without the typical 10-minute first-touch delay.
- **12.3** Pre-tag hook to run `go mod tidy` + `go vet ./...` + `go build ./...` against the materialised tree (today the caller is expected to have validated the tree before invoking Publish; an integrated pre-tag hook would make the flow more robust against an accidentally-broken Emit).
- **12.4** Multi-remote publish (push the same tag to multiple mirror remotes for high-availability).

The Sigstore-cosign signature flow (an optional `<tag>.sig` sibling tag) is phase 13's responsibility and consumes `PublishResult.CommitSHA` as the artefact to sign.
