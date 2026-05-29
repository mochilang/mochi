---
title: "Phase 13. Cosign-on-sibling-tag"
sidebar_position: 15
sidebar_label: "Phase 13. Cosign sibling-tag"
description: "MEP-74 Phase 13 lands the optional Sigstore-cosign signing layer as a self-contained `package3/go/cosign/` module. The signer consumes a `publish.PublishResult` plus an OIDC token, exchanges the token for a keyless Fulcio code-signing certificate, signs the publish commit SHA, logs the signature to Rekor, and attaches the resulting cosign bundle as the annotated message body of a sibling tag `<tag>.sig` that lives on the same remote the publish commit went to. A DryRun flag stops short of the push step (still tags + commits locally). The end-to-end sentinel verifies that a downstream clone of the remote can recover the cosign bundle bytes byte-for-byte via `git cat-file -p <tag>.sig`."
---

# Phase 13. Cosign-on-sibling-tag

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED |
| Started        | 2026-05-30 00:00 (GMT+7) |
| Landed         | 2026-05-30 00:30 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase13CosignSiblingTagSentinel` in
`package3/go/cosign/phase13_test.go` walks a representative calculator package (Version const + Add func) through the full `library.Emit` -> `publish.Publish` -> `cosign.Sign` chain and asserts:

- the sibling tag `<tag>.sig` lands in the same local bare-repo remote (`file://<tmp>/remote.git`) the publish commit was pushed to;
- the sibling tag points to the publish commit SHA (not to a separate commit) so a verifier can resolve `<tag>.sig` and immediately know which revision is signed;
- a downstream `git clone` of the remote followed by `git cat-file -p <tag>.sig` round-trips the cosign bundle bytes byte-for-byte through `ParseSiblingMessage`;
- the fake signer received the publish commit SHA, the publish tag, the default Sigstore audience (`sigstore`), and the default Fulcio + Rekor endpoints.

Plus 19 unit tests in `cosign_test.go`:

- validation
  (`TestValidateAccepts`,
  `TestValidateRejectsShortSHA`,
  `TestValidateRejectsNonHexSHA`,
  `TestValidateRejectsUppercaseSHA`,
  `TestValidateRejectsEmptyTag`,
  `TestValidateRejectsAlreadySigTag`,
  `TestValidateRejectsEmptyWorkspaceRoot`,
  `TestValidateRejectsEmptyOIDC`,
  `TestValidateRejectsEmptyAuthor`),
- happy-path signer + runner sequence
  (`TestSignHappyPath`),
- dry-run skips push (`TestSignDryRunSkipsPush`),
- signer error surfaces, short-circuits runner
  (`TestSignSurfacesSignerError`),
- empty bundle rejection
  (`TestSignRejectsEmptyBundle`),
- validation failure short-circuits the signer
  (`TestSignValidationFailureSkipsSigner`),
- sibling-tag message round-trip
  (`TestBuildAndParseSiblingMessageRoundtrip`,
  `TestParseSiblingMessageRejectsUnknownScheme`,
  `TestParseSiblingMessageRejectsMissingFields`,
  `TestParseSiblingMessageRejectsBadBase64`),
- custom audience + endpoint overrides
  (`TestSignUsesCustomAudienceAndEndpoints`),
- `isFullSHA` predicate
  (`TestIsFullSHA`).

## Lowering decisions

The cosign package is layering-conservative: it imports `package3/go/publish` for the `Runner` interface + `Author` shape and otherwise depends only on the Go stdlib. The signing surface splits into a pure shaping layer (`SignRequest.Validate`), a small network-facing primitive (`Signer.Sign`), and an impure runner (`Sign`) so callers fully validate the request before any side effect, and the unit tests can exercise the full flow against an in-process fake signer.

**The Signer interface is the only network seam.** Production builds wire a real Sigstore client behind the `Signer` interface (OIDC token exchange with Fulcio, keyless certificate issuance, Rekor inclusion-proof log). The interface signature `Sign(commitSHA, tag, audience, oidcToken, fulcioURL, rekorURL) ([]byte, error)` returns the verbatim cosign bundle bytes; the signing package never inspects the bundle past asserting it is non-empty. This keeps the signing-side cryptography deployable as a separate runtime dependency (deferred to phase 13.1) without making the surrounding orchestrator depend on the live Sigstore stack at build time.

**Sibling-tag scheme is `<tag>.sig`, annotated, with a structured message body.** The cosign bundle bytes go into the annotated message body of a sibling tag whose name is `<publish-tag>.sig`. The body has a header line `cosign-sig: mochi-mep74-cosign-v1` (the scheme name + version), the publish `tag:` and `commit:` lines, and a `bundle-base64:` line carrying the raw bundle bytes base64-encoded. Base64 keeps the message text-safe (git restricts what byte sequences can appear in tag messages). The `ParseSiblingMessage` verifier-side helper round-trips the body back to (`tag`, `commit`, `bundle`); a downstream consumer that runs `git cat-file -p <tag>.sig` can recover the bundle without an out-of-band registry.

**The sibling tag points to the publish commit, not to a fresh commit.** `git tag -a <tag>.sig <publish-commit-sha> -m ...` attaches the sibling directly to the publish commit. This means a verifier who resolves `<tag>.sig` (`git rev-list -n 1 <tag>.sig`) immediately knows the signed revision; the bundle's signed payload also binds to that commit SHA so a tampered bundle that points to a different revision will fail to verify. The end-to-end sentinel asserts this round-trip explicitly.

**Validation rejects `<tag>.sig` as the input tag.** `SignRequest.Validate` errors if `Tag` already ends in `.sig`: the caller should pass the publish tag (e.g. `v0.2.0`) and the package will compute the sibling name. Otherwise a typo (`mochi pkg publish --cosign-sign --tag=v0.2.0.sig`) would land a `<tag>.sig.sig` tag, which is harmless but confusing.

**Default endpoints match upstream Sigstore.** `DefaultAudience = "sigstore"`, `DefaultFulcioURL = "https://fulcio.sigstore.dev"`, `DefaultRekorURL = "https://rekor.sigstore.dev"` mirror the values hardcoded in the upstream `cosign sign` CLI. Callers can override per-request via `SignRequest.Audience`, `.FulcioURL`, `.RekorURL` for staging or air-gapped Sigstore deployments.

**The Runner is reused from `publish.Runner`.** The cosign signer uses the same `publish.Runner` interface as the publish driver, so the production runner (`publish.NewExecRunner()`) handles env sanitisation (scrubs `GIT_DIR` / `GIT_WORK_TREE` / `GIT_INDEX_FILE` / `GIT_OBJECT_DIRECTORY`) consistently across publish + sign. The unit-test `recordingRunner` captures every call in order so the happy-path test asserts the exact `git config user.name` / `git config user.email` / `git tag -a <sibling> <sha> -m ...` / `git push origin <sibling>` sequence.

**DryRun is a full local rehearsal.** Like `publish.PublishRequest.DryRun`, the cosign DryRun runs the signer end-to-end (the bundle is produced, the sibling tag is created locally) but skips the `git push`. A single `mochi pkg publish --dry-run --cosign-sign` invocation therefore exercises both flows end-to-end against a local working tree without touching any remote.

**The signer is fail-loud on empty bundles.** If the `Signer` implementation returns a zero-length byte slice (e.g. a misconfigured Fulcio endpoint returns a 200 with no body), `Sign` short-circuits with a wrapped `ErrCosign` before invoking the runner. This is load-bearing: a silently-empty bundle would land an unsigned sibling tag that looks signed.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/cosign/cosign.go` | `SignRequest`, `Author` reuse from publish, `SignResult`, `ErrCosign`, `Signer` interface, `DefaultAudience` / `DefaultFulcioURL` / `DefaultRekorURL`, `Validate`, `Sign`, `buildSiblingMessage`, `ParseSiblingMessage`, `isFullSHA`. |
| `package3/go/cosign/cosign_test.go` | 19 unit tests + `fakeSigner` + `recordingRunner` helpers. |
| `package3/go/cosign/phase13_test.go` | `TestPhase13CosignSiblingTagSentinel` end-to-end against a local bare-repo fixture (publish then cosign-sign then clone-and-verify). |
| `website/docs/implementation/0074/phase-13-cosign.md` | (this page) |

## Test set

- `TestPhase13CosignSiblingTagSentinel`
- 19 unit tests in `cosign_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/cosign/...
ok      mochi/package3/go/cosign        1.2s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/cosign        1.120s
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/library       (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/publish       (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 13 lands the cosign signer as a leaf module gated by the `Signer` interface. The production Sigstore client (live Fulcio + Rekor calls) is reserved for phase 13.1 once the bridge gains a runtime crypto dependency. The CLI wiring (`mochi pkg publish --to=go+git+<repo-url>@<tag> --cosign-sign`) is reserved for phase 13.2 alongside phase 12.1.

Future phase 13.x reservations:

- **13.1** Production `Signer` implementation against live Fulcio + Rekor (OIDC token exchange, certificate issuance, transparency-log inclusion proof).
- **13.2** CLI wiring + `--cosign-sign` flag + OIDC-token-from-env discovery (GitHub Actions, GitLab CI, Buildkite).
- **13.3** Verifier-side helper `mochi pkg verify --cosign-sign` (`git cat-file -p <tag>.sig` + `ParseSiblingMessage` + Sigstore verify).
- **13.4** Sibling-tag delete recovery: handle the edge case where a previous publish left a stale `<tag>.sig` that the signer needs to overwrite.

The goroutine bridge (phase 14) consumes `PublishResult.CommitSHA` like the cosign signer does but for a completely different purpose: phase 14 stitches the cgo handle pool into the wrapper layer so cross-tier channels and callbacks can survive a goroutine yield.
