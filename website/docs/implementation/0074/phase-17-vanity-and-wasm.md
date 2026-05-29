---
title: "Phase 17. Vanity-import resolver + WASI publish gate"
sidebar_position: 19
sidebar_label: "Phase 17. Vanity-import + WASI publish"
description: "MEP-74 Phase 17 lands the final two consume + publish-direction bits: a vanity-import resolver that fetches the `<meta name=\"go-import\">` HTML redirect tag the Go ecosystem uses to delegate non-VCS-rooted import paths (golang.org/x/*, gopkg.in/*, k8s.io/*) to a real git repo, plus a wasm-wasip1 / wasm-js publish gate that walks an ApiSurface and reports every import that would break on the target wasm runtime."
---

# Phase 17. Vanity-import resolver + WASI publish gate

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-74 §Phases](/docs/mep/mep-0074#phases) |
| Status         | LANDED (baseline) |
| Started        | 2026-05-30 00:50 (GMT+7) |
| Landed         | 2026-05-30 00:58 (GMT+7) |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase17VanityResolverSentinel` in `package3/go/vanity/phase17_test.go` stands up an in-process `httptest.NewTLSServer`, serves a `<meta name="go-import" content="vanity.test/x/sync git https://repo.test/sync">` redirect on the `/x/sync?go-get=1` URL, points an `HTTPFetcher` at the test server via a `rewriteTransport` (so the production-shaped `https://vanity.test/x/sync?go-get=1` request hits the local server without DNS), and asserts the resolved `Resolution{Module, VCS, RepoURL}` triple matches the served meta tag. This is the closest reproduction of the live golang.org/x/* path the unit-test suite can stage without making a real network call.

`TestPhase17VanityResolverServer404Surfaces` confirms the resolver wraps a non-2xx status in `ErrVanity` (with the status code in the error message) rather than masking it as a "no go-import meta" parse failure.

`TestPhase17WasmPublishGateRejectsCgoSurface` walks a hand-curated surface that imports both `runtime/cgo` and `syscall/js`, then asserts wasm-wasip1 reports both as violations and wasm-js reports only the cgo one (since wasm-js does have a JS host).

`TestPhase17WasmPublishGatePassesPureSurface` walks a pure-go surface (no cgo, no syscall/js) and asserts both wasm targets accept it cleanly.

Plus 24 unit tests in `vanity_test.go`:

- vanity predicate (`TestIsVanity`, `TestKnownVCSHostsIsSorted`),
- direct + vanity resolver (`TestResolveDirectGithub`, `TestResolveDirectGithubSubpackage`, `TestResolveEmptyPath`, `TestResolveVanityNoFetcher`, `TestResolveVanityViaMapFetcher`, `TestResolveVanityLongestPrefixWins`, `TestResolveVanityRejectsNoMeta`, `TestResolveVanityRejectsNonCoveringMeta`),
- HTML meta-tag scanner (`TestParseGoImportSingleQuoted`, `TestParseGoImportAttrOrderIndependent`, `TestParseGoImportIgnoresMalformedContent`, `TestParseGoImportIgnoresOtherMetas`, `TestExtractAttrUnquoted`),
- in-memory Fetcher (`TestMapFetcherMiss`),
- wasm publish gate (`TestWasmTargetIsValid`, `TestCheckPublishWasip1BansSyscallJS`, `TestCheckPublishJSAllowsSyscallJS`, `TestCheckPublishBansCgo`, `TestCheckPublishBansDebugFamily`, `TestCheckPublishInvalidTargetIsNoop`, `TestIsPublishable`, `TestViolationString`).

## Lowering decisions

The vanity package is a near-leaf module: it imports `package3/go/apisurface` for the wasm publish-gate's surface walk, plus stdlib `net/http`, `io`, `sort`, `strings`. Two distinct concerns share the same directory because they share the same delivery phase (consume-direction vanity + publish-direction wasm gate are both spec'd as phase 17), but the package boundary keeps them as separate top-level surfaces (`Resolve(...)` / `IsVanity(...)` vs `CheckPublish(...)` / `IsPublishable(...)`).

**Vanity detection is a banned-host-set check, not an allow-list.** The Go ecosystem's vanity convention works by delegation: any import path whose first segment is NOT a known VCS host (github.com / gitlab.com / bitbucket.org) is assumed to delegate via the meta tag. Phase 17 hardcodes the closed VCS-host set as `KnownVCSHosts`; everything else is a vanity path. The reason for banned-host rather than allow-list is the same as the tinygo subset's banned-import: the vanity universe is open-ended (any user-controlled domain), but the direct-VCS universe is small and well-known.

**Direct paths skip the fetch.** When a path resolves directly (`github.com/spf13/cobra`), `Resolve` synthesises a `Resolution{Module, VCS: "git", RepoURL: "https://" + module-root}` without making any HTTP call. The module root is the first three segments (host + owner + repo) for hosted-git providers; deeper paths like `github.com/spf13/cobra/internal/util` resolve to the same `github.com/spf13/cobra` module root. This matches the canonical Go module-resolution algorithm and lets the phase 1 proxy client treat both vanity and direct paths through a single Resolution interface.

**Longest-prefix wins on meta-tag matching.** When the fetched HTML body has multiple `<meta name="go-import">` tags (the `golang.org` root vs `golang.org/x/sync` subroot pattern), `ParseGoImport` picks the one whose Module is the longest prefix of `wantPrefix`. The check is `t.Module == wantPrefix || strings.HasPrefix(wantPrefix+"/", t.Module+"/")` so a tag for `example.com/x/sync` covers `example.com/x/sync/internal` (a sub-path) but a tag for `example.com` does NOT cover `example.com/x/sync` if a more-specific tag is also present. This matches the upstream `cmd/go` resolver's behaviour and avoids the "fallback tag eats a more-specific tag" foot-gun.

**The HTML scanner is tag-level, not a full HTML parser.** The Go ecosystem's vanity meta tags are deliberately simple: `<meta name="go-import" content="<root> <vcs> <repo>">` with single or double-quoted values, optional whitespace, free attribute order. A full `golang.org/x/net/html` parser would be heavier and pull in another dep; the tag-level scanner is enough for the fixed grammar. Malformed tags (wrong field count, missing attribute, unquoted value) are silently dropped, mirroring the cmd/go resolver's tolerance.

**The Fetcher interface is the hermetic seam.** `Resolve` takes a `Fetcher` parameter so the unit tests pass a `MapFetcher` (in-memory map) and the production wiring passes an `HTTPFetcher` (real `net/http`-backed). The sentinel uses `httptest.NewTLSServer` plus a `rewriteTransport` to exercise the full HTTP roundtrip path without leaving the test process. The interface is one method (`Fetch(url string) (string, error)`) so a future custom transport (offline cache, vendored mirror, signing-aware fetcher) drops in without touching the resolver.

**Wasm publish gate is per-target.** wasm-wasip1 and wasm-js share most banned imports (`runtime/cgo` is universal because wasm has no working cgo runtime; `debug/elf`, `debug/macho`, `debug/pe` are universal because they're toolchain-coupled; `os/exec` is universal because wasm has no exec; `plugin` is universal because wasm has no plugin loader). Wasm-wasip1 additionally bans `syscall/js` (no JS host); wasm-js does not. The split lives in `wasmBanned(t)` so adding a third wasm target later (e.g., wasm-baremetal) is a one-line case-add.

**Violations are sorted, kind-prefixed, deterministic.** Same shape as the phase 16 tinygo gate: `(Kind, Where)` sort via `sort.SliceStable`, so the wrapper-synthesiser can fold both gates' output into a single deterministic SkipReport stream for the phase 10 wrapper-sha256 pin.

**No live wazero smoke run yet.** A real wazero-host smoke (instantiate the published wasm module, call one export, assert the call succeeds) is phase 17.1: it requires a wazero dependency, a Mochi-side test that emits a wasm module, and a CI runner that can host the wazero instance. The current gate is import-level only; the wazero validation is the next-step verification.

**No live golang.org/x/* fetch in CI.** The sentinel uses an in-process `httptest.NewTLSServer` and rewrites the URL via a custom `RoundTripper` rather than fetching live vanity hosts. The reasons: CI flakiness, network-dependency in the test suite, and the brittleness of pinning a specific meta-tag wording from an upstream host the bridge does not control. Phase 17.2 will add an opt-in `--vanity-live` build tag that exercises a curated set of stable vanity hosts (`golang.org/x/sync`, `gopkg.in/yaml.v3`, `google.golang.org/protobuf`) once a CI runner with reliable outbound HTTPS lands.

## Files changed

| File | Purpose |
|------|---------|
| `package3/go/vanity/vanity.go` | `ErrVanity`, `KnownVCSHosts`, `Resolution`, `IsVanity`, `Fetcher` interface, `HTTPFetcher`, `MapFetcher`, `Resolve`, `ParseGoImport`, internal tag scanner; plus `WasmTarget`, `Violation`, `CheckPublish`, `IsPublishable`. |
| `package3/go/vanity/vanity_test.go` | 24 unit tests covering vanity predicate, direct + meta-redirect resolver, HTML scanner edge cases, wasm publish gate per-target. |
| `package3/go/vanity/phase17_test.go` | `TestPhase17VanityResolverSentinel` (full HTTP roundtrip via httptest.TLSServer + rewriteTransport), `TestPhase17VanityResolverServer404Surfaces`, `TestPhase17WasmPublishGateRejectsCgoSurface`, `TestPhase17WasmPublishGatePassesPureSurface`. |
| `website/docs/implementation/0074/phase-17-vanity-and-wasm.md` | (this page) |

## Test set

- `TestPhase17VanityResolverSentinel`
- `TestPhase17VanityResolverServer404Surfaces`
- `TestPhase17WasmPublishGateRejectsCgoSurface`
- `TestPhase17WasmPublishGatePassesPureSurface`
- 24 unit tests in `vanity_test.go`

Local run on darwin-arm64:

```
$ go test ./package3/go/vanity/...
ok      mochi/package3/go/vanity        0.330s
$ go test ./package3/go/...
ok      mochi/package3/go/apisurface    (cached)
ok      mochi/package3/go/build (cached)
ok      mochi/package3/go/cmd/go-ingest (cached)
ok      mochi/package3/go/cosign        (cached)
ok      mochi/package3/go/emit  (cached)
ok      mochi/package3/go/errors        (cached)
ok      mochi/package3/go/goroutine     (cached)
ok      mochi/package3/go/library       (cached)
ok      mochi/package3/go/lockfile      (cached)
ok      mochi/package3/go/moduleproxy   (cached)
ok      mochi/package3/go/monomorphise  (cached)
ok      mochi/package3/go/publish       (cached)
ok      mochi/package3/go/semver        (cached)
ok      mochi/package3/go/sumdb (cached)
ok      mochi/package3/go/tinygo        (cached)
ok      mochi/package3/go/typemap       (cached)
ok      mochi/package3/go/vanity        0.197s
ok      mochi/package3/go/wrapper       (cached)
```

## Closeout notes

Phase 17 is the final umbrella phase of MEP-74. With this landing, every phase 0-17 has its baseline implementation and test gate green. The remaining work is per-phase sub-phase integration (the N.1+ deferred slots tracked in the phase 0-17 closeouts):

- consumed-direction wrapper-synth integration (phase 6.1+) that wires `tinygo.CheckPackage`, `tinygo.RenderFile`, `monomorphise.Resolve`, `monomorphise.RenderInstance`, `goroutine.NeedsRuntime`, `goroutine.RenderRuntime`, and the channel + callback shim renderers into the per-module wrapper output;
- publish-direction MEP-54 driver integration (phase 11.1+) that calls `library.Emit`, `publish.Publish`, `cosign.Sign`, and the new `vanity.CheckPublish` per target;
- mochi.lock CLI surface (phase 10.1+) that pins per-instance wrapper-sha256 + per-publish target-spec via the manifest tables;
- live-target integration gates (phase 14.5 live cgo, phase 17.1 live wazero, phase 17.2 live vanity fetch) that move the in-process sentinels onto real runtimes.

Future phase 17.x reservations:

- **17.1** wazero host smoke: instantiate the published wasm module via `embedded "github.com/tetratelabs/wazero"`, call one export, assert the round-trip succeeds.
- **17.2** Live vanity fetch via opt-in build tag (`-tags=mochi_vanity_live`) against a curated stable host set.
- **17.3** Per-mochi.toml vanity override table (`[go.vanity.<path>] vcs = "git" repo = "..."`) for offline / vendored vanity bypass.
- **17.4** Negative-cache for failed vanity lookups (so a CI run that hits one bad vanity host does not retry on every subsequent build).
- **17.5** wasm-baremetal target case in `wasmBanned`.

The MEP-74 phase plan is complete; subsequent work moves under the per-phase sub-phase closeouts and the wider MEP-57 polyglot package work that depends on MEP-74's surfaces.
