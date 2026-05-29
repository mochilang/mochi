---
title: "07. Sigstore and Go checksum DB"
sidebar_position: 8
sidebar_label: "07. Sigstore + sum.golang.org"
description: "The sum.golang.org transparency log structure, the Merkle-tree consistency proof protocol, the gosum-cosign workflow draft of 2026-Q1, the optional <tag>.sig sibling tag, the verification path at consume time, and the design trade-off vs MEP-73's Sigstore-keyless-mandatory stance."
---

# 07. Sigstore and Go checksum DB

This note details the supply-chain story: how the bridge verifies imported Go modules against the public checksum DB at consume time, and how the bridge optionally signs published modules with cosign for downstream verification.

## sum.golang.org as a transparency log

The Go team operates two complementary services at the heart of the Go module supply chain:

- **`proxy.golang.org`**: a caching mirror of every public Go module. Fetches modules from their canonical-import-path's git remote on first request, caches the .zip, serves all subsequent requests from cache.

- **`sum.golang.org`**: a Merkle-tree transparency log of every public module version. The log records `(module, version, h1:hash)` triples; the tree head is signed by a Go-team-controlled key; the log is append-only and consistency-proven.

Every `go get` of a public module since Go 1.13 (September 2019) verifies the downloaded module against sum.golang.org by default. The verification:

1. Download the module .zip from the proxy.
2. Compute `h1:<base64(sha256(zip-bytes))>`.
3. Fetch `https://sum.golang.org/lookup/<module>@<version>`. This returns the canonical record body plus the tree's signed head at lookup time.
4. Assert the bridge's computed `h1:` equals the record's `h1:`.
5. Fetch the Merkle tile path proving the record is included in the signed tree.
6. Verify the tree head signature against the Go team's hard-coded public key.

If any step fails, `go get` aborts. This is a hard supply-chain gate, on by default.

## How MEP-74 integrates with sum.golang.org

MEP-74 makes this verification a hard requirement of every `mochi pkg lock` for public modules. The implementation:

```go
// package3/go/sumdb/verify.go (sketch)

type SumdbClient struct {
    BaseURL   string // default "https://sum.golang.org"
    PubKey    ed25519.PublicKey
}

func (c *SumdbClient) Verify(module, version string, computedH1 string) (record SumdbRecord, err error) {
    // 1. Lookup
    resp, err := http.Get(c.BaseURL + "/lookup/" + module + "@" + version)
    // ...
    // 2. Parse record body: "<module> <version> h1:<base64>\n"
    rec := parseRecord(body)
    if rec.H1 != computedH1 {
        return rec, fmt.Errorf("h1 mismatch: expected %s, got %s", rec.H1, computedH1)
    }
    // 3. Verify signed tree head
    if !ed25519.Verify(c.PubKey, rec.TreeHead.Bytes(), rec.TreeHead.Signature) {
        return rec, errors.New("sumdb signature invalid")
    }
    // 4. Fetch Merkle tile path and verify inclusion
    tiles, err := c.FetchTiles(rec.RecordIndex, rec.TreeHead.Size)
    if !inclusionProof(rec, tiles, rec.TreeHead.Hash) {
        return rec, errors.New("sumdb inclusion proof invalid")
    }
    return rec, nil
}
```

The hard-coded public key is the one the Go team publishes at https://sum.golang.org/latest. Mochi pins the key at bridge compile time; key rotation requires a new bridge release.

The verification result (`sumdb-record-hash`, `sumdb-tree-size`) is recorded in `mochi.lock`'s `[[go-package]]` entry. A later `--check` can request a Merkle consistency proof from sum.golang.org showing that the lock-time leaf is still in the current tree.

## Private-module opt-out

Not every module is public-resolvable. Corporate modules at `corp.example.com/internal/foo` legitimately do not appear in sum.golang.org. The bridge supports per-glob opt-out:

```toml
[go.private]
modules = ["corp.example.com/**"]
sumdb-skip = ["corp.example.com/**"]
```

This mirrors the `GOPRIVATE` / `GONOSUMCHECK` Go-side env vars. Modules matching `sumdb-skip` patterns:

- Skip the sum.golang.org cross-check.
- Record `sumdb-verified = false` in `mochi.lock` (audit trail).
- Still verify the BLAKE3-256 of the downloaded .zip against the recorded hash (the bridge's primary integrity check).

The opt-out is per-glob, not global: a project mixing public and private modules verifies the public ones strictly and skips the private ones.

## The gosum-cosign workflow draft

The Go team has not committed to a canonical publish-side signing format as of 2026-05. The closest community draft is the **gosum-cosign workflow** (originated in a discussions post on golang-tools-dev in 2026-Q1):

- After tagging a release `v1.2.3`, the publisher signs the tag's commit SHA with cosign:
  ```
  cosign sign-blob --identity-token <oidc-jwt> --bundle v1.2.3.sig <commit-sha-as-file>
  ```
- The signature bundle is attached as a sibling annotated git tag `v1.2.3.sig`.
- Downstream consumers can fetch the sibling tag and verify against Sigstore's transparency log (Rekor).

MEP-74 supports this workflow under the `--cosign-sign` flag on `mochi pkg publish`. The flag is opt-in for v1 because:

1. The workflow is a draft, not a standard. The Go team may pick a different format.
2. Consumers must know to look for the `<tag>.sig` sibling tag; no Go-stdlib tooling does this yet.
3. Sigstore's trust model is parallel to sum.golang.org's, not a substitute. Both can coexist.

The bridge re-evaluates the default when the Go team publishes a canonical signing spec.

## Comparison with MEP-73's Sigstore-mandatory stance

MEP-73 §02-5 makes Sigstore-keyless OIDC mandatory for crates.io publish. MEP-74 does not. The asymmetry is justified:

- crates.io has a registry POST endpoint that accepts an OIDC token. MEP-73 can require it.
- Go has no central upload endpoint. The publish operation is `git push`. The bridge cannot require the user's git remote to verify a signature it does not understand.
- sum.golang.org provides a separate, well-established transparency log that catches the same class of attacks (post-publish module rewriting). MEP-74 cross-checks against it on every lock.

The net supply-chain guarantee:

| Threat | MEP-73 (Rust) defence | MEP-74 (Go) defence |
|--------|------------------------|----------------------|
| Compromised long-lived publish token | Sigstore-keyless OIDC, no long-lived token | git push auth (user's existing) |
| Post-publish module rewriting | Sigstore Rekor log + Mochi lockfile pin | sum.golang.org Merkle log + Mochi lockfile pin |
| Compromised registry mirror | Sigstore Fulcio cert + Rekor inclusion | sum.golang.org `h1:` cross-check |
| Compromised consumer's local clock | Sigstore RFC3161 timestamp | sum.golang.org tree-head timestamp |
| Source repo rewritten history | Sigstore commit-attestation | git pseudo-version + go.sum |

The defences are different but the coverage is comparable. MEP-74's coverage is arguably stronger on the post-publish-rewriting axis (the Go log has been operational since 2019 with universal adoption; Sigstore Rekor is younger and not universal).

## Operational considerations

- **Latency.** The sum.golang.org lookup adds one HTTP RTT per dep at lock time. With 30 deps, the cumulative cost is ~3-6 seconds on a normal connection. The bridge runs the lookups in parallel via a worker pool of 8.
- **Availability.** sum.golang.org has a public uptime SLA from the Go team. The bridge does not provide a fallback log; if sum.golang.org is unreachable, `mochi pkg lock` fails (with a clear diagnostic suggesting `[go.private] sumdb-skip` as the workaround for the affected module). A `--sumdb-offline` flag exists but is recommended only for air-gapped builds.
- **Geographic latency.** sum.golang.org is CDN-fronted; latency from any continent is &lt; 300ms.

## Cross-references

- [[02-design-philosophy]] §5 for why the cross-check is mandatory by default.
- [[06-go-module-publish-flow]] §5 for the publish-side cosign integration.
- [[12-risks-and-alternatives]] §R5 for the single-signing-key risk.
- [The Go sumdb spec](https://go.dev/ref/mod#authenticating) for the canonical protocol documentation.
- [MEP-73 §07](/docs/research/0073/07-sigstore-cargo-rfc3724) for the sister Rust-bridge approach.
