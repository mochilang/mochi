---
title: "Risks and alternatives"
description: "16 risks (typosquatting, registry compromise, solver pathologies, capability creep, ...) and 12 rejected alternatives (Unison hash-as-name, git-based registry, Mochi-DSL manifest, monorepo-only, vendoring-only, MVS, SAT, ASP, binary lockfile, ...) and 7 v2 candidates."
sidebar_position: 12
---

# 12. Risks and alternatives

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/risks-and-alternatives`.

This note enumerates the risks MEP-57 incurs and the alternatives it rejects. It is the consolidation of the risk-of-the-week discussions from the other research notes.

## Part A. Risks

### A.1 Typosquatting

**Risk**: an attacker publishes `@mochi/strngs` (one letter off) or `lodahs` (visual lookalike) and harvests installs from typos.

**Mitigation**:
- Registry-side similarity check at publish time: reject if Damerau-Levenshtein distance to an existing high-traffic package is <= 1.
- Confusable character detection (CONFUSABLE Unicode property) at publish time.
- `mochi audit` flags installations of low-download-count packages with high-download-count look-alikes.

**Residual**: a sophisticated attacker who registers early before the high-traffic package exists, then waits, escapes the similarity check. Held for v2 community moderation.

### A.2 Registry compromise

**Risk**: an attacker gains write access to `index.mochi.dev` or `blobs.mochi.dev` and publishes a malicious version of a popular package.

**Mitigation**:
- Sigstore + OIDC publish auth ([09-trusted-publishing](./09-trusted-publishing)): a registry-side write cannot mint a valid Sigstore bundle bound to the package's registered publisher.
- Rekor transparency log: a retroactive blob substitution shows up as a Rekor inclusion proof mismatch.
- `mochi audit signatures` reverifies bundles against the cached publish event.
- Content-addressed blobs ([08-content-addressed-store](./08-content-addressed-store)): the URL is the hash, so a blob substitution requires a hash collision.

**Residual**: a long-term TUF root compromise (Sigstore root certificate substitution) defeats all mitigations. The TUF community has weathered several root rotations smoothly; we inherit that risk profile.

### A.3 Solver pathologies

**Risk**: a maliciously crafted dep graph forces the PubGrub solver into worst-case behaviour (exponential incompatibility growth).

**Mitigation**:
- Solver time budget: `mochi lock` aborts with `M057_SOLVER_E001` after 60s wall-clock.
- Bounded incompatibility list size; aborts on overflow.
- Fixtures from `pubgrub-rs` and uv stress tests in CI.

**Residual**: solver pathology against legitimate graphs (no adversary, just bad luck) shows up as a slow lock. v1 accepts; v2 may add caching of solved sub-graphs.

### A.4 Capability creep

**Risk**: maintainers add capabilities to their packages over time, conditioning consumers to accept "everything", defeating the audit signal.

**Mitigation**:
- Patch versions cannot add capabilities ([10-capability-model](./10-capability-model) §8: monotonicity policy).
- Minor versions adding capabilities require an explicit `mochi lock --accept-capabilities`.
- `mochi audit capabilities` shows the cumulative growth and flags packages with rapid capability growth.

**Residual**: consumers who reflexively run `--accept-capabilities` without auditing defeat the model. Mitigation is education and tooling (the audit command shows the diff).

### A.5 Sigstore root rotation

**Risk**: Sigstore's Fulcio root certificate is rotated; old bundles cannot be verified by clients with stale roots.

**Mitigation**:
- TUF-managed root rotation: Sigstore project handles the transition with overlapping validity.
- `mochi audit roots` refreshes the trust set.
- Bundle re-verification windows: `mochi audit signatures --since=<date>` only checks recent bundles by default.

**Residual**: a botched root rotation invalidates the entire publish history. Sigstore project has process maturity; we inherit it.

### A.6 Polyglot version drift

**Risk**: `mochi publish --target=npm` succeeds, `mochi publish --target=pypi` fails. The npm and PyPI versions now diverge: consumers see `1.5.0` on npm but `1.4.9` on PyPI.

**Mitigation**:
- `mochi publish --target=all --strict` aborts the whole fan-out at the first failure.
- Default mode reports the partial state; CI is expected to use `--strict`.
- `mochi audit cross-target` checks for version skew across registries.

**Residual**: a network partition during fan-out can leave divergence. CI re-runs are the recovery.

### A.7 Workspace lockfile contention

**Risk**: two members of a workspace conflict in their downstream requirements (member A requires `@dep ^1.0`, member B requires `@dep ^2.0`).

**Mitigation**:
- Solver detects and either picks a compatible version (if ranges intersect) or fails with a "two members require incompatible majors" error.
- `[workspace.allow-multi-version]` opt-in for genuine majorversion splits.

**Residual**: workspaces with deep cross-member conflicts may need member-by-member workarounds. Accepted complexity.

### A.8 Cache poisoning

**Risk**: an attacker writes to `~/.cache/mochi/registry/blobs/<hex>` directly (local malware) to substitute a dep before install.

**Mitigation**:
- Every cache read verifies the BLAKE3 against the lockfile pin.
- A mismatch hard-fails with `M057_BLOB_E001` (cache poisoning detected).
- The cache layout uses content addressing, so a poisoned blob cannot impersonate a legitimate hash (since `<hex>` *is* the hash).

**Residual**: a lockfile modified by the attacker (to point at a poisoned hash) is the higher-trust attack. Mitigated by `mochi audit signatures` re-verifying Sigstore bundles, which the attacker cannot forge without the publisher's OIDC identity.

### A.9 Network degradation

**Risk**: the registry is slow or unreachable; `mochi lock` hangs or fails.

**Mitigation**:
- Exponential backoff ([07-registry-index](./07-registry-index) §4.4) with bounded retries.
- ETag-based conditional fetch keeps warm-cache lookups cheap.
- `mochi lock --offline` resolves only against the cache.
- Mirrors via `[[registry.alternate]]` provide failover.

**Residual**: a global Cloudflare outage takes down `blobs.mochi.dev` for everyone. Offline mode is the workaround; mirrors are the longer-term answer.

### A.10 Mirror divergence

**Risk**: a mirror serves stale or modified content; the consumer's view diverges from the upstream truth.

**Mitigation**:
- Sigstore verification against bundles signed by the *upstream* publisher detects content modification regardless of mirror.
- Rekor transparency log verification catches mirrors that try to serve a fake bundle.
- `mochi audit mirror <name>` cross-checks a mirror's content against upstream.

**Residual**: a mirror that filters which versions it serves (denying access to a specific patch) is hard to detect automatically. v2 cross-mirror diff is the candidate.

### A.11 Publisher account takeover

**Risk**: an attacker compromises the GitHub account that owns the publisher binding, then publishes a malicious version.

**Mitigation**:
- Sigstore bundles are bound to the OIDC subject; if the attacker controls the GitHub account, they can produce valid bundles. The attack is real.
- `mochi audit signatures` cannot distinguish a legitimate publisher from a compromised one signing under the same identity.
- Detection signals: rapid capability growth (A.4), version-number anomalies, source-tree diff against upstream git tag.

**Residual**: account takeover is an out-of-band risk MEP-57 does not solve. The Sigstore model is "this signature came from this account", not "this account was uncompromised". GitHub's 2FA + hardware key requirements lower the rate; MEP-57 inherits.

### A.12 SBOM tooling immaturity

**Risk**: CycloneDX 1.6 and SPDX 3.0 tools are evolving; mismatched validators reject otherwise-valid SBOMs.

**Mitigation**:
- Emit both formats so any consumer's tool works.
- Use the SPDX-tools and CycloneDX-cli validators in CI.
- Pin emitter versions; bump deliberately.

**Residual**: a tool's bug in 2027 may flag valid 2026 SBOMs. Accepted ecosystem cost.

### A.13 Reproducibility violations

**Risk**: a non-deterministic compilation step produces different bytes on different machines, breaking lockfile reproducibility.

**Mitigation**:
- Phase 17 reproducible build gate verifies byte-identity across CI hosts.
- `SOURCE_DATE_EPOCH` and sorted-tar ensure deterministic tarball.
- The Mochi compiler itself is deterministic by Phase 17 ([implementation tracking](/docs/implementation/0057/phase-17-repro)).

**Residual**: a non-determinism introduced via FFI (e.g. a Python build invoking `pip` which downloads system-dependent wheels) is target-specific. Per-target reproducibility gates catch most; FFI-introduced non-determinism is documented and discouraged.

### A.14 Legal takedowns

**Risk**: a registered package is subject to DMCA, court order, or trademark dispute; we must remove it.

**Mitigation**:
- Tombstone the URL (`410 Gone`) rather than reuse the hash.
- Retain the Sigstore bundle audit trail.
- Document the takedown reason in the index entry's `removed_reason`.

**Residual**: a takedown breaks lockfiles that pin the removed version. The user must re-resolve.

### A.15 Maintainer burnout / abandonment

**Risk**: a popular package's maintainer disappears; security fixes do not ship; consumers cannot upgrade.

**Mitigation**:
- `mochi audit` surfaces packages with no updates for 12+ months in the resolved tree.
- Registry-side "successor" mechanism allows a new maintainer to take over with consumer notification (v2).

**Residual**: an abandoned package is an ecosystem-wide hazard MEP-57 can warn about but not fix.

### A.16 First-publication race

**Risk**: two different parties want to publish `@mochi/foo`; the first to register wins, possibly squatting.

**Mitigation**:
- Anti-squatting policy: registration of `@mochi/`-prefixed scopes is restricted.
- Unscoped names require ownership verification (repo URL must respond to a magic challenge).
- Trademark disputes resolved by registry admin.

**Residual**: ordinary scope squatting (`@goodactor/legit-name` registered first by a squatter) is a known ecosystem problem MEP-57 inherits.

## Part B. Rejected alternatives

### B.1 Unison-style content-addressed names

**Idea**: every function is referenced by the hash of its AST, not by a name. Two definitions with the same hash are the same function.

**Why considered**: hash-as-name eliminates whole classes of versioning bugs (no rebuilds needed when "the same code" moves, no dependency hell because hash-content collisions are impossible).

**Why rejected**:
- Human-readable names are a non-negotiable ergonomic requirement; Unison's design works best in a Unison-shaped editor environment, not general-purpose IDE use.
- Polyglot fan-out requires names compatible with downstream registries (npm, PyPI). Hashes do not map.
- The audit story is harder: a maintainer-published behaviour change to a function changes its hash, but the audit signal is the *name*, not the hash.
- Semver remains the user-facing version surface in v1 (in line with the consensus across surveyed systems).

The dual BLAKE3 + SHA-256 internal addressing is the *compromise*: content addressing for the storage layer, semver for the user surface. The Unison full stack is held for a hypothetical v2 candidate.

### B.2 Git-based registry index

**Idea**: a single git repo at `git://index.mochi.dev/index.git` is the registry index. Clients clone it.

**Why considered**: this is Cargo's legacy model. Familiar, well-tested.

**Why rejected**: Cargo's 2023 GA migration to sparse showed 5-20x speedups; the model is empirically obsolete ([02-design-philosophy](./02-design-philosophy) §4, [03-prior-art-registries](./03-prior-art-registries) §1). Mochi has no legacy to maintain, so we skip straight to sparse.

### B.3 Lua / Starlark / Mochi-DSL manifest

**Idea**: the manifest is executable code (Lua, Starlark, or Mochi itself). Allows computed configurations.

**Why considered**: Bazel's Starlark, Nix's Nix language, Pkl's Pkl, and JavaScript's `package.json` precedent of an executable manifest where users do put code (scripts).

**Why rejected**:
- Chicken-and-egg: a Mochi manifest cannot be Mochi-DSL because the compiler must parse it before fetching deps.
- Non-Mochi parsers (IDEs, editors, CI tooling) cannot evaluate a Mochi-DSL manifest without shipping a Mochi interpreter.
- Configuration-as-code complicates audit (a manifest's content depends on its evaluation environment, not its bytes).

TOML preserves the "configuration is bytes" invariant.

### B.4 Monorepo-only

**Idea**: do not ship a package manager. Require every Mochi project to vendor its deps in a monorepo.

**Why considered**: Google's monorepo model works at Google scale; some users prefer it for trust reasons.

**Why rejected**: third-party reuse is fundamentally about *not* vendoring. The package manager is a precondition for ecosystem growth. Monorepo / vendoring is supported via `mochi vendor` for users who want it; it is not the default.

### B.5 Vendoring-only with signed tarballs

**Idea**: skip the registry entirely. Every dep is referenced by a signed tarball URL. The lockfile records URL + hash + signature.

**Why considered**: simpler architecture; no central index. Go modules' "publish by tagging a git commit" is close to this.

**Why rejected**:
- Discovery is broken: no way to find what packages exist.
- Yanking is impossible (the URL is the contract; the publisher cannot withdraw).
- Capability declarations need a metadata endpoint; tarballs alone do not surface them at solve time without download.

MEP-57's central index is the metadata layer; the content-addressed blob store is the artifact layer. Both are needed.

### B.6 MVS (Minimum Version Selection) solver

**Idea**: copy Go's MVS algorithm. Deterministic, fast, no SAT.

**Why considered**: MVS is simple and works at Google scale.

**Why rejected**:
- Mochi's polyglot fan-out crosses ecosystems with incompatible semver semantics (npm `0.x` allows breaks; Cargo's `^0.5` is strict). MVS assumes uniform semver discipline.
- MVS's "always pick the minimum compatible version" runs against ecosystem expectations (users expect upgrades to happen automatically within ranges).
- The explanation quality is poor: MVS does not record incompatibilities.

PubGrub wins on all three counts ([02-design-philosophy](./02-design-philosophy) §2).

### B.7 SAT solver

**Idea**: classic SAT, npm-style.

**Why considered**: well-understood algorithm, mature solvers (MiniSAT, Glucose).

**Why rejected**: weak explanations ([02-design-philosophy](./02-design-philosophy) §2, [05-solver-design](./05-solver-design) §1). The 2025 IEEE Software empirical study ranked SAT worst for user-comprehension of failures.

### B.8 ASP / Clingo

**Idea**: Spack's model. Express constraints as Answer Set Programming.

**Why considered**: Most powerful expressiveness; can handle hardware-aware constraints.

**Why rejected**: Clingo is a 5MB+ runtime dep. Mochi's CLI is a single static binary. ASP's expressive power is overkill for semver resolution.

### B.9 Binary lockfile

**Idea**: pack the resolved tree as a binary structure for fast load.

**Why considered**: Bun shipped `bun.lockb` 2022-2025 for performance.

**Why rejected**: Bun's reversal in 2025 documents every reason ([02-design-philosophy](./02-design-philosophy) §3). Diff impossibility, merge conflict pain, secret scanning blindness, code review friction. Text wins.

### B.10 Long-lived API tokens for publish

**Idea**: maintainers store a long-lived API token in CI env vars; publish authenticates with the token.

**Why considered**: every existing registry (npm pre-2024, Cargo pre-2025, Hex, NuGet) supports this. Familiar.

**Why rejected**: every major supply-chain incident 2018-2026 traces to a compromised long-lived token ([02-design-philosophy](./02-design-philosophy) §5). Sigstore + OIDC eliminates the long-lived secret entirely.

### B.11 Open capability vocabulary

**Idea**: packages declare capabilities as free-form strings; consumers audit by reading the source.

**Why considered**: more flexibility; no vocabulary fights.

**Why rejected**: fragmented capability vocabularies are unauditable. Every modern capability-aware system (Deno, Pony, WASI Component Model) uses a closed set; the audit surface needs a stable vocabulary ([10-capability-model](./10-capability-model) §1).

### B.12 No capability declarations

**Idea**: ship the package system without any capability model in v1; add later.

**Why considered**: shipping faster, deferring complexity.

**Why rejected**: the xz-utils attack pattern (CVE-2024-3094) makes capability-delta detection a primary supply-chain defence. Skipping the model in v1 means consumers cannot detect maintainer-compromise-induced capability growth. The audit-only stance (no enforcement) is the right v1 compromise: low cost to ship, high audit value.

## Part C. Deferred to v2

### C.1 Federated discovery (no single central index)

**Idea**: decentralised registry where each org runs its own index, and a discovery protocol federates them.

**Why deferred**: v1 needs a single source of truth for the publisher binding policy. Federation makes Sigstore-bound trust policies more complex. Held for v2 once the v1 ergonomics are validated.

### C.2 Transparency log for the index itself

**Idea**: a `sum.mochi.dev` Go-style checksum DB that any client can verify the index against.

**Why deferred**: Rekor already provides per-publish transparency; an additional index-level log is incremental hardening. Held for v2.

### C.3 Cargo-vet-style audit attestations

**Idea**: third-party security audits become first-class attestations attached to specific (name, version) pairs.

**Why deferred**: requires a community of auditors and a registry-side attestation surface. v2 candidate.

### C.4 Effect system at the module boundary

**Idea**: a Mochi-level effect system that statically tracks capability usage and proves the manifest declaration matches the implementation.

**Why deferred**: a substantial language change. Tracked as a future MEP candidate ([10-capability-model](./10-capability-model) §2.1).

### C.5 Path-scoped and host-scoped capabilities

**Idea**: `fs.read.path:/etc`, `net.dial.host:api.example.com` for finer-grained policy.

**Why deferred**: vocabulary explosion vs benefit not yet justified by Mochi's use cases. Held for v2 measurement.

### C.6 Inline-script dependencies (PEP 723-style)

**Idea**: a `# /// script` block at the top of a single `.mochi` file declares its deps for `mochi run`.

**Why deferred**: v1 requires a directory with a manifest. Inline-script form is a v2 candidate.

### C.7 Wasm Component Model integration

**Idea**: capabilities map to component-model imports; the Wasm target is the canonical capability enforcement point.

**Why deferred**: depends on MEP-55 (Wasm target) landing first.

## Cross-references

- Each risk's positive flip-side: see the corresponding research note for the design choice.
- Comparable systems' positions on each alternative: [03-prior-art-registries](./03-prior-art-registries).
- Why each accepted decision was preferred: [02-design-philosophy](./02-design-philosophy).
