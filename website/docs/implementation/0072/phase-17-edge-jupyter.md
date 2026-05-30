---
title: "Phase 17. Edge + Jupyter consume"
sidebar_position: 19
sidebar_label: "Phase 17. Edge + Jupyter"
description: "MEP-72 Phase 17: edge-runtime consume-side gate (Cloudflare Workers + Vercel Edge + Deno Deploy) + Deno Jupyter kernel consume + browser-bundle final gate. The umbrella phase that ties the per-runtime gates to the lockfile."
---

# Phase 17. Edge + Jupyter consume

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-72 §Phases](/docs/mep/mep-0072#phases) |
| Status         | NOT STARTED |
| Tracking issue | (pending) |
| Tracking PR    | (pending) |
| Commit         | (pending) |

## Gate

`TestPhase17EdgeJupyter` in `package3/typescript/edge/phase17_test.go`: subtests `edge_workerd_gate`, `edge_vercel_gate`, `edge_deno_deploy_gate`, `jupyter_consume`, `browser_bundle_size_canary`, `bundle_capabilities_match`, `golden_corpus`. The first three runs the consume-side bridge against three edge-runtime fixtures (`hello-cloudflare-workers/`, `hello-vercel-edge/`, `hello-deno-deploy/`); each fixture imports a small npm or JSR package, runs through the respective edge bundler, and asserts the bundle deploys cleanly to a fixture-mode deploy target. The fourth runs the Deno Jupyter kernel consume path against `jupyter-zod/`: a Mochi cell imports `npm:zod` and runs a parse via the Deno Jupyter kernel; the gate asserts the cell output matches the golden. The fifth re-runs the MEP-52 Phase 17 `TestPhase17BundleSize` canary (50 KB hello-world budget) but now with the `[ts.publish]` browser-bundle-budget override. The sixth asserts every consumed package's `capabilities-declared` matches the runtime's allow-list (e.g., a Cloudflare Workers target rejects packages with `fs` in capabilities). The seventh runs the 24-package fixture corpus + the 6-package browser sub-corpus.

## Lowering decisions

The phase ties together three already-built pieces:

1. **Edge-runtime allow-list** (from Research Note 10 §5): `fetch`, `Request`, `Response`, `Headers`, `crypto.subtle`, `WebSocket` are allowed; `node:fs`, `node:net`, `node:child_process`, `node:worker_threads` are refused; platform-specific globals (`caches` on Cloudflare, `geolocation` on Vercel, `Deno.serve` on Deno Deploy) are allowed per target.

2. **Jupyter kernel consume**: the Deno Jupyter kernel (MEP-52 phase 17.6 already ships it) loads Mochi cells. The MEP-72 contribution is that `import ts "npm:zod" as z` inside a Jupyter cell now resolves to the same bridge-emitted shim as a non-Jupyter Mochi source.

3. **Browser-bundle final gate**: the MEP-52 Phase 17 `TestPhase17BundleSize` is extended with the MEP-72 `[ts.publish]` per-package budget override.

The edge-runtime gate:

```go
func ValidateEdgeTarget(lockfile *Lockfile, target string) error {
    allowed := allowedCapabilities[target]
    for _, pkg := range lockfile.NpmPackages {
        for _, cap := range pkg.CapabilitiesDeclared {
            if !slices.Contains(allowed, cap) {
                return BridgeError{Phase: "edge-gate", Package: pkg.Name,
                    Cause: fmt.Errorf("capability %q not allowed on %q", cap, target)}
            }
        }
    }
    return nil
}
```

The Jupyter consume:

```go
func PrepareJupyterCell(source string, lockfile *Lockfile) (string, error) {
    // 1. Run the MEP-52 phase 17.6 Jupyter parser against source.
    // 2. Resolve any `import ts "..." as <alias>` against the lockfile.
    // 3. Synthesise a deno.json with the right import-map.
    // 4. Hand to the Deno Jupyter kernel.
}
```

The browser-bundle final gate:

```go
func ValidateBrowserBundle(bundlePath string, budget int64) error {
    info, err := os.Stat(bundlePath)
    if err != nil { return err }
    if info.Size() > budget {
        return BridgeError{Phase: "browser-bundle", Cause:
            fmt.Errorf("bundle size %d exceeds budget %d", info.Size(), budget)}
    }
    return nil
}
```

The phase is the umbrella for the consume-side gates against the constrained runtimes. It does NOT add new emission code; it ties the existing per-runtime pieces together via the lockfile's `capabilities-declared` field.

## Files changed

| File | Purpose |
|------|---------|
| `package3/typescript/edge/allowed.go` | per-edge-target capability allow-list |
| `package3/typescript/edge/validate.go` | `ValidateEdgeTarget` |
| `package3/typescript/edge/jupyter.go` | `PrepareJupyterCell` |
| `package3/typescript/browser/validate_size.go` | `ValidateBrowserBundle` |
| `package3/typescript/edge/phase17_test.go` | `TestPhase17EdgeJupyter` sentinel |
| `package3/typescript/edge/testdata/fixtures/*` | edge + Jupyter end-to-end fixtures |

## Test set

7 subtests as listed in the Gate section.

## Cross-references

- [Research note 10 §5 Edge](/docs/research/0072/10-runtime-target-matrix#5-edge-cloudflare-workers-vercel-edge-deno-deploy) — the edge-target capability model.
- [Research note 11 Browser bundle surface](/docs/research/0072/11-browser-bundle-surface) — the browser-bundle gate.
- [MEP-52 phase 17 JSR + Jupyter + browser](/docs/implementation/0052/phase-17-jsr-jupyter-browser) — the underlying Phase 17 targets MEP-72 builds on.
