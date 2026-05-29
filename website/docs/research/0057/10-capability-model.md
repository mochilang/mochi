---
title: "Capability model: closed set, manifest-declared, target-enforced"
description: "Nine closed-set capability identifiers, declaration syntax, consumer pinning, lockfile capabilities-seen annotation, per-target enforcement, comparable systems, xz-utils pattern check, capability monotonicity, tooling, failure modes."
sidebar_position: 10
---

# 10. Capability model: closed set, manifest-declared, target-enforced

**Status**: research note. **Date**: 2026-05-29 (GMT+7).
**Mirrors**: deployed to `/docs/research/0057/capability-model`.

This note specifies the package-boundary capability model. The "why capabilities" rationale is in [02-design-philosophy](./02-design-philosophy) §6; comparable systems are surveyed below.

## 1. The closed capability set

Mochi-57 ships with exactly nine capability identifiers in v1:

| Identifier      | Meaning                                                    |
|-----------------|------------------------------------------------------------|
| `fs.read`       | Read files (any path)                                      |
| `fs.write`      | Create / modify / delete files                             |
| `net.dial`      | Outbound network connections (TCP, UDP, HTTPS clients)     |
| `net.listen`    | Inbound network sockets (servers, listeners)               |
| `env`           | Read / write process environment variables                 |
| `ffi`           | Call into target-language FFI (Go, Python, JS, JVM, ...)   |
| `clock`         | Read the wall clock / monotonic clock                      |
| `random`        | Read non-deterministic randomness (OS RNG)                 |
| `proc.spawn`    | Start subprocesses                                         |

Closed set: extension requires a MEP. Open extension is rejected for the same reasons Deno's permissions, Pony's reference capabilities, and Wasm WASI's interface set are closed: a fragmented vocabulary is unauditable.

### 1.1 Capabilities specifically not included v1

- `fs.read.path`, `net.dial.host`: scoped capabilities are tempting but explode the vocabulary. Held for v2 after measuring real usage.
- `crypto`: cryptographic primitives are pure in v1 (no key material is a capability). Held for v2 when secure-enclave APIs land.
- `gpu`, `audio`, `display`: target-specific concerns deferred to target MEPs (Swift MEP-49 for iOS, Kotlin MEP-50 for Android).
- `time.realtime`: covered by `clock` in v1; specialised real-time capability deferred.
- `db`, `http.server`, `kafka`: library-level concerns, not OS-level. Stay in the library API surface, not the capability vocabulary.

The v1 set is the minimum that catches the supply-chain delta classes documented by NodeShield's CBOM paper (2025): network access added, file write added, subprocess spawn added.

## 2. Declaring capabilities

A package declares the set of capabilities it requires in `mochi.toml`:

```toml
[capabilities]
required = ["fs.read", "net.dial"]
optional = ["proc.spawn"]
```

- `required` capabilities must be available at runtime; their absence is a startup error.
- `optional` capabilities are documented but not asserted; their absence triggers a `capability_missing` runtime fault if the dep tries to use one.

Declaration is per-package, not per-module. A package author cannot say "module X uses fs.read but module Y does not"; the capability is asserted at the package boundary. This matches Roc's platform model and Pony's package-level capability annotations.

### 2.1 Capability inference is out of scope

Static derivation of capabilities (scan the source for `import std/fs` and infer `fs.read`) is *not* part of MEP-57. Reasons:

- Requires a Mochi-level effect system, which is a separate MEP candidate.
- Cannot detect FFI-introduced effects without target-specific analysis.
- Conservatism would over-declare; under-declaration would be unsound.

Instead, the declared set is a publisher promise. `mochi lint capabilities` is a future linter (deferred) that detects obvious gaps; v1 is publisher discipline + consumer audit.

## 3. Consumer-side pinning

A consumer can pin the capability subset a dep is permitted to use:

```toml
[dependencies]
"@mochi/json" = { version = "^1.2", capabilities = ["fs.read"] }
```

If a candidate version's `[capabilities].required` set is not a subset of `["fs.read"]`, the solver rejects it with a `cap_excluded` incompatibility. See [05-solver-design](./05-solver-design) §5.1.

The consumer's manifest can also pin the global allowed set:

```toml
[capabilities]
allowed = ["fs.read", "net.dial"]
```

Now any transitive dep whose `required` set escapes `["fs.read", "net.dial"]` triggers a solver failure. This is the "I am not OK with anything in my tree opening a network socket, full stop" surface.

## 4. The lockfile `capabilities_seen` annotation

After a successful resolution, the lockfile records the per-package required capability set:

```toml
[capabilities_seen]
"@mochi/strings" = []
"@mochi/json"    = ["fs.read"]
```

On `mochi update`, if a candidate version would *add* a capability not in `capabilities_seen` for that package, the solver fails until the user explicitly accepts:

```
$ mochi update @mochi/json
warning: @mochi/json 1.3.0 newly requires capability "net.dial"
  Previously seen capabilities for @mochi/json: ["fs.read"]
  Accept with: mochi lock --accept-capabilities=@mochi/json
  Or pin to a version not adding the capability: mochi update @mochi/json --max=1.2
```

This is the supply-chain delta signal: a previously safe library adding network access (the xz-utils pattern) becomes a *visible* event in CI.

## 5. Enforcement per target

The capability declaration is informational unless a target enforces it. The four enforcement points:

### 5.1 TypeScript / Deno target

The Deno permission model maps directly:

| Capability    | Deno flag                              |
|---------------|----------------------------------------|
| `fs.read`     | `--allow-read`                         |
| `fs.write`    | `--allow-write`                        |
| `net.dial`    | `--allow-net=<host:port>,...`          |
| `net.listen`  | `--allow-net=<host:port>,...`          |
| `env`         | `--allow-env`                          |
| `ffi`         | `--allow-ffi`                          |
| `clock`       | (no Deno flag; always available)        |
| `random`      | (no Deno flag; always available)        |
| `proc.spawn`  | `--allow-run=<cmd>`                    |

The publish pipeline emits a `deno.json` with the inferred permission block:

```json
{
  "tasks": {
    "start": "deno run --allow-read --allow-net main.ts"
  }
}
```

Consumers running the artifact get the Deno permission prompt at startup if any required permission is not granted.

For Node.js / Bun targets: capabilities are documented in the package README and `package.json`. v1 does not enforce at runtime (Node 22's experimental permission model is still flag-gated as of 2026). v2 will adopt when stable.

### 5.2 Python target

A runtime shim wraps capability-sensitive calls:

```python
# generated mochi_runtime/caps.py
from mochi_runtime import _check_capability

def fs_read(path):
    _check_capability("fs.read")
    return open(path, "rb").read()
```

The runtime reads the capability set from a `mochi_caps.json` sidecar at process start. Missing capabilities raise `MochiCapabilityError` (a subclass of `PermissionError`).

This is not a sandbox: a determined adversary in Python can bypass via raw syscalls. The enforcement is *audit* enforcement, not security enforcement. Pair with OS-level sandboxing (Linux user namespaces, macOS sandbox-exec) for security.

### 5.3 Wasm component target

When MEP-55 (Wasm component target) lands, capabilities map to component-model imports:

| Capability    | wasi: interface                        |
|---------------|----------------------------------------|
| `fs.read`     | `wasi:filesystem/types`                |
| `net.dial`    | `wasi:sockets/tcp`                     |
| `env`         | `wasi:cli/environment`                 |
| `clock`       | `wasi:clocks/wall-clock`               |
| `random`      | `wasi:random/random`                   |

The host provides only the interfaces declared; the Wasm component cannot import others. This is the strongest enforcement model and the v2 direction for the runtime.

### 5.4 VM3 (Mochi VM) path

V1: capabilities are logged (`mochi run --trace-caps` shows usage). Not enforced.
V2 candidate: a Mochi-level effect system that statically tracks capability usage and enforces at VM dispatch.

The v1 behaviour is *transparency*, not security: the user can see what their program asked for. The transition to enforcement is a separate MEP.

## 6. Comparable systems

### 6.1 Deno permissions

Deno (2018+) ships flag-gated permissions on the CLI: `--allow-read`, `--allow-write`, `--allow-net`, `--allow-env`, `--allow-run`, `--allow-ffi`. Path / host scoping via `--allow-read=/etc,/var`, `--allow-net=api.example.com`.

Mochi-57 borrows the closed-set design. The path / host scoping is held for v2 (vocabulary explosion vs benefit not yet justified for Mochi's use cases).

### 6.2 Roc platforms

Roc's compile-time effects are declared per platform: a platform decides which effects are available. Packages declare effects they require; the linker verifies subset.

Mochi-57 borrows the platform-declares-effects model conceptually but uses manifest declaration rather than type-level effects (effect system is a separate MEP).

### 6.3 Pony reference capabilities

Pony (2014+) has reference capabilities (`val`, `ref`, `iso`, `tag`) at the type system level. These are about shared-memory concurrency, not OS effects, but the "every reference carries an explicit capability" idea inspired Roc and Mochi.

### 6.4 Wasm Component Model

The Component Model (W3C, 2024 stable) is the most powerful model: imports are declared in the component's WIT (WebAssembly Interface Type) and the host provides only the declared interfaces. Enforcement is at link time, not runtime.

Mochi-57 aligns: in the Wasm target (MEP-55 candidate), the capability declaration in `mochi.toml` becomes the import list in the WIT.

### 6.5 Lavamoat / NodeShield (npm)

Lavamoat (MetaMask, 2019+) and NodeShield (2025) instrument require/import to enforce per-package permission boundaries in Node.js. NodeShield's 2025 paper proposes a "Capability Bill of Materials" (CBOM) extending SBOM with capability data.

Mochi-57's `capabilities_seen` lockfile annotation *is* the CBOM. The Sigstore-bound SBOM emitted by `mochi build --sbom` includes the capability set (Phase 15).

### 6.6 Java SecurityManager (deprecated 2021)

JDK's SecurityManager (1.0, 1995; deprecated JEP 411, 2021) enforced per-class permissions at runtime. Deprecation reasons:

- Performance: every system call paid a permission check.
- Complexity: policy files were unworkable in practice.
- Bypass: reflection routinely defeated the model.

Lessons for Mochi-57:

- Capabilities at the *package boundary* (declared once per dep) avoid the per-call overhead.
- Manifest format avoids policy-file fragmentation.
- The audit-only stance in v1 sidesteps the bypass problem; security enforcement waits for a target with strong sandbox primitives (Wasm components, Deno).

### 6.7 Cargo's `[features]` model

Cargo features are *additive opt-in* for code paths. They are not capabilities (they do not bound OS effects). Mochi's `[features]` is parallel: it enables optional code paths; `[capabilities]` is the separate concern.

## 7. The xz-utils pattern check

The supply-chain attack on xz-utils (CVE-2024-3094, March 2024) added behaviour to 5.6.0 that 5.5.x did not have: the malicious code disabled ifunc resolution to inject into sshd. From a capability perspective, 5.6.0 added effects (proc-level memory write via ifunc, network behaviour via the sshd hook) that previous versions did not have.

If xz-utils had shipped with a capability declaration, the 5.6.0 release would have either:

- Declared the new capabilities, making the addition visible in `mochi update` warnings, or
- Falsely under-declared, in which case audit infrastructure could catch the divergence between declared and observed capabilities.

Mochi-57's v1 catches the first case. The second case is a v2 candidate: a runtime check that observed capabilities are a subset of declared. The check is feasible at the Wasm component target and the Deno target; harder elsewhere.

## 8. Capability monotonicity policy

The registry enforces a monotonicity policy on patch and minor versions:

- **Patch (x.y.Z)**: capability set must not change. Adding `net.dial` in `1.2.5` when `1.2.4` had only `fs.read` is rejected with `M057_PUB_E009`.
- **Minor (x.Y.z)**: capability set may grow. New capabilities are recorded in the index entry.
- **Major (X.y.z)**: capability set may grow or shrink.

This matches semver: patches must not change observable behaviour; minors may add behaviour; majors may break.

Consumers see capability additions as "minor version bumped, capabilities expanded from X to Y" advisories.

## 9. Tooling

- `mochi audit capabilities`: walks the resolved tree, prints per-package capability declarations and any deltas since the last lockfile.
- `mochi why-capability <cap>`: shows which packages in the tree require a given capability and what the consumer-pin policy allows.
- `mochi caps suggest`: experimental v2 surface that infers a suggested capability set by static analysis of the source (high-confidence cases only).

## 10. Failure modes

| Code              | Meaning                                                            |
|-------------------|--------------------------------------------------------------------|
| `M057_CAP_E001`   | Unknown capability identifier in manifest                          |
| `M057_CAP_E002`   | Consumer pin denies a required capability of a dep                 |
| `M057_CAP_E003`   | Lockfile records a capability addition not yet accepted            |
| `M057_CAP_E004`   | Patch version added a capability (monotonicity violation)          |
| `M057_CAP_E005`   | Runtime: a package called a capability it did not declare           |

`M057_CAP_E005` is the runtime audit signal; on the Deno target it's the Deno permission denial; on Python it's the runtime shim error; on Wasm it's the component-model link failure.

## 11. Cross-references

- Rationale: [02-design-philosophy](./02-design-philosophy) §6.
- Roc / Pony / Deno / WASI references: [03-prior-art-registries](./03-prior-art-registries) briefly; this note expands.
- Manifest `[capabilities]` block: [04-manifest-format](./04-manifest-format) §6.
- Solver capability constraints: [05-solver-design](./05-solver-design) §5.1.
- Lockfile `capabilities_seen`: [06-lockfile-format](./06-lockfile-format) §3.4.
- Per-target enforcement details: [11-polyglot-fanout](./11-polyglot-fanout) §6.
- Capability-related risks: [12-risks-and-alternatives](./12-risks-and-alternatives) §4.
