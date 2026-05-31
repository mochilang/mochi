---
title: "MEP-72 Note 01: Language surface"
sidebar_position: 2
sidebar_label: "01. Language surface"
description: "The Mochi-side language surface that MEP-72 adds: the `import ts \"<pkg>@<semver>\" as <alias>` form, the `mochi.toml` `[ts-dependencies]` and `[ts]` and `[ts.publish]` and `[ts.capabilities]` and `[ts.private]` tables, the dual-registry selector (`npm:` and `jsr:` prefixes), the CLI surface (`mochi pkg add ts`, `mochi pkg publish --to=npm` + `--to=jsr` + `--to=both`), and the per-import alias semantics."
---

# 01. Language surface

This note describes the user-facing surface of MEP-72. It is informative; the normative definitions live in [MEP-72 §Specification §1-§5](/docs/mep/mep-0072).

## 1. The `import ts` form

The Mochi grammar's FFI-import production from MEP-1 + MEP-52 phase 12:

```
ImportStmt := "import" Lang? StringLit "as" Ident ("auto")?
Lang := "go" | "python" | "typescript" | "ts" | "rust"
```

MEP-72 adds `ts` as the canonical short form. The full `typescript` keyword from MEP-52 phase 12 remains accepted (so old programs continue to parse; new programs are expected to use `ts`).

The `<spec>` admits five shapes:

```mochi
import ts "zod" as z                     # bare name, npm-default
import ts "zod@^3.23.0" as z             # semver-constrained, npm-default
import ts "npm:zod@^3.23.0" as z         # explicit npm
import ts "jsr:@std/path@^1.0" as path   # explicit JSR
import ts "lodash@git+https://github.com/lodash/lodash#main" as _
import ts "my-local@path+../local-pkg" as mine
```

The `<alias>` introduces a Mochi namespace; `z.string()` calls `z.string()` on the resolved JS import.

### Why bare names default to npm

npm hosts 3.5M+ packages (April 2026). JSR hosts 12K+. A bare-name default to npm covers ~99.7% of the realistic dep graph; the user opts into JSR via the `jsr:` prefix on a per-import basis or via the `[ts] default-registry = "jsr"` global flag.

### Why `npm:` and `jsr:` prefixes

These match the Deno toolchain's existing specifier syntax (`import foo from "npm:foo"`, `import foo from "jsr:@scope/foo"`). The Deno surface is the established convention; MEP-72 inherits it directly.

## 2. The `[ts-dependencies]` table

Modelled on npm's `package.json` `dependencies` and Deno's `imports` import-map:

```toml
[ts-dependencies]
zod = "^3.23.0"
"@hono/zod-validator" = "^0.4"
"jsr:@std/path" = "^1.0"
typescript = { version = "^5.6", dev = true }
react = { version = "^18", peer = true }
"my-local-pkg" = { path = "../my-pkg" }
"my-git-fork" = { git = "https://github.com/example/my-fork", rev = "abc123" }
```

The simple-string and table-of-version forms mirror the existing npm grammar exactly so users can copy from `package.json` without translation. The `dev = true` flag marks the dep as a development-only dep (the bridge skips it from the published library's `dependencies` field, mirroring npm's `devDependencies`). The `peer = true` flag marks the dep as a peer the host package must provide (mirroring npm's `peerDependencies`).

## 3. The `[ts]` table

Mochi-specific build-time knobs:

```toml
[ts]
runtime = "node22"          # node22 | deno2 | bun1.1 | browser | edge
module = "esm"              # esm (default) | cjs (legacy)
target = "es2024"
default-registry = "npm"    # npm (default) | jsr
import-map = "imports.json" # filename for Deno + browser import maps
monomorphise = [
    { item = "zod.ZodObject", T = "MyShape" },
]
```

## 4. The `[ts.publish]` table

Library publish knobs:

```toml
[ts.publish]
shape = "library"   # library (default) | binary
exports = { "." = { "types" = "./dist/index.d.ts", "import" = "./dist/index.js" } }
sideEffects = false
files = ["dist/**", "src/**", "README.md", "LICENSE"]
license = "Apache-2.0 OR MIT"
publish-to = ["npm", "jsr"]
```

The `publish-to` field is a subset of `["npm", "jsr"]`. `mochi pkg publish` honours this when no `--to=...` flag is passed; the CLI flag overrides.

## 5. The `[ts.capabilities]` table

```toml
[ts.capabilities]
net = true        # node:net, node:http, undici, fetch
fs = false        # node:fs
proc = false      # node:child_process
worker = false    # node:worker_threads, Worker, SharedWorker
wasm = false      # WebAssembly.instantiate
eval = false      # eval(), new Function()
```

Capability declarations are monotonic: if the manifest declares `fs = false`, a transitive dep that imports `node:fs` is a lock-time error.

## 6. The `[ts.private]` table

Opt-out for packages that have no Sigstore attestation:

```toml
[ts.private]
packages = ["@corp/internal-*"]
sigstore-skip = ["@corp/internal-*"]
```

## 7. CLI surface

| Command | Purpose |
|---------|---------|
| `mochi pkg add ts <pkg>[@<semver>]` | Adds entry to `[ts-dependencies]`; runs `mochi pkg lock`. |
| `mochi pkg add ts npm:<pkg>[@<v>]` | Explicit npm registry. |
| `mochi pkg add ts jsr:@scope/<pkg>[@<v>]` | Explicit JSR registry. |
| `mochi pkg lock` | Walks `[ts-dependencies]`, fetches tarballs, ingests `.d.ts`, writes `[[npm-package]]` + `[[jsr-package]]`. |
| `mochi pkg lock --check` | Verifies all locked hashes. |
| `mochi pkg lock --sigstore-required` | Hardens lock to refuse un-attested versions. |
| `mochi pkg publish --to=npm` | Builds `TargetNpmLibrary`; dry-run locally, real publish via CI workflow. |
| `mochi pkg publish --to=jsr` | Builds `TargetJsrLibrary`; parallel to npm. |
| `mochi pkg publish --to=both` | Both registries from the same workflow run. |
| `mochi pkg sync ts` | Regenerates Mochi shim files from the locked ApiSurface (without changing the lockfile). |

## 8. Per-import alias semantics

The `<alias>` introduces a Mochi namespace. Item names preserve the original TS casing (no rename), because the runtime resolution targets the original identifier; renaming would break the link. The Mochi side accepts `<alias>.<item>` as the syntactic form even though Mochi convention is snake_case (this is the only place in Mochi where camelCase / PascalCase identifiers are admitted, and it is exactly the existing MEP-52 phase 12 behaviour for `import typescript`).

The `auto` modifier opts into top-level binding (every exported item becomes a top-level Mochi binding rather than namespaced under the alias). This matches MEP-52 phase 12's existing behaviour for `import typescript ... auto`.

## 9. Cross-references

- [MEP-72 §Specification §4](/docs/mep/mep-0072) — the normative grammar.
- [[02-design-philosophy]] — why this surface, what was rejected.
- [[06-npm-jsr-publish-flow]] — what `mochi pkg publish --to=...` does end-to-end.
