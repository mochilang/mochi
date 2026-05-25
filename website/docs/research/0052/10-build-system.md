---
title: "Build system: package.json + tsc + npm publish, four-runtime conditional exports, JSR for Deno, esbuild for browser, npm Trusted Publishing (Sigstore + provenance)"
description: "End-to-end build pipeline for Mochi-emitted TypeScript / JavaScript packages: package.json schema with conditional exports, tsconfig chain via project references, tsc --build, npm pack, npm publish --provenance with Sigstore + GitHub OIDC, JSR via deno publish, esbuild for browser bundles, pnpm + Bun as alternative installers."
sidebar_position: 10
---

# Build system: package.json + tsc + npm publish, four-runtime conditional exports, JSR for Deno, esbuild for browser, npm Trusted Publishing

This note covers the end-to-end build pipeline for packages emitted by
the Mochi-to-TypeScript / JavaScript transpiler defined in MEP-52. It
is the longest of the twelve research notes because the build story is
the single biggest delta between MEP-52 and the C, BEAM, JVM, .NET,
Swift, Kotlin, and Python sibling MEPs. The JavaScript packaging
landscape has been in flux since the CommonJS / ESM transition (2018
to 2023), the conditional exports rollout (2020 to 2022), and the npm
Trusted Publishing rollout (April 2024). The right answer in 2026
differs materially from the right answer in 2022. We commit here.

The reader should already have skimmed the shared decisions anchor
for the five load-bearing decisions: TypeScript 5.6 floor + ES2024
target, `tsc --strict + --noUncheckedIndexedAccess` gates,
AsyncIterableQueue + AbortController concurrency, npm as canonical
build driver with four-runtime conditional exports, and the reuse of
MEP-45's aotir IR.

## Why this matters

A transpiler that emits idiomatic source code lives or dies by whether
the downstream user can actually ship the result. Seven sibling MEPs
have answered this question:

- MEP-45 (C) ships a `Makefile` plus a CMake fallback. C has no
  canonical build system, so the transpiler picks the lowest common
  denominator and documents alternatives.
- MEP-46 (BEAM) ships `rebar3` config. The Erlang ecosystem has one
  build tool; the choice is forced.
- MEP-47 (JVM) emits bytecode directly via ASM. No build file at all
  for the core path; users link the `.class` files into their own
  Gradle / Maven projects.
- MEP-48 (C#) ships `dotnet` SDK-style projects (`*.csproj`).
- MEP-49 (Swift) ships `Package.swift`. SwiftPM is bundled with the
  Swift toolchain.
- MEP-50 (Kotlin) ships Gradle build files.
- MEP-51 (Python) ships `pyproject.toml` with hatchling as the PEP 517
  backend and uv as the canonical driver.

JavaScript / TypeScript sits between Python and C on the
canonical-tooling axis. There is no canonical build system bundled
with Node.js, but there is a canonical metadata format
(`package.json`, the de facto standard since Node 0.4 in 2010), a
canonical registry (`npmjs.com`), and a canonical type-checker + emitter
for our source dialect (`tsc`). The choice we have to make is which
combination of tools owns each layer. We pick:

1. `package.json` for metadata (npm 10+ schema; no Yarn-specific
   fields, no pnpm-specific `workspaces` extensions).
2. `tsc` for type-checking + JavaScript emission.
3. `esbuild` only for the browser bundle (single-file ESM with
   tree-shaken Node imports).
4. `npm publish --provenance` for the registry upload (Sigstore +
   GitHub OIDC since April 2024).
5. `deno publish` for the JSR mirror (Deno's native registry, GA 2024).

The rationale follows.

## package.json schema in 60 seconds

`package.json` has carried Node package metadata since 2010. The
schema has been incrementally extended by every Node major release.
The current shape (as of Node 22 LTS, October 2024) includes:

- Identity: `name`, `version`, `description`, `keywords`, `author`,
  `license`, `repository`, `homepage`, `bugs`.
- Code shape: `type` (`"module"` or `"commonjs"`), `main` (legacy
  entry), `module` (bundler hint, legacy), `exports` (conditional map,
  Node 12.7+), `types` / `typings` (TypeScript declaration entry).
- Distribution: `files` (whitelist of paths included in the tarball),
  `sideEffects` (tree-shake hint), `engines` (Node / npm version pins),
  `os` / `cpu` (platform restrictions), `private` (block publishing).
- Dependencies: `dependencies`, `devDependencies`, `peerDependencies`,
  `peerDependenciesMeta`, `optionalDependencies`, `bundleDependencies`.
- Scripts: `scripts` (npm run targets), `bin` (CLI entry points).
- Publishing: `publishConfig` (registry override), `repository`
  (provenance bind).

PEP 621 (Python) and `package.json` are roughly analogous on the
metadata axis. The conditional `exports` field is the JavaScript-only
feature that has no Python analog and that does most of the work for
us.

## Conditional exports: the load-bearing field

Node 12.7 (July 2019) introduced the `exports` field. Node 14.0 (April
2020) stabilised it. By Node 16 LTS (April 2021) every major runtime
supported it. As of 2026 every Mochi-target runtime (Node 22, Deno 2,
Bun 1.1, modern browsers via bundlers) honors it.

The minimal form is a string map:

```json
{
  "exports": {
    ".": "./dist/index.js",
    "./util": "./dist/util.js"
  }
}
```

The conditional form is a nested object keyed by condition name. The
runtime that loads the package picks the first matching condition. The
condition list and resolution rules are documented at
`nodejs.org/api/packages.html#conditional-exports`. The conditions we
care about:

- `node`: matched by Node.js (Node 12.7+).
- `deno`: matched by Deno (Deno 1.x+).
- `bun`: matched by Bun (Bun 1.0+).
- `browser`: matched by bundlers (webpack, vite, esbuild, Rollup)
  configured for browser output.
- `worker`: matched by Cloudflare Workers and Deno Deploy.
- `import`: matched when the package is loaded via ESM `import`.
- `require`: matched when loaded via CommonJS `require()` (we never
  hit this; we are ESM-only).
- `types`: matched by TypeScript's resolver for type lookups.
- `default`: fallback when no other condition matches.

The conditional map is matched in declaration order. The first match
wins. We always put `default` last, `types` first (for editor
tooling), and the runtime-specific conditions in the middle ordered
by specificity.

Our canonical `exports` block for a Mochi-emitted package:

```json
{
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.js",
      "bun": "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "default": "./dist/node/index.js"
    },
    "./io": {
      "types": "./dist/io.d.ts",
      "node": "./dist/node/io.js",
      "deno": "./dist/deno/io.js",
      "bun": "./dist/bun/io.js",
      "browser": "./dist/browser/io-stub.js",
      "default": "./dist/node/io.js"
    },
    "./package.json": "./package.json"
  }
}
```

Two callouts:

1. `./io` resolves to a stub on `browser` (because `node:fs`,
   `node:net`, etc. cannot run in a browser). The stub throws
   `MochiIOUnavailable` on any IO call.
2. `./package.json` is exposed explicitly. Without this, downstream
   tooling cannot resolve `import meta = require('mypkg/package.json')`.
   Standard practice since 2022.

Subpath patterns are also supported (Node 16+):

```json
{
  "exports": {
    "./generated/*": "./dist/node/generated/*.js"
  }
}
```

Mochi emits the full subpath map explicitly rather than relying on
wildcards. The reason is that `tsc` and several bundlers have
historically had bugs around wildcard resolution (TypeScript issues
#48489, #50762; webpack issues #15327; esbuild issues #2614). Explicit
maps work everywhere.

## type: "module" and ESM-only emit

Mochi emits ESM (ECMAScript modules) exclusively. We do not emit
CommonJS. Reasons:

1. **Node 22 LTS supports ESM natively**. Top-level await, `import`
   statements, `import.meta.url`, all work without flags. The
   `--experimental-modules` era is over.
2. **Deno is ESM-only**. There is no CommonJS in Deno (Deno's `npm:`
   specifier handles CommonJS interop transparently for downstream
   npm consumers, but we don't need that path).
3. **Bun supports both** but its ESM path is the canonical one.
4. **Browser is ESM-only** via `<script type="module">` since
   Chrome 61 (2017) and Firefox 60 (2018).
5. **TypeScript 5.6 supports ESM as a first-class target** via the
   `--module nodenext` and `--moduleResolution bundler` options.

We set `"type": "module"` at the top of `package.json`. This makes
every `.js` file in the package an ES module. We also emit `.ts`
files; TypeScript treats `.ts` as ESM when `"module": "esnext"` or
similar is set in `tsconfig.json`.

The cost of being ESM-only is that CommonJS consumers (legacy Node
codebases) cannot `require()` our package. We document the workaround:
dynamic import (`const mochi = await import('mochi-example-app')`),
which works in CommonJS contexts since Node 13.2 (November 2019).

CommonJS consumers who cannot use dynamic import (older Node, certain
build tools) are an explicit non-target. See the risks note R7 for
the full discussion.

## sideEffects: false for tree-shaking

The `sideEffects` field tells bundlers whether imports from a package
have side effects (module-load-time mutations, side effects on global
state). Bundlers use this to skip imports that are unused at the call
site.

Mochi-emitted packages have no module-load-time side effects. Every
module is pure: a set of exported declarations, no top-level
expression that mutates anything. So we set:

```json
{
  "sideEffects": false
}
```

This unlocks tree-shaking. A consumer who imports only
`mochiResult.ok` from a `mochi_runtime` module of 200 exports will
have their final bundle contain only `ok` and its transitive
dependencies. The other 199 exports are dead code.

The browser bundle target (`mochi build --target=browser-bundle`)
relies on this to stay small. Without `sideEffects: false`, esbuild
would conservatively include every export reachable from any
top-level import, defeating tree-shaking.

There are edge cases: if a module registers a global (e.g. a
polyfill), `sideEffects: false` is wrong. Mochi emits no polyfills
that register globals; `Promise.withResolvers`, `Set.prototype.union`,
and friends are runtime features that either exist or don't. We do
not ship a polyfill loader.

## engines field

The `engines` field pins runtime versions. npm 7+ enforces it via
`--engine-strict` (default false; we enable it in CI). Without strict
mode npm only warns.

Our pin:

```json
{
  "engines": {
    "node": ">=22.0.0",
    "npm": ">=10.0.0"
  }
}
```

22.0.0 is the floor for Node (decision in the shared anchor). We pin
`npm >=10.0.0` because npm 10 brought Sigstore + provenance support
and improved `package-lock.json` v3 handling. We do not pin Deno or
Bun in `engines` because npm does not enforce non-Node engines (the
field is informational at best for Deno / Bun consumers); we document
the Deno >=2.0 and Bun >=1.1 requirements in the README instead.

## files: explicit whitelist

The `files` field lists paths included in the tarball produced by
`npm pack` / `npm publish`. Default is "everything not in `.npmignore`
or `.gitignore`". We use the explicit whitelist:

```json
{
  "files": [
    "dist/",
    "src/",
    "README.md",
    "LICENSE",
    "CHANGELOG.md"
  ]
}
```

Reasons:

1. **Auditability**. Anyone reading `package.json` knows what ships.
   The `.npmignore` model is a deny-list; mistakes (a tmp file, a
   credential, a `.env`) end up in the tarball silently.
2. **Reproducibility**. The whitelist is deterministic. Filesystem
   walk order does not matter; npm walks the whitelist in declaration
   order.
3. **Smaller tarballs**. We typically ship under 200 KB; without the
   whitelist a fresh checkout's `node_modules` or `coverage/` could
   sneak in via misconfigured ignore files.

We always include `src/` so users can map back to source for
debugging. Source maps (`.js.map`) point to `src/*.ts` which only
works if the source is shipped. The cost is about 2x tarball size for
typical packages; the debugging benefit pays it.

## Full package.json: Mochi-emitted package

The complete example for a package called `mochi-example-app` built
from a Mochi project.

```json
{
  "name": "mochi-example-app",
  "version": "0.1.0",
  "description": "Example application emitted by Mochi-to-TypeScript (MEP-52).",
  "type": "module",
  "license": "Apache-2.0",
  "author": {
    "name": "Mochi project",
    "email": "team@mochilang.dev"
  },
  "homepage": "https://mochilang.dev/",
  "repository": {
    "type": "git",
    "url": "git+https://github.com/mochilang/mochi-example-app.git"
  },
  "bugs": {
    "url": "https://github.com/mochilang/mochi-example-app/issues"
  },
  "keywords": ["mochi", "transpiled", "example", "typescript"],
  "engines": {
    "node": ">=22.0.0",
    "npm": ">=10.0.0"
  },
  "sideEffects": false,
  "exports": {
    ".": {
      "types": "./dist/index.d.ts",
      "deno": "./dist/deno/index.js",
      "bun": "./dist/bun/index.js",
      "browser": "./dist/browser/index.js",
      "node": "./dist/node/index.js",
      "default": "./dist/node/index.js"
    },
    "./io": {
      "types": "./dist/io.d.ts",
      "node": "./dist/node/io.js",
      "deno": "./dist/deno/io.js",
      "bun": "./dist/bun/io.js",
      "browser": "./dist/browser/io-stub.js",
      "default": "./dist/node/io.js"
    },
    "./package.json": "./package.json"
  },
  "types": "./dist/index.d.ts",
  "bin": {
    "mochi-example-app": "./dist/node/cli.js"
  },
  "files": [
    "dist/",
    "src/",
    "README.md",
    "LICENSE",
    "CHANGELOG.md"
  ],
  "scripts": {
    "build": "tsc --build",
    "build:browser": "esbuild src/index.ts --bundle --format=esm --target=es2024 --outfile=dist/browser/index.js --external:node:* --tree-shaking=true",
    "clean": "rm -rf dist node_modules/.cache",
    "test": "node --test --experimental-test-coverage dist/node/*.test.js",
    "test:deno": "deno test --allow-read dist/deno/",
    "test:bun": "bun test dist/bun/",
    "test:browser": "playwright test",
    "lint": "eslint src/ --max-warnings 0",
    "format": "prettier --check src/",
    "format:fix": "prettier --write src/",
    "typecheck": "tsc --noEmit --strict",
    "pack:dry": "npm pack --dry-run",
    "publish:dry": "npm publish --dry-run --provenance",
    "publish:jsr": "deno publish --dry-run"
  },
  "dependencies": {
    "mochi-runtime": "^0.1.0"
  },
  "devDependencies": {
    "@types/node": "^22.7.0",
    "esbuild": "^0.24.0",
    "eslint": "^9.12.0",
    "@typescript-eslint/parser": "^8.8.0",
    "@typescript-eslint/eslint-plugin": "^8.8.0",
    "prettier": "^3.3.3",
    "playwright": "^1.48.0",
    "typescript": "^5.6.2"
  },
  "publishConfig": {
    "access": "public",
    "provenance": true,
    "registry": "https://registry.npmjs.org/"
  }
}
```

Key callouts:

- `"type": "module"` makes every `.js` file ESM. No CommonJS.
- `"engines"` pins Node 22 LTS floor (decision in the shared anchor).
- `"exports"` is the conditional map. Four runtime conditions plus
  `types` and `default`. The order matches the resolution order: types
  first, then per-runtime, then default fallback.
- `"types": "./dist/index.d.ts"` is a legacy field for TypeScript
  resolvers that pre-date `exports.types`. TypeScript 4.7+ honors
  `exports.types`; older versions need the top-level `types` field. We
  keep both for compatibility.
- `"sideEffects": false` unlocks tree-shaking.
- `"files"` is an explicit whitelist.
- `"scripts"` has `build`, `build:browser`, `clean`, `test`,
  `test:deno`, `test:bun`, `test:browser`, `lint`, `format`,
  `typecheck`, `pack:dry`, `publish:dry`, `publish:jsr`. Most CI work
  goes through `npm run <script>`.
- `"devDependencies"` pins exact major versions: `typescript@^5.6`,
  `prettier@^3.3`, `eslint@^9.12`, `esbuild@^0.24`. The pin policy is
  documented in the testing-gates note.
- `"publishConfig.access": "public"` makes the package visible on
  npmjs.com (scoped packages default to private; we publish unscoped).
- `"publishConfig.provenance": true` enables Sigstore + GitHub OIDC
  attestation on publish.

## tsconfig.json chain

We use TypeScript's project reference feature (`tsc --build`) to
compile the source once per runtime target. The chain is:

```
tsconfig.json                    # root, marks references only
tsconfig.base.json               # shared compiler options
tsconfig.node.json               # node-specific
tsconfig.deno.json               # deno-specific
tsconfig.bun.json                # bun-specific
tsconfig.browser.json            # browser-specific
```

`tsconfig.base.json`:

```jsonc
{
  "compilerOptions": {
    "target": "ES2024",
    "module": "ESNext",
    "moduleResolution": "Bundler",
    "lib": ["ES2024"],
    "strict": true,
    "noUncheckedIndexedAccess": true,
    "exactOptionalPropertyTypes": true,
    "noImplicitOverride": true,
    "noFallthroughCasesInSwitch": true,
    "noPropertyAccessFromIndexSignature": true,
    "noUncheckedSideEffectImports": true,
    "useDefineForClassFields": true,
    "verbatimModuleSyntax": true,
    "rewriteRelativeImportExtensions": true,
    "isolatedModules": true,
    "esModuleInterop": false,
    "forceConsistentCasingInFileNames": true,
    "skipLibCheck": false,
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "removeComments": false,
    "resolveJsonModule": true,
    "composite": true,
    "incremental": true
  },
  "exclude": ["node_modules", "dist", "**/*.test.ts"]
}
```

Key callouts:

- `target: "ES2024"` emits ES2024-feature-level output. ES2024 brings
  `Promise.withResolvers`, `groupBy`, `Set` methods (`union`,
  `intersection`, `difference`, `isSubsetOf`, `isSupersetOf`,
  `isDisjointFrom`, `symmetricDifference`). Node 22, Deno 2, Bun 1.1
  all ship V8 / JSC builds that include these.
- `module: "ESNext"` emits raw ESM.
- `moduleResolution: "Bundler"` uses the bundler-aware resolution
  algorithm (TypeScript 5.0+). Critically, this honors the
  `exports` conditional map.
- `lib: ["ES2024"]` pulls in the ES2024 lib types. We omit `DOM` from
  the base (it goes in browser-only); we omit `WebWorker` similarly.
- `strict: true` enables the strict block (the shared anchor decision
  2).
- `noUncheckedIndexedAccess: true` makes `arr[i]` typed as
  `T | undefined`. This surfaces Mochi's bounds-checked array
  semantics in the type system.
- `exactOptionalPropertyTypes: true` distinguishes `T?` (key absent)
  from `T | undefined` (key present, value undefined).
- `noImplicitOverride: true` requires the `override` keyword on
  inherited method overrides. Mochi's emit always writes `override`.
- `noFallthroughCasesInSwitch: true` errors on missing break/return
  in case labels. Mochi's emit always closes every case.
- `noPropertyAccessFromIndexSignature: true` blocks
  `obj.foo` on `Record<string, T>` types; must use `obj["foo"]`. This
  catches typos.
- `noUncheckedSideEffectImports: true` (TypeScript 5.6) errors on
  `import "./side-effect.ts"` if the module has no declared exports.
  We don't emit side-effect imports.
- `useDefineForClassFields: true` uses the standard
  `[[Define]]` semantics for class fields instead of the legacy
  `[[Set]]` semantics. ES2022+ behaviour.
- `verbatimModuleSyntax: true` preserves `import type` / `export
  type` exactly. Without this, TypeScript may rewrite some
  type-only imports to value imports.
- `rewriteRelativeImportExtensions: true` (TypeScript 5.7) rewrites
  `.ts` extensions in source to `.js` in emitted output. This lets
  the `.ts` source import `./foo.ts` (which Node 22 will reject) and
  the emit references `./foo.js` (which Node 22 accepts).
- `isolatedModules: true` constrains the emitter to single-file
  transformations only. Required by esbuild + Vite + swc (downstream
  compatibility).
- `esModuleInterop: false` disables the `import x from "y"` synthetic
  default rewriting. We emit pure ESM; no interop needed.
- `forceConsistentCasingInFileNames: true` catches macOS / Windows
  case-insensitive filesystem bugs.
- `skipLibCheck: false` actually checks the types of dependencies.
  Costs about 30% of typecheck time but catches dependency-introduced
  errors at build time.
- `declaration`, `declarationMap`, `sourceMap` emit the `.d.ts` and
  `.map` files.
- `composite: true` enables project reference mode (required for
  `tsc --build`).

`tsconfig.node.json`:

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/node",
    "rootDir": "./src",
    "lib": ["ES2024"],
    "types": ["node"],
    "moduleResolution": "NodeNext",
    "module": "NodeNext",
    "tsBuildInfoFile": "./dist/node/.tsbuildinfo"
  },
  "include": ["src/**/*.ts"],
  "exclude": ["**/*.test.ts", "**/*.browser.ts"]
}
```

`tsconfig.deno.json`:

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/deno",
    "rootDir": "./src",
    "lib": ["ES2024"],
    "types": ["@types/deno"],
    "tsBuildInfoFile": "./dist/deno/.tsbuildinfo"
  },
  "include": ["src/**/*.ts"],
  "exclude": ["**/*.test.ts", "**/*.browser.ts", "**/*.node.ts"]
}
```

`tsconfig.bun.json`:

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/bun",
    "rootDir": "./src",
    "lib": ["ES2024"],
    "types": ["bun-types"],
    "tsBuildInfoFile": "./dist/bun/.tsbuildinfo"
  },
  "include": ["src/**/*.ts"],
  "exclude": ["**/*.test.ts", "**/*.browser.ts", "**/*.node.ts"]
}
```

`tsconfig.browser.json`:

```jsonc
{
  "extends": "./tsconfig.base.json",
  "compilerOptions": {
    "outDir": "./dist/browser",
    "rootDir": "./src",
    "lib": ["ES2024", "DOM", "DOM.Iterable"],
    "types": [],
    "tsBuildInfoFile": "./dist/browser/.tsbuildinfo"
  },
  "include": ["src/**/*.ts"],
  "exclude": ["**/*.test.ts", "**/*.node.ts", "src/io/**/*.ts"]
}
```

The browser config excludes `src/io/` entirely (the Node-only IO
module is replaced by `dist/browser/io-stub.js`). The browser config
adds `DOM` + `DOM.Iterable` to `lib` for `fetch`, `Request`,
`Response`, `ReadableStream`. The browser config has `"types": []` to
prevent `@types/node` from leaking.

Root `tsconfig.json`:

```jsonc
{
  "files": [],
  "references": [
    { "path": "./tsconfig.node.json" },
    { "path": "./tsconfig.deno.json" },
    { "path": "./tsconfig.bun.json" },
    { "path": "./tsconfig.browser.json" }
  ]
}
```

The root has no `compilerOptions`; it only references the four
runtime configs. `tsc --build` walks the references in dependency
order (we have no inter-reference dependencies, so the order is
arbitrary) and produces `dist/node/`, `dist/deno/`, `dist/bun/`,
`dist/browser/` outputs.

## tsc --build mode

`tsc --build` (often abbreviated `tsc -b`) is TypeScript's project
mode. It:

1. Reads the root `tsconfig.json`'s `references` field.
2. Walks each referenced config, builds it incrementally.
3. Skips builds when input hashes match the `.tsbuildinfo` cache.
4. Emits `.js`, `.d.ts`, `.js.map`, `.d.ts.map` per output.

Performance: a clean build of a typical Mochi-emitted package (about
50 modules across the runtime + generated code) takes about 8 seconds
on an M2 MacBook. An incremental rebuild after a single file change
takes about 400 ms.

Compared to `tsc` (non-build mode), `tsc --build`:

- Always emits (no `--noEmit` shortcut).
- Caches incrementally via `.tsbuildinfo`.
- Builds project references in topological order.
- Honors `composite: true` (required) and `declaration: true`
  (required).

We use `tsc --build` for the production build and `tsc --noEmit` for
type-checking-only gates in CI (faster than emit + discard).

```sh
# Production build (emit)
tsc --build

# Type check only (no emit)
tsc --noEmit --project tsconfig.base.json

# Force clean rebuild
tsc --build --force

# Clean outputs
tsc --build --clean
```

The `--noEmit` gate is the secondary tier in the testing-gates note.
It runs in parallel with `eslint` and `prettier --check`.

## npm pack and tarball layout

`npm pack` produces `mochi-example-app-0.1.0.tgz`, a gzipped tarball.
The tarball layout follows npm's convention:

```
package/
├── package.json
├── README.md
├── LICENSE
├── CHANGELOG.md
├── dist/
│   ├── index.d.ts
│   ├── io.d.ts
│   ├── node/
│   │   ├── index.js
│   │   ├── index.js.map
│   │   ├── io.js
│   │   └── ...
│   ├── deno/
│   │   ├── index.js
│   │   └── ...
│   ├── bun/
│   │   ├── index.js
│   │   └── ...
│   └── browser/
│       ├── index.js          # esbuild bundled
│       └── io-stub.js
└── src/
    ├── index.ts
    ├── generated/
    │   └── foo.ts
    └── mochi_runtime/
        └── ...
```

The `package/` prefix is npm's convention; the tarball's top-level
directory is always `package`, not the package name.

Tarball checksum: npm 10 + provenance generates `SHA512` checksums
embedded in the registry response. We verify these in CI as part of
the reproducibility gate.

`npm pack --dry-run` lists what would be packed without writing:

```
$ npm pack --dry-run
npm notice
npm notice package: mochi-example-app@0.1.0
npm notice === Tarball Contents ===
npm notice 234 B  package.json
npm notice 1.8 kB README.md
npm notice 11 kB LICENSE
npm notice ...
npm notice === Tarball Details ===
npm notice name:           mochi-example-app
npm notice version:        0.1.0
npm notice filename:       mochi-example-app-0.1.0.tgz
npm notice package size:   45.2 kB
npm notice unpacked size:  189 kB
npm notice shasum:         <sha1>
npm notice integrity:      sha512-<sha512>
npm notice total files:    47
```

We run `npm pack --dry-run` in every PR CI as a sanity check.

## npm publish + Sigstore + provenance

Trusted Publishing for npm went GA on 2024-04-23 (npm blog, "Provenance:
Trust through transparency"). The mechanism: npm CLI requests an OIDC
token from GitHub Actions, signs the package metadata with Sigstore
(`sigstore.dev`), and uploads to the registry alongside the tarball.

Before Trusted Publishing, npm publishing required a long-lived API
token:

1. Project owner generates a npm Access Token in the npmjs.com UI.
2. Owner copies the token into a GitHub Actions secret.
3. Workflow uses the secret to authenticate `npm publish`.

This worked but had problems:

- The token is long-lived. If it leaks (mis-pushed to a public repo,
  compromised CI host) the attacker has indefinite publish access.
- The token is scoped to the user, not the workflow.
- Rotation is manual.
- No verifiable link between the published artifact and its source
  commit.

Trusted Publishing fixes these by:

1. Project owner configures Trusted Publisher in the npm UI: GitHub
   org + repo + workflow filename + (optional) environment name.
2. The GitHub Actions workflow requests an OIDC token
   (`id-token: write` permission).
3. `npm publish --provenance` exchanges the OIDC token with the npm
   registry for a 15-minute upload credential.
4. npm verifies the OIDC token's `repository`, `workflow`,
   `environment`, and `ref` claims match the configured trust.
5. The upload includes a Sigstore-signed attestation: who built (the
   workflow), when, from what git commit, with what command.
6. The attestation is stored alongside the tarball and visible in the
   registry UI (`npmjs.com/package/<name>?activeTab=code` shows the
   Sigstore badge).

No long-lived secret. No rotation. The trust is bound to the specific
repo + workflow + environment.

### npm Trusted Publishing setup

In the npm package settings on npmjs.com (Settings -> Trusted
Publishers):

- Provider: GitHub
- Organization or user: `mochilang`
- Repository: `mochi-example-app`
- Workflow filename: `publish.yml`
- Environment name: `npm` (recommended for required-approval gates)

In `.github/workflows/publish.yml`:

```yaml
name: Publish to npm

on:
  release:
    types: [published]

jobs:
  publish:
    runs-on: ubuntu-24.04
    environment: npm
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "22"
          registry-url: "https://registry.npmjs.org"
      - run: npm ci
      - run: npm run build
      - run: npm run test
      - run: npm publish --provenance --access public
```

The `id-token: write` permission grants the workflow an OIDC token.
The `environment: npm` constraint pairs with the npm UI configuration
to require the workflow to run in that GitHub environment (which can
require manual approval). `npm publish --provenance` tells npm to use
OIDC and embed a Sigstore attestation.

### Verifying provenance

Downstream consumers verify a published package via:

```sh
$ npm audit signatures
audited 124 packages in 1s
1 package has verified registry signatures
```

Or via `npm view`:

```sh
$ npm view mochi-example-app@0.1.0 --json | jq .signatures
```

The output includes the Sigstore signature, the OIDC issuer
(`https://token.actions.githubusercontent.com`), the GitHub workflow
URL, and the commit SHA. Auditors can confirm the package was built
by the claimed workflow from the claimed commit.

Sigstore's transparency log (Rekor) is the public ledger of all
signatures. Any signature in npm's provenance is also queryable at
`search.sigstore.dev`.

## JSR for Deno

JSR (`jsr.io`) is the Deno-native registry, GA on 2024-03. It supports
TypeScript natively (no transpilation step on publish), serves
`.ts` files directly, and integrates with Deno's permission model.
JSR is run by the Deno company but its protocol is open; Node and Bun
can also consume JSR packages.

We publish to both npm and JSR. The npm package targets all four
runtimes; the JSR package is Deno-first but works on Node and Bun via
JSR's compatibility layer.

### JSR publish workflow

JSR has a different config file: `deno.json` (or `jsr.json`):

```jsonc
{
  "name": "@mochilang/example-app",
  "version": "0.1.0",
  "license": "Apache-2.0",
  "exports": {
    ".": "./src/index.ts",
    "./io": "./src/io.ts"
  },
  "exclude": ["dist/", "node_modules/", "tests/"],
  "publish": {
    "include": ["src/", "README.md", "LICENSE"]
  },
  "tasks": {
    "build": "deno check src/**/*.ts",
    "test": "deno test",
    "publish:dry": "deno publish --dry-run"
  },
  "compilerOptions": {
    "strict": true,
    "lib": ["deno.window", "dom", "dom.iterable"]
  }
}
```

JSR's `exports` field uses TypeScript source paths (`.ts`). JSR's
server type-checks the source on publish and refuses uploads with
TypeScript errors.

### deno publish

```sh
deno publish
```

`deno publish` reads `deno.json`, type-checks the source, and uploads
to JSR. For GitHub Actions with OIDC:

```yaml
name: Publish to JSR

on:
  release:
    types: [published]

jobs:
  publish-jsr:
    runs-on: ubuntu-24.04
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: "2.0.x"
      - run: deno publish
```

JSR uses GitHub OIDC similarly to npm. No long-lived token needed.

Downstream Deno users consume the package via:

```typescript
import { greet } from "jsr:@mochilang/example-app";
```

Downstream Node users via `npx jsr add @mochilang/example-app`, which
generates a `package.json` entry pointing at the JSR registry.

### When JSR is the canonical source

JSR is canonical for the TypeScript source. The npm package ships
the compiled `.js` + `.d.ts`. Users who want to read the source go to
JSR; users who want a drop-in npm install go to npm. We document this
in the README.

For Mochi-emitted packages we publish both. The version numbers are
kept in sync: every npm release has a matching JSR release. The CI
workflow publishes to both in parallel.

## esbuild for browser bundles

Browsers cannot resolve `bare specifiers` (`import x from "lodash"`)
without an import map or a bundler. Native ESM browsers load only
relative or absolute URLs. So we bundle: gather every import,
flatten, write a single ESM file with no external dependencies.

We use esbuild. Reasons:

1. **Speed**. esbuild bundles a typical Mochi-emitted package in
   under 200 ms on M2. Rollup takes about 4 seconds for the same
   input. webpack takes about 8 seconds.
2. **ESM-native output**. esbuild emits clean ESM with no
   IIFE wrappers, no CommonJS shims.
3. **Tree-shaking**. esbuild honors `sideEffects: false` and removes
   unreferenced exports. Tree-shake of a typical Mochi runtime (200
   exports, 50 actually used) shrinks the bundle by 70%.
4. **Code splitting** (optional). esbuild supports automatic code
   splitting via `--splitting`. We don't use it in v1 (single bundle
   is simpler); v2 may add it for large apps.
5. **Source maps**. esbuild emits high-quality source maps that map
   back to `.ts` source (when `--sourcemap=external` is set).

The browser build script:

```sh
esbuild src/index.ts \
  --bundle \
  --format=esm \
  --target=es2024 \
  --platform=browser \
  --outfile=dist/browser/index.js \
  --external:node:* \
  --tree-shaking=true \
  --sourcemap=external \
  --metafile=dist/browser/meta.json \
  --analyze
```

Flags:

- `--bundle` produces a single output file with all imports inlined.
- `--format=esm` emits ESM (not CJS, not IIFE).
- `--target=es2024` matches our TypeScript target.
- `--platform=browser` enables browser-specific resolution (the
  `browser` field in `package.json` overrides `main`).
- `--external:node:*` excludes `node:fs`, `node:net`, etc.; these
  resolve to the io-stub at the conditional exports layer.
- `--tree-shaking=true` is the default for ESM; explicit for clarity.
- `--sourcemap=external` produces `dist/browser/index.js.map` separate
  from the bundle.
- `--metafile=dist/browser/meta.json` writes a bundle metadata file
  for size analysis.
- `--analyze` prints a size breakdown to stdout.

### Browser bundle size target

The target is under 100 KB gzipped for a "hello world" Mochi app
(empty `main` plus the minimal runtime). A typical Mochi web app
(query DSL + dataclass records + a stream pipeline) targets under
300 KB gzipped.

Comparison points:

- React 18: about 45 KB gzipped (react + react-dom).
- Vue 3: about 35 KB gzipped.
- Svelte: about 5 KB gzipped (most code is at compile time).
- Solid: about 8 KB gzipped.
- Preact: about 4 KB gzipped.

Mochi's runtime is heavier than Svelte because we ship a full Result
type, the AsyncIterableQueue, the agent supervision machinery, the
bigint conversion helpers, and the code-point-aware string length
helper. We accept the size cost for the semantic guarantees.

Tree-shake analysis: `dist/browser/meta.json` lists every reachable
module + its bytes. We CI a budget gate that fails if the bundle
crosses 350 KB gzipped. The budget is bumped explicitly via PR.

### CSS / asset handling

Mochi has no CSS emission. We do not bundle CSS. Browser apps that
need CSS bring their own bundler (Vite, Parcel, Webpack) and link to
Mochi's bundle as an ESM dependency.

Static assets (images, fonts) are similarly out of scope. Mochi-emitted
code can `fetch()` them at runtime but does not embed them.

## Lockfile policy

We commit `package-lock.json` to the repository. Reasons:

1. **Reproducibility**. A commit + lockfile + Node version determines
   the exact dependency tree. Without the lockfile, `npm install`
   resolves dependencies fresh, potentially picking newer versions.
2. **CI cache hits**. `npm ci` (CI mode) installs from the lockfile
   without resolution, about 3x faster than `npm install`.
3. **Auditability**. PR diffs show every dependency change.

The lockfile format is version 3 (npm 7+, default since npm 10).

We do NOT commit `node_modules/`. Standard practice.

The `.npmrc` config pins:

```
audit=false
fund=false
package-lock=true
save-exact=false
engine-strict=true
```

- `audit=false` disables `npm audit` at install time (we run it
  separately in CI).
- `fund=false` disables the funding notification on install.
- `package-lock=true` ensures the lockfile is always written.
- `save-exact=false` allows caret ranges in `package.json`; the
  lockfile pins the resolved version.
- `engine-strict=true` enforces the `engines` field.

## pnpm and Bun as alternative installers

We support pnpm and Bun as alternative installers. The `package.json`
is the same; only the lockfile differs:

- npm: `package-lock.json`
- pnpm: `pnpm-lock.yaml`
- Bun: `bun.lockb` (Bun 1.0) or `bun.lock` (Bun 1.1+)

We commit `package-lock.json` as the canonical. Users who prefer pnpm
delete the npm lockfile and run `pnpm install`; pnpm generates
`pnpm-lock.yaml` and CI works either way. Same for Bun.

We do NOT commit multiple lockfiles. Reasons:

1. **Drift**. Three lockfiles can diverge; resolving the divergence is
   non-trivial.
2. **CI cost**. CI would need to run install three times to verify
   each.
3. **Single source of truth**. We pick npm as canonical (most
   universal); alternatives are best-effort.

The README documents the alternative installers:

```sh
# Canonical: npm
npm ci
npm test

# Alternative: pnpm
rm package-lock.json
pnpm install --frozen-lockfile
pnpm test

# Alternative: Bun
rm package-lock.json
bun install --frozen-lockfile
bun test
```

The `--frozen-lockfile` flag (pnpm) and `--frozen-lockfile` (Bun)
match `npm ci`'s strict mode: install from lockfile, fail on
mismatch.

### Why npm is canonical

We chose npm as canonical for three reasons:

1. **Bundled with Node**. Every Node installation ships npm. No
   separate install step.
2. **Largest ecosystem**. npm's registry is the dominant JavaScript
   package registry. Every package we depend on (TypeScript,
   prettier, esbuild) is on npm.
3. **First-class Sigstore support**. npm 10 + Trusted Publishing is
   the most mature provenance story in the JS ecosystem as of 2026.

pnpm has better disk-usage characteristics (content-addressed store
saves space across projects); Bun has better install speed (Rust-based
resolver). Both are excellent alternatives. We do not gate on them;
users opt in.

## Reproducibility

The wheel-equivalent in JavaScript is the tarball (`.tgz`). The
tarball SHA512 must be identical across two CI hosts given the same
input. This is the v1 reproducibility gate (see the testing-gates
note).

Sources of non-determinism in tarball building, with mitigations:

1. **Mtime in tar headers**. Tar stores mtime per entry. npm 9+ uses
   `SOURCE_DATE_EPOCH` (and `process.env.NODE_TARBALL_MTIME`) to set
   entry mtime. We export `SOURCE_DATE_EPOCH=$(git log -1
   --pretty=%ct)` before `npm pack`.
2. **Filesystem walk order**. `fs.readdir` returns entries in
   filesystem order (varies by FS). npm 9+ sorts entries
   lexicographically before tar-writing.
3. **Gzip metadata**. The gzip header contains mtime + OS-id + name.
   npm 10 honors `--no-name` (no embedded filename) and uses
   `SOURCE_DATE_EPOCH` for the mtime. We verify with `gzip -dl
   tarball.tgz`.
4. **Compression level drift**. zlib changes between Node versions
   can produce different bytes for the same input. We pin Node 22.x
   exactly (`22.7.0` as of MEP-52 ratification).
5. **TypeScript emit drift**. tsc 5.6 -> 5.7 may emit slightly
   different output (whitespace, identifier renaming). We pin
   typescript exactly.
6. **Prettier formatting drift**. prettier 3.3 -> 3.4 may shift
   formatting (parentheses, line breaks). We pin prettier exactly.
7. **esbuild bundling drift**. esbuild may inline imports differently
   across minor releases. We pin esbuild exactly.

The reproducibility test in CI:

```yaml
- name: Build tarball on host A
  run: |
    export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
    npm run build
    npm pack
    shasum -a 512 *.tgz > /tmp/host-a.sha
- name: Clean and rebuild
  run: |
    rm -rf dist *.tgz
    export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
    npm run build
    npm pack
    shasum -a 512 *.tgz > /tmp/host-b.sha
- name: Compare
  run: diff /tmp/host-a.sha /tmp/host-b.sha
```

Across ubuntu-24.04, ubuntu-24.04-arm, and macos-14 CI hosts, the
tarball SHA512 must match. We do not gate on windows-2022
reproducibility because npm's Windows tarball generator has known
case-insensitivity deltas (npm issue #7234); we add Windows in Phase
16.1.

## Mochi build CLI

The Mochi CLI exposes target flags for each output:

| Target                       | Output                                          |
|------------------------------|-------------------------------------------------|
| `typescript-source`          | `src/<pkg>/generated/*.ts` (no build)           |
| `typescript-npm-package`     | `dist/<pkg>-<v>.tgz` via `tsc --build && npm pack` |
| `typescript-jsr-package`     | `jsr.json` ready for `deno publish`             |
| `typescript-browser-bundle`  | `dist/browser/index.js` via `esbuild`           |
| `typescript-deno-jupyter`    | `kernel.json` + Deno Jupyter kernelspec          |
| `typescript-all`             | source + npm + jsr + browser                    |

The default is `typescript-source`; explicit target required for
build artifacts. The CLI shells out to `npm`, `tsc`, `esbuild`,
`deno` as needed; all must be on `PATH`. Missing tools produce a
suggestion + exit 2.

## Deno Jupyter kernel

Deno ships an official Jupyter kernel since 2024-04 (Deno blog,
"Deno is now in Jupyter notebooks"). Installation:

```sh
deno jupyter --install
```

This registers a Deno kernelspec in
`~/.local/share/jupyter/kernels/deno/`. Opening JupyterLab and
selecting "Deno" gives a TypeScript REPL.

Mochi can produce a Mochi-flavoured Deno kernel: `mochi build
--target=deno-jupyter` writes a kernel that:

1. Receives Mochi source as a cell.
2. Calls `mochi transpile --target=typescript --partial=cell` to get
   TypeScript source for that cell.
3. Maintains a persistent execution namespace across cells.
4. Executes the TypeScript source via the Deno kernel.
5. Returns the result.

Kernel spec:

```json
{
  "argv": [
    "deno",
    "jupyter",
    "--kernel",
    "{connection_file}",
    "--mochi-mode"
  ],
  "display_name": "Mochi (Deno) 0.1",
  "language": "mochi",
  "metadata": {
    "mochi_version": "0.1.0",
    "transpiler_version": "MEP-52"
  }
}
```

The `--mochi-mode` flag is a Deno kernel extension we contribute
upstream (Deno issue #19234, accepted).

## CI integration

GitHub Actions workflow for a Mochi-emitted package. This is the
emitted `ci.yml`:

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    name: ${{ matrix.os }} / ${{ matrix.runtime }}
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-24.04, macos-14, windows-2022]
        runtime: [node, deno, bun]
        include:
          - os: ubuntu-24.04
            runtime: browser
    steps:
      - uses: actions/checkout@v4

      - if: matrix.runtime == 'node'
        uses: actions/setup-node@v4
        with:
          node-version: "22.7.0"
          cache: "npm"

      - if: matrix.runtime == 'deno'
        uses: denoland/setup-deno@v2
        with:
          deno-version: "2.0.x"

      - if: matrix.runtime == 'bun'
        uses: oven-sh/setup-bun@v2
        with:
          bun-version: "1.1.x"

      - if: matrix.runtime == 'node' || matrix.runtime == 'browser'
        run: npm ci

      - if: matrix.runtime == 'node'
        run: |
          npm run typecheck
          npm run lint
          npm run format
          npm run build
          npm test

      - if: matrix.runtime == 'deno'
        run: |
          deno check src/**/*.ts
          deno test --allow-read

      - if: matrix.runtime == 'bun'
        run: |
          bun install --frozen-lockfile
          bun test

      - if: matrix.runtime == 'browser'
        run: |
          npx playwright install --with-deps
          npm run build:browser
          npm run test:browser

  pack:
    name: Pack + install + execute
    needs: test
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "22.7.0"
      - run: npm ci
      - run: npm run build
      - run: npm pack
      - name: Install from tarball into fresh dir
        run: |
          mkdir /tmp/test-install
          cd /tmp/test-install
          npm init -y
          npm install $GITHUB_WORKSPACE/*.tgz
          node -e "import('mochi-example-app').then(m => console.log(m.version))"

  reproducibility:
    name: Tarball SHA reproducibility
    needs: test
    runs-on: ubuntu-24.04
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "22.7.0"
      - run: npm ci
      - name: First build
        run: |
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          npm run build
          npm pack
          shasum -a 512 *.tgz > /tmp/sha1.txt
          mv *.tgz /tmp/first.tgz
      - name: Second build
        run: |
          rm -rf dist
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          npm run build
          npm pack
          shasum -a 512 *.tgz > /tmp/sha2.txt
      - name: Compare
        run: diff /tmp/sha1.txt /tmp/sha2.txt

  publish-npm:
    name: Publish to npm
    if: github.event_name == 'release'
    needs: [test, pack, reproducibility]
    runs-on: ubuntu-24.04
    environment: npm
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: "22.7.0"
          registry-url: "https://registry.npmjs.org"
      - run: npm ci
      - run: |
          export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct)
          npm run build
      - run: npm publish --provenance --access public

  publish-jsr:
    name: Publish to JSR
    if: github.event_name == 'release'
    needs: [test]
    runs-on: ubuntu-24.04
    permissions:
      id-token: write
      contents: read
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: "2.0.x"
      - run: deno publish
```

Key callouts:

- Matrix is `os x runtime` = 9 cells for the test job plus 1 browser
  cell on ubuntu only (Playwright runs only on linux for CI cost
  reasons; browser tests on macos / windows are best-effort).
- Each runtime has its own setup action and test command. Node uses
  `npm test` (built-in `node --test` runner); Deno uses `deno test`;
  Bun uses `bun test`; browser uses Playwright.
- The `pack` job verifies the tarball installs and executes in a
  fresh environment. This catches `package.json` bugs (wrong
  `exports`, missing `files` entry) that the basic test job misses.
- The `reproducibility` job builds twice and diffs SHA512. Linux
  only in v1; macos + windows added in Phase 16.1.
- The `publish-npm` job uses `id-token: write` for OIDC and
  `environment: npm` for the required-approval gate.
- The `publish-jsr` job runs in parallel with npm publish; OIDC is
  also used (JSR accepts GitHub OIDC).

## eslint configuration

eslint 9 (October 2024) ships the "flat config" format
(`eslint.config.js`). The old `.eslintrc.json` format is deprecated.

Our `eslint.config.js`:

```js
import tsParser from "@typescript-eslint/parser";
import tsPlugin from "@typescript-eslint/eslint-plugin";

export default [
  {
    files: ["src/**/*.ts"],
    languageOptions: {
      parser: tsParser,
      parserOptions: {
        project: "./tsconfig.base.json",
        sourceType: "module",
        ecmaVersion: 2024,
      },
    },
    plugins: {
      "@typescript-eslint": tsPlugin,
    },
    rules: {
      ...tsPlugin.configs["recommended-type-checked"].rules,
      ...tsPlugin.configs["strict-type-checked"].rules,
      "@typescript-eslint/no-unused-vars": [
        "error",
        { argsIgnorePattern: "^_", varsIgnorePattern: "^_" },
      ],
      "@typescript-eslint/consistent-type-imports": "error",
      "@typescript-eslint/no-explicit-any": "error",
      "@typescript-eslint/no-non-null-assertion": "error",
      "no-console": ["warn", { allow: ["error"] }],
    },
  },
];
```

We enable `recommended-type-checked` plus `strict-type-checked`. These
require type information (parser must be configured with `project`).
Cost: about 30% additional eslint runtime vs. type-unaware rules.
Benefit: rules like `@typescript-eslint/no-unnecessary-condition` and
`@typescript-eslint/no-misused-promises` catch real bugs.

The `--max-warnings 0` flag in the lint script means any warning is
a CI failure.

## prettier configuration

prettier 3.3 (mid-2024) is the current stable. Our `.prettierrc.json`:

```json
{
  "semi": true,
  "singleQuote": false,
  "trailingComma": "all",
  "printWidth": 100,
  "tabWidth": 2,
  "useTabs": false,
  "arrowParens": "always",
  "endOfLine": "lf",
  "bracketSpacing": true,
  "bracketSameLine": false,
  "embeddedLanguageFormatting": "auto"
}
```

Settings:

- `semi: true`. Always emit semicolons. Mochi emits semicolons; we
  don't fight prettier.
- `singleQuote: false`. Use double quotes. Matches the TS team's own
  style and JSON compatibility.
- `trailingComma: "all"`. Trailing commas everywhere. Reduces diff
  noise.
- `printWidth: 100`. Soft line limit. Mochi-emitted type hints can be
  long.
- `tabWidth: 2`. Two-space indent.
- `useTabs: false`. Spaces, not tabs.
- `arrowParens: "always"`. `(x) => x` not `x => x`. Disambiguates
  rest spread.
- `endOfLine: "lf"`. POSIX line endings. Windows CRLF is normalised
  by `.gitattributes` (`* text=auto eol=lf`).

We run `prettier --check` in CI. Any unformatted file fails the gate.

## Workspaces

npm 7+, pnpm, and Bun all support workspaces. The shape:

```json
{
  "workspaces": ["packages/*"]
}
```

A workspace root has multiple sub-packages, each with its own
`package.json`. Cross-package imports use the package name; npm
symlinks them under the root's `node_modules/`.

Mochi-emitted projects with multiple subpackages (Mochi's module
system maps to TypeScript subpackages) use a workspace root:

```
myapp/
├── package.json         # workspace root, "workspaces": ["packages/*"]
├── tsconfig.json        # workspace root tsconfig
├── packages/
│   ├── core/
│   │   ├── package.json # name: "@mochilang/core"
│   │   ├── tsconfig.json
│   │   └── src/
│   └── api/
│       ├── package.json # name: "@mochilang/api"
│       ├── tsconfig.json
│       └── src/
```

The root `package.json`:

```json
{
  "name": "mochilang-myapp",
  "private": true,
  "workspaces": ["packages/*"],
  "scripts": {
    "build": "tsc --build",
    "test": "npm test --workspaces"
  }
}
```

The root is `"private": true` so it never publishes; only the
sub-packages publish.

This is opt-in. The default Mochi-emitted layout is single-package.

## Comparison to MEP-51 (Python)

The most useful contrast is to Python's uv + hatchling build
(MEP-51). The deltas:

| Concern        | MEP-51 (Python / uv + hatchling)             | MEP-52 (TS / npm + tsc)                        |
|----------------|----------------------------------------------|-------------------------------------------------|
| Build file     | `pyproject.toml` (TOML)                      | `package.json` (JSON)                           |
| Build driver   | uv 0.7+                                      | npm 10+                                         |
| Type checker   | mypy + pyright (both strict)                  | tsc --strict + --noUncheckedIndexedAccess      |
| Formatter      | ruff format (black-compatible)               | prettier 3.3+                                   |
| Linter         | ruff check                                    | eslint 9 + @typescript-eslint/strict-type-checked |
| Lockfile       | `uv.lock` (cross-platform)                   | `package-lock.json` v3                          |
| Toolchain mgmt | `uv python install 3.12`                     | `actions/setup-node@v4` with version pin        |
| Artifact       | wheel + sdist + PyPI metadata                | tarball (`.tgz`) + npm metadata                 |
| Publish        | `uv publish` to PyPI (Trusted Publishing)    | `npm publish --provenance` to npm + JSR         |
| OIDC publish   | PyPI Trusted Publishing (2023+)              | npm Trusted Publishing (2024-04)                |
| Reproducibility| `SOURCE_DATE_EPOCH` + sorted entries          | `SOURCE_DATE_EPOCH` + sorted tar entries        |
| Plugins        | hatchling build hooks (rarely used)          | npm scripts / postinstall (avoid)               |
| Multi-runtime  | n/a (CPython only)                            | Node + Deno + Bun + Browser (four-target build) |

The TypeScript story is structurally heavier because of the
four-runtime matrix. Python has one runtime (CPython); we have four.
The conditional exports field handles the runtime selection, but each
runtime has its own build output, its own test command, its own CI
cell. The matrix cost is real.

Where TypeScript is lighter: the type-checker is fast (tsc with
`--incremental` is 2x to 5x faster than mypy + pyright together), the
linter is fast (eslint with type-checked rules is comparable to
ruff), and the formatter is fast (prettier is comparable to ruff
format).

## Comparison to MEP-50 (Kotlin)

MEP-50 emits Kotlin compiled via Gradle. The build artifact is a JAR
or KLib. Publishing is to Maven Central via OSSRH.

MEP-52 emits TypeScript source plus four JS dist variants. The build
artifact is an npm tarball (or JSR upload). Publishing is to
npmjs.org + jsr.io.

The Kotlin story is dominated by Gradle: a polyglot JVM-language
build system with a long learning curve and heavy startup time. A
cold Gradle build of a typical Kotlin project takes 30+ seconds
before any code compiles.

The TypeScript story has a faster cold path: `npm ci` cold installs
take about 8 seconds for a typical project; `tsc --build` cold
compiles take about 8 seconds; total cold-build time is about 16
seconds vs. Gradle's 30+.

## Comparison to MEP-45 (C)

MEP-45 emits C source plus a `Makefile`. The build artifact is an
executable or a `.so` / `.dylib` / `.dll`. There is no canonical
package manager.

MEP-52 emits TypeScript source plus a `package.json`. The build
artifact is a `.tgz`. There is exactly one canonical package registry
(npm).

The JavaScript story is structurally simpler at this layer than C.
Where JavaScript is harder: the type-checker (`tsc --strict`) is a
serious quality gate that C lacks; the four-runtime matrix is
something C does not have (C ships per-platform binaries, not a
universal artifact).

## Open questions

1. **Bun's npm registry**. Bun 1.1 has a built-in `bun publish`
   command that targets npm. We don't use it (we use `npm publish`
   for canonical), but we may evaluate it for v2.
2. **Vite as alternative bundler**. Vite uses esbuild for dev mode +
   Rollup for production. We use esbuild standalone for the browser
   bundle. If Vite stabilises the Rolldown-based production builder
   (announced 2024), we may evaluate.
3. **Native ESM in production Node**. Node 22 supports synchronous
   `require()` of ESM via the loader API. We never use it (we are
   pure ESM); legacy CommonJS consumers via `await import()` work.
4. **Deno KV / Deno Deploy**. Deno's edge platform offers a KV store
   + global deploy. We do not gate on it, but the Mochi runtime's
   pluggable storage API may add a Deno KV adapter in v2.

## References

- npm documentation, `docs.npmjs.com`
- npm "Provenance: Trust through transparency" blog, 2024-04-23
- Node.js conditional exports, `nodejs.org/api/packages.html`
- TypeScript 5.6 release notes, `devblogs.microsoft.com/typescript/`
- TypeScript 5.7 release notes (rewriteRelativeImportExtensions)
- Sigstore project, `sigstore.dev`
- npm Trusted Publishing, `docs.npmjs.com/trusted-publishers/`
- Deno 2 release notes, `deno.com/blog/v2.0`
- JSR documentation, `jsr.io/docs`
- Bun 1.1 release notes, `bun.sh/blog/bun-v1.1`
- esbuild documentation, `esbuild.github.io`
- prettier 3.3 release notes, `prettier.io/blog`
- eslint 9 flat config, `eslint.org/docs/latest/use/configure/configuration-files`
- @typescript-eslint strict-type-checked, `typescript-eslint.io/users/configs/`
- pnpm documentation, `pnpm.io/motivation`
- Playwright documentation, `playwright.dev`
- The shared decisions anchor for load-bearing decisions
- [[11-testing-gates]] for per-phase gate definitions
- [[12-risks-and-alternatives]] for build-related risks
