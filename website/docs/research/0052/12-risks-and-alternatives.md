---
title: "Risks and alternatives: 15 risks + 6 rejected alternatives (Babel, esbuild-only, JSDoc-only, Webpack, Rollup, RxJS) + 4 future candidates (WASM via WASI, ts-blank-space for zero-build, Bun-native compile, Cloudflare Workers)"
description: "Risk register (15 entries) plus rejected alternatives (6 entries) plus future-track candidates (4 entries) for the Mochi-to-TypeScript transpiler. Concrete mitigations referencing real Node bug-tracker items, TC39 proposals, and tool versions."
sidebar_position: 12
---

# Risks and rejected alternatives

This note collects the risks the Mochi-to-TypeScript transpiler
accepts on its current path, the alternative paths the team
explicitly rejected, and the future-track candidates we may revisit
in v2.

The risk register is fifteen entries. The alternatives section is six
entries. The future section is four entries.

Each entry uses the same structure: title, description, likelihood,
impact, mitigation, owner. Alternatives have: title, description,
evaluation, decision, references. Future candidates have: title,
description, gating signal.

See the shared decisions anchor for the load-bearing decisions these
risks accept, [[10-build-system]] for the build pipeline that
several risks attack, and [[11-testing-gates]] for the gate plan
that catches them.

## Risk register

### R1: Promise.withResolvers polyfill on older runtimes

**Description**: `Promise.withResolvers()` is ES2024. Mochi's
agent's `call(req)` pattern depends heavily on it: the mailbox push
embeds a resolver that the handler invokes to fulfil the reply
promise. Without `Promise.withResolvers`, every `call` would need a
manual `new Promise((resolve) => { ... })` wrapper, which is more
verbose and obscures the intent.

Runtime support as of 2026-05-23:

- Node: 22.0.0+ (April 2024). Backported to 20.13.0 (May 2024) but
  not 18.x.
- Deno: 1.39+ (December 2023).
- Bun: 1.0.21+ (January 2024).
- V8 Chrome: 119+ (October 2023).
- SpiderMonkey Firefox: 121+ (December 2023).
- JavaScriptCore Safari: 17.4+ (March 2024), iOS 17.4+ (March 2024).

Users on Safari 17.3 or older, iOS 17.3 or older, or any Node
20.12.x miss the API. We need a polyfill for those targets.

**Likelihood**: medium. The browser long tail has Safari users on
17.0-17.3 for several months after each release.

**Impact**: medium. Missing the API is a hard runtime error on the
first agent `call`.

**Mitigation**:

- The runtime stub `mochi_runtime/promise.ts` includes a polyfill
  guarded by feature detection:
  ```typescript
  if (typeof Promise.withResolvers !== "function") {
    Promise.withResolvers = function<T>() {
      let resolve!: (value: T | PromiseLike<T>) => void;
      let reject!: (reason?: unknown) => void;
      const promise = new Promise<T>((res, rej) => {
        resolve = res;
        reject = rej;
      });
      return { promise, resolve, reject };
    };
  }
  ```
- The polyfill is tree-shaken out of Node 22 / Deno 2 / Bun 1.1
  builds via `sideEffects: false` + dead-code elimination (the
  feature check is statically true on those runtimes, so esbuild
  removes the branch).
- The browser bundle includes the polyfill unconditionally (since
  we cannot know the user's browser version at build time).
- Documented in the README "browser compatibility" section.

**Owner**: runtime maintainer.

### R2: bigint performance on V8

**Description**: V8's bigint implementation is materially slower
than its number implementation for typical arithmetic. Benchmarks
from V8 team (2023-Q4, "BigInt in V8" blog series):

- `a + b` for `number`: about 1 ns on M2.
- `a + b` for `bigint` (under 2^64): about 10 ns on M2.
- `a + b` for `bigint` (above 2^256): about 50 ns on M2.
- `a * b` for `bigint`: about 30 ns for small, growing with operand
  size.

Mochi's `int` defaults to `bigint` for safety. CPU-bound code that
does heavy integer arithmetic (datalog evaluation, set operations,
hash computations) takes about 10x as long when using bigint vs
number.

**Likelihood**: high (always present on integer-heavy workloads).

**Impact**: medium for v1 (we target IO-bound async, not CPU-bound
arithmetic). High when users try arithmetic-heavy workloads.

**Mitigation**:

- The lowering pass's monomorphisation analysis specialises `int`
  to `number` when the IR proves the value fits in
  [-(2^53-1), 2^53-1] and the producer never overflows. The
  decision is per-IR-type, not global.
- Document the monomorphisation rules in the lowering note.
  Users can read the emit's type annotations to confirm `number`
  vs `bigint` choice.
- The Mochi optimiser inserts overflow guards for `number`
  arithmetic when the static analysis is uncertain. The guards
  throw `MochiOverflowError` at runtime; the runtime semantics
  remain correct.
- For workloads that need fast arbitrary-precision arithmetic,
  v2 candidate: WASM-compiled GMP via `mochi.runtime.gmp`. See F1
  below.

**Owner**: lowering maintainer.

### R3: tree-shake of mochi_runtime

**Description**: The Mochi runtime stub (`mochi_runtime/`) has about
200 exports across IO, agents, streams, Result, MochiResult, query
helpers, datalog evaluation, bigint conversion, code-point string
helpers, and more. A typical Mochi app uses about 30-50 of these.
The other 150 must be tree-shaken out.

Tree-shaking depends on:

1. `sideEffects: false` in `package.json` (set; see
   [[10-build-system]]).
2. ESM-only emit (set; we never emit CommonJS).
3. Pure top-level expressions in every runtime module (verified by
   eslint `no-side-effects-in-initialization` rule).
4. Static imports only (no dynamic `await import()` in the
   user-callable path).

If any of these break, the bundle includes dead code.

**Likelihood**: medium. Adding a new runtime helper that has a
top-level side effect (e.g. registering a global) silently disables
tree-shaking.

**Impact**: medium. Bundle size grows by up to 4x without
tree-shaking. The browser bundle budget (350 KB gzipped) is then
exceeded and the gate fails.

**Mitigation**:

- The eslint config enables `no-side-effects-in-initialization` on
  the runtime stub directory.
- The browser bundle CI gate checks the gzipped size; failure
  surfaces the regression.
- Periodic audit: every quarter we run `esbuild --analyze` on a
  reference Mochi app and review the bundle composition. New
  reachable modules trigger a justification.
- Document the side-effect-free contract in the runtime stub's
  README.

**Owner**: runtime maintainer.

### R4: npm supply-chain attacks

**Description**: npm has had several high-profile supply-chain
attacks: event-stream (2018), bootstrap-sass (2019),
flatmap-stream / event-stream / chalk (2018-2019), node-ipc / peace
notmuch / colors.js (2022). The attack pattern: a maintainer of a
deep-dependency package introduces malicious code in a patch
release; downstream packages auto-bump and ship the malware.

Mochi's runtime is a single npm package (`mochi-runtime`); Mochi
apps depend transitively on whatever the runtime imports. Every
direct dependency is an attack surface.

**Likelihood**: high (this has happened multiple times).

**Impact**: high. A compromised runtime steals environment
variables, exfiltrates source code, or pivots into CI secrets.

**Mitigation**:

- **Sigstore + provenance**: every published version of
  `mochi-runtime` is signed via npm Trusted Publishing (Sigstore
  + GitHub OIDC; GA April 2024). The signature attests that the
  build ran from the verified workflow on the verified commit. See
  [[10-build-system]] for the publish workflow.
- Downstream consumers verify with `npm audit signatures`. CI
  fails on signature mismatch.
- **Minimal runtime dependencies**. `mochi-runtime` itself has
  zero runtime dependencies (no `dependencies` in `package.json`).
  Its transitive footprint is zero. Mochi apps' attack surface is
  only `mochi-runtime` itself.
- **Lockfile commit**: `package-lock.json` is committed.
  `npm ci` installs from the lockfile without resolution; no
  silent upgrade.
- **Engine pinning**: `engines.node` pins 22.0.0+; users on
  older Node cannot install (engines-strict is enabled via
  `.npmrc`).
- Monthly review of any new dependency addition (PR template
  question: "does this PR add a dependency?").

**Owner**: security maintainer.

### R5: TypeScript major-version bumps breaking type narrowing

**Description**: TypeScript major versions (5.0 -> 5.6 -> 6.0)
sometimes change type narrowing semantics in ways that surface as
new strict-mode errors on previously-clean code. Examples from the
TypeScript release history:

- 5.0: `const`-assertion changes affected discriminated union
  narrowing in some patterns (TS issue #50465).
- 5.4: `NoInfer<T>` utility type changed generic inference
  (TS issue #56261).
- 5.5: regex literals with control flow narrowing (TS issue
  #57389).
- 5.6: `noUncheckedSideEffectImports` is opt-in; default false. We
  opt in; bumping to 5.7 may make it default with stricter
  behaviour.

A bump that introduces new errors on Mochi-emitted code means the
emit must change to satisfy the new semantics, or the gate fails.

**Likelihood**: high (TypeScript ships every quarter).

**Impact**: medium. The fix is mechanical (change the emit
pattern); the cost is the audit + regression.

**Mitigation**:

- Pin `typescript@5.6.2` exactly in `package.json` devDependencies.
- The testing-gates note's tool-version table tracks the pin.
- Bump quarterly via a dedicated audit PR. The PR records every
  new diagnostic + decision (fix emit | suppress in fixture |
  file upstream bug).
- File upstream bugs aggressively. The Mochi team has filed three
  bugs against TypeScript as of 2026-Q1; two are resolved.
- Avoid features at the bleeding edge: we use TypeScript features
  that have been stable for at least one minor release before
  pinning.

**Owner**: type-system maintainer.

### R6: Deno / Bun divergence from Node

**Description**: Deno 2 and Bun 1.1 aim for Node compatibility but
have known divergences:

- Module resolution: Deno's URL-based imports vs Node's bare
  specifiers (mostly resolved via Deno's `npm:` and `jsr:`
  specifiers, but edge cases remain).
- `node:fs` vs `Deno.readFile`: Bun has both; Deno has `node:fs`
  for compatibility but the canonical API is `Deno.readFile`.
- Test runners: Node 22's `node:test`, Deno's `deno test`, Bun's
  `bun test` are three separate runners with three separate
  expectation formats.
- Performance characteristics: Bun's startup is about 4x faster
  than Node; Deno's is about 1.5x faster. The actual workload
  performance varies.
- Stream APIs: Web Streams (`ReadableStream`, `WritableStream`)
  are standardised but the implementations differ subtly (Node's
  was added 18.0; Deno's predates; Bun's is independent).

Mochi-emitted code that works on Node may fail on Deno or Bun.

**Likelihood**: medium. Active gap-closing on all three runtimes,
but new gaps emerge with each release.

**Impact**: medium. A gap discovered post-release means
documenting "skip on Bun" or "skip on Deno" for the affected
feature.

**Mitigation**:

- The four-runtime test matrix (see [[11-testing-gates]]) catches
  divergences at CI time, not user time.
- The conditional exports map can route Deno / Bun / Node to
  different built artifacts when needed. Mochi-emitted code stays
  in the common subset; runtime-specific shims live in
  `mochi_runtime/io/{node,deno,bun,browser}.ts`.
- Track upstream compatibility tables: `nodejs.compat.deno.com`
  (Deno), `bun.sh/docs/runtime/nodejs-apis` (Bun).
- File upstream bugs when divergences are found in fixtures.

**Owner**: runtime maintainer.

### R7: ESM-only excluding CommonJS consumers

**Description**: Mochi emits ESM exclusively. Some downstream
projects are still CommonJS-only:

- Older Node apps stuck on CommonJS due to internal tooling
  constraints.
- Some build tools (older Webpack configs, older Jest setups) that
  pre-date ESM support.
- Tools that use `require.resolve` introspection patterns broken
  by ESM.

CommonJS consumers cannot `require("mochi-example-app")`. They must
use dynamic `await import()` (works in CommonJS since Node 13.2,
November 2019).

**Likelihood**: medium. CommonJS-only consumers are a shrinking
fraction of the ecosystem but not zero.

**Impact**: low. The workaround (dynamic import) is documented and
widely understood.

**Mitigation**:

- Document the dynamic import recipe in the README's
  "CommonJS consumers" section.
- We do NOT ship a `--module CommonJS` build. The cost
  (`exports.require` field, dual-package hazard, double bundling)
  outweighs the benefit.
- Users who absolutely need CommonJS can use a third-party tool
  (`@rollup/plugin-cjs`, esbuild's `--format=cjs`) to wrap
  Mochi-emitted code locally.

**Owner**: docs maintainer.

### R8: TC39 stage churn on iterator helpers

**Description**: ES2024 iterator helpers (`Iterator.from`,
`Iterator.prototype.map`, `.filter`, `.take`, `.drop`, `.toArray`)
were Stage 4 in early 2024. Browser implementations rolled out
through 2024 (Chrome 122, Firefox 131, Safari 18.0).

Runtime support as of 2026-05-23:

- Node 22+: supported.
- Deno 2.0+: supported.
- Bun 1.1+: supported.
- Chrome 122+: supported (Feb 2024).
- Firefox 131+: supported (Oct 2024).
- Safari 18.0+: supported (Sept 2024).

There is a long tail of users on Safari 17.x (released through
2024) and Chrome older than 122. Iterator helpers are not in their
runtime.

**Likelihood**: medium. The polyfill is small and well-tested.

**Impact**: low. Missing the methods produces a clear runtime
error; the polyfill resolves it.

**Mitigation**:

- The runtime stub ships an iterator-helpers polyfill in
  `mochi_runtime/iterator-helpers.ts`. Feature-detection guards.
- The Mochi emit prefers iterator helpers when available; falls
  back to manual loops when not.
- Browser bundle includes the polyfill unconditionally.
- Documented in the README "browser compatibility" section.

**Owner**: runtime maintainer.

### R9: prettier 3 vs 4 churn

**Description**: prettier 4 is in the planning stage as of 2026.
The release notes for prettier 4 mention breaking changes:

- Default `printWidth` may change from 80 to 100.
- `trailingComma` default already changed in 3.0 from `"es5"` to
  `"all"`; further tweaks possible.
- New CSS / Markdown formatting rules unlikely to affect us, but
  the formatter's TypeScript output may shift in subtle ways
  (parenthesisation, line breaks).

A bump from prettier 3 to 4 would invalidate every fixture's
formatter-stable check (Tier 4). We'd need to re-format the entire
emit corpus and audit the diffs.

**Likelihood**: medium. prettier 4 is on the roadmap but not
shipped as of 2026-05-23.

**Impact**: medium. Mechanical re-formatting; not a semantic
regression.

**Mitigation**:

- Pin `prettier@3.3.3` exactly.
- Watch for prettier 4 release announcements (planned for late
  2026 / early 2027).
- When the bump happens, snapshot the diff via
  `prettier --write` on the entire fixture set + `git diff`.
  Review for surprises. Re-format the emit to match.

**Owner**: format maintainer.

### R10: eslint 9 flat config migration churn

**Description**: eslint 9 (October 2024) shipped the "flat config"
format (`eslint.config.js`). The old `.eslintrc.json` format is
deprecated in 9.0 and slated for removal in 10.0.

Our config uses the flat format already. The risk: the
`@typescript-eslint` plugin's flat-config API surface is still
stabilising. typescript-eslint 8.x supports flat config natively;
8.0 was the first stable release with flat support.

A future typescript-eslint major bump (9.0 in 2026) may change the
plugin API. Our config would need updates.

**Likelihood**: medium. typescript-eslint releases majors about
yearly.

**Impact**: low. The config changes are mechanical.

**Mitigation**:

- Pin `eslint@9.12.0` and `@typescript-eslint/*@8.8.0` exactly.
- Bump quarterly with audit.
- Document the migration path in the testing-gates note.

**Owner**: lint maintainer.

### R11: Sigstore TUF metadata stale

**Description**: Sigstore uses TUF (The Update Framework) for
metadata. The TUF root keys are rotated periodically (the Sigstore
project rotated keys in 2022 and 2024). A user's local TUF cache
can become stale; subsequent verifications fail until the cache is
refreshed.

This affects `npm audit signatures`: if the user's npm + Sigstore
client has stale TUF metadata, the verification fails even though
the signature is valid.

**Likelihood**: low. npm 10+ refreshes TUF metadata automatically;
stale cache is rare.

**Impact**: low. The user sees a clear error and runs `npm config
set sigstore-rekor-url https://rekor.sigstore.dev` or equivalent
refresh command.

**Mitigation**:

- Document the troubleshooting recipe in the README's "verifying
  provenance" section.
- The Mochi CI workflow refreshes Sigstore TUF metadata on every
  CI run by running `npm install -g @sigstore/sigstore-tuf` and
  triggering a verification.
- File upstream bugs against npm / Sigstore when verification
  fails unexpectedly.

**Owner**: security maintainer.

### R12: AbortController vs AbortSignal in older runtimes

**Description**: `AbortController` and `AbortSignal` have been
stable in Node since 15.0 (October 2020) and standardised in the
DOM. Mochi's agent supervision relies heavily on them: the parent
scope's `AbortSignal` is forked to each child agent.

`AbortSignal.timeout(ms)` (helper to create a signal that aborts
after `ms`) is newer: ES2023, supported in Node 19+, Deno 2+,
Bun 1.0+, Chrome 103+, Firefox 100+, Safari 16.4+.

`AbortSignal.any([signals])` (combine multiple signals) is ES2024,
supported in Node 22+, Deno 2+, Bun 1.1+, Chrome 119+, Firefox
124+, Safari 17.4+.

**Likelihood**: medium. The base `AbortController` is universal;
`AbortSignal.timeout` is widely supported; `AbortSignal.any` is
newer.

**Impact**: medium. Missing `AbortSignal.any` falls back to
manual signal combination (about 10 lines of code). Missing
`AbortSignal.timeout` falls back to `setTimeout` + manual abort.

**Mitigation**:

- The runtime stub includes polyfills for `AbortSignal.timeout`
  and `AbortSignal.any`, guarded by feature detection.
- Polyfills are tree-shaken out on modern runtimes.
- Documented in the README "browser compatibility" section.

**Owner**: runtime maintainer.

### R13: npm Trusted Publishing OIDC failures

**Description**: Trusted Publishing uses OIDC tokens minted by
GitHub Actions and traded with npm for short-lived publish
credentials. The OIDC exchange can fail:

- GitHub Actions OIDC service outage (rare; happens about once a
  quarter for 5-15 minutes).
- npm Trusted Publisher misconfiguration (workflow name doesn't
  match, environment name doesn't match, ref doesn't match).
- Token claim drift: GitHub updated claim format in late 2024;
  npm's verifier caught up but old configs broke briefly.
- Sigstore Rekor outage: the transparency log is a hard dependency
  for provenance.

A failed OIDC exchange blocks the release. The release engineer
needs a fallback to ship the bits.

**Likelihood**: medium (OIDC failures happen).

**Impact**: medium. A blocked release is annoying but recoverable.

**Mitigation**:

- Maintain a fallback npm access token in the project's GitHub
  secrets, named `NPM_FALLBACK_TOKEN`. The token is granular,
  scoped to the project only with publish permissions. Document
  the manual fallback recipe:
  ```sh
  echo "//registry.npmjs.org/:_authToken=$NPM_FALLBACK_TOKEN" > .npmrc
  npm publish --access public
  ```
- Rotate the fallback token every 90 days via a calendar reminder.
- The fallback token is NOT used in normal CI. It exists only for
  the OIDC-failure path.
- Test the OIDC path quarterly via the
  `tests/transpiler3/typescript/phase18_trusted_publishing_test.go`
  dry-run.
- Monitor Sigstore status at `status.sigstore.dev`.

**Owner**: release engineer.

### R14: reproducibility breakage from filesystem ordering

**Description**: npm 9+ sorts tarball entries before writing. But
subtle filesystem ordering effects can still leak: symlinks (macOS
HFS+ legacy, no longer default), case-insensitive filenames on
macOS APFS (default for non-case-sensitive volumes), Windows NTFS
short-name aliasing, directory mtime stamps.

A single non-deterministic mtime in an `index.ts` written by the
emit pass breaks the tarball SHA512 match across hosts.

**Likelihood**: medium. We hit this once in early Phase 15 testing
(macOS case-insensitive filesystem reordered `Foo.ts` and
`foo.ts`).

**Impact**: medium. Reproducibility is a v1 gate; failures block
release.

**Mitigation**:

- Mochi emits all files with `Bun.write` / `Deno.writeFile` /
  `fs.writeFile` plus an explicit `fs.utimes(path, epoch, epoch)`
  setting mtime to `SOURCE_DATE_EPOCH`.
- The Mochi lowering pass sorts emit output by filepath
  (lexicographic bytes, ASCII order). No reliance on map iteration
  order or filesystem walk.
- We forbid two filenames that differ only in case (`Foo.ts` and
  `foo.ts`). The lowering pass errors on this at emit time. macOS
  APFS case-insensitive volumes get a clean emit.
- The reproducibility CI job runs on linux ext4 + macOS APFS +
  linux aarch64 (Phase 16). Windows reproducibility is Phase 16.1.
- See [[10-build-system]] reproducibility section for the full
  recipe.

**Owner**: emit maintainer.

### R15: tsc false positives on legit Mochi patterns

**Description**: Some Mochi patterns lower to typed TypeScript that
tsc flags as incorrect even though the runtime behaviour is fine.
Examples we have seen during phase prototyping:

- Mochi's exhaustive `match` lowered to a TS `switch` with a
  final `default: { const _: never = x; throw ... }`. tsc 5.5
  flagged this as "unreachable" in some discriminated-union
  shapes (fixed in 5.6).
- Mochi's generic agent with class fields and type parameters:
  tsc 5.4 inferred the wrong variance under
  `useDefineForClassFields: true` (fixed in 5.5).
- Mochi's structural typing via interface with mixed sync + async
  methods: tsc 5.5 flagged on `noUncheckedIndexedAccess` when the
  interface had an index signature (fixed in 5.6).
- Mochi's Result chain via `.map().andThen().orElse()`: tsc
  reports incorrect type narrowing on the closure-bound generic.
  Bug filed (TS #59232); not yet fixed.
- eslint `@typescript-eslint/no-misused-promises` flags Mochi's
  agent `loop()` method (an async method invoked without await
  inside the constructor). We suppress via runtime void operator
  (`void this.loop()`).

**Likelihood**: high (active set; new false positives expected
quarterly).

**Impact**: low to medium. Each false positive is a CI breakage.
We either change the emit pattern, suppress per-file with
`// @ts-expect-error`, or wait for upstream fix.

**Mitigation**:

- Maintain a per-pattern decision log in
  `internal/transpiler3/typescript/typecheck_quirks.md` (internal,
  not deployed). Each row: pattern, version, decision, upstream
  issue, expected fix release.
- Avoid `// @ts-ignore` lines in emitted code. Suppressions are
  easy to spread; we prefer fixing the emit pattern.
- Escalate per-pattern: every new false positive becomes a Mochi
  team triage item with one of {fix emit, suppress, wait}.
- Quarterly bump of `typescript` + `@typescript-eslint` pins
  includes review of the quirks log.

**Owner**: type-system maintainer.

## Rejected alternatives

### F1: Compile via Babel

**Description**: Babel (`babeljs.io`) is the historically dominant
JavaScript transpiler. It supports JSX, TypeScript syntax (via
`@babel/preset-typescript`), and stage-0 to stage-4 proposals. The
pitch: emit JavaScript directly via Babel, skip TypeScript
entirely.

We considered emitting Babel-flavoured JavaScript with JSDoc type
hints. The pitch: smaller emit, no TypeScript dependency, faster
build times.

**Evaluation**:

- Babel does not type-check. It strips TypeScript annotations
  without verifying them. We would lose Tier 2 of the gate
  hierarchy.
- Babel's TypeScript support is "best effort"; some valid
  TypeScript constructs are silently miscompiled. Examples from
  the Babel issue tracker: `satisfies` operator (Babel #14906),
  const type parameters (Babel #15518), `using` declarations
  (Babel #15915).
- Build pipeline duplication: even if we used Babel for emit,
  we'd still need tsc for type-checking. Two tools, two configs,
  two version-pin chains.
- The tsc emitter is mature and tracks the language spec exactly.
  Babel lags by 6-12 months on new features.
- Source maps from Babel are competent but tsc's are equal.
- Babel runtime helpers (`@babel/runtime`) are about 30 KB; we
  would need to ship them.

**Decision**: reject. tsc owns the emit pipeline. Babel is not
used.

**References**:

- Babel documentation, `babeljs.io/docs/`
- TypeScript vs Babel comparison, TypeScript GitHub wiki
- Babel issues #14906, #15518, #15915

### F2: esbuild as the emitter (skip tsc)

**Description**: esbuild can also strip TypeScript annotations and
emit JavaScript. It's much faster than tsc (about 30x). The pitch:
use esbuild for emit, skip tsc entirely, rely on the user's IDE for
type-checking.

**Evaluation**:

- esbuild does not type-check. Same gap as Babel. We'd lose Tier
  2 of the gate hierarchy.
- esbuild's TypeScript support is limited to syntactic stripping;
  it does not understand semantic features like `const`
  assertions, `satisfies`, or generic type inference. Some
  Mochi-emitted patterns would not survive.
- esbuild has different parse behaviour from tsc in edge cases
  (`enum` reverse mappings, namespace declarations). We'd need to
  constrain the emit to "the intersection of what tsc and esbuild
  agree on", which is narrower than just tsc.
- We already use esbuild for the browser bundle (where its speed
  matters for bundling, not type stripping). Using it as the
  emitter would double its scope.
- No project references support: esbuild treats each file
  independently. We lose the multi-target build (Node / Deno /
  Bun / browser via separate tsconfigs).

**Decision**: reject as the primary emitter. esbuild remains the
browser bundler. tsc is the primary emitter.

**References**:

- esbuild TypeScript documentation,
  `esbuild.github.io/content-types/#typescript`
- esbuild's stance on type-checking,
  `github.com/evanw/esbuild/issues/2914`

### F3: JSDoc-only (no .ts files)

**Description**: TypeScript supports type annotations via JSDoc
comments on `.js` files. `@param`, `@returns`, `@type`,
`@template`. The pitch: emit pure JavaScript with JSDoc, skip
TypeScript syntax entirely. Users can use the package without
TypeScript tooling.

**Evaluation**:

- JSDoc is verbose. A simple typed function like
  `function add(a: number, b: number): number { return a + b }`
  becomes:
  ```javascript
  /**
   * @param {number} a
   * @param {number} b
   * @returns {number}
   */
  function add(a, b) { return a + b }
  ```
  Three lines of comment per function. Mochi's emit corpus would
  triple in line count.
- JSDoc has incomplete support for advanced TypeScript features.
  `satisfies`, `const` type parameters, mapped types, conditional
  types, template literal types are all painful or impossible in
  JSDoc.
- tsc supports `.js` + JSDoc via `--allowJs --checkJs`. The
  type-check works, but the IDE experience is less ergonomic than
  on `.ts` files (smaller hovers, less precise completions).
- Source map debugging in `.js` with JSDoc is identical to plain
  `.js`. No win.
- Some users want to read source. `.ts` source with concise
  inline types is more readable than `.js` with verbose JSDoc.

**Decision**: reject. Emit `.ts` source. The compiled `.js`
artifacts in `dist/` have JSDoc-style comments stripped (tsc emits
clean JS).

**References**:

- JSDoc documentation, `jsdoc.app`
- TypeScript JSDoc reference,
  `typescriptlang.org/docs/handbook/jsdoc-supported-types.html`
- TC39 Type Annotations Proposal (Stage 1, 2022),
  `github.com/tc39/proposal-type-annotations`

### F4: Webpack as the bundler

**Description**: Webpack (`webpack.js.org`) is the historically
dominant JavaScript bundler. The pitch: ship a Webpack config for
the browser bundle instead of esbuild.

**Evaluation**:

- Webpack is slow. A typical Mochi-app bundle takes about 8
  seconds on M2 vs esbuild's 200 ms. The 40x difference matters
  for iteration speed.
- Webpack configuration is famously complex. The minimum config
  to bundle ESM with TypeScript + tree-shaking + source maps is
  about 100 lines. esbuild does the same in 10 lines of CLI
  flags.
- Webpack 5+ supports ESM but defaults to CommonJS for legacy
  reasons. Configuring for ESM-only output requires fighting the
  defaults.
- Webpack's tree-shaking is competent but not as aggressive as
  esbuild's or Rollup's. Mochi's runtime stub would tree-shake
  about 10-20% less effectively (about 30 KB extra in the bundle).
- Plugin ecosystem: Webpack's plugin model is mature and
  extensive. esbuild's plugin API is younger but covers our
  needs. The plugins we use (sourcemap, minify, ESM) are all
  built in to esbuild.
- Long-running daemon mode: Webpack's `--watch` is faster than its
  cold build, but esbuild's `--watch` is faster than Webpack's
  `--watch`.

**Decision**: reject. esbuild is the browser bundler.

**References**:

- Webpack documentation, `webpack.js.org`
- Webpack ESM support,
  `webpack.js.org/configuration/output/#outputmodule`
- esbuild vs Webpack benchmark,
  `esbuild.github.io/faq/#benchmark-details`

### F5: Rollup as the bundler

**Description**: Rollup (`rollupjs.org`) is the second
historically dominant bundler. It pioneered tree-shaking and was
the bundler of choice for libraries (Vue, React, Svelte all used
Rollup for production builds at various points).

**Evaluation**:

- Rollup is faster than Webpack but slower than esbuild (about
  4 seconds vs 200 ms for our typical app).
- Rollup's tree-shaking is excellent; comparable to esbuild's.
- ESM-native output: Rollup defaults to ESM, no fighting required.
- Plugin ecosystem: Rollup's `@rollup/plugin-typescript`,
  `@rollup/plugin-node-resolve`, `@rollup/plugin-commonjs` cover
  our needs. esbuild does the same without plugins.
- Rolldown (`rolldown.rs`, in development): a Rust rewrite of
  Rollup that promises esbuild-class speed with Rollup's API.
  Announced 2024, stable timeline unclear.
- Library-author ergonomics: Rollup's output options
  (`output.format = "esm"`, `output.preserveModules`, etc.) are
  more granular than esbuild's. For shipping a library to npm
  (where preserving the module graph matters for downstream
  tree-shaking), Rollup has an edge.

**Decision**: reject for v1. esbuild's speed wins. Rollup remains
on the v2 evaluation list if Rolldown stabilises and offers
materially better library output (preserveModules + tree-shake
hints).

**References**:

- Rollup documentation, `rollupjs.org`
- Rolldown announcement,
  `voidzero.dev/posts/announcing-rolldown`
- Library bundling guide,
  `web.dev/articles/the-state-of-bundlers`

### F6: RxJS for streams + agents

**Description**: RxJS (`rxjs.dev`) is the dominant reactive
streams library for JavaScript. The pitch: lower Mochi streams to
`Observable<T>` and agents to RxJS `Subject<T>`. Mochi gets all
the operators (`map`, `filter`, `mergeMap`, `combineLatest`,
`debounceTime`, etc.) for free.

**Evaluation**:

- RxJS is a hard dependency. Mochi-emitted code would always pull
  RxJS (about 70 KB minified, 25 KB gzipped). Tree-shaking helps
  but most apps use a non-trivial subset.
- The RxJS mental model (cold vs hot observables, subjects vs
  observables, schedulers) is non-trivial. Users debugging Mochi
  code would need to learn RxJS too.
- Cancellation in RxJS uses `Subscription.unsubscribe()`, not
  `AbortController`. Mochi's agent supervision uses
  `AbortController` natively. Mapping the two is possible but
  awkward.
- Async iterators are part of the language standard; observables
  are not. Async iterators are interoperable with `for await`,
  iterator helpers, and the entire ES2024 toolchain. Observables
  require RxJS-specific operators.
- TypeScript inference works well with async iterators
  (`AsyncIterable<T>`); RxJS's `Observable<T>` requires more
  ceremony.
- Mochi-emitted code becomes less portable: a user reading the
  source would need RxJS knowledge.

**Decision**: reject. `AsyncIterableQueue` + `AbortController` are
the canonical concurrency primitives. RxJS is an opt-in
integration the user can layer on top.

**References**:

- RxJS documentation, `rxjs.dev`
- Async iterators TC39 proposal,
  `github.com/tc39/proposal-async-iteration`
- Comparison: async iterators vs observables,
  `staltz.com/why-we-need-an-explicit-asynciterable-iterator-protocol`
- The shared decisions anchor (decision 3) rejects RxJS

## Future-track candidates

### F1: WebAssembly via WASI

**Description**: WebAssembly System Interface (WASI) is a
standardised host API for WASM modules. WASI 0.2 (released
2024-Q1) provides POSIX-like file IO, network sockets, clock
access, and randomness. A Mochi program compiled to WASI runs on
any WASI-capable host: wasmtime, Wasmer, wasmtime in Node 22+,
Deno 1.39+ (with `--unstable-wasi`).

The pitch: a single WASM artifact runs everywhere WASI runs. The
distribution story (no Node, no Deno, no Bun required at the host)
is simpler than the current four-runtime approach.

**Gating signal**: WASI 0.2 is GA but the ecosystem (Node WASI
support, Deno WASI support, Bun WASI support) is uneven as of
2026-Q1. v2 candidate when at least three of the four runtimes
have stable WASI 0.2 support.

Mochi would need a TypeScript-to-WASM compiler. AssemblyScript
(`assemblyscript.org`) is the canonical TypeScript-subset to WASM
compiler, but its TypeScript support is a subset (no `any`, no
unions, limited generics). We'd need to either constrain the
emit further to AssemblyScript's subset (limiting the language) or
build a custom emitter (high cost).

An alternative: compile Mochi to C via MEP-45, then to WASM via
clang or emscripten. This bypasses TypeScript entirely. The MEP-45
WASM target is on its own future-track list.

### F2: ts-blank-space for zero-build

**Description**: ts-blank-space (`bloomberg.github.io/ts-blank-space`)
is a TypeScript-to-JavaScript transpiler that strips types by
replacing them with spaces, preserving line and column positions.
The output requires no source map: the JS file is byte-for-byte
aligned with the TS source.

Node 22.6 added `--experimental-strip-types` (April 2024) using a
ts-blank-space-style approach. Running `.ts` files directly without
a build step is now possible: `node --experimental-strip-types
foo.ts`. Node 23 made this stable.

Deno has supported direct `.ts` execution since v1; Bun has
supported it since v1.

The pitch: skip the build step entirely. Mochi emits `.ts` source;
users run it directly. No `dist/`, no `tsc --build`, no `npm
pack`.

**Gating signal**: Node 24 LTS (October 2026) is expected to make
`--experimental-strip-types` the default behavior. v2 of MEP-52
(planned 2028-Q1) can offer a "zero-build" mode that emits only
`.ts` source, with the user invoking via direct `.ts` execution.

Trade-offs:

- Pro: zero build time. No `dist/` directory. Smaller npm tarball.
- Con: type-stripping doesn't catch type errors at install time.
  The user's first run is when typos surface.
- Con: `tsc --build` outputs are already incremental. Cold builds
  are about 8 seconds; warm are 400 ms. The build cost is not
  enormous.
- Con: the dist `.js` is what npm consumers want for ESM-only
  Node / Bun / Deno. Zero-build means users do their own tsc.

We may offer both modes in v2: `mochi build --target=ts-zero-build`
for the source-only path, `mochi build --target=npm-package` for
the current build path.

### F3: Bun-native compile

**Description**: Bun 1.1 has a `bun build --compile` command that
produces a standalone executable bundling the Bun runtime + the
user's code. The output is a single binary on linux x86_64, linux
aarch64, macos arm64, or windows x86_64. No Node, no install, no
package.json required at the host.

The pitch: a single binary distribution channel, like Go or Rust.
Mochi apps ship as `mochi-app` and run.

**Gating signal**: Bun 1.1 GA (April 2024). The `--compile`
feature is stable but the resulting binary size is about 95 MB for
a hello-world (Bun bundles its entire runtime). v2 candidate when
Bun's binary size drops below 50 MB (Bun team has stated this is a
2026-2027 target).

Trade-offs:

- Pro: simple distribution.
- Con: binary size. 95 MB for hello world; about 120 MB for a real
  app. Go is 5 MB / 15 MB; Rust is 1 MB / 5 MB; Bun is 20x larger.
- Con: cross-compilation. Bun's compile is host-local; you cannot
  compile a linux binary from macos. We'd need a CI matrix to
  build for each target.
- Con: ties Mochi to Bun specifically. Users on Deno or Node would
  not have an equivalent.

The Mochi binary-distribution story is better served by MEP-45 (C)
than by Bun compile.

### F4: Cloudflare Workers

**Description**: Cloudflare Workers (`workers.cloudflare.com`) is
a serverless platform that runs JavaScript on V8 isolates at
Cloudflare's edge network. The runtime API is Web Workers + Web
Streams + Web Crypto + Fetch + a Cloudflare-specific KV / Durable
Objects / R2 layer.

The pitch: Mochi-emitted code targeting Cloudflare Workers can
deploy globally with no server management. The runtime is fast
(cold start under 5 ms) and the platform is generous (100k
requests / day free tier).

**Gating signal**: Cloudflare Workers' Node compatibility mode
(`nodejs_compat` flag) shipped in 2024. It supports `node:crypto`,
`node:buffer`, `node:util`, `node:http`, `node:stream`, `node:fs`
(limited). Mochi-emitted code that uses these can deploy via
`wrangler deploy`.

v2 candidate when:

1. Cloudflare's `nodejs_compat` covers all `node:*` APIs Mochi's
   runtime uses (Mochi's `node:net` / `node:dgram` are gaps).
2. Wrangler's TypeScript support matures (TypeScript 5.6 floor
   support landed in 2024-Q4).
3. The `worker` condition in our `exports` map can route to a
   Workers-specific bundle.

The Workers target would be a fifth runtime in the matrix (Node,
Deno, Bun, browser, Workers). We do not gate on it in v1 because
the matrix cost is already significant.

Alternative edge platforms (Deno Deploy, Fastly Compute, AWS
Lambda@Edge, Vercel Edge Functions) have similar APIs and are
covered by the same Workers candidate. The first one we support
will be Workers because of its market share.

## Summary

The risk register has 15 entries, all with concrete mitigations.
The load-bearing risks (R1 Promise.withResolvers polyfill, R2
bigint perf, R3 tree-shake, R4 supply-chain) are V8-era risks we
accept as the cost of targeting JavaScript. The build-pipeline
risks (R5 TypeScript bumps, R6 Deno / Bun divergence, R13 OIDC
failure, R14 reproducibility) are operational and tracked in
[[10-build-system]] and [[11-testing-gates]].

The rejected alternatives are Babel (F1), esbuild-only (F2),
JSDoc-only (F3), Webpack (F4), Rollup (F5), RxJS (F6). The
decisions are documented above.

The future-track candidates are WASI (F1), ts-blank-space zero-build
(F2), Bun-native compile (F3), Cloudflare Workers (F4). All four
are v2 gated on external signals (WASI ecosystem maturity, Node
24 LTS stripping defaults, Bun binary size, Cloudflare nodejs_compat
coverage).

## References

- ECMAScript proposals (Stage 4 in 2024): iterator helpers,
  `Promise.withResolvers`, `Set` methods (`union`, etc.), regular
  expression `/v` flag, `Object.groupBy`, `using` declarations
- Node 22 LTS release notes, `nodejs.org/en/blog/release/v22.0.0`
- Deno 2.0 release notes, `deno.com/blog/v2.0`
- Bun 1.1 release notes, `bun.sh/blog/bun-v1.1`
- WASI documentation, `wasi.dev`
- AssemblyScript documentation, `assemblyscript.org`
- ts-blank-space documentation,
  `bloomberg.github.io/ts-blank-space`
- Node `--experimental-strip-types` PR,
  `github.com/nodejs/node/pull/53725`
- Cloudflare Workers Node compat,
  `developers.cloudflare.com/workers/runtime-apis/nodejs/`
- Sigstore project, `sigstore.dev`
- npm Trusted Publishing, `docs.npmjs.com/trusted-publishers/`
- TypeScript issues #50465, #56261, #57389, #59232
- npm issue #7234 (Windows tarball case sensitivity)
- The shared decisions anchor
- [[10-build-system]]
- [[11-testing-gates]]
