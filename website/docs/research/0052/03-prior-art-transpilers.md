---
title: "MEP-52 research note 03, Prior art and source-to-TypeScript/JavaScript tooling"
description: "Survey of source-to-JS transpilers, type checkers, alternative TypeScript stacks, and language-to-JS pipelines that informed MEP-52. From Babel and SWC to ReScript, Fable, PureScript, Kotlin/JS, ScalaJS, GopherJS, AssemblyScript, Pyodide, JSII, and the sibling Mochi backends."
sidebar_position: 3
---

# MEP-52 research note 03, Prior art and source-to-TypeScript/JavaScript tooling

Author: research pass for MEP-52 (Mochi to TypeScript / JavaScript transpiler).
Date: 2026-05-23 16:50 (GMT+7).
Sources: TC39 proposal-tracker at github.com/tc39/proposals; TypeScript
release notes at devblogs.microsoft.com/typescript; Babel docs at
babeljs.io; SWC documentation at swc.rs; esbuild manual at
esbuild.github.io; the Bun blog at bun.sh; sucrase project on
github.com/alangpierce/sucrase; ts-blank-space release notes from Bloomberg
(github.com/bloomberg/ts-blank-space); CoffeeScript manual at
coffeescript.org; ReScript at rescript-lang.org; the legacy BuckleScript
documentation at bucklescript.github.io; Fable at fable.io and the F#
foundation blog; Elm at elm-lang.org; PureScript at purescript.org;
Kotlin/JS at kotlinlang.org; Scala.js at scala-js.org; GopherJS at
github.com/gopherjs/gopherjs; TinyGo at tinygo.org with its WASM target;
Pyodide at pyodide.org; Brython at brython.info; Transcrypt at
transcrypt.org; Skulpt at skulpt.org; AssemblyScript at
assemblyscript.org; Hegel at hegel.js.org; Flow at flow.org; JSII at
github.com/aws/jsii; the Mochi sibling research bundles (MEP-45 through
MEP-51); the WHATWG HTML Living Standard for ECMAScript module loading;
the V8 / SpiderMonkey / JavaScriptCore blogs for engine-level data;
Node.js 22 LTS release notes (April 2024); Deno 2.0 release notes
(October 2024); Bun 1.1 release notes (April 2024); the W3C TC39
ECMAScript 2024 specification (June 2024); npm 10 release notes
(September 2023); pnpm 9 and Yarn 4 docs; the JSR (jsr.io) launch blog
from the Deno team (March 2024); the WebAssembly Component Model draft
(2025-Q3).

This note surveys the prior art that a Mochi-to-TypeScript transpiler
must build on, learn from, or deliberately diverge from. The survey
groups projects by *kind*: (1) JavaScript-to-JavaScript transformers
and bundlers (Babel, SWC, esbuild, sucrase, the Bun transpiler), (2)
TypeScript-as-tooling (tsc, ts-blank-space, ts-node, tsx), (3)
JavaScript-supersets that compile down (CoffeeScript, LiveScript,
Dart-on-JS), (4) statically-typed languages with a JS backend (ReScript,
ReasonML / BuckleScript, Fable, Elm, PureScript, Kotlin/JS, Scala.js,
GopherJS, ClojureScript), (5) cross-language runtime hosts on JS or
WASM (Pyodide, Brython, Transcrypt, Skulpt, Blazor WebAssembly), (6)
WebAssembly-first stacks with JS interop (AssemblyScript, TinyGo,
Emscripten, wasm-bindgen, the WebAssembly Component Model), (7)
alternative type checkers and inference engines (Hegel, Flow, JSII),
and (8) the Mochi sibling backends (MEP-45 through MEP-51). Each entry
names the project, summarises what it does, states the specific lesson
MEP-52 takes, and either inherits or rejects the pattern with the
reason.

The single biggest takeaway from this survey: **TypeScript-as-emit-target
is the modern lingua franca of compile-to-JS**. Of the ~30 projects
surveyed, the post-2020 designs (Bun, esbuild, SWC, ts-blank-space,
JSII, Fable since v4, Kotlin/JS since 1.8) all converge on emitting
TypeScript source rather than untyped JavaScript. The legacy designs
(CoffeeScript, Elm, PureScript, GopherJS, ClojureScript) emit raw
JavaScript with custom shim runtimes, which made them difficult to
type-check, hard to integrate with modern toolchains, and outside the
TypeScript-LSP feedback loop that today's developers expect. MEP-52
joins the modern cohort. The second takeaway: **no widely-adopted
typed-source-language-to-TypeScript transpiler exists with the
specific shape Mochi wants**. JSII (AWS, TypeScript to multi-language)
goes the opposite direction. Fable / Kotlin/JS / Scala.js are the
nearest analogues, all of which lower a typed source language onto JS
or TS with a hand-rolled runtime; their lessons translate directly.
MEP-52 occupies the niche of "small typed functional-imperative
language compiles to strict TypeScript with a tiny ESM runtime", which
is genuinely under-explored. The third takeaway: **the build system,
not the transpiler, dominates user pain**. Every successful
compile-to-JS language ships with a turnkey build that takes the user
from `mylang init` to `mylang publish` without intermediate manual
`tsc` or `npm` invocations. MEP-52's `mochi build` umbrella commands
mirror this lesson; see [[10-build-system]] for the full design.

## 1. Babel, the legacy JavaScript-to-JavaScript transformer

**What it is.** Babel (babeljs.io, originally `6to5` by Sebastian
McKenzie, 2014) is a configurable JavaScript-to-JavaScript transformer.
It parses modern JS (ESnext) and emits down to an older target
(typically ES5 or ES2015). Babel popularised the "use tomorrow's
JavaScript today" pattern: write code with new syntax, transpile to
broadly-supported syntax, ship. As of 2026 Babel 7.24 is the current
stable line; Babel maintains the parser, traverse, types, and a plugin
ecosystem of several thousand transforms.

Babel is the canonical example of a *plugin-driven* JS transformer.
The pipeline is: `parser` produces an AST, `traverse` walks it, `types`
provides node constructors, plugins apply transformations as visitors,
`generator` emits the result. Babel can also strip TypeScript
annotations via `@babel/preset-typescript`, but it does not type-check.

**Lesson for MEP-52.** Babel showed the world that a JS source-to-source
transformer is viable, modular, and useful. The plugin architecture
let the community ship transforms for every TC39 proposal at every
stage; this is how `async/await`, destructuring, optional chaining,
nullish coalescing, decorators all became practical in production
before native engine support landed. Babel also normalised the
"separate type-checking from emit" separation that informs the modern
TS pipeline (tsc for types, SWC / esbuild / Babel for emit).

**What we take.** The pipeline shape (parse, transform, emit) is the
right model for `transpiler3/typescript/`. We use the same separation
of concerns: `lower/` builds the TS tree, `emit/` prints it, and a
`tsc --noEmit` pass independently type-checks. We deliberately keep
Babel out of the build, because:

**What we reject.** Babel is too slow (~5-10x slower than SWC and
~20-50x slower than esbuild on equivalent workloads). Babel does not
type-check; it only strips annotations. Babel's plugin sprawl is a
liability when you want a deterministic emit; we control the entire
pipeline ourselves and need no plugins.

## 2. SWC, the Rust-written Babel replacement

**What it is.** SWC (swc.rs, by Donny Wals at Vercel, 2018) is a
JavaScript / TypeScript transformer written in Rust. It is API- and
config-compatible with Babel for the most common use cases, and runs
~10-20x faster. Next.js (Vercel's React framework) replaced Babel
with SWC as the default transformer in Next.js 12 (October 2021).
Turbopack (Vercel's bundler successor to webpack) uses SWC. Deno
uses SWC internally for its TypeScript stripping. As of 2026 SWC 1.x
is the stable line; SWC 2.0 is in beta.

SWC handles: ES2024 to ES5 / ES2015 transpilation, TypeScript
annotation stripping, JSX transformation, minification (via swc_minify),
and source maps. SWC does not type-check (same as Babel).

**Lesson for MEP-52.** A Rust-side transformer is fast enough to run
in CI per save. SWC's "single-binary, no Node prereq" deployability is
attractive (matches Mochi's own Go-side compiler shape: the Mochi CLI
is a single Go binary). SWC's TS strip mode validates that "TypeScript
source ships as JavaScript with annotations removed" is a viable
production pattern.

**What we take.** The bytecode-style fast emit ethos. Our Go-side
emitter prints TypeScript directly, never round-tripping through a
JavaScript parser. The "do not type-check in the same pass as emit"
separation matches what SWC does, deliberately.

**What we reject.** Embedding SWC in the Go build. We already have a
working Go-side IR and emitter from MEP-45 (aotir); pulling SWC's Rust
toolchain into the Mochi build would add platform and CI complexity for
no immediate benefit. We ship `prettier` (a TypeScript user-space
formatter) as a post-pass; that's where SWC's territory ends and ours
begins.

## 3. esbuild, the Go-written ultra-fast bundler and transpiler

**What it is.** esbuild (esbuild.github.io, by Evan Wallace of Figma,
2020) is a JavaScript / TypeScript bundler and minifier written in Go.
It is the speed-of-light reference for JS tooling: typical bundle of a
50 kLOC TS project takes ~100ms (vs ~10-30s for webpack and 1-3s for
SWC). Vite (Evan You's frontend dev server, the React / Vue / Svelte
darling) uses esbuild for dev mode and Rollup for production bundles.
As of 2026 esbuild 0.24+ is the current stable line; the API and CLI
have been stable since 2021.

esbuild handles: ES2024 to legacy transpilation, TS annotation
stripping, JSX, CSS, asset bundling, tree-shaking, dead-code
elimination, source maps, code splitting. esbuild does *not*
type-check (consistent pattern with Babel and SWC).

**Lesson for MEP-52.** A Go-side JS tool is plausibly fast enough to
serve as Mochi's browser-bundle backend. esbuild's tree-shaking proves
that ESM with `"sideEffects": false` and named exports is sufficient
to eliminate dead code in a small bundle.

**What we take.** esbuild is the canonical browser-bundle path for
MEP-52: `mochi build --target=browser-bundle` shells out to esbuild
with our generated `.ts` source. The CLI integration is one-line:
`esbuild --bundle --format=esm --target=es2024 dist/node/index.js
--outfile=dist/browser/index.js`. We rely on esbuild for the bundle
graph, tree-shaking, and minification.

**What we reject.** We do not use esbuild as the primary tsc
replacement. `tsc --noEmit` is the type-checker; esbuild is the
bundler. We do not let esbuild emit our `.ts` source to `.js` because
`tsc` already does that under our explicit control, and esbuild's
emit lacks `.d.ts` generation (which we need for npm publish).

## 4. The TypeScript compiler itself (tsc)

**What it is.** tsc (typescriptlang.org, originally by Anders
Hejlsberg's team at Microsoft, 2012) is the official TypeScript
compiler. It both type-checks and emits JavaScript. As of 2026 the
stable line is TypeScript 5.6 (September 2024), which added iterator
helper types, `--noUncheckedSideEffectImports`, and
`--rewriteRelativeImportExtensions`. TypeScript 5.7 (November 2024)
added `--build` performance improvements; 5.8 (March 2025) added
typed implementation of decorators with metadata; 5.9 (Q3 2025) added
the `using` / `Symbol.dispose` runtime helpers (`__addDisposableResource`).

tsc is *the* canonical TS toolchain. Every other JS/TS tool defers
to tsc for type-checking: WebStorm and VS Code use the language
service, ESLint uses `@typescript-eslint/parser` which bridges to
tsc's checker, Vitest and Jest defer to tsc for `.d.ts` generation,
JSDoc tooling uses tsc.

**Lesson for MEP-52.** tsc is the gate. Anything Mochi emits must pass
`tsc --noEmit --strict --noUncheckedIndexedAccess
--exactOptionalPropertyTypes`, or the emit is wrong. Mochi does not
re-implement type-checking; it leans on tsc as the verifier. This
matches MEP-51's choice to lean on mypy and pyright.

**What we take.** Everything. tsc is the compiler. `tsc --build` is
the dist builder. `tsc --noEmit` is the type-check gate. `tsc --watch`
is the dev-mode incremental verifier. The four `tsconfig.*.json` files
(node, deno, bun, browser) all run through tsc.

**What we reject.** Pulling tsc into the Go-side compiler binary.
tsc is a TypeScript / JavaScript program; pulling it into a Go binary
via embedded V8 or QuickJS is technically possible (see GopherJS for
the upside-down version) but unnecessary. Mochi's CLI shells out to
the user's local `tsc` binary, or installs one into a project-local
`node_modules/typescript`.

## 5. sucrase, the type-stripping fast TS transpiler

**What it is.** sucrase (github.com/alangpierce/sucrase, by Alan Pierce,
2018) is a TypeScript-and-JSX-stripping transpiler written in
TypeScript. It is the unmaintained predecessor to ts-blank-space (see
next entry); it does single-pass parsing and emits stripped JS in
under 100ms for a 10 kLOC project. sucrase does not type-check, does
not bundle, and does not implement most modern TS features. Used
internally at Benchling, Modulz, and several other JS startups
2018-2023. As of 2026 the project is in maintenance mode.

**Lesson for MEP-52.** The "type-strip-only" mode for dev iteration is
useful. Node 22's `--experimental-strip-types` flag (April 2024)
acknowledges this: developers want to `node mod.ts` directly without
`tsc` in the loop for fast iteration. The pattern matters for the dev
inner loop.

**What we take.** Mochi's dev mode (`mochi run app.mochi`) can emit
`.ts` and then invoke `node --experimental-strip-types
out.ts` directly, skipping `tsc` entirely. This matches sucrase's
philosophy: type annotations are *information*, not *runtime
behaviour*, and a strip pass is enough for dev.

**What we reject.** sucrase as a runtime dependency. The Node 22
strip-types flag is the modern equivalent and ships in the runtime;
we use it directly. sucrase was a stopgap while Node lacked native
TS support.

## 6. Bun's transpiler

**What it is.** Bun (bun.sh, by Jarred Sumner, 2021, first stable
release September 2023, Bun 1.1 April 2024) is a JavaScript runtime
plus an integrated transpiler, bundler, package manager, and test
runner. The runtime is JavaScriptCore (the Safari engine) plus Zig-
written infrastructure. Bun's transpiler handles TS, JSX, and modern
JS in a single Zig-side pass that approaches esbuild speeds. Bun
runs `.ts` files directly via `bun run mod.ts` without a separate
transpile step; the runtime transparently strips types.

As of 2026 Bun 1.2 is the current stable line. Bun is heavily used by
React / Next.js developers as a faster `npm` and `node` replacement.

**Lesson for MEP-52.** Bun proved that a single-binary all-in-one
JS/TS runtime is desirable. Bun's `bun run mod.ts` flow is what
Node 22's `--experimental-strip-types` is catching up to. Bun's
package manager is much faster than npm (`bun install` for a fresh
React app is ~3s vs ~20s for npm).

**What we take.** Bun is a tier-1 runtime target in MEP-52. The
generated `package.json` `"engines"` field pins `"bun": ">=1.1.0"`.
Bun consumes our published npm package via the `"bun"` conditional
export. Bun's test runner (`bun test`) is supported as an alt for
Vitest / Jest in `mochi test`.

**What we reject.** Bun as the *only* runtime. The four-runtime
target matrix (Node, Deno, Bun, browser) is mandated by user
diversity; Bun-only would alienate the Deno-and-Node camps.

## 7. ts-blank-space, the modern TS-strip emitter

**What it is.** ts-blank-space (github.com/bloomberg/ts-blank-space,
by Bloomberg's open-source group, 2024) is a TypeScript-to-JavaScript
transformer that replaces type annotations with whitespace, preserving
exact source-line numbers and column offsets. The output is a
character-for-character match with the TS source where types are
replaced by spaces and TS-only syntactic forms (interface, enum, etc.)
are rewritten as zero-width inert syntax. The motivation: native TS
support in Node, Deno, and browsers is hampered by the source-map
overhead of full transpilation; ts-blank-space eliminates the source
map need by preserving offsets exactly.

ts-blank-space underpins the TC39 "Type Annotations" proposal (Stage 1
since 2022; Microsoft + Bloomberg + Igalia co-champion). The proposal
is that JavaScript engines would *parse and ignore* type annotations,
making TS syntax a native JS subset with no transpile step.

**Lesson for MEP-52.** The future of TS is "engines parse it natively;
no transpile step". MEP-52's bet on TypeScript-as-source-of-truth is
on the right side of this trend. Today we transpile via tsc to ship
to old environments; tomorrow we may ship `.ts` directly.

**What we take.** The source-offset-preserving emit pattern. Our
Go-side emitter is careful to preserve column offsets where possible,
so source maps are small (or trivially regenerable). The "treat TS as
a JS dialect, not a separate language" mental model.

**What we reject.** Adopting ts-blank-space as our emit backend. We
control the entire pipeline; our `tsc --build` to `dist/*.js` flow is
the canonical path. ts-blank-space is interesting as an alt mode, not
the default.

## 8. CoffeeScript, the original compile-to-JS language

**What it is.** CoffeeScript (coffeescript.org, by Jeremy Ashkenas,
2009) is a Ruby/Python-flavoured language that compiles to JavaScript.
Significant whitespace, list comprehensions, implicit returns, fat-arrow
functions for `this`-binding, splats for variadic args, destructuring,
existential operator (`?`). CoffeeScript was hugely popular 2010-2014
in the Ruby on Rails community (it was bundled with Rails 3.1 default
asset pipeline). As of 2026 CoffeeScript 2.7 is the current stable
line; the language is in long-term maintenance mode and has lost most
of its userbase to ES6+ (which absorbed many of CoffeeScript's
features) and TypeScript.

**Lesson for MEP-52.** CoffeeScript demonstrated both the upside and
the long-tail risk of compile-to-JS languages. Upside: many of its
syntactic innovations (`=>`, destructuring, classes, splats, string
interpolation) were absorbed into ES6 and beyond, validating that
JavaScript syntax can borrow from typed-functional languages.
Downside: when the host language's natural syntax catches up with
the source language, the source language becomes redundant.
CoffeeScript's value proposition collapsed once ES6 shipped.

**What we take.** The lesson that the source language must add *more
than syntactic sugar*. Mochi adds type safety, ADTs, agents, the
query DSL, the Datalog engine, MochiResult, and the cross-runtime
compatibility story; these are not absorbable by JS evolution in any
realistic timeframe. The risk is mitigated.

**What we reject.** Significant whitespace. Mochi uses braces and
explicit blocks (matching the C / Go / TypeScript / Rust / Swift
mainstream); the Python-significant-whitespace family is its own
camp (see MEP-51).

## 9. LiveScript, Iced CoffeeScript, and the CoffeeScript adjacent

**What they were.** LiveScript (livescript.net, by George Zahariev,
2011) is a CoffeeScript-derived language with stronger functional
programming features: pipe operators, partial application, currying,
Haskell-style list comprehensions, pattern matching on function
arguments. Iced CoffeeScript (maxtaco.github.io/coffee-script/, by
Maxwell Krohn, 2011) added explicit `await` / `defer` to CoffeeScript
before ES7 async/await existed.

Both are in long-term maintenance, with negligible new adoption.

**Lesson for MEP-52.** LiveScript proved that functional patterns
(pipes, partial application, pattern matching) are valuable in a
JS-targeted language. ES2024+ has not fully absorbed all of them
(the pipe-operator proposal is Stage 2; pattern matching is Stage 1).
Mochi's pipeline DSL fills this gap on the Mochi side; the lowering
to TS is by `.then`-chained method calls or local helpers.

**What we take.** The validation that pipe-style data transformation
is desirable. Mochi's `|` operator lowers to a chain of `.map / .filter
/ .reduce` calls on the TS side.

**What we reject.** Stage 2 TC39 proposals as lowering targets. Mochi
emits today's TS, not tomorrow's; the pipe operator is too unstable
to encode.

## 10. ClojureScript, Clojure on JavaScript

**What it is.** ClojureScript (clojurescript.org, by Rich Hickey and
team, 2011) is the JavaScript backend for Clojure. It compiles Clojure
source to Google Closure Compiler-compatible JS, then runs the GCC
advanced-optimisation pass for tree-shaking and minification. ClojureScript
inherits Clojure's persistent data structures (Hash Array Mapped Tries
for vectors and maps), STM-style atom semantics, and immutability-by-
default. As of 2026 ClojureScript 1.11+ is current; the userbase is
solid but small (~10-20k active developers per the State of Clojure
2024 survey).

ClojureScript ships a substantial runtime (`cljs.core`, ~400KB
unminified, ~50KB after GCC advanced optimisation) implementing the
persistent collections, multimethods, protocols, and atoms.

**Lesson for MEP-52.** A Lisp on JavaScript is a hard sell to the
TypeScript-curve-shaped audience. ClojureScript's persistent
collections are technically excellent but add weight; for a small-
runtime language Mochi must avoid this trap. Mochi uses mutable
`Map<K, V>` and `Array<T>` directly (with `readonly` views when the
type permits), not persistent HAMTs.

**What we take.** The principle that immutable-by-default data shapes
help reason about programs. Mochi's `let` is immutable; `var` is
mutable. The compiler emits `const` / `let` accordingly.

**What we reject.** Persistent data structures. The user pays for
HAMTs in bundle size and indirection; for the common case (small
collections, infrequent updates) the cost outweighs the structural-
sharing benefit. Mochi uses TS's native mutable structures with
defensive copy on the API boundary if needed.

## 11. ReScript, the rebrand of BuckleScript / ReasonML

**What it is.** ReScript (rescript-lang.org, by Hongbo Zhang and the
ReScript Association, 2020) is the modern face of the OCaml-to-JS
toolchain that started as BuckleScript (2015) and was wrapped in
ReasonML syntax (2016) before being rebooted as ReScript with its own
syntax (2020). ReScript compiles a typed OCaml-derived language to
small, readable JavaScript. It is the de-facto language of choice for
typed FP in the JS ecosystem and is used in production at Facebook
(specifically the Messenger.com web client and the Hack PHP type
checker UI) and at many smaller shops.

ReScript has Hindley-Milner type inference (no annotations needed in
most places), variants (sum types), records, modules, parametric
polymorphism, and a small efficient FFI to JavaScript. As of 2026
ReScript 12 is the current stable line; it ships with a fast
incremental build (`rescript build` is sub-second on warm builds for
~10 kLOC).

**Lesson for MEP-52.** ReScript is the closest functional cousin to
Mochi-on-JS. The same problem space (typed source language compiles
to readable JS) and the same constraints (small runtime, fast build,
good interop). ReScript's choice to emit readable JS (variable names
preserved, no obfuscation, no large runtime injection) is exactly the
right call.

**What we take.** Emit *readable* TypeScript. Variable names mangled
only to avoid reserved-word collisions (`class_` for a Mochi `class`
identifier). Sum types lower to discriminated unions exactly as
ReScript lowers variants. The `null` for option (vs `undefined`)
choice matches ReScript's modeling of `option<'a>` as `null` in the
emitted JS.

**What we reject.** OCaml syntax. Mochi's syntax is closer to Swift /
Kotlin / Rust on the surface, while the type system internals share
DNA with the ML family. ReScript's choice to break from OCaml syntax
in v12 validates that "looks like the host" matters; Mochi looks like
a modern braced ML.

## 12. ReasonML and BuckleScript (legacy)

**What they were.** ReasonML (reasonml.github.io, by Cheng Lou at
Facebook, 2016) was an alternative OCaml syntax that compiled either
to native OCaml (via the `bsb` build tool) or to JavaScript (via
BuckleScript, the OCaml-to-JS compiler by Hongbo Zhang). The pair was
the canonical typed-FP-on-JS stack from 2016-2020 until ReScript
absorbed them.

**Lesson for MEP-52.** The "two syntaxes for one type system" approach
adds confusion. Mochi has one canonical syntax. The "OCaml-to-JS
backend wrapped in a new syntax" pattern was historically necessary
because OCaml's own syntax was off-putting to JS developers; Mochi
avoids this by having a single syntax from day one.

**What we take.** The transparent FFI design (ReasonML's `[@bs.send]`,
`[@bs.module]`, `[@bs.val]` attributes for JS interop). Mochi's `@js`
attribute on a Mochi function declares it as a JS-runtime FFI, and the
generator emits the appropriate `import` and call site.

**What we reject.** OCaml-style `let rec` for mutual recursion. Mochi
detects mutual recursion in the IR and emits hoisted `function`
declarations in JS, which natively allow forward reference within a
file.

## 13. Fable, F# to JavaScript / TypeScript

**What it is.** Fable (fable.io, by Alfonso García-Caro Núñez and the
F# Foundation, 2016) is an F#-to-JavaScript compiler. F# is Microsoft
Research's ML-derived statically typed functional language. Fable
takes F# source, compiles via the F# Compiler Service (FCS) to F#
AST, then emits JavaScript (or TypeScript, since Fable 4.0 in 2023).
Fable is heavily used in the SAFE Stack (F# full-stack web framework)
and at several Swedish and Italian shops.

Fable 4 introduced TypeScript as a first-class output target,
replacing JavaScript-with-JSDoc as the recommended emit mode. Fable
4's TS output is strict-clean. As of 2026 Fable 5 is the current
stable line, with refined emit for async / Task interop.

**Lesson for MEP-52.** Fable's pivot from JS-with-JSDoc to
TypeScript-source emit is the single closest precedent for MEP-52's
strategy. The reasoning (TS gives IDE feedback, type-checking, and
publishing affordances that JSDoc cannot reach) is the same as ours.
Fable's choice to emit TS rather than depend on a runtime tsc
compilation step is the right call.

**What we take.** The TypeScript-source-of-truth choice. The strict
TS emit (Fable's output passes `tsc --strict`). The hand-rolled
runtime in the emitted bundle (Fable's `fable-library.fs` is the
analogue of Mochi's `mochi_runtime/`). The choice to emit ES modules
exclusively (no UMD, no CommonJS).

**What we reject.** F#'s syntactic baggage (e.g. the OCaml-derived
`<-` for mutation, the F#-specific computation expression sugar).
Mochi's syntax is closer to Rust / Swift.

## 14. Elm, the Haskell-flavoured front-end language

**What it is.** Elm (elm-lang.org, by Evan Czaplicki, 2012) is a
pure functional language for browser front-ends. It compiles to
JavaScript and ships with the Elm Architecture (a Redux-style
unidirectional data flow pattern). Elm is famously zero-runtime-error
in production because of its strict type system, mandatory total
function coverage, and lack of `null` / `undefined`. Elm's userbase
peaked around 2018-2020; the language has been in slow-evolution mode
since 0.19.1 (October 2019), with no major release through 2026.

Elm's compiler is famously friendly: error messages explain themselves
in plain English, and the official "Elm errors are awesome" page is a
showcase. Elm's bundle output is small (~10-30KB for a real app) and
tree-shaken aggressively.

**Lesson for MEP-52.** Elm validated three things: (1) typed-FP
front-end is viable in production; (2) total functions and exhaustive
case-checking eliminate whole classes of bugs; (3) friendly compiler
errors are a moat that competing languages need to match. Elm's slow
release cadence is also a cautionary tale: a one-person-led language
is fragile.

**What we take.** The exhaustive `match` semantics. Mochi's
`match expr { ... }` is a compile-time error if a case is missing on
a sum type. The TS lowering uses an exhaustiveness check via a
`never`-typed default branch (the standard TS idiom).

**What we reject.** Elm's lack of an FFI escape hatch (Elm requires
"ports" for JS interop, which is awkward for libraries needing
to call into the JS ecosystem). Mochi's `@js` attribute is a direct,
typed FFI.

## 15. PureScript, Haskell on JavaScript

**What it is.** PureScript (purescript.org, by Phil Freeman, 2013) is
a strongly typed Haskell-like language that compiles to JavaScript. It
has Hindley-Milner type inference with extensions (row types, type
classes, higher-kinded types, functional dependencies). PureScript
emits CommonJS by default (with an ESM mode added in 0.15) and ships
a small runtime (`purescript-prelude`). As of 2026 PureScript 0.15.10
is the stable line; the userbase is small (~2-5k active developers)
but devoted, with significant usage in Lumi (DevOps SaaS) and at
several smaller shops.

PureScript's selling points: typeclasses, ad-hoc polymorphism, lazy
evaluation by default (via `Data.Lazy`), monad transformers, free
monads, applicative functors.

**Lesson for MEP-52.** PureScript demonstrates the upper bound of
"how Haskell-y can a JS-compiled language be before the bundle and
runtime cost dominate". The answer: pretty Haskell-y, but the
ecosystem is small. Mochi steers away from full type-class polymorphism
because the type erasure to TS would require runtime dictionaries,
which add bundle weight.

**What we take.** Strong type inference at the local level (not
type-class-heavy, but ML-style). The compiler is responsible for
inferring types most of the time; the user writes annotations only
at module boundaries.

**What we reject.** Type classes. Mochi uses interfaces (structural,
duck-typed) for the polymorphism cases that type classes would cover;
the resulting TS is `interface` declarations, which TS implements
natively. Lazy evaluation. Mochi is strict.

## 16. Kotlin/JS, Kotlin compiled to JavaScript

**What it is.** Kotlin/JS (kotlinlang.org/docs/js-overview.html, by
JetBrains, 2017) is the JavaScript backend for the Kotlin compiler.
Kotlin/JS shares the front-end and IR with Kotlin/JVM and Kotlin
Native, then emits JavaScript via either the legacy backend (Kotlin
1.0-1.6) or the IR backend (Kotlin 1.7+, becoming default in 1.8).
The IR backend supports TypeScript declaration emit, dead-code
elimination, ES module output, and a smaller runtime.

As of 2026 Kotlin 2.0 is the current stable line; Kotlin/JS shares
~95% of the codebase with Kotlin Multiplatform's web target. The
userbase is moderate (a few percent of all Kotlin developers).

**Lesson for MEP-52.** Kotlin/JS proved that a typed JVM-derived
language can have a credible JS backend if it commits to (a) IR-driven
emission, (b) TS .d.ts generation, (c) hand-tuned runtime keeping
the bundle small. Kotlin/JS's struggle was always bundle size: pre-IR
backends produced 100-200KB minified runtimes; the IR backend brought
this to 30-60KB.

**What we take.** Everything. Kotlin/JS is the closest sibling to
MEP-52 in shape (typed source language, JS / TS target, hand-rolled
runtime). The `mochi_runtime/` is sized as a target equivalent
(<= 50KB minified core). The .d.ts generation matches Kotlin/JS's
TypeScript declaration emit.

**What we reject.** Kotlin/JS's choice to emit JavaScript primarily
and TS declarations secondarily; MEP-52 inverts this, emitting TS as
primary and using `tsc` to derive the `.d.ts` and `.js` together.

## 17. Scala.js, Scala compiled to JavaScript

**What it is.** Scala.js (scala-js.org, by Sébastien Doeraene, 2013)
is the JavaScript backend for the Scala compiler. Scala.js compiles
Scala source through scalac, then a Scala.js-specific backend pass
emits IR that lowers to JavaScript. Scala.js targets ES2015 by
default and ES2020 since Scala.js 1.10. Dead code elimination is
aggressive (typical bundle is ~50-300KB for a real app).

As of 2026 Scala.js 1.16 is the current stable line, paired with
Scala 3.4. Scala.js has steady usage in financial services (where
Scala on JVM is common) and in some React-based front-ends via
Slinky.

**Lesson for MEP-52.** Scala.js is another "typed JVM language on
JS" sibling. Its size optimisation track record (every release shaves
a few KB off the minimum bundle) is instructive: the runtime is
a continuous engineering target, not a one-shot design.

**What we take.** Continuous runtime size optimisation as a release
goal. Every MEP-52 phase landing has a target bundle-size delta;
regressions block the phase. The detail of how Scala.js encodes
case classes (a `Symbol`-keyed `$classData` field for runtime type
info) informs Mochi's discriminator-tag lowering for sum types
(though Mochi uses string literals on the `kind` field, which is
lighter).

**What we reject.** Scala's complex implicit-conversion machinery.
The TS emit would have to model implicits as runtime dispatch, which
is expensive. Mochi's coercions are explicit.

## 18. GopherJS, Go compiled to JavaScript

**What it is.** GopherJS (github.com/gopherjs/gopherjs, by Richard
Musiol, 2013) is a Go-to-JavaScript transpiler. It compiles Go source
(including standard library packages) to JavaScript that runs in
Node or the browser. GopherJS preserves Go's concurrency model
(goroutines, channels) by implementing a userland scheduler in JS
on top of cooperative coroutines.

GopherJS has been in slow-maintenance mode since ~2020 as TinyGo's
WebAssembly target became the preferred way to run Go in the
browser. As of 2026 GopherJS is on Go 1.21 compatibility with no
guarantee of newer-version support.

**Lesson for MEP-52.** GopherJS's userland scheduler for goroutines
is instructive: it shows what it costs to emulate native concurrency
primitives on JS's single-threaded event loop. The cost is ~5-10x
runtime overhead vs native Go and a substantial runtime (~500KB
minified for any non-trivial program).

**What we take.** Avoid emulating heavyweight concurrency primitives.
Mochi agents are mailboxes over `AsyncIterableQueue` (a few hundred
lines of TS, ~1KB minified), not a userland goroutine scheduler.

**What we reject.** Userland schedulers entirely. The JS event loop is
the scheduler; we ride on top of it via `async` / `await` and
`Promise.withResolvers`.

## 19. TinyGo's WebAssembly target

**What it is.** TinyGo (tinygo.org, by Ayke van Laethem and the TinyGo
community, 2018) is an alternative Go compiler optimised for small
binaries and embedded targets. Its `wasm` and `wasm-wasi` targets
produce WebAssembly modules suitable for the browser or for WASI
runtimes (wasmtime, wasmer, Wasmer Edge). TinyGo's WASM output is
~30-100KB for a real program (vs ~2-5MB for stock Go's
`GOOS=js GOARCH=wasm`).

As of 2026 TinyGo 0.32 is the current stable line; it tracks Go
1.22 features (excluding the heaviest reflection machinery).

**Lesson for MEP-52.** TinyGo proved that a Go-derived runtime on
WASM is feasible if the toolchain is willing to drop large reflection,
heavy goroutine schedulers, and certain stdlib packages. For Mochi,
this is a hint about an eventual `--target=wasm-component` path; see
[[12-risks-and-alternatives]] §B3 for the WebAssembly Component Model
discussion.

**What we take.** "Drop reflection from the runtime" is the right
default. Mochi has no `reflect`-equivalent in the v1 runtime.

**What we reject.** WASM as the v1 emit target. WASM is the v2 path;
v1 is TypeScript source.

## 20. Pyodide, Python on WebAssembly

**What it is.** Pyodide (pyodide.org, by Mozilla Research, 2019,
maintained by Hood Chatham et al. as of 2026) is CPython compiled to
WebAssembly via Emscripten. Pyodide ships the full CPython interpreter
plus a large library set (NumPy, pandas, scikit-learn, matplotlib) as
prebuilt WASM packages. The download is ~10MB for the core interpreter
and ~50-100MB with the numeric stack.

Pyodide is the de-facto "Python in the browser" runtime. JupyterLite,
the Mozilla Hubs runtime, several MOOCs (e.g. parts of MIT 6.0001)
use Pyodide.

**Lesson for MEP-52.** Hosting a full alternate-language runtime on
WASM is a viable but heavyweight choice. Pyodide accepts ~10MB to
ship Python; MEP-52's TS-source-emit approach instead lets the user
bundle just their own code plus a tiny `mochi_runtime/`, on the order
of 10-30KB.

**What we take.** Pyodide's lesson on package distribution (use
WHL-like prebuilt artifacts, never compile from source on the
client). Mochi's npm publish flow ships prebuilt TS and JS; the user
never compiles Mochi on their CI.

**What we reject.** Heavy runtime hosting. Mochi is a TS-source-emit
language, not a "ship the Mochi interpreter to the browser" runtime.

## 21. Brython, Python in the browser (no Emscripten)

**What it is.** Brython (brython.info, by Pierre Quentel, 2014) is a
Python interpreter written in JavaScript that runs in the browser.
Brython parses Python source on the fly and executes it via a JS-side
runtime. As of 2026 Brython 3.12 is the current stable line.

Brython is much lighter than Pyodide (~500KB minified core) but
sacrifices CPython compatibility (most C-extension packages are
unavailable; numerical performance is poor).

**Lesson for MEP-52.** A pure-JS interpreter for an alternate language
is feasible but always trails the canonical implementation on
compatibility and performance. Mochi avoids this trap by being the
canonical implementation from day one.

**What we take.** Nothing operationally; Brython is a cautionary tale
of the "JS-implemented interpreter for another language" pattern.

**What we reject.** Implementing Mochi as a JS interpreter shipped to
the browser. Mochi compiles to TS / JS source statically.

## 22. Transcrypt, Python-to-JavaScript transpiler

**What it is.** Transcrypt (transcrypt.org, by Jacques de Hooge,
2014) is a Python-to-JavaScript source-to-source transpiler. It
takes Python 3 source and emits readable JavaScript with Python
semantics preserved (mostly). Transcrypt does not type-check and
does not produce TypeScript.

As of 2026 Transcrypt 3.9 is the current stable line. Its userbase
is small (~hundreds of active users) and overlap with Brython and
Skulpt is high.

**Lesson for MEP-52.** Python-to-JS transpilers exist but have not
captured a meaningful market because the language semantics gap
(dynamic dispatch, monkey-patching, dict-as-namespace) is hostile
to clean static emit.

**What we take.** Nothing directly. Mochi is statically typed; the
Python-on-JS class of transpilers does not inform our design.

**What we reject.** Source-to-source transpilation without a typed
intermediate. Mochi's aotir IR is the typed intermediate; the TS
emit is straightforward because the types are pinned.

## 23. Skulpt, Python in the browser for education

**What it is.** Skulpt (skulpt.org, by Scott Graham et al., 2007) is
a Python-in-the-browser implementation written in JavaScript, aimed
at education. Used in interactive textbook platforms (e.g. the Runestone
Interactive textbook series). Smaller scope than Brython (no full
stdlib).

**Lesson for MEP-52.** Education-focused niches can support a narrow
alt-runtime. Mochi has no equivalent education-narrow target in v1;
the audience is general production.

**What we take.** Nothing operationally.

**What we reject.** The interpreter-in-JS approach.

## 24. AssemblyScript, TypeScript-subset to WebAssembly

**What it is.** AssemblyScript (assemblyscript.org, by Daniel
Wirtz et al., 2017) is a strict TypeScript-subset language that
compiles to WebAssembly. AssemblyScript looks like TypeScript but
omits dynamic features (no `any`, no Object methods like
`getPrototypeOf`, no runtime reflection) and adds WASM-specific
features (typed memory access, manual memory layout, SIMD
intrinsics).

AssemblyScript is the only "TypeScript-like to WASM" production-grade
language. Userbase is moderate (~5-10k active developers), used in
blockchain (the Massa, NEAR, and the COSM platforms), in some
high-performance browser code (image processing libraries), and in
edge compute (Fastly Compute, Cloudflare Workers).

As of 2026 AssemblyScript 0.27 is the current stable line.

**Lesson for MEP-52.** AssemblyScript demonstrated that a TypeScript-
like syntax with stricter semantics is a viable language design
point. The lesson for Mochi: stricter-than-TS is acceptable if it
buys clearer semantics. Mochi is stricter than TS in several
dimensions (mandatory exhaustiveness, no `any`, no implicit
coercion).

**What we take.** The validation that "TS syntax with stricter
semantics" is a viable design point. Mochi's emitted TS is, in
practice, a strict subset of TS (no `any`, no unchecked casts, no
implicit `this`-bound functions).

**What we reject.** AssemblyScript's WASM-first emit. Mochi's v1
target is TS source on JS engines; WASM is a future target.

## 25. Hegel, the alternative TS type checker

**What it is.** Hegel (hegel.js.org, by Artem Khramov, 2020) is a
soundness-focused alternative TypeScript type checker. Hegel adds
exhaustive null-checking, exhaustive union narrowing, and stricter
generic inference than tsc. The project is small and slow-moving;
maintenance ceased ~2022 as TypeScript 4.x and 5.x absorbed many
of Hegel's improvements.

**Lesson for MEP-52.** Alternative type checkers for TS exist but
have not displaced tsc. The complexity and incremental-build
performance of tsc are hard to match outside Microsoft's resourcing.

**What we take.** Nothing operationally.

**What we reject.** Building an alt type checker. We use tsc.

## 26. Flow, Facebook's type system

**What it is.** Flow (flow.org, by the Facebook (now Meta) team,
2014) is a static type checker for JavaScript. Flow predates
TypeScript's mainstream adoption and was the dominant typed-JS
solution inside Meta until ~2020 when Meta started migrating
internally to TypeScript. As of 2026 Flow is still maintained for
Meta-internal use but has minimal external adoption.

Flow's syntax is similar to TypeScript but not compatible; Flow's
type inference is sometimes stronger (e.g. better union narrowing
in early versions), but the ecosystem (libraries, IDEs, tooling)
overwhelmingly settled on TypeScript.

**Lesson for MEP-52.** "Network effects in tooling matter more than
type-system elegance." Flow had a technically excellent type
checker but lost to TypeScript because of (a) Microsoft's marketing
and IDE integration, (b) DefinitelyTyped (the community type-stub
repo that gave TS instant interop with all of npm), (c) tsc's
emit-and-check single-tool simplicity. Mochi joins the TypeScript
ecosystem rather than try to bring a parallel one.

**What we take.** Type stub interop with `npm`. Mochi reads
`@types/foo` packages for FFI signatures when wrapping an npm
dependency.

**What we reject.** Flow-style type annotations. We emit TS syntax,
which is what the ecosystem expects.

## 27. JSII, AWS's TypeScript-to-multi-language

**What it is.** JSII (github.com/aws/jsii, by AWS, 2018) is the tool
that powers AWS CDK (Cloud Development Kit). It takes a TypeScript
library and emits language bindings (Python, Java, C#, Go) so that
the same library can be consumed from multiple languages. The runtime
trick: JSII runs the TypeScript code in a separate Node process and
the language bindings RPC to it.

JSII is widely deployed via AWS CDK (hundreds of thousands of users).
As of 2026 JSII v5 is the current stable line, with improved type
emission for Python (PEP 695 generics) and Go (Go 1.18+ generics).

**Lesson for MEP-52.** JSII goes the opposite direction (TS to other
languages); Mochi goes Mochi to TS. The relevant lesson is the JSII
RPC-bridge runtime trick: it's a clever way to ship a TS library to
non-TS consumers without re-implementing in each language. Mochi
doesn't need this trick because the emit is direct, not bridge-based.

**What we take.** The validation that "TS is the canonical type-rich
source of truth" is a defensible architectural choice (since AWS bets
on it for CDK).

**What we reject.** RPC bridges. Mochi is a compiler, not a bridge.

## 28. Dart and Flutter's web target

**What it is.** Dart (dart.dev, by Google, 2011) is a typed
object-oriented language that targets multiple runtimes: Dart VM
(native), JavaScript (via dart2js, a whole-program optimiser), and
WebAssembly (via dart2wasm, in beta as of Dart 3.5). Flutter
(flutter.dev) is Google's cross-platform UI framework built on Dart.

Dart for the web went through several backend redesigns:
`frog` (deprecated), `dart2js` (the production JS compiler since
~2015), and now `dart2wasm`. dart2js produces aggressively
tree-shaken JS (whole-program optimisation), typical bundle ~150-300KB
for a Flutter web app.

**Lesson for MEP-52.** Whole-program optimisation is powerful but
heavy. dart2js takes seconds-to-minutes on a real codebase. Mochi's
modular incremental emit is faster but produces less-optimised
output; we rely on the bundler's tree-shaking (esbuild, Rollup) for
the size win.

**What we take.** The lesson that whole-program optimisation can
unlock significant bundle savings; relegated to a v2 `--target=
typescript-whole-program` path if user demand emerges.

**What we reject.** Dart's class-heavy syntax. Mochi is closer to
ML than to Java in shape.

## 29. The Mochi sibling backends (MEP-45 to MEP-51)

**MEP-45 (Mochi to C).** Lowers aotir to C source via Mochi-built C
syntax tree; clang or gcc compiles to native. The aotir IR is shared.
Lesson: a typed IR survives lowering to any backend with the right
runtime support library.

**MEP-46 (Mochi to BEAM via Core Erlang).** Compiles aotir to Core
Erlang source; rebar3 runs the Erlang compiler. Lesson: actor / agent
semantics translate cleanly to BEAM's process model. The MEP-52
agent model (AsyncIterableQueue + AbortController) is a JS-runtime
analogue of BEAM's process + supervisor.

**MEP-47 (Mochi to JVM bytecode via ASM).** Compiles aotir to JVM
bytecode directly using the ASM bytecode-manipulation library. No
source-emit step; we go straight to .class files. Lesson: when the
host runtime has a stable bytecode format, skipping the source step
is fast and reliable. MEP-52 inverts this (TS source, not JS engine
bytecode), because JS engine bytecode is proprietary and unstable.

**MEP-48 (Mochi to C# via Roslyn).** Compiles aotir to C# source via
a Mochi-side C# syntax tree, then dotnet compiles to MSIL. Lesson: a
clean syntax tree printer beats string concatenation for source emit.
MEP-52 uses the same printer-based design.

**MEP-49 (Mochi to Swift).** Compiles aotir to Swift source for SwiftPM
to build. Lesson: typed throws (Swift's `throws(SpecificError)`)
mirror Mochi's MochiResult shape closely; the Swift emit is clean.
MEP-52's MochiResult discriminated union is the JS / TS analogue.

**MEP-50 (Mochi to Kotlin).** Compiles aotir to Kotlin source via
KotlinPoet. Lesson: Kotlin's `Channel` + `SupervisorJob` map cleanly to
Mochi agents; the JS equivalent (`AsyncIterableQueue` +
`AbortController`) is the chosen MEP-52 shape. MEP-50's per-phase
test gate (vm3 byte-equal stdout) is the same shape MEP-52 uses.

**MEP-51 (Mochi to Python).** Compiles aotir to Python source via
libcst, then uv builds wheels. Lesson: the "typed source language to
typed Python via libcst, type-checked by pyright, formatted by ruff"
shape is the direct sibling of MEP-52's "typed source to TS via Mochi-
built syntax tree, type-checked by tsc, formatted by prettier". The
shared 18-phase plan, the shared aotir IR, the shared MochiResult
discriminated union: all carry over to MEP-52 unchanged.

Across all six siblings, the consistent themes are: (1) one IR
(aotir), six emits, (2) typed source language to typed target where
possible, (3) hand-rolled small runtime per backend, (4) byte-equal
vm3 stdout as the gate, (5) 18-phase build-out matching the same
checkpoints.

## 30. WebAssembly Component Model

**What it is.** The WebAssembly Component Model (W3C, draft 2024+;
implementation in wasmtime, wasmer, Wasmer Edge, Spin) is the next-
generation WASM ABI that adds typed interfaces, language-agnostic
imports / exports, and resource handles. As of 2026 the spec is at
Phase 4 (in major implementations); Spin (Fermyon's serverless
runtime) and Wasmer Edge ship Component-Model-compliant runtimes in
production.

The Component Model's WIT (WebAssembly Interface Type) IDL defines
typed contracts that any source language can implement; bindgen tools
exist for Rust, Go (TinyGo), C, Python (componentize-py),
JavaScript (jco), and others.

**Lesson for MEP-52.** A future `--target=wasm-component` path is
the long-term ambition. It would let Mochi run server-side on any
WASM runtime (Spin, Wasmer Edge, Cloudflare Workers via wasmer-js),
in browsers via the WASI Preview 2 shim, and with strong typed
interfaces. For v1, this is deferred; TS source-emit is the target.

**What we take.** The Component Model's WIT-style typed interfaces
inform our `.d.ts` design: every exported Mochi symbol gets a typed
TS declaration, mirroring what a WIT export would look like.

**What we reject for v1.** WASM-Component-Model as the v1 target.
Too early; the ecosystem (specifically the language bindings, the
debugger, the source-map story) is not mature enough yet.

## 31. The TC39 proposals pipeline as moving target

**What it is.** TC39 (tc39.es) is the standards committee that
governs ECMAScript. Proposals move through Stages 0-4:

- Stage 0: strawperson, unstable
- Stage 1: proposal, formal champion required
- Stage 2: draft, working spec text exists
- Stage 3: candidate, near-final, implementation expected
- Stage 4: finished, in the next ECMAScript edition

As of 2026, the Stage-3-or-4 features that affect Mochi:

- **Stage 4 (in ES2024 / ES2025)**: `Promise.withResolvers`, `Set`
  methods (intersection, union, difference, isSubsetOf), `Object.groupBy`
  / `Map.groupBy`, `Iterator.from` + iterator helpers, `Symbol.dispose`
  / `using`, decorators (with metadata), `Atomics.waitAsync`,
  `String.prototype.isWellFormed` / `toWellFormed`.
- **Stage 3 (likely ES2026)**: explicit resource management (already
  Stage 4 for `using`), Float16Array, `RegExp.escape`, decorator
  metadata, immutable arraybuffer methods, async context (very useful
  for Mochi's stream / agent context propagation).
- **Stage 2 (uncertain)**: pipe operator `|>`, pattern matching
  (`when` expression), partial application syntax, range / iterator
  range syntax, records and tuples (the immutable-data proposal).

**Lesson for MEP-52.** The TS 5.6 + ES2024 floor (see
[[02-design-philosophy]] §3) is chosen precisely to ride the Stage-4
features without depending on Stage-3 promises. Stage 2-and-below
features are not lowering targets; if we want pipes, we lower to
`.then` chains.

**What we take.** Aggressive use of Stage 4 features:
`Promise.withResolvers` for `agent.call`, `Set` methods for set
operations, `Object.groupBy` for the group-by query DSL, `Iterator`
helpers for the iterator pipeline, `using` for resource scopes.

**What we reject.** Anything below Stage 4. The risk of betting on
a Stage-3 feature that gets re-spec'd is too high; Mochi's released
binary will out-live the proposal's evolution.

## 32. Vite, Rollup, webpack, Parcel, the bundler menagerie

**What they are.** The JS bundler ecosystem is dominated by:

- **Vite** (vitejs.dev, by Evan You, 2020): dev server using
  esbuild, production build using Rollup. Default for Vue, React,
  Svelte starter projects since 2022. As of 2026 Vite 6 is current.
- **Rollup** (rollupjs.org, by Rich Harris, 2015): ESM-native
  bundler, tree-shaking pioneer. Used by Vue and Vite for prod
  builds. As of 2026 Rollup 4 is current.
- **webpack** (webpack.js.org, by Tobias Koppers, 2012): the legacy
  giant. Still default for some React stacks (especially CRA, deprecated
  in 2023). As of 2026 webpack 5 is current but slowly being displaced
  by Vite, Turbopack, Rspack.
- **Parcel** (parceljs.org, by Devon Govett, 2017): zero-config
  bundler. Smaller community.
- **Rspack** (rspack.dev, by ByteDance, 2023): Rust-written webpack-
  compatible bundler. Fast, growing community.
- **Turbopack** (turbo.build, by Vercel, 2022): Rust-written
  successor to webpack, bundled in Next.js.

**Lesson for MEP-52.** The bundler choice belongs to the user, not
to Mochi. Mochi emits ES modules with `"sideEffects": false` and
clean `package.json` `"exports"`; any modern bundler tree-shakes
correctly.

**What we take.** Make the emit "bundler-neutral". The user picks
Vite, Rollup, esbuild, Parcel, Rspack, Turbopack, or webpack. Our
docs recommend Vite for app dev and esbuild for library bundles, but
neither is hardcoded.

**What we reject.** Hardcoding a bundler choice. The npm publish flow
uses `tsc --build` for `.d.ts` + `.js` emit, and the optional
browser-bundle target uses esbuild because it's fast and dependency-
free, but the user can swap.

## 33. ESLint, Biome, Oxc, Prettier (linters and formatters)

**What they are.**

- **ESLint** (eslint.org, by Nicholas Zakas, 2013): the canonical
  JS / TS linter. ESLint 9 (April 2024) introduced the new flat
  config (`eslint.config.js`) replacing `.eslintrc`. As of 2026
  ESLint 9.10 is current.
- **Biome** (biomejs.dev, by the Romefronturn team, 2023, fork of
  the failed Rome project): a Rust-written linter + formatter. As
  of 2026 Biome 1.8 is current; gaining traction as a faster Prettier
  + ESLint replacement.
- **Oxc** (oxc-project.github.io, by Boshen and team at the Void(0)
  group, 2023): another Rust-written linter + parser + transformer
  + minifier suite. As of 2026 Oxc is in beta but improving fast.
- **Prettier** (prettier.io, by Christopher Chedeau, 2017): the
  canonical opinionated formatter. As of 2026 Prettier 3.3 is
  current.

**Lesson for MEP-52.** The linter / formatter ecosystem is in flux;
betting on a single tool is risky. Mochi's emit should pass *both*
ESLint and Biome (and Prettier formatting); if either accepts our
output, the user can choose.

**What we take.** Prettier 3.x is the default formatter (line width
80, single quotes, no semicolons except where required by ASI). ESLint
with `@typescript-eslint/strictTypeChecked` preset is the default
linter. Both are run during `mochi test`.

**What we reject.** Biome as the primary formatter for now; ecosystem
is still maturing. Documented as an alt in the build docs.

## 34. Deno, Bun, and the runtime alternatives

**What they are.**

- **Deno** (deno.com, by Ryan Dahl (the same Ryan who founded Node),
  2018): TypeScript-native runtime. Built on V8 (Chrome's JS engine)
  and Rust. Deno 1.0 in 2020; Deno 2.0 in October 2024 added Node
  compatibility (`npm:` specifiers, npm CompatLayer, package.json
  support). Deno includes a built-in formatter (`deno fmt`), linter
  (`deno lint`), test runner (`deno test`), task runner (`deno task`),
  and language server. As of 2026 Deno 2.2 is current.
- **Bun** (see entry 6 above).
- **WinterCG** (wintercg.org, since 2022): the Web-interoperable
  Runtimes Community Group, coordinating common JS API surface across
  Node, Deno, Bun, Cloudflare Workers, Vercel Edge, Netlify Edge,
  Fastly Compute. Tracks fetch, URL, ReadableStream, AbortSignal,
  Crypto, Web Workers, and similar APIs.

**Lesson for MEP-52.** The JS runtime story is no longer Node-only.
Mochi targets four runtimes from day one (see
[[02-design-philosophy]] §9). WinterCG-aligned APIs (fetch, AbortSignal,
Crypto) are safe to use in the runtime; Node-specific APIs (`node:fs`,
`node:net`) are isolated.

**What we take.** Cross-runtime API discipline. The runtime uses
WinterCG-blessed APIs (fetch, URL, AbortSignal, Crypto.subtle,
ReadableStream where needed) and isolates Node-only APIs (`node:fs`,
`node:net`, `node:cluster`) behind a `mochi_runtime/io/` module that
gets stubbed out in browser builds.

**What we reject.** Node-only runtime. The conditional exports map
(`node`, `deno`, `bun`, `browser`, `default`) ensures each runtime
gets the right code path.

## 35. JSR (jsr.io), the Deno-native registry

**What it is.** JSR (jsr.io, by the Deno team, launched March 2024) is
a JavaScript / TypeScript module registry alternative to npm. JSR's
selling points: TypeScript source as the canonical published artifact
(not transpiled JS); semver and scoped packages; automatic provenance
attestation; one publish command for Node, Deno, Bun, browser
consumption. JSR is interoperable with npm (you can `npm install
@jsr/scope__name` via npm's `--registry` flag).

As of 2026 JSR has a few thousand packages and growing.

**Lesson for MEP-52.** JSR is the "ship TS source" registry, exactly
the model MEP-52 endorses. Publishing a Mochi-emitted package to JSR
is a one-line operation: `deno publish` reads our `deno.json` (a
sibling file to `package.json`) and uploads. JSR handles the .d.ts
generation, provenance, and the Deno-native experience.

**What we take.** JSR as the Deno-native secondary publish target.
The `mochi publish --registry=jsr` flag publishes to jsr.io
alongside `mochi publish --registry=npm` for npmjs.org.

**What we reject.** JSR as the *only* registry. npm is still the
canonical (~10x the package count, ~100x the install rate).

## 36. The historical curio: Closure Compiler

**What it is.** Closure Compiler (developers.google.com/closure/compiler,
by Google, 2009) is a Java-written JavaScript optimiser and minifier.
It does whole-program optimisation, dead-code elimination, name
mangling, and (uniquely) type-checking via JSDoc annotations. Used
heavily inside Google (Gmail, Google Maps, Google Docs) and at some
external shops. ClojureScript uses Closure for the final minify pass.

As of 2026 Closure Compiler is still maintained by Google but has
been largely displaced externally by terser (a smaller, JS-written
minifier), esbuild's built-in minifier, and the SWC minifier.

**Lesson for MEP-52.** Whole-program optimisation works but is heavy.
Closure's "advanced optimisations" mode produces 30-50% smaller
bundles than terser but requires JSDoc type annotations and breaks
many libraries.

**What we take.** Nothing operationally.

**What we reject.** Whole-program optimisation in v1. Module-level
emit + bundler tree-shaking is fast enough.

## 37. The runtime-format zoo: Iterator.from, Map.groupBy, Set methods

**Why this matters as prior art.** A subtle prior-art question for
MEP-52: which ECMAScript shapes are *new enough* that Mochi can use
them but *old enough* that they ship in every tier-1 runtime?

The answer is dictated by ES2024 (June 2024) and the runtime support
matrix:

| Feature                           | Node 22  | Deno 2.0 | Bun 1.1 | Chrome | Firefox | Safari |
|-----------------------------------|----------|----------|---------|--------|---------|--------|
| `Promise.withResolvers`           | 22.0+    | 2.0+     | 1.0+    | 119+   | 121+    | 17.4+  |
| `Set.intersection / union / etc.` | 22.0+    | 2.0+     | 1.1+    | 122+   | 127+    | 17.0+  |
| `Object.groupBy / Map.groupBy`    | 22.0+    | 2.0+     | 1.1+    | 117+   | 119+    | 17.4+  |
| `Iterator.from` + helpers         | 22.0+    | 2.0+     | 1.1+    | 122+   | 131+    | 18.4+  |
| `Symbol.dispose` / `using`        | 24.0+    | 2.2+     | 1.2+    | 134+   | 135+    | (none) |
| `Array.fromAsync`                 | 22.0+    | 2.0+     | 1.1+    | 121+   | 115+    | 16.4+  |

(Versions as of 2026-05; sources: MDN's ES2024 compatibility tables,
the Node.js V8 update history, the Deno 2.0 release notes, the Bun
1.1 release notes, and the Chromium / Firefox / Safari release notes.)

**Lesson for MEP-52.** Almost all of ES2024 is available across all
four runtimes as of mid-2026. Mochi can lean on Stage 4 features freely.
The one outlier is `Symbol.dispose` / `using` (Safari support pending
as of 2026-05); Mochi's runtime polyfills this in browser builds.

**What we take.** Stage 4 features as a freely-available palette.

**What we reject.** Stage 3 or below as lowering targets.

## 38. Hand-rolled JS libraries we audit, not depend on

For each major runtime concern, we audit a popular hand-rolled JS
library and decide whether to depend on it (no), vendor a subset (no),
or write the equivalent in `mochi_runtime/` (yes). The audited
libraries:

- **Lodash** (lodash.com, by John-David Dalton, 2012). Utility belt;
  4000+ functions covering arrays, objects, strings, dates. As of
  2026 Lodash 4.17 is the long-stable line. Mochi reject: the
  standard library already covers what we need; pulling Lodash adds
  ~70KB minified.
- **Date-fns** (date-fns.org, by Sasha Koss, 2014). Modular date
  library. As of 2026 date-fns 3.6 is current. Mochi reject for v1:
  we use the native `Temporal` API (Stage 4 as of 2024) for date /
  time, polyfilled for older runtimes.
- **Temporal Polyfill** (github.com/js-temporal/temporal-polyfill).
  The reference polyfill for `Temporal`. Used internally by
  `mochi_runtime/time/` to provide consistent date handling pre-native.
  Mochi vendor: yes for `time.now() / time.parse() / time.format()`.
- **Effect** (effect.website, by Michael Arnaldi, 2023): a TypeScript
  effect-system library. Pipes, generators, schemas, structured
  concurrency. Excellent design but heavy: full Effect is ~100KB
  minified. Mochi reject: Mochi has its own concurrency primitives
  (`AsyncIterableQueue` + `AbortController`); we don't need a
  competing one.
- **fp-ts** (gcanti.github.io/fp-ts, by Giulio Canti, 2017): older,
  more academic Haskell-style FP library for TS. Effect's predecessor
  (Effect's author Michael Arnaldi was an fp-ts contributor).
  Mochi reject: similar reasoning to Effect.
- **RxJS** (rxjs.dev, by Ben Lesh and team, 2015): observable
  streams library. Standard for Angular. Used widely in React via
  redux-observable. Mochi reject: Mochi streams are
  `AsyncIterable<T>`, not Observables; the model is different.
- **immer** (immerjs.github.io/immer, by Michel Weststrate, 2017):
  immutable updates with mutable-syntax draft. Excellent ergonomics
  for Redux-style state. Mochi reject: Mochi records are frozen and
  re-emit immutable copies in a generated `with` method; we don't
  need a runtime draft library.
- **ts-results / neverthrow** (npm packages by various authors): Rust-
  style Result types in TS. Mochi reject: we ship our own
  MochiResult (Ok / Err) in `mochi_runtime/result.ts`; the runtime
  cost is ~50 lines.
- **zod / yup / joi / superstruct / valibot** (validation libraries).
  Mochi reject for v1: type-safe validation is a v2 concern. Mochi's
  generated record types are TS-strict, but runtime validation is
  out of scope for the initial release; planned for v2 via a generated
  `parse(json: unknown): MochiResult<T, ParseError>` method on each
  record (see [[02-design-philosophy]] §6 and
  [[12-risks-and-alternatives]] §C5).

## 39. The CLI competitors: Hardhat, Foundry, Forge

**Why this is even on the list.** A pattern of late-2010s and 2020s
JS-adjacent CLIs (Hardhat for Solidity, Foundry for Solidity, Forge
for Rust + Solidity, Forge for Slack) demonstrates that the CLI UX
is a first-class design consideration. The `mochi build`, `mochi test`,
`mochi publish`, `mochi run` CLI surface borrows the layout these
tools have converged on.

**Lesson.** The CLI's commands should map 1:1 to user intents, not
to internal pipeline stages. The user wants "build my package"; they
do not want to think about `tsc --build`, `esbuild`, `prettier
--check`, `eslint`, `vitest run`, and `npm pack` as separate commands.
Mochi's CLI wraps these.

**What we take.** Single-word verbs (`build`, `test`, `run`, `publish`,
`init`, `add`, `remove`, `format`, `lint`). Avoid the npm legacy of
arbitrary noun-verb concatenations.

## 40. What we do NOT take

A few specific anti-patterns from prior art that we deliberately
reject:

- **Heavy runtime in the bundle.** Pyodide (10MB), Brython (500KB),
  ClojureScript (50KB+). Mochi's `mochi_runtime/` is < 30KB minified
  and tree-shakes to less if the user doesn't use agents, streams,
  or the Datalog engine.
- **Custom module systems.** ClojureScript's Closure-Library modules,
  Elm's module system, PureScript's effect modules. Mochi uses
  vanilla ESM (`import / export`) and lets the bundler / runtime
  handle module loading.
- **Source-language interpreter shipped to the runtime.** Brython,
  Skulpt. Mochi compiles ahead of time.
- **One-off bundlers per language.** ClojureScript shadow-cljs,
  Elm's elm make, PureScript's spago, Scala.js sbt. Mochi uses the
  ambient JS ecosystem's bundlers (Vite, esbuild, Rollup).
- **Custom package registries with closed ecosystems.** PureScript's
  spago registry (resolved 2023), Elm's elm-packages. Mochi publishes
  to npm and JSR, the open registries.
- **Type-class polymorphism with runtime dispatch.** PureScript's
  type classes emit runtime dictionaries. Mochi uses interfaces
  (compile-time structural matching, no dictionary cost).
- **Whole-program optimisation in the dev loop.** Closure advanced,
  dart2js, ClojureScript advanced. Slow. Mochi does module-level
  emit with bundler tree-shaking; saving 10% bundle size is not
  worth a 10x build slowdown.
- **Userland concurrency schedulers.** GopherJS goroutines, Scala.js
  fibers. Heavy runtime. Mochi uses the host's `async / await` and
  `Promise` directly; the agent shape is a thin wrapper.
- **Significant whitespace.** CoffeeScript, Python-like syntaxes.
  Mochi uses braces.
- **Implicit conversions / coercions.** Scala implicits, TypeScript's
  weak `==`. Mochi requires explicit casts.
- **Reflection-heavy runtimes.** Java-style `getClass()`, ClojureScript
  `type`. Mochi has no runtime reflection.

## 41. Direct comparison table: source-to-JS / source-to-TS landscape

| Project              | Year | Target lang           | Type system           | Runtime size  | TS emit | Use today  |
|----------------------|------|-----------------------|------------------------|---------------|---------|------------|
| Babel                | 2014 | JS                    | none                  | none          | strips  | yes        |
| SWC                  | 2018 | JS                    | none                  | none          | strips  | yes        |
| esbuild              | 2020 | JS                    | none                  | none          | strips  | yes        |
| tsc                  | 2012 | JS or TS              | full                  | none          | passthrough | yes    |
| sucrase              | 2018 | JS                    | none                  | none          | strips  | legacy     |
| Bun transpiler       | 2023 | JS                    | none                  | runtime       | strips  | yes        |
| ts-blank-space       | 2024 | JS                    | none                  | none          | blanks  | yes        |
| CoffeeScript         | 2009 | JS                    | none                  | none          | no      | legacy     |
| LiveScript           | 2011 | JS                    | none                  | none          | no      | legacy     |
| ClojureScript        | 2011 | JS                    | dynamic               | ~50KB         | no      | niche      |
| ReScript             | 2020 | JS                    | strong (HM + ext)     | ~5KB          | yes     | yes        |
| ReasonML/Bucklescript| 2016 | JS                    | strong (OCaml)        | ~5KB          | no      | legacy     |
| Fable                | 2016 | JS or TS              | strong (F#)           | ~20KB         | yes     | yes        |
| Elm                  | 2012 | JS                    | strong (ML-ish)       | ~10-20KB      | no      | niche      |
| PureScript           | 2013 | JS                    | strong (Haskell-ish)  | ~30KB         | no      | niche      |
| Kotlin/JS            | 2017 | JS                    | strong (Kotlin)       | ~30-60KB      | yes (.d.ts) | yes    |
| Scala.js             | 2013 | JS                    | strong (Scala)        | ~50-300KB     | yes (.d.ts) | yes    |
| GopherJS             | 2013 | JS                    | strong (Go)           | ~500KB        | no      | legacy     |
| TinyGo WASM          | 2018 | WASM                  | strong (Go)           | ~30-100KB     | n/a     | yes        |
| Pyodide              | 2019 | WASM (CPython)        | dynamic               | ~10MB         | n/a     | niche      |
| Brython              | 2014 | JS interpreter        | dynamic               | ~500KB        | no      | niche      |
| Transcrypt           | 2014 | JS                    | dynamic (Python)      | ~50KB         | no      | niche      |
| Skulpt               | 2007 | JS interpreter        | dynamic               | ~300KB        | no      | edu        |
| AssemblyScript       | 2017 | WASM                  | strong (TS subset)    | tiny          | partial | niche      |
| Dart (dart2js)       | 2011 | JS                    | strong (Dart)         | ~150-300KB    | partial | yes (Flutter)|
| MEP-52 (Mochi to TS) | 2026 | TS source             | strong (Mochi -> TS)  | <30KB         | yes     | new        |
| JSII                 | 2018 | inverse (TS -> Py/Java/Go) | strong (TS)      | RPC bridge    | yes     | yes (AWS CDK) |

**Reading the table.** MEP-52 sits in the upper-right region:
strongly-typed source language, ES module output, TS-source emit,
small runtime. The closest peers are ReScript, Fable, and Kotlin/JS.
All three are alive and well in 2026; the design space is validated.

## 42. Cross-MEP runtime diff

This restates what each Mochi backend ships as its runtime, for
contrast. The lesson: runtime size is the recurring engineering
target across all six emit backends.

| Backend | Runtime location                  | Runtime artifact size | Notes                              |
|---------|-----------------------------------|------------------------|------------------------------------|
| MEP-45 (C)   | `transpiler3/c/runtime/`     | ~5KB ELF symbols       | mochi_alloc, mochi_str, mochi_list |
| MEP-46 (Erlang) | `mochi_runtime` OTP app   | ~40KB beam files       | actor / mailbox / supervisor       |
| MEP-47 (JVM) | `mochi-runtime.jar`          | ~50KB                  | virtual-thread agent, MochiResult  |
| MEP-48 (C#)  | `Mochi.Runtime.dll`          | ~100KB                 | Channel-based agent, Task          |
| MEP-49 (Swift) | `MochiRuntime.swiftmodule` | ~50KB                  | actor + AsyncStream                |
| MEP-50 (Kotlin) | `mochi-runtime.jar`       | ~60KB                  | Channel + SupervisorJob            |
| MEP-51 (Python) | `mochi-runtime/`          | ~10KB pure Python      | asyncio.Queue + TaskGroup          |
| MEP-52 (TS / JS) | `mochi_runtime/`         | <30KB minified ESM     | AsyncIterableQueue + AbortController |

MEP-52 sits at the small end of the runtime-size distribution, on par
with MEP-45 (C) and MEP-51 (Python). The discipline is intentional:
every byte of runtime is a byte the user must download and parse.

## 43. The "Mochi to TypeScript" specific competitive landscape

To restate the unique niche MEP-52 occupies:

- **Not Babel / SWC / esbuild / sucrase / ts-blank-space.** Those are
  JS-source-to-JS-source tools. Mochi has its own source language.
- **Not tsc itself.** tsc is the TS compiler, the dependency, not a
  competitor.
- **Not CoffeeScript / LiveScript / ClojureScript / Elm.** Those emit
  untyped JS. Mochi emits typed TS.
- **Not Pyodide / Brython / Skulpt.** Those host Python in JS. Mochi
  is its own language.
- **Closest cousins**: ReScript, Fable, Kotlin/JS, Scala.js, Dart-to-
  JS. These are all "typed source language compiles to JS / TS";
  Mochi joins this family.
- **Unique to Mochi**: (a) Mochi is a *small-runtime, multi-target*
  language (six backends, MEP-45 to MEP-52, share one IR), (b) the
  query DSL, the Datalog engine, the agent / stream / dataset
  pipeline triad are unique to Mochi's design, (c) the cross-runtime
  matrix (Node + Deno + Bun + browser) from day one is more aggressive
  than any sibling.

## 44. Cross-references

See:

- [[01-language-surface]] for the Mochi -> TS lowering shapes that
  inform what prior art applies.
- [[02-design-philosophy]] for the load-bearing design choices and
  the reasoning that draws on this prior art.
- [[04-runtime]] for the detailed runtime contract (`mochi_runtime/`).
- [[05-codegen-design]] for the codegen pipeline (aotir to TS syntax
  tree to printed source).
- [[10-build-system]] for the npm / tsc build chain.
- [[07-runtime-portability]] for the four-runtime matrix (Node, Deno,
  Bun, browser) details.
- [[08-dataset-pipeline]] for the I/O and dataset framing.
- [[10-build-system]] for `mochi build` UX.
- [[11-testing-gates]] for the per-phase test surface.
- [[12-risks-and-alternatives]] for the rejected paths and the v2
  options.
- [[../0051/03-prior-art-transpilers]] for the Python sibling's prior
  art survey (CPython interpreters, Cython, Pyodide, etc.).
- [[../0050/03-prior-art-transpilers]] for the Kotlin sibling's prior
  art survey (KMP, Kotlin compiler internals, etc.).

## 45. A closing note on the "transpiler" word

A meta-note. The word "transpiler" has lost precision in 2020s
discourse: people use it for Babel (JS-to-JS), tsc (TS-to-JS), SWC
(any-to-JS), and Mochi (Mochi-to-TS) interchangeably. The technical
precision was once "source-to-source compiler at the same abstraction
level"; today, the word means "anything that produces source-language
output". MEP-52 sometimes calls itself a "transpiler" for SEO and
familiarity but is, strictly, a *compiler* with TypeScript as the
target language. The IR-driven pipeline, the type-system lowering, the
runtime semantics: all are compiler-level transformations. The fact
that the output looks like source code is incidental.

The same blurring affected Babel itself: the official Babel docs say
"Babel is a JavaScript compiler" and the Babel team has long argued
the "transpiler" label is reductive. We agree.

## 46. End of survey

This concludes the prior-art survey for MEP-52. Approximately 35
projects across 8 categories were considered. The chosen design
(TypeScript source as canonical output, ES2024 floor, four-runtime
matrix, npm as canonical registry with JSR as secondary, tsc as
type-check gate, hand-rolled `mochi_runtime/`, AsyncIterableQueue +
AbortController for agents, MochiResult discriminated union for
errors) is informed by every project on this list. The closest design
peers are ReScript, Fable, and Kotlin/JS; the closest sibling within
Mochi is MEP-51 (Python). The prior-art delta MEP-52 introduces:
small runtime + four-runtime matrix + npm-and-JSR dual publish + JSR
provenance + Deno Jupyter + browser bundle + the aotir-IR-driven
codegen + the typed Mochi source language as the source of truth.

The competitive landscape is solid; the design point is differentiated;
the underlying lessons from prior art are absorbed without inheriting
their weaknesses. MEP-52 ships.
