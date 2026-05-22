# MEP-47 research note 03, Prior art: languages targeting JVM + AOT toolchains + Android (2014-2026)

Author: research pass for MEP-47.
Date: 2026-05-23 (GMT+7).
Method: structured web research; report distilled below, cross-referenced
against the MEP-45 (C) and MEP-46 (BEAM) prior-art surveys.

The report is the canonical survey for the MEP body's "Rationale" and
"Prior Art" sections. References at the foot are the authoritative source
list. Cross-references to sibling notes use double-bracket slugs
([[02-design-philosophy]], [[05-codegen-design]], [[07-jvm-target-portability]],
[[09-agent-streams]]) where applicable.

---

## Survey: state of the art in compiling high-level languages to the JVM (2014-2026)

This survey covers production JVM transpilers, AOT toolchains, and the
Android pipeline relevant to designing a Mochi-to-JVM transpiler in 2026
against a JDK 21 LTS floor (with JDK 25 LTS as the secondary target).
It is structured by *system* (sections 1-12), then by *toolchain* (sections
13-18), then *Android* (sections 19-22), then *libraries* (sections 23-27),
closing with distilled design lessons (section 28).

## 1. Kotlin

Kotlin (JetBrains, 1.0 in Feb 2016; v2.0 with the K2 compiler in May 2024;
v2.1 in Nov 2024; v2.3 in late 2025 with K2 fully stable; v2.2.20 added
`invokedynamic` for `when` expressions). The K2 compiler is the second
rewrite of the front-end and the architectural baseline a Mochi-to-JVM
project should study most closely.

K2 splits compilation into four stages: parse → FIR (Frontend Intermediate
Representation, a mutable tree enriched with semantic information; this
replaces the K1 era's PSI tree plus separate `BindingContext`) → IR
(Backend Intermediate Representation, shared across the JVM, JS, Native,
and Wasm backends via `Fir2Ir`) → bytecode emit. JetBrains reports up to
94% compilation-speed gains versus K1 and around 376% faster analysis;
the architectural unification is what enables fair compiler-plugin APIs
(per the new `FirStatusTransformerExtension`, `FirDeclarationGenerationExtension`,
etc., now used by Spring's all-open plugin and `kotlinx.serialization`).

Lambdas are the single feature most worth copying. Since Kotlin 1.5, the
`-Xlambdas=indy` flag opted in to `invokedynamic` + `LambdaMetafactory`
emission; since Kotlin 2.0 (May 2024), `invokedynamic` is the *default*.
Bootstrap methods are stored in the class file once; the VM materialises
the lambda class lazily on first call, which both shrinks JAR size and
defers cost. Non-capturing lambdas reuse a singleton (much like Java's
`LambdaMetafactory`). Capturing lambdas allocate the function object plus
a `Ref` wrapper per mutable `var` capture, on every invocation, which is
the documented allocation hot spot. Two drawbacks remain in 2026:
`invokedynamic`-compiled lambdas cannot be serialised, and the experimental
`reflect()` API does not see them. Kotlin 2.2.20 extended `invokedynamic`
to type-check `when` expressions, generating switch-shaped bytecode
similar to Java's `switch` instead of cascading `instanceof` chains, when
all branches are `is`/null checks against the same subject. Mochi's
ADT match should target the same pattern.

Coroutines are the second feature to study. A `suspend fun` is rewritten
at the IR level via CPS transformation: a hidden `Continuation` parameter
is appended, the return type becomes `Any?` (so the function can return
the result or the sentinel `COROUTINE_SUSPENDED`), and the body becomes
a state machine. `CoroutineTransformerMethodVisitor` is the bytecode-level
pass that inserts a `TABLESWITCH` on an `int` label field, spills locals
into typed fields named by JVM descriptor (`L$0`, `L$1`, `I$0`, `J$0`...),
and emits `invokeSuspend` as the resume entry point. The compiler also
performs a tail-call analysis: if all suspension points are tail calls,
the state machine is elided entirely. A continuation object for a four-parameter,
five-local suspend method holds at least ten fields. Mochi's agents/streams
([[09-agent-streams]]) should use this exact shape; the lesson is that the
state-machine cost is *zero allocations per yield once spilled*, which is
how Kotlin/coroutines beats thread-per-task at p99 latency.

Generics survive type-erasure with the documented limitations: `reified`
parameters require `inline` functions; `Class<T>` arguments are smuggled
through `LambdaMetafactory` for cross-platform reflection in serialisation
libs. Concurrency is officially `kotlinx.coroutines` with structured
concurrency (`Job`/`CoroutineScope`/cancellation propagation); the JDK
floor is JDK 8 for the language and JDK 11 for the standard library, with
JDK 17 increasingly the minimum for new libraries. Deployment is normal
`jar`/`uber-jar` plus, for Android, the Kotlin-specific tooling (KSP,
incremental compilation). Kotlin Multiplatform (KMP) is officially stable
since November 2023; Compose Multiplatform for iOS hit Stable in v1.8.0
(May 2025), Wasm in Beta as of late 2025.

**Lesson for MEP-47:** the K2 pipeline (parse → FIR → IR → multi-backend)
is the cleanest known architecture for a multi-target compiler with
front-end pluggability. Mochi's existing vm3 IR is a reasonable FIR-shape
candidate; introduce a Mochi-IR layer (analogous to Kotlin's shared IR)
that the JVM, C ([[MEP-45]]), and BEAM ([[MEP-46]]) backends all consume.
*Always* emit `invokedynamic` for closures (`LambdaMetafactory`) on JDK 21+;
emit CPS state machines with `TABLESWITCH` for `async`/streams; mirror
Kotlin 2.2.20's `invokedynamic`-for-typed-match trick for Mochi ADT
pattern matching.

## 2. Scala 3

Scala 3 (a.k.a. Dotty; v3.0 in May 2021; v3.3 LTS in Oct 2023; v3.6 in
2025; v3.8 in early 2026 with Scala-3-compiled stdlib). The compiler is
itself written in Scala 3. The pipeline is parse → typer → erasure →
PostTyper → many lowering phases → TASTy emit + classfile emit. The
distinguishing IR is **TASTy** (Typed Abstract Syntax Trees), a high-level
binary interchange format that preserves union types, intersection types,
opaque type aliases, capture-checking annotations, and full positions
after type-checking. The TASTy version number tracks language minor
versions (Scala 3.0 = TASTy 28.0-0); within a major TASTy version,
forward and backward reads are supported.

Why TASTy matters: classfiles erase type information (a `List[String]`
becomes `List[Object]` in bytecode), but downstream tools (the language
server, separate compilation, macros, tasty-query for static analysis)
need the precise types. TASTy is the canonical preserved IR. Scala 3.8
(early 2026) made an ecosystem-shaping change by *compiling the standard
library with Scala 3* instead of reusing the Scala 2.13 stdlib, which
introduced TASTy-level features (union types, capture-checking) that
Scala 2.13 cannot consume; the Scala 2 ↔ Scala 3 cross-compile direction
of "Scala 2.13 reads Scala 3 TASTy" was dropped at 3.8, while the
"Scala 3 reads Scala 2 artifacts" direction remains supported indefinitely.

Sum types lower to sealed class hierarchies plus generated `unapply`
extractors; pattern matching compiles via the standard Maranget decision
tree (same algorithm as Erlang, OCaml, Rust). A long-standing optimisation
gap is documented: even for tag-only enums like `enum CompOp { case Eq, Neq }`,
Dotty does not emit a `tableswitch` automatically; the `@switch` annotation
forces verification, and workarounds match on `.ordinal`. String matches
likewise generate cascading `if`/`else` rather than the hash-based
`tableswitch` that `javac` produces. **Mochi should not depend on the
language's match compiler to produce a `tableswitch` for tag-only ADTs;
emit the `tableswitch` directly at the bytecode layer, taking the Maranget
DAG output as input.**

Closures use the same `invokedynamic` + `LambdaMetafactory` infrastructure
as Java and Kotlin. Concurrency is officially `cats-effect` v3, `zio` v2,
`Ox` (loom-based, JDK 21 virtual threads), and `Akka`/`Pekko` (actor model).
Scala 3 explicitly targets the JDK 8 bytecode floor for compatibility but
modern artifacts increasingly target JDK 11/17; deployment is normal jar/
sbt-assembly. JDK 21 virtual threads (Project Loom) are now the
recommended concurrency primitive for new Scala libs in 2026.

**Lesson for MEP-47:** TASTy is the model for "preserve full source semantics
in the artifact." Mochi will probably not need a TASTy-shape format
day one, but should reserve a `.mochi-tasty` slot in the jar layout for
type information so that future incremental compilation, IDE tooling,
and Datalog/agent metadata survive the lossy JVM bytecode emit. The
Maranget decision tree is the standard, *but* the JVM backend must
explicitly lower flat ADT matches to `tableswitch`/`lookupswitch` rather
than trusting the C-style fallback.

## 3. Clojure

Clojure (Rich Hickey, 2007; v1.12.2 in Aug 2025) is the dominant dynamic
JVM language and the canonical "Lisp on JVM that works in production."
The compiler is in `Compiler.java` (yes, the Clojure compiler is itself
in Java, by design) and emits bytecode directly via ASM. Functions become
classes implementing `IFn` with `invoke(arg0...)` overloads up to 20 args
plus a varargs tail; closures are these same classes with captured locals
stored as final fields set by the constructor. Persistent collections
(`PersistentVector`, `PersistentHashMap`) are HAMT/RRB-tree implementations
borrowed from Bagwell and adapted; vectors are 32-way tries with array
leaves; maps use CHAMP (Compressed Hash-Array Mapped Prefix-tree).

Clojure deliberately did *not* adopt `invokedynamic` for its internal
dispatch. The rationale (Alex Miller, Ghadi Shayban, Clojure dev list,
2022 and 2024): all Clojure fns share the same `IFn` interface, so the
indirection that `invokedynamic` is designed to amortise does not exist;
Vars are mutable boxes that you `alter-var-root!` to change a fn, not
recompile or relink. Reflection (for Java interop only) is detected at
compile time and reported as a warning when types are missing; type hints
(`^String`) eliminate it. The `invokedynamic` discussion remains
exploratory in 2026; it has not been merged.

Deployment is uber-jar via `tools.build`/`tools.deps` or `leiningen`;
GraalVM native-image works but requires `-H:+ReportExceptionStackTraces`,
manual reflection config, and frequent use of the `clj-easy/graal-build-time`
helper because Clojure's runtime initialisation includes reading the
classpath at static-init time, which violates native-image's closed-world
assumption. Babashka (Michiel Borkent) is the production-relevant fork
that ships a SCI (Small Clojure Interpreter) compiled as a GraalVM native
binary, used for shell scripting and CI; it gives ~10ms startup vs the
JVM's 500ms.

**Lesson for MEP-47:** Clojure's "everything is an `IFn`" interface lets
the JIT inline aggressively without per-callsite cache machinery, but it
also locks you into single dispatch. Mochi's static type system can do
better by lowering known-arity calls to direct `invokevirtual`/`invokestatic`
and only falling back to an `Fn` interface for first-class values.
Persistent collections are *not* optional for a functional language on
JVM; copy Clojure's HAMT/RRB designs (or use Vavr's, which are Apache-licensed
adaptations).

## 4. Groovy

Groovy (Apache, v1.0 in 2007; v4.0 in 2022; v5.0 in 2024-2025) is the
oldest still-active dynamic JVM language. It has two compilation modes:
the legacy "callsite-array" path (compatible back to JDK 6) and the
`indy` path (`-indy` flag, default in many distributions for JDK 8+).
With `indy`, dispatch routes through `invokedynamic` bootstrap methods
registered with the `IndyInterface`; without, every call goes through a
generated `$getCallSiteArray()` plus `CallSite.call()` interface, which
adds two interface dispatches per call.

AST transformations are Groovy's claim to fame and the lineage for
later compile-time metaprogramming systems (Manifold, kapt, KSP, Lombok).
Global transforms apply to any source unit on the compile classpath; local
transforms are activated by annotations (`@Immutable`, `@Delegate`,
`@TupleConstructor`, etc.) and can hook into nine compile phases
(INITIALIZATION, PARSING, CONVERSION, SEMANTIC_ANALYSIS, CANONICALIZATION,
INSTRUCTION_SELECTION, CLASS_GENERATION, OUTPUT, FINALIZATION).
SEMANTIC_ANALYSIS is the earliest phase where a local transform may run.

Groovy's commercial relevance peaked around 2014 (Gradle, Jenkins
pipelines), declined as Kotlin and Scala overtook it, but remains the
default DSL for Gradle build files (now slowly migrating to Kotlin DSL)
and the scripting language for Jenkins/Grails.

**Lesson for MEP-47:** Groovy's nine-phase AST-transform model is overkill
for Mochi, but the *two-mode codegen* idea (legacy interface dispatch as a
safe fallback, `invokedynamic` as the optimised default) is worth
borrowing for downlevel JDK targets. If Mochi wants a JDK 8 escape valve,
two emit modes solve it cleanly.

## 5. JRuby and TruffleRuby

**JRuby** (Charles Nutter, Thomas Enebo; v9.4 in 2023 supports MRI Ruby
3.1, v10.0 in 2025 supports Ruby 3.4) is the long-lived Ruby-on-JVM
implementation. It uses ASM to emit bytecode; methods are JVM methods
on a per-class basis with `invokedynamic`-routed call sites since JDK 7.
JRuby is roughly 2× faster than CRuby on long-running steady-state
workloads with the server VM (`--server`) and `invokedynamic` enabled,
but its startup penalty is severe: a Rails boot costs 30-60s on JRuby
versus 3-8s on MRI. Strings are backed by `byte[]` and therefore capped at
2^31-1 bytes per string.

**TruffleRuby** (Chris Seaton, Benoit Daloze, Oracle Labs; in production
since 2017, now a flagship GraalVM language) takes a radically different
approach: a Truffle AST interpreter that the Graal JIT specialises into a
single optimised native method per Ruby method, achieving peak performance
that beats MRI's YJIT on the `yjit-bench` suite (railsbench, etc.). The
trade-off is *warmup*: TruffleRuby needs 30-120s to reach peak. Native-image
TruffleRuby reduces startup at the cost of peak. Continuations (`callcc`)
"are unlikely to ever be implemented" because Truffle's model is incompatible
with stack capture.

**Lesson for MEP-47:** Truffle is a credible alternative to bytecode emit
for dynamic-typed languages, but Mochi is statically typed and would lose
more than it gains by going Truffle (Truffle's advantage is the
profile-driven specialisation that static typing makes irrelevant). The
*operational* lesson is that 2026 JVM startup is dominated by class
loading and JIT warmup, both addressed by Project Leyden's AOT cache and
CRaC (see §15-§16); a Mochi JVM target should be designed around these
from day one, not as an afterthought.

## 6. Jython, Kawa, ABCL

**Jython** (Frank Wierzbicki, Jim Baker; v2.7 in 2015, last stable in 2020;
Python 3 support has been "in progress" since 2017) is the canonical
dynamic-language-on-JVM cautionary tale. Jython 3 is architecturally
sound on paper (PEG parser, `MethodHandle`-filled type slots, no GIL,
`invokedynamic` call sites) but stalled because the Python ecosystem
depends on C extensions (`numpy`, `scipy`) that Jython cannot run. As of
May 2026, no official Python 3 release exists; an unofficial `jython3`
fork by `aaveshdev` exists but is not production-validated. GraalPy
(Truffle-based) has effectively absorbed the Python-on-JVM use case.

**Kawa** (Per Bothner, since 1996; v3.1.2 in 2024) is the longest-lived
Scheme on JVM and a model for compile-on-the-fly bytecode emit. Each
REPL form compiles to bytecode immediately via Kawa's own bytecode
writer (predates ASM in this codebase). Continuations are limited:
`call/cc` is implemented via thrown `CalledContinuation` exceptions and
can only unwind, not resume into multiple branches. André Bask's thesis
(2018) implements full first-class continuations on JVM via a syntax-tree
transformation (generalised stack inspection à la Pettyjohn 2005), enabled
as an opt-in compiler pass.

**ABCL** (Armed Bear Common Lisp; Erik Hülsmann, Mark Evenson; v1.9.2 in
2024) is the only mostly-complete Common Lisp on JVM. It compiles to
JVM bytecode, can run SBCL-targeted code with light modifications, and
is the standard answer for "Common Lisp in a JVM ecosystem." Continuations
use Kawa's exception-throwing scheme; the CLOS implementation is
performant if not as fast as SBCL's.

**Lesson for MEP-47:** stack-capturing continuations on JVM are a research
project; do *not* take this on for Mochi. JDK 21 virtual threads (Loom)
solve the use case for which `call/cc` historically existed (coroutines,
generators, async I/O) without the language-implementer needing to roll
stack switching. Use Loom.

## 7. Eta and Frege (Haskell dialects)

**Eta** (TypeLead Inc., Rahul Muttineni; v0.8 in 2018, last commit 2019,
effectively abandoned in 2026) was a fork of GHC retargeted to emit JVM
bytecode. The pipeline mirrored GHC's: Haskell → Core → STG → Java
bytecode. The runtime implemented the Spineless Tagless G-machine (STG)
in pure Java with thunks as heap objects, lazy evaluation via mutating
thunk pointers, and a `trampoline` function in `Data.Function` for
explicit tail-call optimisation (because the JVM lacks tail-call
elimination for arbitrary functions). The Eta team's commercial pitch
(EtaPad, Stack-on-JVM) ran out of funding around 2019; the project's
GitHub last had meaningful activity in early 2020. The technical
post-mortem is the *runtime cost of laziness*: every thunk allocation,
every `update` mutation, every black-hole synchronisation traveled
through the JVM heap and GC, which on a moving GC like G1 created the
worst-case patterns (massive numbers of small short-lived objects with
mutating references).

**Frege** (Ingo Wechsung, since 2011; still maintained 2025 with commits
through Nov 2025) is the surviving alternative: a Haskell-2010 dialect
on JVM that supports almost the entire Haskell 2010 standard library but
*not* the extensions that the Hackage ecosystem assumes (no
`MultiParamTypeClasses`, no `TypeFamilies`, limited `GADTs`). Frege does
not aim for Hackage compatibility. Laziness uses a similar thunk runtime,
but the language's scope keeps the surface area manageable; runtime
sizes are smaller; deployment is a normal jar plus the `frege-core` runtime.

**Lesson for MEP-47:** lazy evaluation on the JVM is *the* recurring
disaster in this space. Eta and Frege both prove that you can implement
it, and Eta's death proves that you should not. Mochi is strict by
construction; preserve this. If a Mochi user wants lazy semantics, give
them `lazy<T>` as an opt-in box type with explicit `force()`, not a
language-wide change.

## 8. Ceylon (archived: what we can learn)

Ceylon (Gavin King, Red Hat; v1.0 in 2013; Eclipse Foundation handover
in 2017; repository archived April 2023). Ceylon shipped union/intersection
types, reified generics on JVM *and* JavaScript, declaration-site
variance, principal-type local inference, and first-class modules.
Technically novel: union/intersection types in Ceylon predate and
influenced TypeScript's analogous features. Ceylon also got reified
generics through a custom `TypeDescriptor` mechanism (passed as hidden
arguments), avoiding type erasure at runtime.

Why Ceylon died (the documented post-mortems on Hacker News, InfoWorld,
Eclipse forum threads):

1. *No internal dogfooding.* Red Hat never adopted Ceylon for its own
   products (JBoss, Wildfly stayed on Java); contrast Rust at Mozilla,
   Kotlin at JetBrains. Without an internal driver, the language had no
   forcing function to fix usability gaps.

2. *Late to a saturated market.* By 2013, Scala had 7 years of head start,
   Kotlin had announced in 2011, Groovy was entrenched in build tooling.
   Ceylon offered "better Java" but so did everyone else.

3. *Bespoke standard library.* Ceylon insisted on its own
   `ceylon.collection`, `ceylon.io`, `ceylon.process` etc., which made
   interop with the JVM ecosystem (which lives in `java.util.*`,
   `java.nio.*`, `java.lang.Process`) painful. Compare with Kotlin's
   strategy of using `java.util.List` directly and adding extensions.

4. *Eclipse handover as soft exit.* Vendor-neutral foundation moves are
   often interpreted (correctly) as a sponsor reducing investment, not
   broadening it. Mochi should not assume that "give it to Eclipse/CNCF/
   Linux Foundation" rescues a stalled language.

**Lesson for MEP-47:** the Mochi-on-JVM ecosystem strategy must lean
hard on the JVM stdlib (`java.util.List`, `java.util.Map`,
`java.time`, `java.nio.file`, virtual threads via `Thread.ofVirtual()`,
`java.net.http.HttpClient`) and add Mochi semantic veneers as
*extensions*, not replacements. See [[07-jvm-target-portability]] for
the binding strategy.

## 9. Fantom

Fantom (Brian Frank, Andy Frank; v1.0 in 2008; v1.0.83 in 2024-2025,
still active) is an under-known JVM language that anticipated several
Kotlin/Scala features: nullable types in the type system, immutability
by default, actor concurrency, a portable widget toolkit (FWT). Code
compiles to a custom bytecode (`FCode`) packed into `.pod` files (ZIP
archives containing FCode, docs, resources); `pods` are the deployment
unit, conceptually similar to OSGi bundles. At runtime, FCode is
translated to JVM bytecode (or JavaScript) on load.

Two design lessons stand out. First, Fantom's `pod` format is what
Scala 3 eventually arrived at with TASTy: a per-module artifact carrying
more than classfiles. Second, Fantom's commercial niche is *industrial
control* (the SkySpark building-automation platform is its main user),
which kept the language alive long past its general-language traction
peak. The .NET/CLR backend exists but is in "prototype" status and not
actively developed in 2026; JVM and JavaScript are the focus.

**Lesson for MEP-47:** carry typing/metadata in the deployment artifact
(a `.mochi-meta` block alongside the classfiles in a jar). Fantom and
Scala 3 independently arrived here; Kotlin's `@Metadata` annotation
(stored on classfiles) is the lightweight version of the same idea.

## 10. Apache Polyglot DSL languages

GraalVM's polyglot interop turned the JVM into a polyglot runtime. As
of GraalVM 24 (2025), the official languages with Truffle implementations
are JavaScript (GraalJS), Python (GraalPy), Ruby (TruffleRuby), R
(FastR; in maintenance), LLVM bitcode (Sulong, for C/C++/Rust interop),
and WebAssembly (GraalWasm). Truffle Java (Espresso) is a Truffle-based
*Java interpreter* used for Java-on-Native-Image scenarios and language
isolation. The Polyglot API lets a Mochi program call into JS/Python/Ruby
contexts at runtime with zero serialisation overhead, in a single JVM
process.

This is the strongest argument for targeting GraalVM as a deployment
mode: a single Mochi binary can host JS plugins, Python data-science
notebooks, and Ruby scripts without separate processes. The downside is
that GraalVM polyglot is *not* part of the OpenJDK distribution; users
must explicitly install the GraalVM JDK.

## 11. Manifold

Manifold (Scott McKinney, since 2017; v2025.x supports JDK 8 through 25
and "latest") is a Java compiler *plugin* that adds extension methods,
properties, operator overloading, structural typing, type-providers
for JSON/YAML/XML/GraphQL/CSV/SQL, optional parameters, tuple expressions,
and a C-style preprocessor (`#define`, `#if`, `#elif`, `#endif`).
Manifold's preprocessor handles tiered symbol definition (build.properties,
`-Akey=value` javac args, environment symbols `JAVA_9_OR_LATER`, Android
build-variant symbols `DEBUG`/`BUILD_TYPE`/`FLAVOR`).

Manifold is fundamentally a compile-time metaprogramming engine sitting
inside `javac`. It is *not* a transpiler; Mochi will not emit Manifold
plugins. But Manifold's type-provider design (point at a JSON schema, get
a typed Java class instantly without code generation steps) is exactly
the user-facing model Mochi's `dataset` integration should reach for:
binding Mochi types to external schemas without intermediate `gen` steps.

## 12. Smaller / archived JVM languages worth noting

- **Caramel-on-JVM** (no such project exists; Caramel was OCaml-to-BEAM
  per [[MEP-46 prior art]]). Listed here for clarification.

- **X10** (IBM Research, 2004-2018) was a parallel-programming JVM
  language that introduced the "places" model for distributed memory.
  Archived. Its async/finish/at constructs influenced Habanero-Java and,
  indirectly, the Loom virtual-thread design.

- **Whiley** (David Pearce, since 2008) features extended static checking
  via SMT solvers and predicate types. Still active 2025; targets JVM
  via classfile emit. Niche, but a good study for Mochi's optional
  refinement-types story.

- **Mirah** (Charles Nutter, 2008-2014): Ruby-syntax surface compiling to
  JVM bytecode with type inference. Abandoned. The lesson: surface
  language without an ecosystem (no IDE, no build tool integration) does
  not survive.

- **Xtend** (Eclipse, since 2012): Java-shaped DSL with extension methods,
  template expressions, lambdas; transpiles to *Java source*, not
  bytecode, relying on `javac`. Still maintained for Eclipse Modeling
  Framework users. The "transpile to Java source" strategy avoids the
  bytecode-emission complexity entirely but gives up control over
  generated code shape, debuggability via line directives, and access to
  JVM features not exposed in source (e.g. `invokedynamic`, custom
  method handles). **Mochi should not take this path.**

- **Bali Phaser / J3** (academic, 2020): research compilers targeting
  Java 17+ with refinement types. Reference points, not production.

## 13. ASM (the dominant bytecode library)

ASM (ObjectWeb consortium; v9.7 in 2024 supports Java 21/22/23; v9.8
adds Java 24; v9.9 in 2025 supports Java 25; v10.0 expected mid-2026
with broader records/sealed/value-class support). ASM is the lowest-level
mainstream Java bytecode lib, the de facto standard since 2002, and the
implementation substrate for Spring Framework, AspectJ, Hibernate,
Mockito (via Byte Buddy), Gradle, IntelliJ IDEA, and the OpenJDK itself
(`com.sun.tools.attach`, `jpackage`). ASM offers visitor APIs (`ClassVisitor`,
`MethodVisitor`, `FieldVisitor`) and tree APIs (`ClassNode`, `MethodNode`)
for both reading and writing. The visitor API is streaming, zero-copy,
and memory-efficient; the tree API is more ergonomic but allocates the
full graph.

ASM versioning matters: each Java release adds new class-file format
versions (Java 21 = 65, Java 22 = 66, Java 23 = 67, Java 24 = 68, Java 25 =
69). Using ASM 9.4 to emit Java 21 bytecode produces a `UnsupportedClassVersionError`
in many real-world configurations because constants such as `Opcodes.V21` do
not exist in the older ASM. For MEP-47's JDK 21 floor, **ASM 9.7+ is
mandatory; ASM 9.9 is recommended for JDK 25 LTS forward compatibility.**

Operationally, ASM is what you should emit *to*. Mochi's IR-to-bytecode
emit pass writes `ClassVisitor`/`MethodVisitor` calls; never assemble
text or generate `.class` byte-by-byte.

## 14. Byte Buddy and Janino (higher-level libs)

**Byte Buddy** (Rafael Winterhalter, since 2014; v1.15 in 2025) is the
DSL-on-top-of-ASM that powers Mockito, Hibernate, JPA providers, APM
agents (New Relic, Datadog), and most Java agents. Its key feature is
*runtime attachment*: load Byte Buddy via the JVMTI agent mechanism,
intercept any class load, rewrite bytecode on the fly. It also offers a
"main jar" multi-release model that supports JVMs from Java 5 onward in
a single artifact, which matters for agent authors who must run on legacy
JVMs. Byte Buddy repackages ASM internally as `net.bytebuddy.jar.asm`
to avoid version conflicts with user-supplied ASM. For build-time class
generation, Byte Buddy provides Maven and Gradle plugins.

**Janino** (Arno Unkrig, since 2001; v3.1.12 in 2024) is an embedded
Java *compiler* that produces bytecode in-memory from Java *source* (one
expression, one block, one class body, or full source files). Used by
Apache Flink, Apache Spark, Apache Calcite, JBoss Drools, and Logback to
JIT-compile user expressions into JVM methods. Janino is independent of
`tools.jar` and the JDK's `javax.tools.JavaCompiler` API, making it the
lightweight alternative when you cannot assume a JDK is present (only a
JRE).

**Eclipse JDT Core compiler (ECJ)** is the alternative: a full incremental
Java compiler that ships as a library, used by the Eclipse IDE for
on-keystroke compilation. It supports Java source levels well ahead of
the bundled OpenJDK in IDE distributions. ECJ is roughly 10× larger than
Janino but compiles full Java; if Mochi ever needed to compile generated
Java source as an intermediate step (it should not), ECJ is the
production-tested option.

**Lesson for MEP-47:** emit ASM bytecode directly from Mochi-IR; *do not*
go via Java source. Reserve Byte Buddy as an emit option only if Mochi
adds runtime instrumentation/agent features. Avoid Janino-style "generate
Java source then compile" except for prototyping.

## 15. GraalVM native-image (AOT)

GraalVM native-image (Oracle Labs; v24 in 2025 with JDK 24 baseline; v25
LTS in late 2025 with JDK 25 baseline) is the production AOT compiler
for JVM languages. The model: at build time, perform whole-program
points-to analysis from the `main` entry point; only reachable classes,
methods, and fields are included in the binary; everything else is
elided. The result is a native ELF/PE/Mach-O executable starting in
40-100 ms versus 1-2 s for the equivalent on the JVM.

The dominant constraint is the **closed-world assumption**: classes
loaded at runtime via `Class.forName(name)` where `name` is data-driven
must be declared at build time in `reflect-config.json`, `resource-config.json`,
`serialization-config.json`, etc. Dynamic proxies via `java.lang.reflect.Proxy`
need `proxy-config.json`. JNI access needs `jni-config.json`. The
GraalVM Tracing Agent (`-agentlib:native-image-agent`) runs the app once
on the JVM and produces all of these configs from observed behaviour.
Frameworks like Spring Boot 3+ (Spring Native), Quarkus, and Micronaut
ship metadata bundled in their jars, so a stock Spring application builds
to a native image without manual config in 2026.

Benchmarks (2025-2026, AMD EPYC 7763 / Spring Boot 3.2): native-image
reduces startup by ~70% (1.42 s → 407 ms cold) but reduces peak throughput
by ~8% on long-running CPU-bound workloads. For short-lived serverless
functions, native-image is ~22% faster *overall* because the JVM never
finishes its warm-up.

Key flags as of GraalVM 25: `--gc=G1` (G1 GC; default is the simpler
SerialGC), `--enable-preview`, `-O3` (the new optimisation level
introduced in 23.x), `-march=native` (CPU-specific instructions),
`--initialize-at-build-time` / `--initialize-at-run-time` (class-init
control), `--no-fallback` (fail rather than emit a fallback JVM image).

**Lesson for MEP-47:** GraalVM is the default story for "deploy Mochi as
a single-file native binary" alongside MEP-45's C path. Mochi's
generated code must avoid reflective patterns that defeat points-to
analysis: no `Class.forName(arg)`, no `MethodHandles.lookup()` on
user-provided names, declare all `invokedynamic` bootstrap methods
statically. The build pipeline in [[10-build-system]] should support
`mochi build --target=jvm --aot=graalvm-native-image` as a first-class
mode.

## 16. Project Leyden, CRaC, jlink, jpackage

The mainline OpenJDK answer to native-image's startup advantage.

**Project Leyden** (premain branch) ships incrementally as JEPs. JEP 483
(JDK 24, March 2025) moved class loading and linking to a one-time
training phase, storing fully linked classes in an `.aot` cache. JEP 514
and JEP 515 (JDK 25, September 2025) added the simplified workflow
(`-XX:AOTMode=create` / `-XX:AOTMode=on`, `-XX:AOTCache=app.aot`) and
method-profile capture. JEP 516 (JDK 26) ships a baseline AOT cache for
JDK classes, so even apps with no training run gain a small startup win.
Two features remain in the experimental premain branch: AOT *code*
compilation (JEP draft 8335368, storing pre-JIT'd native code) and AOT
dynamic-proxy generation (for Spring/Hibernate). The Leyden cache is
machine-specific, GC-specific, JVM-build-specific; not portable across
machines. Reported gains: ~41% faster Spring PetClinic startup with the
JDK 25 cache.

**CRaC** (Coordinated Restore at Checkpoint; OpenJDK Project, not yet
mainline) uses CRIU (Checkpoint/Restore In Userspace) at the OS level to
snapshot a warmed-up JVM and restore it later. Restoration takes
~50-300 ms regardless of the original warmup cost. The Java API requires
applications to implement `org.crac.Resource` and handle
`beforeCheckpoint()` (close file handles, drain connection pools) and
`afterRestore()` (reopen, refresh tokens). CRaC ships in production via
BellSoft Liberica (JDK 17, 21), Azul Zulu, and Ubuntu (`java-21-openjdk-crac-amd64`
package as of Sep 2025). Spring Boot 3.2+, Quarkus, and Micronaut all
support CRaC out of the box.

**jlink** (since JDK 9) produces a custom JRE image containing only the
JDK modules the app actually uses, often 30-50 MB versus the full JDK's
~300 MB. Combined with `--strip-debug --no-man-pages --no-header-files
--compress=2`, the runtime image is small enough to embed in containers
without a multi-stage Docker build penalty.

**jpackage** (since JDK 14) wraps a jlink runtime plus the app jar into
a platform-native installer (`.dmg`, `.msi`, `.deb`, `.rpm`, `.app`,
`.pkg`). It is the JVM equivalent of Cosmopolitan (see [[MEP-45 prior art §9]])
but per-platform rather than polyglot.

**Lesson for MEP-47:** for Mochi's "ship a self-contained Mochi program"
story, jlink + jpackage is the *default* answer on JDK 21+; GraalVM
native-image is the *opt-in* answer for serverless / CLI / smallest
binary. CRaC is the high-throughput / fast-restart answer for server
workloads. The build system in [[10-build-system]] should support all
three.

## 17. OpenJ9 AOTC

IBM/Eclipse OpenJ9 (formerly J9; open-sourced 2017; v0.52 paired with
JDK 21 in 2024) is the OpenJDK-alternative JVM. Its AOT story predates
Leyden by a decade: a *Shared Class Cache* (SCC), populated by a "cold
run" of the JVM, stores class metadata and AOT-compiled native code
that subsequent JVM instances load from disk. AOT-compiled code is
slower than JIT-compiled code (because it cannot assume runtime
invariants and must include validation/relocation records) but faster
than interpretation. Enabled by `-Xshareclasses`; the JVM heuristics
decide what to AOT compile based on observed startup phases.

OpenJ9's footprint is the selling point: routinely 50% less heap than
HotSpot for the same workload. In containerised microservice deployments,
this is significant. IBM continues to ship OpenJ9 as the JVM under
WebSphere Liberty.

**Lesson for MEP-47:** Mochi-emitted classfiles should be JVM-vendor-portable
(no HotSpot intrinsics, no OpenJ9-only APIs). Standard JDK Class-File
API output works equally well on HotSpot, OpenJ9, Azul Zing, GraalVM JIT.

## 18. JDK Class-File API (JEP 484)

JDK 24 (March 2025) finalised JEP 484: the standard `java.lang.classfile`
API. This is *finally* a JDK-bundled alternative to ASM, designed by
Brian Goetz and Adam Sotona. It exposes `ClassFile`, `ClassModel`,
`MethodModel`, `CodeBuilder`, `ClassTransform`, etc. Compared to ASM:
fluent API, immutable models, transformer composition built-in, no
external dependency, evolves in sync with the JDK's class-file format
changes.

For a 2026 Mochi-to-JVM transpiler, the choice between ASM and
`java.lang.classfile` comes down to JDK floor:

- Mochi running *on* JDK 21+ as the host (compiler runs on JDK 21):
  ASM is still required for emit, because `java.lang.classfile` is
  JDK 24+.
- Mochi running on JDK 24+ as the host: `java.lang.classfile` is the
  forward-looking choice.

Since Mochi's compiler is in Go (per the project setup), neither matters
for the compiler implementation; what matters is which library to *bundle*
as the runtime if Mochi emits classfiles at build time via a Java helper.
The clean answer is: emit classfiles directly from Go (with our own
small ASM-equivalent in Go) and skip the dependency entirely. See
[[05-codegen-design]].

## 19. Android: D8, R8, ART

The Android pipeline replaced Sun's `dx` and Guardsquare's ProGuard in
2017-2018 with Google's D8 (DEX compiler) and R8 (shrinker/optimiser/obfuscator).
D8 converts JVM bytecode (`.class`) to DEX bytecode (`.dex`); R8 performs
tree-shaking, code shrinking, optimisation, and obfuscation, all in a
single tool. As of Android Studio Iguana / AGP 8.5 (2024-2025), R8 is the
default and ProGuard is fully deprecated.

The Android Runtime (ART) replaced Dalvik in Android 5.0 (API 21, 2014),
ending the Dalvik era. Dalvik was a JIT-only register-based VM; ART
introduced ahead-of-time compilation via `dex2oat`. Android 7 (API 24,
2016) added back a profile-guided JIT to complement the AOT step, giving
the modern *hybrid* model: install → interpreted → JIT (records hot
methods to a profile) → idle-time `dex2oat` runs and produces optimised
`.oat`/`.odex` files using the profile → subsequent runs use the AOT
code. On modern Pixel devices, Google Play Cloud Profile ships a
pre-trained profile via the `.dm` (DEX metadata) file, so the AOT step
runs at install time with the cloud profile.

The 2025 ART team shipped an 18% reduction in compile time without
regressing code quality, distributed via the June 2025 Android release
and the end-of-year release; importantly, Android 12+ devices receive
these via Mainline updates, decoupling ART improvements from full OS
upgrades.

The DEX format uses 16-bit method/field indices, hence the **64K method
limit per DEX file** that motivated multidex.

## 20. Android multidex and API level matrix

The 64K reference limit per DEX file (65,536 entries: methods, fields,
classes) bit many large apps. Pre-Android-5.0 (API ≤ 20, Dalvik): apps
must enable multidex explicitly via `multiDexEnabled true` + the
`androidx.multidex:multidex:2.0.1` library + extending `MultiDexApplication`
or declaring it in the manifest. API 21+ (ART): multidex is automatic;
ART pre-compiles all DEX files at install into a single OAT, and the
runtime cost evaporates.

The minimum API level matrix for 2026 Android deployment is:

- `minSdk = 21` (Android 5.0, Lollipop, 2014) is the modern *practical*
  floor: ART, automatic multidex, AndroidX support, AAPT2 baseline.
- `minSdk = 24` (Android 7.0, Nougat, 2016) lets you use Java 8 language
  features without desugaring overhead and gets `java.time` natively.
- `minSdk = 26` (Android 8.0, Oreo, 2017) is what most new apps target;
  background-execution limits, notification channels, runtime permissions.
- `minSdk = 31` (Android 12, 2021) is the floor for many modern Jetpack
  libraries.
- `compileSdk` should match the *latest* Android SDK at all times (35 in
  2024, 36 in 2025); `targetSdk` is the Google Play required floor and
  has been advancing roughly one major Android release per year.

Kotlin Multiplatform Mobile (KMM; now usually called just KMP) is
officially production-ready as of November 2023; Compose Multiplatform
for iOS reached Stable with v1.8.0 in May 2025; production users in
2025-2026 include Cash App, JetBrains, Physics Wallah (17M MAU), Wrike,
BiliBili, H&M. Google officially recommends KMP for sharing business
logic between Android and iOS.

## 21. The KMP / Mochi parallel

KMP is the most actionable model for "one Mochi codebase, multiple
targets" because it answers the same architectural question: how do you
share code while letting each platform supply its own runtime? KMP's
answer is:

- `commonMain` source set is the portable code (in Kotlin).
- `androidMain` provides Android-specific JVM bindings.
- `iosMain` provides iOS-specific Native bindings.
- `jvmMain`, `jsMain`, `wasmJsMain`, `nativeMain` for other targets.
- `expect`/`actual` declarations let `commonMain` reference a type or
  function and require each platform's source set to supply the
  implementation.

Mochi can plausibly fold its existing target-portability story into the
same pattern: a Mochi module declares its target compatibility, and
target-specific implementations live in adjacent files (`foo.mochi`,
`foo.jvm.mochi`, `foo.c.mochi`, `foo.beam.mochi`). See [[07-jvm-target-portability]].

## 22. ART JIT versus dex2oat in 2026

The ART team's modern direction (per Android 14, 15, 16) is:
- More work on profile collection (system_server collects, cloud
  distributes, the device augments).
- More aggressive runtime profile-guided recompilation.
- Better support for Java 17 / 21 language features through D8 desugar.
- Eventual ART support for `java.lang.invoke` enhancements and Loom-style
  virtual threads (work in progress as of Android 15; not yet ART-native
  in 2026).

D8 supports desugaring Java 8 lambdas, default methods, try-with-resources,
`java.util.stream`, `java.time`, `java.nio.file` (partially), and parts of
`java.util.concurrent`. The desugar process turns these into compatible
DEX code that runs on older Android. For Mochi targeting Android, the
strategy is: emit JVM bytecode at the JDK 8 source level for portability,
let D8 desugar to DEX, and let R8 optimise.

## 23. Lessons from compiler-plugin frameworks (kapt, KSP, Lombok, Quarkus Gizmo)

**kapt** (Kotlin Annotation Processing Tool) was the original
annotation-processor bridge for Kotlin, but slow because it generates
Java stubs and invokes `javac` annotation processors against them. It is
being phased out in favour of:

**KSP** (Kotlin Symbol Processing; v2.x in 2024-2025) is a Kotlin-native
processor API. KSP processors see the resolved Kotlin symbol graph, not
Java stubs, and run in the K2 compilation pipeline. KSP is roughly 2×
faster than kapt and supports incremental compilation properly. Room,
Moshi, Dagger Hilt, Glide, and most modern Android libraries have shipped
KSP processors.

**Lombok** (Reinier Zwitserloot, since 2009; v1.18.32 in 2024) hijacks
`javac` via internal APIs to inject AST nodes pre-compilation. The
`@Data`, `@Getter`/`@Setter`, `@Builder`, `@Slf4j` annotations are
ubiquitous in Java codebases. Lombok's hack relies on `--add-opens
jdk.compiler/com.sun.tools.javac.*=ALL-UNNAMED`, which has been
increasingly hostile in JDK 17+ and is openly opposed by the JDK team
(Brian Goetz's "Lombok is not a real annotation processor" position).
Manifold offers an alternative.

**Quarkus Gizmo** (Red Hat, since 2019; tracks Quarkus releases) is a
small bytecode-generation library on top of ASM, used by Quarkus extensions
to generate JVM classes *at build time* that replace the runtime
reflection/CDI that Spring uses. The BytecodeCreator interface exposes
common operations (locals, branches, calls, field access) without the
full ASM verbosity. Quarkus uses Gizmo to generate native-image-friendly
DI proxies, JAX-RS endpoints, ORM relations, all resolved at build time.

**Lesson for MEP-47:** Mochi's own emit layer should be a small,
opinionated Go library that targets exactly the JVM features Mochi needs
(records, sealed classes, lambdas via `invokedynamic`, `tableswitch`,
virtual-thread invocation). This is the Gizmo strategy, sized for the
Mochi feature surface. Don't write a general-purpose ASM clone; write
a Mochi-specific emitter that maps Mochi-IR to bytecode patterns.

## 24. Persistent collections for functional languages

Clojure's `PersistentVector` (32-way bit-partitioned trie), `PersistentHashMap`
(HAMT), and `PersistentTreeMap` (red-black) are the canonical JVM
implementations. **Vavr** (Java port of Scala collections by Daniel
Dietrich; v0.10 in 2024) is the Apache-licensed library most often used
by non-Clojure code. **PCollections** (Harold Cooper) is older, smaller,
and used in many academic/open-source projects.

Mochi's [[04-runtime]] story should ship a single persistent collection
suite, ideally a thin Mochi-runtime wrapper around Vavr or a vendored
copy of Clojure's collections. Do not roll your own HAMT; the
Bagwell-derived implementations have been battle-tested for 15+ years.

## 25. Java records, sealed classes, pattern matching (JDK 21 baseline)

JDK 16 finalised records; JDK 17 finalised sealed classes; JDK 21
finalised pattern matching for `switch` (JEP 441) and record patterns
(JEP 440). For Mochi, this is the *exact* lowering target for ADTs:

- Mochi `type Foo = Bar(int) | Baz(string)` → `sealed interface Foo
  permits Bar, Baz` + `record Bar(int x) implements Foo` + `record Baz(String s)
  implements Foo`.
- Mochi `match` → JDK 21 `switch` with record patterns.

The bytecode for `switch` on sealed types in JDK 21 emits `typeswitch`
via the `SwitchBootstraps.typeSwitch` invokedynamic bootstrap, not a
naive `instanceof` chain. The compiler computes the case order to
produce dense matching. **Mochi should lean on JDK 21's typeswitch
instead of emitting its own decision tree** for the JVM target.

## 26. Java 21 virtual threads (Project Loom)

JDK 21 (Sep 2023) shipped virtual threads as the default green-thread
primitive. `Thread.ofVirtual().start(runnable)` or
`Thread.startVirtualThread(runnable)`. They scale to millions per JVM;
underlying carrier is a `ForkJoinPool` of OS threads sized to the CPU
count. JDK 24 (March 2025) added JEP 491: virtual threads pin less,
removing the synchronized-method pinning surprise that bit early adopters.

For Mochi agents/streams ([[09-agent-streams]]), virtual threads are the
straightforward implementation: each agent is a virtual thread, message
queues are `java.util.concurrent.LinkedTransferQueue` or
`SynchronousQueue`, supervision uses structured concurrency
(`StructuredTaskScope`, finalised in JDK 25 per JEP 505).

This is the *single most important JDK 21+ feature* for Mochi-to-JVM:
it lets agents map 1:1 to threads without any of the BEAM-style runtime
scheduling that MEP-46 must reimplement.

## 27. Project Valhalla, type classes (forward-looking)

Project Valhalla's value-class story (JEP 401, value classes and objects
in preview as of early-access JDK 27 builds) is the long-running attempt
to flatten Mochi-style records into stack-allocated value-shape memory.
Critically, value classes give up identity (no `==` for reference equality,
no synchronization, no null-distinguishable instances) in exchange for
flat-memory layout. For Mochi's records and tuple types, this is the
ideal target *once it ships*; in 2026 it is still preview.

A late-2025 Valhalla prototype by Maurizio Cimadamore and Brian Goetz
explores **type classes** (Haskell-style ad-hoc polymorphism) for Java
generics, motivated by "how do you write one generic algorithm uniformly
over `int`, a value class, and a regular object type?" This is exploratory
work; not slated for any JDK release.

**Lesson for MEP-47:** target JDK 21 LTS in 2026 (universally supported);
target JDK 25 LTS as the secondary baseline (Leyden AOT cache, finalised
structured concurrency, scoped values); plan a Valhalla-aware refactor
for JDK 30+ when value classes ship in general availability.

## 28. Distilled lessons for a Mochi-to-JVM transpiler in 2026

1. **Lower through staged IRs, never directly to bytecode.** Mochi-AST →
   Mochi-IR (shared with the C and BEAM backends per [[MEP-45]] and
   [[MEP-46]]) → JVM-IR (Kotlin K2's shared Backend-IR is the model) →
   ASM emit. Each pass is debuggable in isolation; the JVM-IR layer is
   where invokedynamic decisions, virtual-thread scheduling, and Loom
   integration are made.

2. **Closures via invokedynamic + LambdaMetafactory by default**, mirroring
   Kotlin 2.0+ and modern Scala 3. Anonymous-class lambdas are a JDK 5
   compatibility relic; the JDK 21 baseline makes invokedynamic the
   right answer always. Mochi's existing fat-pointer closure representation
   (per [[MEP-45]] §15) translates well; the env becomes the
   `LambdaMetafactory`'s captured args.

3. **Async/streams via CPS state machines**, exactly the Kotlin coroutine
   pattern. Spill locals into a continuation object with `L$0`/`I$0`
   naming, drive resumption with a `TABLESWITCH` over an int label
   field. Combine with JDK 21 virtual threads for the scheduler. *Do not*
   implement stack-capturing continuations; Loom solves the use case.

4. **Mochi agents map 1:1 to virtual threads.** Each agent is a
   `Thread.ofVirtual()`; message queues are
   `java.util.concurrent.LinkedTransferQueue`; supervision uses
   `StructuredTaskScope` (finalised in JDK 25). This is the single
   biggest "free win" the JVM offers over the C backend.

5. **ADTs lower to sealed interfaces + records.** Mochi `type Foo =
   Bar(int) | Baz(string)` → `sealed interface Foo permits Bar, Baz` +
   `record Bar(int x)` + `record Baz(String s)`. Pattern matching emits
   JDK 21 `typeswitch` via `SwitchBootstraps.typeSwitch`. For tag-only
   enums, emit an actual `java.lang.Enum` subclass with explicit
   `tableswitch`. Do *not* trust the Scala/Dotty match compiler's
   default lowering; emit the `tableswitch` ourselves at the JVM-IR layer.

6. **Standard library: lean on `java.*` aggressively.** Ceylon's bespoke
   stdlib was a major contributor to its death. Mochi's runtime should
   be a thin veneer over `java.util.List`/`Map`/`Set`/`Optional`/`stream`,
   `java.time`, `java.nio.file`, `java.net.http.HttpClient`,
   `java.util.concurrent`. Add Mochi semantic helpers as extension-style
   static methods, not type wrappers.

7. **Persistent collections: vendor Vavr (Apache 2.0) or copy Clojure's
   HAMT.** Do not implement a new HAMT. Persistent collections are a
   solved problem; the implementations are 20 years mature.

8. **Bytecode emit: use ASM 9.9+ from Go.** Implement a small Go-side
   bytecode writer for the subset of class-file features Mochi uses
   (per [[05-codegen-design]]); skip the Java-side ASM dependency. The
   JDK 24 standard `java.lang.classfile` API is forward-looking but
   adds a JDK 24+ floor for tooling we do not need to enforce.

9. **AOT story: three modes, not one.**
   - `mochi build --target=jvm --aot=jlink-jpackage` for desktop apps and
     CLIs (custom JRE + native installer, no GraalVM dependency).
   - `mochi build --target=jvm --aot=graalvm-native-image` for serverless
     and smallest binary (~10ms startup, ~50MB; requires GraalVM JDK).
   - `mochi build --target=jvm --aot=leyden` for server workloads
     (warmup training run produces `.aot` cache; restore in 200ms; full
     JIT preserved).
   - CRaC is an opt-in `--restore=checkpoint.tar` flag for the second
     and third modes (see [[10-build-system]]).

10. **Android target: emit JDK 8 source-level bytecode**, target
    `minSdk = 21` minimum (modern practical floor, ART era, auto-multidex),
    let D8/R8 handle DEX conversion and tree-shaking. Test on the
    cloud-profile-trained ART AOT path. Do not roll a separate DEX
    backend; D8 is the boundary.

11. **Avoid the Ceylon/Eta failure modes.** Concretely: (a) dogfood
    Mochi-on-JVM in the Mochi compiler itself (the Mochi standard
    library should ideally compile via the JVM backend before any release
    ships); (b) lean on existing JVM libraries instead of reinventing
    them; (c) treat the BEAM ([[MEP-46]]) and JVM backends as siblings,
    not competitors, so neither becomes the under-tested fork; (d) ship
    Mochi-strict semantics, never lazy; (e) don't write a
    transpile-to-Java-source backend (Xtend's model), emit bytecode
    directly.

12. **Reserve a `.mochi-meta` block in jar artifacts** for the analogue
    of Scala 3's TASTy or Kotlin's `@Metadata`: type information,
    Datalog facts, agent topology, query DSL ASTs, LLM prompt templates.
    Mochi's classfiles are the lossy ground truth; the metadata block
    is the precise truth. Future IDE tooling and incremental compilation
    will need this; bake the layout in at v0.

13. **Multi-vendor JVM portability is free** if Mochi avoids HotSpot
    intrinsics. Generated bytecode runs identically on HotSpot, OpenJ9,
    Azul Zing, GraalVM. Test the gate on at least two vendors per
    [[11-testing-gates]].

14. **JDK 21 LTS floor in 2026; JDK 25 LTS as the secondary baseline.**
    JDK 21 has the production-supported features Mochi needs (virtual
    threads, sealed/records/pattern-matching, `MethodHandle`s) and is
    widely deployed by mid-2026. JDK 25 adds Leyden AOT caching, the
    final `StructuredTaskScope`, and the `java.lang.classfile` API.
    Targeting both means generating JDK 21-compatible bytecode while
    using JDK 25-specific runtime features behind feature flags.

## Open questions to flag

- **Should Mochi expose Java interop syntactically?** Kotlin's success
  is largely because `kotlin.collections.List` is just `java.util.List`
  with extensions, and Kotlin code can call any Java method without
  ceremony. Ceylon's failure is partly because its types were *not*
  Java types, requiring conversion at boundaries. Mochi should pick: do
  records lower to `record`s that are usable from Java? Are Mochi closures
  callable as `java.util.function.Function`? See [[06-type-lowering]].

- **How does Mochi's query DSL lower on the JVM?** BEAM gets Mnesia
  built-in (per [[MEP-46]]); the JVM has no equivalent. Candidates: H2
  embedded, DuckDB Java bindings, RocksDB+JNI, or compile queries to
  Stream-API operations against in-memory collections. See
  [[08-dataset-pipeline]] (likely a sibling note).

- **How does Mochi's LLM/Datalog feature interact with native-image's
  closed-world?** If LLM prims dispatch through dynamic strategies
  (chosen by config), native-image will not see them. We probably need
  build-time strategy registration, similar to Quarkus extensions.

- **Should Mochi-to-JVM share a fat jar layout with KMP?** If yes, we
  can plug Mochi modules into Kotlin Multiplatform's build pipeline as
  "another target" (`mochi { jvm(); js(); native(); android() }`).
  Tempting but probably not worth the coupling.

- **Tail-call elimination on JVM.** The JVM has no general TCO. Mochi's
  `func f() = f()`-shape recursion must either trampoline (Eta's
  approach, costs allocation per call) or compile self-recursive tail
  calls to `goto`s in the same method (Scala's `@tailrec`, only works
  for self-recursion). Mutual recursion needs trampolining.
  Recommendation: detect self-recursion at the IR level and emit `goto`;
  warn on mutual recursion; provide an explicit `@trampoline` opt-in.

## Sources

(URLs and references gathered during this research pass.)

Kotlin K2 / FIR / IR / coroutines / lambdas:
- Kotlin Blog, "What's new in Kotlin 2.2.20" (kotlinlang.org/docs/whatsnew2220.html)
- JetBrains, "K2 Compiler Performance Benchmarks", blog.jetbrains.com/kotlin/2024/04/...
- YouTrack KT-45375 ("Generate all Kotlin lambdas via invokedynamic + LambdaMetafactory by default")
- ej-technologies blog (Jan 2024), "How invokedynamic makes lambdas fast"
- Dove Letter, "Kotlin Coroutines: How suspend Compiles to a State Machine"
- droidcon (Nov 2025), "Inside Kotlin Coroutines: State Machines, Continuations, and Structured Concurrency"
- DoorDash blog, "The Beginner's Guide to Kotlin Coroutine Internals"
- Kotlin K2 migration guide (kotlinlang.org/docs/k2-compiler-migration-guide.html)
- Karakun, "Kotlin K2 Compiler" (Sep 2025)

Scala 3 / TASTy:
- docs.scala-lang.org/scala3/guides/tasty-overview.html
- scala.org/blog "State of the TASTy reader and Scala 2.13 ↔ Scala 3 compatibility" (Feb 2026)
- scalacenter/tasty-query GitHub
- VirtusLab blog, "How to mine Scala3 compiler metadata with TASTy files"
- Scala Contributors, "Scala 3 pattern matching on Enum is not tableSwitch"

Clojure, Groovy, JRuby, TruffleRuby, Jython, Kawa, ABCL:
- clojure.org/news/2025/08/25/clojure-1-12-2
- clojure.org/about/dynamic
- clojure-goes-fast.com, "Performance nemesis: reflection"
- groovy-lang.org/indy.html; docs.groovy-lang.org/latest/html/documentation/invokedynamic-support.html
- chrisseaton.com/truffleruby/announcement
- truffleruby/doc/user/compatibility.md
- jruby/jruby Wiki PerformanceTuning
- earthly.dev/blog/jruby/
- jython.org/jython-3-roadmap.html; aaveshdev/jython3 GitHub
- gnu.org/software/kawa; lwn.net/Articles/623349
- andrebask.github.io/thesis (First-Class Continuations on the JVM)
- notes.eatonphil.com/practical-common-lisp-on-the-jvm.html

Eta, Frege, Fantom, Ceylon:
- eta-lang.org; typelead/eta GitHub
- Frege/frege GitHub; tomassetti.me/exploring-frege
- en.wikipedia.org/wiki/Fantom_(programming_language); fantom.org
- en.wikipedia.org/wiki/Ceylon_(programming_language)
- infoworld.com "Red Hat's Ceylon language is an unneeded tempest in a teapot"
- ceylon-lang.dev (preservation project)

AOT toolchains:
- graalvm.org/latest/reference-manual/native-image/basics
- johal.in "GraalVM 24 Native Image: 70% Faster Java Startup vs JVM"
- stevenpg.com "Project Leyden vs GraalVM Native Image"
- openjdk.org/projects/leyden; openjdk/leyden GitHub (premain branch)
- javacodegeeks.com (March 2026), "Project Leyden's AOT Code Cache"
- openjdk.org/projects/crac; crac.org; bell-sw.com/blog/how-to-use-crac-with-java-applications
- documentation.ubuntu.com/ubuntu-for-developers/tutorials/crac-use
- docs.azul.com/crac
- baeldung.com/jlink; redhat docs "Using jlink to customize Java runtime environment"
- eclipse.dev/openj9/docs/aot; blog.openj9.org "Intro to Ahead Of Time Compilation"

JDK 21+/Valhalla/Loom:
- openjdk.org/projects/valhalla; openjdk.org/projects/valhalla/value-objects
- inside.java/2025/10/27/try-jep-401-value-classes
- JEP 484 (Class-File API), JEP 491 (virtual threads pinning fix), JEP 505 (StructuredTaskScope)

Android:
- source.android.com/docs/core/runtime/jit-compiler; source.android.com/docs/core/runtime/configure
- proandroiddev.com "Android CPU, Compilers, D8 & R8"
- developer.android.com/build/multidex
- infoq.com/news/2025/12/android-art-jit-aot-improvement
- developer.android.com/kotlin/multiplatform
- blog.jetbrains.com/kotlin/2025/05/compose-multiplatform-1-8-0
- blog.jetbrains.com/kotlin/2025/08/kmp-roadmap-aug-2025
- guarana-technologies.com/blog/kotlin-multiplatform-production

Libraries (ASM, Byte Buddy, Janino, Gizmo, Manifold):
- asm.ow2.io; asm.ow2.io/versions.html
- en.wikipedia.org/wiki/ObjectWeb_ASM
- bytebuddy.net; raphw/byte-buddy GitHub
- janino-compiler.github.io/janino; janino.net
- quarkusio/gizmo GitHub; the-main-thread.com "Build-Time Brilliance"
- manifold.systems; github.com/manifold-systems/manifold
