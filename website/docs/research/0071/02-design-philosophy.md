---
title: "02. Design philosophy"
sidebar_position: 3
sidebar_label: "02. Design philosophy"
description: "Why a bidirectional bridge, why PEP 561 stubs over alternatives, why a synthesised CPython wrapper module over direct ctypes / cffi, why the async bridge sits on `asyncio.run` per call with persistent opt-in, why Sigstore-keyless OIDC + PyPI Trusted Publishing is the only publish path, why uv is the resolver."
---

# 02. Design philosophy

This note explains the load-bearing decisions in MEP-71. Each section frames one choice, the alternatives weighed, the reason the chosen path wins, and the consequence the user inherits. The decisions here are referenced by every other note in the bundle.

## 1. Why a bidirectional bridge

The simpler design would be one-way: either Mochi imports Python (consume), or Mochi publishes to PyPI (publish), but not both. Each direction in isolation is a smaller engineering surface and a smaller blast radius.

The reasons we ship both:

1. **Symmetric MEP-73 / MEP-74 precedent.** The Rust and Go bridges are bidirectional, and the polyglot package surface is the headline feature of the MEP-57 wave. Asymmetry would be a usability cliff: "I can `import rust` and `mochi pkg publish --to=crates.io`, but only `import python`, not publish to PyPI" leaves a hole that users would notice immediately.
2. **The publish direction is where Mochi differentiates.** Mochi's type system, package model, and reproducibility story are stronger than Python's; publishing Mochi-authored libraries to PyPI gives Python users access to the Mochi ecosystem with no boilerplate on their side. This is the long-term ecosystem play.
3. **Consume direction is the dev-experience anchor.** Python's ecosystem (numpy, pandas, requests, fastapi, pytorch) is the deepest in computing. Without the consume direction, Mochi is a curiosity. The consume direction is what makes Mochi usable for real work on day one.
4. **Shared infrastructure across directions.** Stub parsing, wheel handling, the sparse-index client, the wrapper synthesiser, the capability database, and the Sigstore client are all 1:1 reused across both directions. The marginal cost of adding publish on top of consume (or vice versa) is one of the engineering teams in the project.

The cost is real: two sets of CI, two sets of fixtures, two sets of error codes. The 18-phase delivery plan accounts for this by interleaving the directions (phases 0-8 consume, 9-10 publish, 11-13 cross-cutting, 14-18 hardening).

## 2. Why PEP 561 stubs are the canonical type source

The Python ecosystem has three layers of type information:

1. **Inline annotations** in the source itself (PEP 526 / 484), gated by a `py.typed` marker per PEP 561.
2. **Sibling stub distributions** (`<name>-stubs`), the Stuart pattern, also PEP 561.
3. **Typeshed**, the centralised monorepo of ~200 third-party stubs maintained by the Python typing community.
4. **Stubgen fallback** via `mypy.stubgen --inspect-mode`, which imports the package live and inspects function signatures, classes, and methods via reflection.

MEP-71 codifies a four-tier precedence: inline (if `py.typed`) → sibling `-stubs` (if present) → typeshed (if covered) → stubgen fallback (if `[python].stubgen.fallback = true`). This is the same precedence pyright and mypy already use, so the bridge's type discovery is observably consistent with how Python's own type tooling resolves types.

Alternatives considered and rejected:

- **Mochi-specific stub format.** Would require every Python package to ship a Mochi-flavoured stub, defeating the consume-without-boilerplate goal.
- **Runtime inspection only.** Inspecting via `inspect.signature` at import time works for many packages but loses type-only constructs (TypeVar, Protocol, Generic[T]) and degrades on C-extensions where signatures live in docstrings.
- **typeshed only.** Skips packages typeshed doesn't cover. A large fraction of PyPI's long tail is uncovered, and waiting for typeshed PRs to merge for every package is not a viable consumption path.

The PEP 561 four-tier ladder gives us widest coverage with the strongest types: when the package author opted in (py.typed or -stubs), we get their own types; when they didn't, we get typeshed's; when typeshed didn't either, we get stubgen's best-effort approximation; when that fails too, we report a `SkipReason` and continue.

See [[04-pep561-stub-ingest]] for the full ingest pipeline and the partial-stub story.

## 3. Why a synthesised CPython wrapper module, not direct ctypes

There are three ways to call from Mochi (Go) into Python from first principles:

1. **Pure ctypes.** Mochi emits Go code that opens libpython.so, finds `Py_Initialize`, and calls every function through CFFI. The user writes no Python at all.
2. **Direct CPython C API embedding.** Mochi links libpython directly (no ctypes layer), calls `PyImport_ImportModule`, `PyObject_CallFunction`, etc. directly from Go. This is what gopy does.
3. **Synthesised Python wrapper module.** Mochi emits a small `_mochi_wrap.py` module per imported package that imports the package, exposes a typed surface, and hands handles back through a thin C boundary. The CPython side does the GIL juggling; the Mochi side gets a typed extern fn.

We chose path 3. Reasons:

- **GIL handling lives in CPython.** Acquiring and releasing the GIL from Go through CFFI is correct but verbose, and CPython 3.13t free-threaded mode (PEP 703) changes the rules in ways that pure-ctypes code would silently break under. Letting CPython manage its own GIL inside the wrapper module isolates the GIL concern from the rest of the bridge.
- **Reuse of MEP-51 Phase 12 sidecar pattern.** Mochi's Python transpiler already uses `<modname>_externs.py` sidecars to host hand-written extern functions. The synthesised wrapper module is the same shape, just generated rather than hand-written. The MEP-51 infrastructure (sidecar discovery, import-time stitching, error coercion) loads MEP-71 wrappers for free.
- **Mochi's runtime objects map cleanly to Python objects.** A Mochi `list[int]` is a CPython `list`; a Mochi `dict[str, int]` is a CPython `dict`; a Mochi `int` is a CPython `int`. Roundtripping through the wrapper module means no marshalling layer is needed for any datatype that already has a CPython counterpart.
- **Async bridges naturally.** The wrapper module is the right place to call `asyncio.run` or to register a Python coroutine on an event loop; if we tried to do this from Go directly, we would need to reproduce asyncio's internal coroutine protocol.

Path 1 (pure ctypes) lost on the GIL story plus the C-extension boundary (calling a numpy ufunc through ctypes is essentially impossible). Path 2 (gopy-style direct embed) lost on the maintenance burden: gopy has not seen a release in over two years, and tracking CPython API changes from outside CPython is a known time sink.

See [[09-abi-stability]] and [[10-gil-and-cextensions]] for the deeper boundary issues this choice addresses.

## 4. Why asyncio.run per call, with persistent loop opt-in

The async surface is the hardest cross-language design in the bridge. The choices were:

- **Per-call `asyncio.run`.** Every Mochi-to-Python async call wraps the coroutine in `asyncio.run`. Simple, no shared state, but pays the event-loop spin-up cost on every call (~0.5-1ms).
- **Persistent loop on a dedicated thread.** One asyncio event loop runs forever in a Python thread; Mochi marshals coroutines onto it via `asyncio.run_coroutine_threadsafe`. Faster per call, but the loop becomes shared state with all the lifetime hazards that entails.
- **uvloop / trio.** Faster runtimes with different semantics. trio in particular has a different cancellation model than asyncio.

We chose per-call as the default, with `[python].async-mode = "persistent"` as the opt-in. Reasons:

- **Per-call is correct by construction.** Each call is isolated; no cross-call event-loop state can leak. Cancellation, timeouts, and exceptions all behave as if the user wrote `asyncio.run(...)` themselves.
- **Persistent is faster but state-leaky.** If a user holds a reference to a Future from one call and tries to await it from another, behaviour depends on which event loop the Future was created on; with persistent mode there is one loop and it works, with per-call mode each loop is gone after `run` returns. The persistent opt-in makes this state-sharing explicit.
- **uvloop / trio are libraries, not modes.** Users who want them install them as Python deps and call `uvloop.install()` or `trio.run` in their own code. The bridge does not bless one runtime over another.

The async surface is documented in detail in [[08-async-bridge]].

## 5. Why Sigstore-keyless OIDC + PyPI Trusted Publishing is the only publish path

PyPI's traditional publish flow used API tokens generated through the web UI and stored in CI secrets. The current best-practice flow is PyPI Trusted Publishing (GA 2023-Q2 for GitHub Actions, expanded to GitLab CI, Google Cloud, and ActiveState in 2024), which uses OIDC tokens exchanged for short-lived upload credentials.

MEP-71 supports only the Trusted Publishing path. Reasons:

- **No long-lived secrets.** The Trusted Publishing flow has no static credentials anywhere; the OIDC token is fetched per-upload and the upload credential expires within minutes. Compromise of a CI secret store does not give the attacker upload rights.
- **PEP 740 attestations.** PyPI launched cryptographic attestations in 2024-Q4, signed via Sigstore. Trusted Publishing is the only flow that wires the attestation generation in; legacy token uploads cannot attest.
- **Supply-chain pressure.** The 2024 Top.gg colorama campaign and the 2024-2025 typosquat waves demonstrated that unsigned PyPI uploads are an active threat. Mochi-published packages will have provenance from day one.

Alternative considered: support both legacy token and Trusted Publishing. Rejected because it adds a code path that maps directly onto a known-broken security posture. Users who cannot use Trusted Publishing (private internal PyPI mirrors that don't speak OIDC) can use `mochi pkg publish --to=private-index` once that target lands; the public PyPI publish path is OIDC-only.

See [[07-sigstore-pypi-trusted-publishing]] for the protocol details.

## 6. Why uv as the resolver

Python has four mature resolvers: pip's, Poetry's, PDM's, and uv's. uv (Astral, written in Rust, PubGrub forking resolver) is the youngest but the fastest by 10-100x on cold resolves and the only one that produces a universal lockfile (uv.lock v1, with a `revision` field for cache invalidation, plus PEP 751 pylock.toml export).

Reasons we chose uv:

- **Speed.** Mochi's `mochi pkg lock` runs in CI on every change. A 10s lock cost (uv) is acceptable; a 5min lock cost (pip-tools) is not.
- **Universal lockfile.** uv.lock captures all platforms in one file; Poetry and PDM produce per-platform lockfiles that must be regenerated for every target. Mochi's cross-platform target matrix (Linux x64/arm64, macOS x64/arm64, Windows x64, WASI, Pyodide) needs a universal lockfile.
- **PEP 751 conformance.** uv exports pylock.toml, which lets Mochi-built packages interoperate with non-Mochi Python tooling. Other resolvers either don't export PEP 751 or have only partial support.
- **Rust toolchain alignment.** MEP-73's Rust bridge already requires Rust toolchain support. uv's Rust runtime is acquired through the same install path; no separate Python-side tool acquisition.

The alternative was pip's resolver: stable, ubiquitous, but slow and without a universal lockfile. Poetry: opinionated, slower than uv, partial PEP 751 support. PDM: PEP 582-focused, slower than uv. uv wins on every axis except "youngest, so smallest install base," which is offset by Astral's commitment to long-term maintenance.

See [[12-risks-and-alternatives]] §R10 for the uv-stability risk and the backup plan.

## 7. Why a closed type-mapping table

Python is dynamically typed; Mochi is statically typed. The bridge has to draw a line: which Python types does Mochi understand, and which become opaque handles?

The closed table covers, in both directions:

- **Scalars**: `bool`, `int` (with arbitrary-precision boundary at sys.maxsize), `float`, `str`, `bytes`, `None`.
- **Collections**: `list[T]`, `tuple[T1, T2, ...]`, `dict[K, V]`, `set[T]`, `frozenset[T]`.
- **Algebraic**: `Optional[T]`, `Union[T1, T2]` (when reducible to a Mochi sum type), `Literal["a", "b"]`.
- **User-defined**: `@dataclass`, `TypedDict`, `NamedTuple`, `Enum`, `Protocol` (structural).
- **Generic**: `Callable[[A, B], R]`, `Iterator[T]`, `Iterable[T]`, `AsyncIterator[T]`, `Generator[Y, S, R]`.
- **Special**: `Any` (becomes Mochi `any` boxed opaque), `NoReturn`, `Self`, `TypeVar` (resolved by monomorphisation rule).

Outside the table, the type becomes an opaque `PyObject` handle. The user can pass it around, store it, and pass it back to Python, but cannot project fields or call methods on it from Mochi.

The reason this is closed: the type table is the contract surface between two type systems with fundamentally different semantics. If we tried to support every Python typing construct (especially the dynamic ones like `__class_getitem__`, runtime-checkable Protocols, `__init_subclass__`), the type table would balloon into an interpreter for the typing module. Closing the table at a stable subset makes the bridge predictable.

See [[05-type-mapping]] for the full table and the refusal cases.

## Cross-references

- [[03-prior-art-bridges]] for what we learned from other Python bridges.
- [[04-pep561-stub-ingest]] for the stub discovery pipeline.
- [[05-type-mapping]] for the closed type table.
- [[08-async-bridge]] for the async runtime choice.
- [[10-gil-and-cextensions]] for the GIL and C-extension boundary.
- [MEP-71 §1 Abstract](/docs/mep/mep-0071#abstract) for the same decisions in normative form.
