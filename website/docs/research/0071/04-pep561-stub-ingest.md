---
title: "04. PEP 561 stub ingest"
sidebar_position: 5
sidebar_label: "04. PEP 561 stub ingest"
description: "The PEP 561 four-tier precedence (`py.typed` inline → `<name>-stubs` sibling → typeshed → stubgen fallback), the stubgen inspect mode, the partial-stub story, the typeshed monorepo, the Python-side parser shape, the `.pyi` AST."
---

# 04. PEP 561 stub ingest

This note covers how MEP-71 discovers the type information that drives the wrapper synthesiser. PEP 561 (published 2017, normative for all of Python's typing ecosystem) defines four channels through which type information reaches a downstream consumer; MEP-71 walks all four in a strict precedence order.

## The PEP 561 four-tier precedence

For each Python package the user imports, the bridge looks for type information in this order, stopping at the first hit:

1. **Inline annotations, gated by `py.typed`.** If the installed package contains a marker file `<package>/py.typed`, the package author has opted in to distributing inline annotations as the canonical types. The bridge parses the `.py` source files directly to extract annotations.
2. **Sibling stub distribution `<name>-stubs`.** If the user has installed a sibling package whose distribution name is `<name>-stubs` (PEP 561 §5), the stubs in that package shadow any inline annotations. Example: `types-requests` provides stubs for `requests`. Sibling stubs win over inline because they often correct or supplement inline annotations that lag behind reality.
3. **Typeshed third-party stubs.** Typeshed is a community monorepo at https://github.com/python/typeshed maintaining stubs for ~200 untyped or partially-typed packages. The stubs are versioned and published via the `types-<name>` distributions. The bridge bundles the typeshed snapshot at lock time so the version is reproducible.
4. **Stubgen fallback.** When none of the above produces a stub, the bridge falls back to mypy's `stubgen --inspect-mode`. Stubgen imports the package live in a sandboxed subprocess and uses `inspect` to read function signatures, class definitions, and method signatures. The output is a best-effort `.pyi` with `Any` where types could not be inferred.

The precedence is intentionally strict: a hit at level 1 (inline `py.typed`) takes the inline annotations exclusively, never falling through to typeshed. This matches pyright's and mypy's resolution order, so type information observed by the bridge is identical to what those tools would see.

Disabling tiers: `[python].stubgen.fallback = false` skips tier 4; missing types become a `SkipReason::NoStubs` and the package is treated as untyped (every item is `any` in Mochi). `[python].typeshed = false` skips tier 3; useful for users who want to enforce that every dep ships its own types.

## Why the precedence is the way it is

The reasoning behind each step:

- **Inline wins over typeshed.** When a package author has added `py.typed`, they have explicitly opted in. Their inline annotations are the ground truth; typeshed's older or third-party-maintained stubs should not override them.
- **`<name>-stubs` wins over inline.** When inline is wrong (rare but happens, e.g., functions with `*args, **kwargs` that have known concrete signatures), the community publishes a stubs package to correct it. The stubs distribution is the authoritative source.
- **Typeshed third before stubgen.** Typeshed stubs are hand-written by typing experts. They handle Protocol, TypeVar, ParamSpec, TypeVarTuple, runtime-checkable, and many other typing constructs that stubgen approximates badly.
- **Stubgen last.** Stubgen is the best-effort fallback. It runs the package; if the package has import-time side effects (network calls, GPU initialisation), stubgen surfaces those in the sandbox log. The result is `Any`-heavy but better than nothing.

## The PEP 561 partial-stub story

A subtle PEP 561 detail: stubs distributions can mark themselves as partial (PEP 561 §6) by including a `partial\n` line in the `py.typed` marker. A partial stubs distribution tells the type checker "I have stubs for the symbols I list; for symbols I don't list, fall through to the next tier."

The bridge respects this: when `<name>-stubs` is marked partial, the bridge unions its symbols with the next tier's (typeshed or stubgen). When unmarked, the stubs distribution is taken as complete and other tiers are skipped for that package.

Partial stubs are common for packages where the typed surface is incomplete (only the most-used classes typed) but the untyped surface still works at runtime.

## The stubgen pipeline

Stubgen runs as a subprocess in the lock-time pipeline:

```
$ python -m mypy.stubgen --inspect-mode --output-dir <cache> <package>
```

The `--inspect-mode` flag (added mypy 1.0) tells stubgen to import the package and reflect on it via `inspect.signature`, not just parse the AST. Reflection picks up signatures from C extensions (which have no AST) and from dynamically-defined classes (e.g., `dataclass` and `attrs` generated `__init__`).

Sandboxing: stubgen runs in a venv built specifically for the package. The venv has only the package and its deps installed; nothing of the host's Python environment leaks. Network is denied by default (the bridge spawns stubgen with a no-network seccomp filter on Linux, NetworkExtension on macOS); packages that try to fetch on import surface as a `SkipReason::ImportTimeNetwork`.

Output: stubgen writes `.pyi` files mirroring the package's module structure. The bridge content-addresses these and stores them under `<cache>/python/stubs/<package>/<version>/<stub-hash>/`. The hash goes into mochi.lock as `stub-sha256` for reproducibility.

## The typeshed snapshot

Typeshed is updated continuously. To keep lockfiles reproducible, the bridge pins typeshed to a specific git commit at lock time (recorded in mochi.lock as `typeshed-revision`). On `mochi pkg lock --check`, the same typeshed revision is fetched and the stubs hashes are recomputed.

A `mochi pkg lock --update-typeshed` flag advances to the latest typeshed main, regenerates stubs for any affected packages, and writes new hashes into the lockfile. This is the only path that changes typeshed-derived types across a lock-check cycle.

## The `.pyi` AST

Once a stub source is selected (regardless of tier), the bridge parses it with a stub-specific Python AST walker. The walker understands:

- Function definitions: `def f(x: int, y: str = "...", *args: int, **kwargs: str) -> bool: ...`
- Overloaded functions: `@overload def f(x: int) -> int: ...` followed by concrete implementations.
- Classes: `class C(Generic[T]): ...` with method definitions inside.
- Type aliases: `Vector = list[float]` and PEP 695 `type Vector = list[float]`.
- Protocols: `class Hashable(Protocol): def __hash__(self) -> int: ...`
- TypedDict: `class Config(TypedDict): host: str; port: int`
- NamedTuple: `class Point(NamedTuple): x: int; y: int`
- Enum / IntEnum / StrEnum: PEP 663 / 663-derived enum bodies.
- TypeVar / ParamSpec / TypeVarTuple: PEP 484 / 612 / 646.
- Generic aliases: `list[int]`, `dict[str, int]`, `tuple[int, ...]`.

What the walker explicitly does not handle (refusals):

- `Callable` with `ParamSpec` that has captures from the enclosing scope.
- Conditional types based on `if sys.version_info >= (3, 12)`. The walker takes the current `requires-python` constraint and evaluates the condition statically; ambiguous cases refuse.
- `cast`, `assert_type`, `reveal_type`: runtime typing constructs that don't appear in stubs but appear in inline `py.typed` sources. Ignored.
- Forward references via `if TYPE_CHECKING:`: resolved by importing the type-checking-only branch and re-running the walker.

Refusals emit a `SkipReason::UnsupportedTypingConstruct` and the affected item becomes `any` in the Mochi shim.

## The Python-side parser shape

The bridge is written in Go. The PEP 561 ingest pipeline lives at `package3/python/stubs/`:

- `package3/python/stubs/discovery.go`: walks the four tiers for one package, returns a resolved stub-source path.
- `package3/python/stubs/typeshed.go`: manages the typeshed git checkout, pins the revision, computes per-package stub hashes.
- `package3/python/stubs/stubgen.go`: orchestrates the stubgen subprocess, enforces the sandbox, captures stderr for diagnostics.
- `package3/python/stubs/parser.go`: Go-native `.pyi` parser. We do not call out to Python's ast module because the lockfile pipeline must be reproducible byte-for-byte across Python versions.
- `package3/python/stubs/apisurface.go`: walks the parsed `.pyi` AST and emits a Mochi-shaped ApiSurface struct (function list, class list, type-alias list).

The ApiSurface struct is the input to the wrapper synthesiser ([[05-type-mapping]]) and to the publish-side stub emitter (for publishing Mochi packages as PEP 561 typed packages on PyPI).

## Cross-references

- [[05-type-mapping]] for how each `.pyi` type becomes a Mochi type.
- [[02-design-philosophy]] §2 for why PEP 561 is the canonical type source.
- [[03-prior-art-bridges]] for how other bridges discover types.
- [PEP 561](https://peps.python.org/pep-0561/) for the normative reference.
- [Typeshed](https://github.com/python/typeshed) for the third-party stubs monorepo.
- [mypy stubgen](https://mypy.readthedocs.io/en/stable/stubgen.html) for the fallback tool.
