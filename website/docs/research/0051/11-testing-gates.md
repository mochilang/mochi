---
title: "MEP-51 testing gates: per-phase fixtures, vm3 byte-equal, mypy + pyright strict, reproducibility"
description: "The test gate plan for MEP-51 (Mochi-to-Python). Master gate is vm3 byte-equal stdout; secondary gates are mypy --strict and pyright --strict; tertiary is ruff fixed-point; quaternary is wheel build + install smoke test. Per-phase gate definitions for all 18 phases."
---

# Testing gates: per-phase fixtures, vm3 byte-equal, type-check strictness, reproducibility

This note defines the test gate plan that MEP-51 must clear at each
of its 18 phases. The structure mirrors MEP-50 (Kotlin) and MEP-49
(Swift), with deltas for the Python-specific gates (mypy + pyright)
and the build-artifact reproducibility gate that has no equivalent in
the JVM or Swift sibling MEPs.

See `the shared-decisions anchor` for the load-bearing decisions and
`[[10-build-system]]` for the build pipeline this note tests.

## The gate hierarchy

MEP-51 has four ordered gate tiers. A fixture passes Phase N only if
it clears every tier in order.

### Tier 1 (master): vm3 byte-equal stdout

The Mochi reference interpreter `vm3` runs the source `.mochi`
fixture and captures stdout. The transpiler emits Python source from
the same fixture. CPython 3.12 runs the emitted Python and captures
stdout. The two stdouts must be byte-identical.

This is the only gate that compares observable behaviour. The other
three tiers gate the artifact, not the behaviour.

byte-identical means:

- Same UTF-8 bytes.
- Same `\n` line endings (POSIX `\n`, never `\r\n`, even on Windows).
- Same trailing newline presence/absence.
- Same numeric formatting (e.g. floats as `1.5` not `1.50` or `1.5e0`).

The `expect.txt` golden file is the byte-equal target. It is
generated once from `vm3` and committed; subsequent CI runs compare
the emitted Python's stdout to `expect.txt`.

Normalised line endings: the CI runner sets `git config core.autocrlf
false` and `git config core.eol lf` to prevent Windows from rewriting
line endings on checkout. We document this in the contributor guide.

Test runner: `tests/transpiler3/python/runner.go` (Go test driver,
mirrors `tests/transpiler3/c/runner.go`).

### Tier 2 (secondary): mypy --strict + pyright --strict

Both type checkers run on the emitted Python with strict mode
enabled. Both must report zero errors.

mypy invocation:

```
uv run mypy --strict --python-version 3.12 --no-incremental src/
```

Flags:
- `--strict` enables all of `--check-untyped-defs --disallow-any-generics --disallow-incomplete-defs --disallow-subclassing-any --disallow-untyped-calls --disallow-untyped-decorators --disallow-untyped-defs --no-implicit-optional --no-implicit-reexport --strict-equality --warn-redundant-casts --warn-return-any --warn-unused-configs --warn-unused-ignores`.
- `--python-version 3.12` pins to the floor.
- `--no-incremental` disables the `.mypy_cache` because we run on
  fresh CI containers and incremental cache state varies.

pyright invocation:

```
uv run pyright --pythonversion 3.12
```

`pyright` uses `[tool.pyright]` in `pyproject.toml` for config; the
`typeCheckingMode = "strict"` field in `pyproject.toml` already
enforces strict mode (see `[[10-build-system]]`).

Both checkers must report zero errors. Warnings are allowed but
tracked; we audit warnings monthly to escalate persistent ones to
errors (see `[[12-risks-and-alternatives]]` R2 for the divergence
story).

#### Why both checkers

mypy and pyright disagree on roughly 15% of strict-mode edge cases.
Examples from the pyright issue tracker (cross-referenced with mypy
issues):

- PEP 695 type alias variance defaults: mypy infers covariant by
  default; pyright infers invariant by default for class-bound
  generics. Mochi's emit must produce code that satisfies both.
- `TypedDict` totality: mypy is stricter on `total=False` access;
  pyright requires explicit `get` for optional keys.
- `Protocol` runtime checkability: mypy ignores at type-check time;
  pyright raises if `@runtime_checkable` is missing on a Protocol used
  in `isinstance`.
- Generic class inheritance: pyright allows narrower type arguments;
  mypy enforces invariance.

Passing both gates narrows the emit to the intersection of correct
typed Python. This costs CI time (about 1.5x the type-check budget)
but catches real bugs.

### Tier 3 (tertiary): ruff check + ruff format fixed-point

Lint and format. Two invocations:

```
uv run ruff check src/ tests/
uv run ruff format --check src/ tests/
```

`ruff check` runs the linter. Zero errors required. The selected
rules are in `[tool.ruff.lint]` in `pyproject.toml` (see
`[[10-build-system]]`).

`ruff format --check` runs the formatter in check-only mode. If any
file would be reformatted, the exit code is non-zero. The gate fails.

This is a fixed-point check: emit -> format -> emit again must
produce identical output. We test by running `ruff format` once and
comparing the diff:

```
$ uv run ruff format src/
$ git diff --quiet src/  # must succeed
```

If `git diff` shows changes, the emit is not formatter-stable. We fix
the emitter to produce formatter-stable output.

ruff format is black-compatible; we discuss the relation in
`[[10-build-system]]`. We do not run black separately.

### Tier 4 (quaternary): wheel build + install + smoke test

```
uv build
uv pip install --system dist/*.whl
python -c "import <pkg>; <smoke test>"
```

The wheel must build without errors. The wheel must install into a
fresh venv without errors. The smoke test imports the top-level
package and runs a `--version` or equivalent.

Build errors usually mean `pyproject.toml` mis-emission (e.g. wrong
package layout, missing `py.typed`). Install errors usually mean a
dependency declaration mismatch. Smoke-test errors mean the emit
breaks at module import time (most often: typo in a generated
`__init__.py`).

This gate runs after Tier 1-3 pass. It's deliberately last because
it's the slowest (cold install on Windows can take 30 seconds).

## Additional gates (per phase)

Some phases add specific gates beyond the four tiers.

### PyPI Trusted Publishing dry-run (Phase 18)

```
uv publish --dry-run --trusted-publishing always
```

`--dry-run` verifies the OIDC exchange + upload signature without
actually uploading. We run this in PR CI to catch credential
misconfiguration before release.

### Reproducibility (Phase 15+)

Two builds, two hosts, byte-identical wheel SHA256:

```
HOST_A: SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) uv build --wheel
HOST_B: SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) uv build --wheel
diff <(shasum -a 256 host_a/dist/*.whl) <(shasum -a 256 host_b/dist/*.whl)
```

Hosts cover linux x86_64 + linux aarch64 + macos arm64 by Phase 16.
Windows is excluded from reproducibility through Phase 16 because
of filesystem case-insensitivity deltas; Phase 16.1 adds Windows.

### ipykernel notebook execution (Phase 17)

Spawn JupyterLab, open a `.ipynb` fixture, execute all cells, capture
each cell's outputs, diff against `expect.ipynb`.

```
uv run jupyter nbconvert --to notebook --execute fixtures/<name>.ipynb \
    --output /tmp/actual.ipynb
diff <(jq '.cells[].outputs' fixtures/<name>.expect.ipynb) \
     <(jq '.cells[].outputs' /tmp/actual.ipynb)
```

The diff must be empty. `jq` strips notebook-format metadata (cell
ids, execution counts) that vary between runs.

## Per-phase gate definitions

Each phase has a Go test wrapper in `tests/transpiler3/python/` that
sets up fixtures, runs the transpiler, and walks the gate tiers. The
test wrapper for Phase N is `phase<N>_test.go` with a top-level test
`TestPhase<N>...Python`.

The fixture directory layout:

```
tests/transpiler3/python/
├── runner.go
├── phase1_helloworld_test.go
├── phase2_scalars_test.go
├── phase3_1_lists_test.go
├── phase3_2_maps_test.go
├── phase3_3_sets_test.go
├── phase3_4_list_of_records_test.go
├── phase4_records_test.go
├── ...
├── phase18_pypi_test.go
└── fixtures/
    ├── phase1/
    │   └── helloworld/
    │       ├── source.mochi
    │       ├── expect.txt
    │       └── meta.toml
    ├── phase2/
    │   └── ...
    └── ...
```

Each fixture has:
- `source.mochi`: the Mochi source.
- `expect.txt`: the byte-equal stdout target.
- `meta.toml`: optional metadata (e.g. `skip_pyright = true` if a
  fixture probes a known pyright divergence).

### Phase 1: hello world

**Goal**: `print("hello world")` round-trips through emit and runs.

**Fixtures**: 1.
- `helloworld`: prints `hello world\n`.

**Gate**: Tiers 1-4. mypy strict on an `__init__.py` + `__main__.py` +
`generated/foo.py` triplet. Wheel builds and installs.

**Output**:
```python
# generated/foo.py
def main() -> None:
    print("hello world")
```

**Test**: `TestPhase1HelloWorldPython`.

### Phase 2: scalars

**Goal**: `int`, `float`, `bool`, `str`, `bytes` lower correctly, arithmetic
and comparisons work, formatting matches Mochi's.

**Fixtures**: 12.
- `int_arith`: `1 + 2 * 3`
- `int_overflow`: arbitrary precision big ints
- `float_arith`: IEEE 754 addition
- `float_special`: NaN, +inf, -inf
- `bool_logic`: `and`, `or`, `not`, short-circuit
- `string_concat`: `"a" + "b"`
- `string_len`: code-point length
- `string_unicode`: emoji + combining marks
- `string_format`: `f"{x:.2f}"`
- `bytes_literal`: `b"\x00\x01"`
- `bytes_concat`: byte concatenation
- `print_mixed`: print of mixed types via `__str__`

**Gate**: Tiers 1-4. Special attention to float formatting: Python's
default `repr(1.5)` is `"1.5"`, matching Mochi. `repr(0.1 + 0.2)` is
`"0.30000000000000004"`; Mochi prints `0.3`. The emitter must use
explicit format strings to align with Mochi's output. See the
`float_special` fixture for the NaN edge case (Mochi prints `NaN`,
Python prints `nan`; we override).

### Phase 3.1: lists

**Goal**: `list[T]` lowering, including literals, indexing, slicing,
`len`, `append`, comprehensions.

**Fixtures**: 18.
- `list_literal_int`, `list_literal_str`, `list_literal_mixed_into_union`
- `list_index_positive`, `list_index_negative`, `list_index_out_of_bounds`
- `list_slice_basic`, `list_slice_step`, `list_slice_negative_step`
- `list_len`, `list_append`, `list_extend`, `list_pop`
- `list_iter_for`, `list_iter_index`
- `list_comprehension_simple`, `list_comprehension_filter`,
  `list_comprehension_nested`

**Gate**: Tiers 1-4. mypy strict requires `list[T]` annotations on all
list-typed parameters and returns. pyright strict requires the same.
Out-of-bounds indexing: Mochi raises a structured error; Python raises
`IndexError`. The emitter wraps Python's `IndexError` in a
MochiResult-aware translator (see Phase 11).

### Phase 3.2: maps (dict)

**Goal**: `map<K, V>` lowering to `dict[K, V]`, including literal,
indexing, `len`, `keys`, `values`, `has`, for-each iteration.

**Fixtures**: 22.
- `dict_literal_str_int`, `dict_literal_int_str`,
  `dict_literal_str_str`, `dict_literal_str_list`,
  `dict_literal_str_record`
- `dict_index_get`, `dict_index_missing_raises`,
  `dict_index_missing_default`
- `dict_set`, `dict_delete`, `dict_update`
- `dict_len`, `dict_keys`, `dict_values`, `dict_items`
- `dict_has_key`
- `dict_iter_for`, `dict_iter_keys`, `dict_iter_values`,
  `dict_iter_items`
- `dict_comprehension`, `dict_merge`

**Gate**: Tiers 1-4. Mochi's `m["k"]` on missing key raises a typed
error; Python's `d["k"]` raises `KeyError`. Emit uses `d["k"]`
unchanged (the error translation happens at the Mochi-aware boundary
when crossing into a `Result`-returning context). For
`dict.get("k", default)` Mochi has `m.get("k", default)` lowering
directly.

### Phase 3.3: sets

**Goal**: `set<T>` lowering to `set[T]`. Insertion order is NOT
guaranteed by Python's `set` (unlike `dict`). For order-sensitive
sets, emit uses `dict.fromkeys(...).keys()` as an ordered-set
substitute.

**Fixtures**: 14.
- `set_literal`, `set_add`, `set_remove`, `set_contains`,
  `set_iter`, `set_len`, `set_union`, `set_intersection`,
  `set_difference`, `set_symmetric_difference`,
  `set_comprehension`, `set_from_list`, `set_ordered_iter` (uses
  `dict.fromkeys`), `set_frozen` (uses `frozenset`).

**Gate**: Tiers 1-4. The `set_ordered_iter` fixture is tricky: Mochi
guarantees insertion-ordered iteration on sets; Python `set` does
not. The emitter detects when ordering is observed (e.g. via `for`
iteration printing) and substitutes `dict.fromkeys(...)`. The
detection is done in lowering, not emit (see `MEP-51` §6).

### Phase 3.4: list of records

**Goal**: `list<Record>` where Record is a frozen dataclass.

**Fixtures**: 16.
- `list_record_basic`, `list_record_filter`, `list_record_sort`,
  `list_record_map`, `list_record_index`, `list_record_append`,
  `list_record_nested`, `list_record_with_option`,
  `list_record_with_list`, `list_record_with_map`,
  `list_record_query` (select-from-where), `list_record_groupby`,
  `list_record_distinct`, `list_record_aggregate`,
  `list_record_join`, `list_record_serialise_jsonl`.

**Gate**: Tiers 1-4. Dataclass emission uses `@dataclass(frozen=True,
slots=True)` (PEP 557 + PEP 622-ish slots). mypy strict requires
explicit type annotations on all dataclass fields; the emit always
includes them. pyright strict requires `field()` with
`default_factory` for mutable defaults; the emit always uses this
pattern.

### Phase 4: records (frozen dataclass)

**Goal**: standalone record types, equality, hashing, `__repr__`.

**Fixtures**: 20.
- `record_basic`, `record_equality`, `record_hash`, `record_repr`,
  `record_nested`, `record_with_optional`, `record_with_list`,
  `record_with_map`, `record_with_set`, `record_pattern_match`,
  `record_clone_with`, `record_serialise`, `record_deserialise`,
  `record_generic`, `record_generic_pep695`,
  `record_recursive` (linked-list node), `record_field_default`,
  `record_field_factory`, `record_inheritance` (Mochi disallows;
  fixture asserts emit-time error), `record_slots_size`.

**Gate**: Tiers 1-4. `record_slots_size` checks that emitted classes
have `__slots__` (no `__dict__`) by `sys.getsizeof(instance)` being
less than a struct-equivalent.

### Phase 5: sum types

**Goal**: PEP 695 type alias for sum types, dataclass variants,
exhaustive `match`.

**Fixtures**: 18.
- `sum_basic_two_variants`, `sum_three_variants`,
  `sum_variant_with_data`, `sum_variant_no_data`,
  `sum_nested`, `sum_recursive` (tree), `sum_generic`,
  `sum_match_exhaustive`, `sum_match_non_exhaustive_error`,
  `sum_match_guards`, `sum_match_wildcard`,
  `sum_serialise`, `sum_deserialise`,
  `sum_option_some_none`, `sum_result_ok_err`,
  `sum_either_left_right`, `sum_complex_records`,
  `sum_with_options_in_variants`.

**Gate**: Tiers 1-4. PEP 695 `type Foo = A | B | C` syntax requires
3.12+. mypy 1.13+ handles PEP 695 well; pyright 1.1.380+ also. We
pin both versions exactly (see "test stability" below).

Exhaustiveness: Python's `match` does not require exhaustiveness, but
mypy strict (`--strict` plus `--warn-unreachable`) flags unreachable
branches. We emit a final `case _:` that raises a synthetic
`assert_never` to satisfy both checkers. typing.assert_never (3.11+)
gives a type-narrowing guarantee.

### Phase 6: closures + higher-order

**Goal**: closures capture variables, lambdas have correct
inferred types, higher-order functions (map, filter, reduce,
fold) work.

**Fixtures**: 16.
- `closure_basic`, `closure_mutable_capture`,
  `closure_immutable_capture`, `closure_late_binding`,
  `closure_lambda_one_arg`, `closure_lambda_multi_arg`,
  `closure_higher_order_map`, `closure_higher_order_filter`,
  `closure_higher_order_reduce`, `closure_higher_order_compose`,
  `closure_curry`, `closure_partial_application`,
  `closure_returns_closure`, `closure_captures_self`,
  `closure_recursive`, `closure_in_method`.

**Gate**: Tiers 1-4. `closure_late_binding` checks that the
classic Python closure-in-loop gotcha (variable captured by
reference, not value) is handled by emit using a default-argument
trick or `functools.partial`.

### Phase 7: query DSL

**Goal**: Mochi's `from x in xs select x.y where ... order by ...`
DSL lowers to list / generator expressions + `itertools` where
appropriate.

**Fixtures**: 20.
- `query_select`, `query_where`, `query_orderby`,
  `query_groupby`, `query_distinct`, `query_join_inner`,
  `query_join_left`, `query_join_right`, `query_join_full`,
  `query_aggregate_count`, `query_aggregate_sum`,
  `query_aggregate_avg`, `query_aggregate_max`,
  `query_aggregate_min`, `query_nested`,
  `query_subquery`, `query_correlated_subquery`,
  `query_lazy_iterator`, `query_async_iterator`,
  `query_pipeline_dataflow`.

**Gate**: Tiers 1-4. `query_lazy_iterator` checks that the emit uses
a generator (not eager list comp) when the result is iterated only
once. `query_async_iterator` uses `AsyncIterator[T]` from
`collections.abc`.

### Phase 8: datalog

**Goal**: Mochi's datalog blocks compile to seminaive evaluation in
Python.

**Fixtures**: 12.
- `datalog_transitive_closure`, `datalog_path_finding`,
  `datalog_ancestor`, `datalog_same_generation`,
  `datalog_negation_stratified`, `datalog_negation_unstratified_error`,
  `datalog_arithmetic`, `datalog_aggregation_count`,
  `datalog_aggregation_sum`, `datalog_recursion_terminates`,
  `datalog_recursion_unsound_warning`, `datalog_large_dataset_perf`.

**Gate**: Tiers 1-4 plus a runtime budget check: each datalog fixture
must finish within 5 seconds on the CI host's reference machine
(github-hosted ubuntu-24.04, 4-core).

### Phase 9: agents

**Goal**: `agent` syntax lowers to a class with `asyncio.Queue`
mailbox and `asyncio.TaskGroup` supervision.

**Fixtures**: 18.
- `agent_basic`, `agent_state_mutation`, `agent_message_handler`,
  `agent_cast`, `agent_call_reply`, `agent_call_timeout`,
  `agent_supervisor_one_for_all`, `agent_supervisor_one_for_one`,
  `agent_supervisor_rest_for_one` (custom),
  `agent_cancellation_propagation`,
  `agent_exception_group_aggregation`, `agent_spawn_child`,
  `agent_child_failure_restart`, `agent_graceful_shutdown`,
  `agent_message_ordering_fifo`, `agent_backpressure_bounded_queue`,
  `agent_two_agents_interact`, `agent_pool_of_workers`.

**Gate**: Tiers 1-4 plus an asyncio-event-loop debug check
(`PYTHONASYNCIODEBUG=1`) for unawaited coroutines or
double-cancellation. The check fails the gate on any debug warning.

### Phase 10: streams

**Goal**: `stream<T>` lowers to `AsyncIterator[T]`. Stream
combinators (map, filter, fold, take, drop, zip) emit as async
generators.

**Fixtures**: 14.
- `stream_basic`, `stream_finite`, `stream_infinite_take`,
  `stream_map`, `stream_filter`, `stream_fold`,
  `stream_zip`, `stream_flatmap`, `stream_throttle`,
  `stream_buffer`, `stream_close_on_drop`,
  `stream_backpressure`, `stream_error_propagation`,
  `stream_two_consumers_split`.

**Gate**: Tiers 1-4. `stream_close_on_drop` checks `aclose()` is
called when the iterator is GC'd. Python's `gc.callbacks` is used to
verify; `__del__` on async iterators is unreliable.

### Phase 11: async coloring, MochiResult, ExceptionGroup

**Goal**: every async-capable function returns a coroutine; sync
functions stay sync; errors are `MochiResult[T, E]` not exceptions
for explicit error paths.

**Fixtures**: 20.
- `result_ok`, `result_err`, `result_match`, `result_map`,
  `result_chain`, `result_from_exception`,
  `result_into_exception_at_boundary`,
  `exception_group_aggregation`,
  `taskgroup_one_failure`, `taskgroup_two_failures`,
  `taskgroup_cancellation`, `taskgroup_nested`,
  `taskgroup_timeout_outer`, `taskgroup_timeout_inner`,
  `async_function_definition`, `async_function_call`,
  `async_function_concurrency_limit`,
  `async_generator_yield`, `async_generator_close`,
  `async_with_context_manager`.

**Gate**: Tiers 1-4. `async_with_context_manager` tests `async with`
+ `__aenter__` / `__aexit__`. mypy strict requires the protocol;
pyright strict checks the same.

### Phase 12: FFI

**Goal**: Mochi `extern` declarations lower to ctypes calls for
native libraries and to direct Python imports for pure-Python deps.

**Fixtures**: 14.
- `ffi_ctypes_libc_strlen`, `ffi_ctypes_libc_qsort`,
  `ffi_ctypes_struct_pack`, `ffi_ctypes_callback`,
  `ffi_pure_python_import`, `ffi_typed_stub_only`,
  `ffi_cffi_alternative` (skipped if cffi not installed),
  `ffi_error_propagation`, `ffi_memory_safety`,
  `ffi_thread_safety`, `ffi_release_gil_decorator`,
  `ffi_platform_specific_linux`, `ffi_platform_specific_macos`,
  `ffi_platform_specific_windows`.

**Gate**: Tiers 1-4. Platform-specific fixtures use `@pytest.mark.skipif`
based on `sys.platform`. The runner uses `--platform=<plat>`
matching to gate which fixtures count per host.

### Phase 13: LLM provider dispatch

**Goal**: Mochi's `llm.chat` lowers to a `mochi-runtime[ai]` call
that dispatches to the right provider (Anthropic, OpenAI, local
Ollama).

**Fixtures**: 8 (with `--llm=mock` to avoid network).
- `llm_basic_completion`, `llm_streaming_completion`,
  `llm_tool_use`, `llm_multi_turn`,
  `llm_provider_anthropic`, `llm_provider_openai`,
  `llm_provider_local_ollama`, `llm_error_handling`.

**Gate**: Tiers 1-4. Real network calls are gated to a nightly run
with `--llm=real` and credentials from secrets; the PR CI uses the
mock provider.

### Phase 14: fetch (httpx)

**Goal**: Mochi `fetch` (HTTP GET/POST) lowers to httpx calls.

**Fixtures**: 10.
- `fetch_get_text`, `fetch_get_json`, `fetch_post_json`,
  `fetch_redirect`, `fetch_timeout`, `fetch_auth_basic`,
  `fetch_auth_bearer`, `fetch_streaming_response`,
  `fetch_error_status`, `fetch_proxy`.

**Gate**: Tiers 1-4. Fixtures hit a local httptest server
(`httpx.MockTransport`); no real network in CI.

### Phase 15: wheel + sdist build via uv

**Goal**: `uv build` produces wheel + sdist that install and run.

**Fixtures**: 4.
- `wheel_basic_install`, `wheel_with_extras`,
  `sdist_basic_install`, `sdist_with_extras`.

**Gate**: Tier 4 dominates. The fixture runs `uv build`, installs the
wheel into a fresh venv, runs a smoke test, then repeats with the
sdist. The build is gated on `dist/*.whl` and `dist/*.tar.gz` both
existing with the expected name.

### Phase 16: reproducible build

**Goal**: byte-identical wheel SHA256 across hosts.

**Fixtures**: 2.
- `reproducibility_basic`, `reproducibility_with_extras`.

**Gate**: Build on linux-x86_64, linux-aarch64, macos-arm64; compare
SHA256. Windows reproducibility deferred to Phase 16.1.

Sub-phase 16.1 (Windows reproducibility): adds a third fixture and
extends the SHA comparison to windows-x86_64. Outstanding issues:
filesystem case sensitivity in zip entries, CRLF/LF handling in
generated sources (we already normalise but the gate verifies).

### Phase 17: Jupyter ipykernel

**Goal**: Mochi code runs cell-by-cell in JupyterLab.

**Fixtures**: 10.
- `notebook_helloworld`, `notebook_variable_persistence`,
  `notebook_function_redefinition`, `notebook_import_in_cell`,
  `notebook_error_handling`, `notebook_async_cell`,
  `notebook_plot_matplotlib`, `notebook_query_dsl`,
  `notebook_record_definition`, `notebook_multi_cell_workflow`.

**Gate**: Tiers 1-3 do not apply directly (the source is a .ipynb,
not a .mochi file). Tier 4 is replaced by the notebook execution
diff:

```
jupyter nbconvert --execute fixtures/<name>.ipynb --output /tmp/actual.ipynb
diff (filtered) /tmp/actual.ipynb fixtures/<name>.expect.ipynb
```

Filter: remove `execution_count`, `id`, cell metadata; preserve
`outputs` text/plain entries.

### Phase 18: PyPI Trusted Publishing

**Goal**: end-to-end publish flow including OIDC and PEP 740
attestation.

**Fixtures**: 1.
- `publish_dryrun_trusted_publishing`.

**Gate**: `uv publish --dry-run --trusted-publishing always` exits 0
when the OIDC token claims match the configured PyPI trust. Real
publish runs only on release tags, not PR CI.

## Total fixture count target

Approximate target by Phase 18: 400 fixtures.

Running total (cumulative):
- After Phase 1: 1
- After Phase 2: 13
- After Phase 3.1: 31
- After Phase 3.2: 53
- After Phase 3.3: 67
- After Phase 3.4: 83
- After Phase 4: 103
- After Phase 5: 121
- After Phase 6: 137
- After Phase 7: 157
- After Phase 8: 169
- After Phase 9: 187
- After Phase 10: 201
- After Phase 11: 221
- After Phase 12: 235
- After Phase 13: 243
- After Phase 14: 253
- After Phase 15: 257
- After Phase 16: 259
- After Phase 17: 269
- After Phase 18: 270

The 400 target leaves room for ad-hoc fixtures added post-phase
(regression captures, user bug reports, edge cases). We expect about
130 such fixtures to land between Phase 18 ratification and v1
release.

## Go test wrappers

Following the existing `tests/transpiler3/c/` precedent, each phase
gets a Go test file:

```go
// tests/transpiler3/python/phase1_helloworld_test.go
package python_test

import (
    "testing"

    "mochi/tests/transpiler3/python/runner"
)

func TestPhase1HelloWorldPython(t *testing.T) {
    runner.RunPhase(t, "phase1", "helloworld")
}
```

The `runner.RunPhase` helper:

1. Loads the fixture directory under `fixtures/<phase>/<name>/`.
2. Reads `meta.toml` for skip flags.
3. Runs vm3 on `source.mochi` to capture the reference stdout.
4. Compares vm3 stdout to `expect.txt` (sanity check).
5. Invokes the Mochi transpiler with `--target=python`.
6. Writes emitted Python to a temp directory.
7. Runs Tier 2: `uv run mypy --strict src/`.
8. Runs Tier 2: `uv run pyright`.
9. Runs Tier 3: `uv run ruff check src/` + `uv run ruff format --check src/`.
10. Runs the Python: `python -m <pkg>`.
11. Compares Python stdout to `expect.txt` (Tier 1 master gate).
12. Runs Tier 4: `uv build` + `uv pip install` + smoke test.

Failures at any tier print a diff and exit with a tier-specific code
so CI can surface which tier failed.

The runner is parallelised: each fixture runs in its own goroutine
with its own temp dir. The Tier 4 build step is the bottleneck (~5
seconds per fixture); we cap parallelism at `GOMAXPROCS`.

## CI matrix

The full per-release CI matrix:

| OS              | Python    | Arch    | Notes                  |
|-----------------|-----------|---------|------------------------|
| ubuntu-24.04    | 3.12.0    | x86_64  | floor                  |
| ubuntu-24.04    | 3.12.7    | x86_64  | latest patch           |
| ubuntu-24.04    | 3.13.0    | x86_64  | next-floor candidate   |
| ubuntu-24.04-arm | 3.12.7   | aarch64 | ARM verification       |
| macos-14        | 3.12.7    | arm64   | Apple Silicon          |
| windows-2022    | 3.12.7    | x86_64  | Windows                |

6 cells. Tier 1-4 gates run on every cell. Reproducibility runs on the
three non-Windows cells. ipykernel runs only on ubuntu-24.04 +
3.12.7 x86_64 (Phase 17).

Each cell takes about 8 minutes to clear all four tiers for the full
fixture set as of Phase 18 (270 fixtures, 400 with regression). Total
CI wall-clock per release: about 50 minutes for the test job, plus 5
minutes for build / reproducibility / publish.

We do NOT test on macOS x86_64. GitHub deprecated x86_64 macOS runners
in 2024; ARM is the default. Users running x86_64 macOS (still
sold as "Intel Macs" through 2023) get a documented best-effort
support level; we cannot run gates there.

## Test stability: pinned tool versions

The gate must not drift due to checker upgrades. We pin exact
versions:

| Tool      | Version  | Reason                                  |
|-----------|----------|-----------------------------------------|
| mypy      | 1.13.0   | PEP 695 stable, no flagged regressions  |
| pyright   | 1.1.380  | strict mode parity with mypy 1.13       |
| ruff      | 0.6.9    | format stable + PEP 695 lint complete   |
| pytest    | 8.3.x    | latest stable                            |
| uv        | 0.7.0    | first stable lockfile format            |

The pins live in `pyproject.toml` `[project.optional-dependencies].dev`
and in `uv.lock` (auto-pinned from the spec).

We bump pins quarterly in a dedicated PR with the diff of new
warnings (any new strict-mode error becomes a fix-in-PR; any new
warning becomes an audit ticket).

## mypy plugin: no

Some projects ship a mypy plugin to teach mypy about emitted code
patterns (Pydantic does this, attrs does this until type narrowing
matured). We do not ship a Mochi mypy plugin. Reasons:

1. Plugins are mypy-specific. pyright has no plugin API. We'd have to
   double-implement.
2. PEP 695 plus PEP 698 plus `Self` (PEP 673) gives us the type
   primitives we need to express Mochi's type system directly. No
   inference gaps.
3. Plugin maintenance burden: every mypy major release breaks plugin
   ABI. Pydantic 1's plugin was a perennial source of bug reports
   until Pydantic 2 reduced its scope.

Mochi-emitted code stands alone in mypy strict mode without a plugin.
This is a hard requirement; if we hit a pattern that mypy can't infer
without a plugin, we change the emit, not add a plugin.

## Golden file management

Each fixture has `expect.txt` (Tier 1 master gate) and optionally
`expect.ipynb` (Phase 17). Conventions:

- Line endings: LF (`\n`), not CRLF. Enforced via `.gitattributes`
  `* text=auto eol=lf`.
- Trailing newline: file ends with `\n`. Mochi's print adds a
  trailing newline by default.
- UTF-8 encoding, no BOM.
- For floats: emit uses Python's `f"{x:.6g}"` to truncate
  representation, then matches vm3's identical truncation.

Regenerating goldens: `mochi tests regen --phase=<n>` runs vm3 on
every fixture in the phase and overwrites `expect.txt`. This is a
last-resort tool; in normal flow goldens are stable.

## Error path testing

Not every fixture tests the success path. Some test the error path:

- Phase 3.1: `list_index_out_of_bounds` tests Python `IndexError`
  becoming a Mochi-aware error message.
- Phase 5: `sum_match_non_exhaustive_error` tests that the emit
  catches a non-exhaustive match at compile time (Mochi
  exhaustiveness checker) and emits an `assert_never` that mypy +
  pyright enforce.
- Phase 9: `agent_child_failure_restart` tests cancellation +
  restart paths.

Error-path fixtures have `expect.txt` matching the expected error
message verbatim. Error message stability is a contract: changing the
emitted error text breaks fixtures. We bump fixtures intentionally
when the error format is improved.

## Platform-specific tests

Some fixtures test platform-specific behaviour. Phase 12 (FFI) has
`ffi_platform_specific_linux`, `ffi_platform_specific_macos`,
`ffi_platform_specific_windows`. The runner reads `meta.toml`:

```toml
platforms = ["linux"]
```

and skips the fixture on other platforms. Skipping is logged in CI
output, not silent.

Phase 16 reproducibility excludes Windows by similar mechanism: the
reproducibility job has `if: runner.os != 'Windows'` in the workflow.

## Performance gates

Some phases enforce a wall-clock budget per fixture:

- Phase 8 (datalog): 5 seconds per fixture on github-hosted
  ubuntu-24.04.
- Phase 9 (agents): 2 seconds per fixture (longer ones use
  asyncio.timeout(2)).
- Phase 13 (LLM with mock): 1 second per fixture.

Wall-clock gates are noisy on shared runners. We use a 2x tolerance
(`timeout * 2` triggers a flake re-run before failing). After two
consecutive fails the test is marked failed.

## Snapshot stability

The emitted Python source itself is not a gate. We do not snapshot
emit output. Reasons:

- Emit output changes frequently as the emitter is refactored.
- The gate is on observable behaviour (Tier 1) and the static
  artifact (Tier 4), not on the intermediate source.
- Snapshotting emit output creates churn: every refactor breaks
  thousands of golden files.

If a developer wants to inspect the emit, `mochi transpile
--target=python --print-source` prints to stdout without writing
files. No CI gate compares the print.

## Comparison to MEP-45 (C) and MEP-50 (Kotlin) test gates

| Concern              | MEP-45 (C)              | MEP-50 (Kotlin)         | MEP-51 (Python)              |
|----------------------|-------------------------|-------------------------|------------------------------|
| Master gate          | vm3 byte-equal stdout   | vm3 byte-equal stdout   | vm3 byte-equal stdout         |
| Compile gate         | `clang -fsyntax-only`   | `kotlinc -Werror`       | mypy + pyright (both strict) |
| Lint gate            | `clang-tidy` subset     | `ktlint`                | `ruff check`                  |
| Format gate          | `clang-format --dry-run`| `ktlint --format check` | `ruff format --check`         |
| Build gate           | `make` + executable run | `gradle build` + jar    | `uv build` + wheel install   |
| Reproducibility      | `SOURCE_DATE_EPOCH`     | `gradle --reproducible` | `SOURCE_DATE_EPOCH` + sort   |
| Notebook gate        | n/a                     | Kotlin Notebook         | ipykernel                     |
| OIDC publish         | n/a (no central registry)| central-portal OIDC    | PyPI Trusted Publishing       |

Three differences worth calling out:

1. **Two type checkers**. Only MEP-51 runs two type checkers. MEP-50
   has one (`kotlinc`), MEP-45 has none beyond clang. The dual gate
   costs us about 30% of total CI time but catches edge cases
   neither checker would alone.
2. **Notebook gate**. MEP-50 has Kotlin Notebook (Jupyter via the
   Kotlin kernel); MEP-51 has ipykernel via direct Python. Same
   shape, different plumbing.
3. **Reproducibility scope**. MEP-45 reproducibility is per-platform
   (the wheel-equivalent is an executable; cross-platform identical
   binaries are not achievable). MEP-50 and MEP-51 have a
   cross-platform artifact (jar / wheel), so identical-bytes is
   achievable and gated.

## References

- PEP 484, 526, 561, 585, 591, 612, 646, 647, 673, 692, 695, 698 (typing)
- PEP 654 (ExceptionGroup), PEP 678 (BaseException.add_note)
- mypy docs, `mypy.readthedocs.io`
- pyright docs, `microsoft.github.io/pyright/`
- ruff docs, `docs.astral.sh/ruff/`
- `the shared-decisions anchor` for load-bearing decisions
- `[[10-build-system]]` for the build pipeline this gate tests
- `[[12-risks-and-alternatives]]` for divergence + tool-version risks
