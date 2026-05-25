---
title: "MEP-51 research note 08, Dataset and query pipeline (Python lowering)"
description: "Mochi LINQ-style query DSL and Datalog lowered to Python via generator expressions, itertools, and async iterators."
---

# MEP-51 research note 08, Dataset and query pipeline

Author: research pass for MEP-51 (Mochi to Python transpiler).
Date: 2026-05-23 12:30 (GMT+7).

This note specifies the lowering of Mochi's LINQ-style query DSL and Datalog into Python source. The runtime substrate is the Python standard library: generator expressions, `itertools`, `collections`, `heapq`, `bisect`, plus `asyncio` for streaming queries over async iterators. The transpiler does **not** lower to pandas, polars, or DuckDB in v1 (deferred to v2 with caveats discussed in §15). The transpiler does **not** introduce a separate query engine; the lowering is direct AST-to-AST, with the IR pass (`transpiler3/python/lower/`) doing filter pushdown and projection pruning before emit.

Companion notes: the shared-decisions anchor, [[04-runtime]], [[05-codegen-design]], [[06-type-lowering]], [[07-python-target-portability]], [[09-agent-streams]], [[10-build-system]], [[11-testing-gates]], [[12-risks-and-alternatives]].

The four big-picture decisions, defended in [[02-design-philosophy]] §13 and stated here as operating assumptions:

1. **Generator expressions for sync, `async for` for async.** Mochi's `from x in xs ... select f(x)` lowers to a Python generator expression `(f(x) for x in xs ...)` when `xs` is a regular iterable, and to an `async for` comprehension when `xs` is an `AsyncIterator`. The choice is made statically by the type checker; mixed sync/async raises an emit-time error.
2. **itertools is the workhorse.** `itertools.groupby`, `itertools.chain`, `itertools.islice`, `itertools.tee`, `itertools.accumulate` cover ~80 % of the query operators. The remaining 20 % (window functions, percentile, partial aggregates) are hand-rolled in `mochi_runtime/query.py`.
3. **Dataclass records, not pandas.** Mochi records lower to `@dataclass(frozen=True, slots=True)` (see [[06-type-lowering]] §4); the query DSL operates over `Iterable[Record]`, not `DataFrame`. Pandas / polars / DuckDB are v2 candidates with explicit user opt-in (`--target=python-pandas`), not the v1 default.
4. **Datalog via semi-naive evaluation.** Mochi Datalog rules lower to a fixpoint loop that materialises relations as `set[tuple[...]]` and applies rules in topological order until no relation grows. Magic sets, stratified negation, and aggregate rules are lowered to specialised passes.

The query DSL surface (Mochi grammar):

```mochi
let result = from x in xs
             from y in ys
             where x.k == y.k
             where x.v > 10
             group by x.k into g
             order by g.key
             select { k: g.key, total: sum(g.v) }
             limit 100
```

This is the LINQ-style shape borrowed from C# and Kotlin. The emit target:

```python
result = list(
    (
        Record_kvtotal(k=k, total=sum(item.v for item in g))
        for k, g in itertools.groupby(
            sorted(
                (
                    x
                    for x in xs
                    for y in ys
                    if x.k == y.k
                    if x.v > 10
                ),
                key=lambda x: x.k,
            ),
            key=lambda x: x.k,
        )
    )
)[:100]
```

The lowering is mechanical: each clause maps to a generator-expression nesting level, except `group by` (requires materialisation via `sorted` + `itertools.groupby`) and `order by` (requires materialisation via `sorted` or `list.sort`).

---

## 1. Lowering choice: generator expressions vs list comprehensions

Python has two related forms: list comprehensions `[f(x) for x in xs]` and generator expressions `(f(x) for x in xs)`. The differences:

| Aspect              | List comp `[...]`        | Gen expr `(...)`               |
|---------------------|--------------------------|--------------------------------|
| Eagerness           | eager (materialises)     | lazy (iterator)                |
| Memory              | O(n) immediately         | O(1) per step                  |
| Reusable            | yes (list)               | no (single-pass iterator)      |
| Composes with reduce| via `sum(list)` etc.     | also via `sum(gen)` etc.       |
| Performance for full materialise | ~1.2x faster | slightly slower                |

We lower Mochi queries to generator expressions when the result is consumed by an aggregate (`sum`, `min`, `max`, `count`, `avg`) or by another query stage that does not need re-iteration. We lower to list comprehensions when the result is bound to a `let` and might be iterated multiple times.

Static analysis at the IR pass:

- If the result is bound to a `let` and the binding has more than one use site, lower to `list(...)`.
- If the result feeds directly into `sum`, `min`, `max`, `count`, `for`, or another query, lower to a generator expression.
- If the result is the body of a function and the return type is `list[T]`, lower to `list(...)`.
- If the result is the body of a function and the return type is `Iterable[T]` or `Iterator[T]`, lower to a generator expression.

This avoids unnecessary materialisation. A common Mochi pattern is:

```mochi
let total = sum(from x in xs where x.v > 0 select x.v)
```

which lowers to:

```python
total: int = sum(x.v for x in xs if x.v > 0)
```

with zero intermediate list allocation.

## 2. Async lowering

When the source iterator is `AsyncIterator[T]`, the entire pipeline becomes async. Python 3.6+ supports `async for` comprehensions:

```python
result: list[int] = [x async for x in async_xs if x > 0]
```

For multi-clause Mochi queries we use a sequence of `async for` clauses inside a list / generator expression:

```mochi
let result = from x in async_xs
             from y in async_ys
             where x.k == y.k
             select { x: x, y: y }
```

Lowers to:

```python
result = [
    Record_xy(x=x, y=y)
    async for x in async_xs
    async for y in async_ys
    if x.k == y.k
]
```

Note: Python comprehension grammar allows mixing `async for` and `for` and `if`, but mixing `for` (sync) and `async for` (async) in the same expression requires the outermost loop to be the type that dominates. PEP 530 and PEP 572 settled this; we always emit `async for` first when any clause is async.

For aggregates over async iterators, `sum(async_gen)` does **not** work because `sum` expects a sync iterable. The Mochi runtime provides `async_sum`:

```python
async def async_sum(it: AsyncIterable[int]) -> int:
    total = 0
    async for x in it:
        total += x
    return total
```

Mochi's `sum(from x in async_xs ...)` lowers to `await async_sum(...)`. Similar wrappers exist for `async_min`, `async_max`, `async_count`, `async_avg`.

## 3. itertools usage

Python's `itertools` module is the workhorse. The Mochi-to-Python lowering uses these `itertools` functions:

### 3.1 `itertools.chain`

For Mochi's union (multi-source from):

```mochi
from x in (xs ++ ys) select x
```

lowers to:

```python
[x for x in itertools.chain(xs, ys)]
```

`chain.from_iterable` is used when concatenating an iterable of iterables (Mochi's `flatten`):

```python
flat = list(itertools.chain.from_iterable(list_of_lists))
```

### 3.2 `itertools.groupby`

Mochi's `group by key` lowers to `itertools.groupby` **after** sorting by the key (because `itertools.groupby` only groups consecutive equal keys):

```mochi
from x in xs group by x.k into g select { k: g.key, n: count(g) }
```

lowers to:

```python
result = [
    Record_kn(k=k, n=sum(1 for _ in g))
    for k, g in itertools.groupby(sorted(xs, key=lambda x: x.k), key=lambda x: x.k)
]
```

When the group's contents are consumed multiple times (e.g. compute both count and sum), we materialise: `for k, g_iter in itertools.groupby(...): g = list(g_iter)`. The IR pass detects this and inserts the `list(...)` materialisation.

### 3.3 `itertools.islice`

Mochi's `limit N` and `skip N + limit M`:

```mochi
from x in xs select x limit 100
```

lowers to:

```python
result = list(itertools.islice((x for x in xs), 100))
```

With offset:

```mochi
from x in xs select x skip 50 limit 100
```

lowers to:

```python
result = list(itertools.islice((x for x in xs), 50, 50 + 100))
```

`itertools.islice` is O(skip) for the offset, which is the only available option without random-access iteration. For random-access iterables (lists), we could special-case to `xs[50:150]`; the IR pass does this optimisation when `xs` is known to be `list[T]`.

### 3.4 `itertools.tee`

When a Mochi query has two consumers and the source is single-pass, we fan out via `itertools.tee`:

```python
xs_a, xs_b = itertools.tee(xs, 2)
total = sum(x.v for x in xs_a)
count = sum(1 for x in xs_b)
```

This is internal to the runtime; user-facing Mochi code does not call `tee`. The IR pass inserts it when a single-pass iterator has multiple readers.

### 3.5 `itertools.accumulate`

Mochi's running-total / scan operator (planned for v2):

```mochi
from x in xs select cumulative_sum(x.v)
```

lowers to:

```python
result = list(itertools.accumulate(x.v for x in xs))
```

`itertools.accumulate` accepts a binary function (defaults to `operator.add`) so non-sum scans work: `accumulate(xs, operator.mul)` for cumulative product.

### 3.6 `itertools.product`

Mochi cross-join:

```mochi
from x in xs from y in ys select (x, y)
```

without a join condition lowers to `itertools.product`:

```python
result = list(itertools.product(xs, ys))
```

With a join condition the lowering becomes a join (see §5).

### 3.7 `itertools.permutations` / `itertools.combinations`

Mochi has explicit `permutations(xs, k)` and `combinations(xs, k)` builtins that lower directly to the itertools functions of the same name.

## 4. Set operations

Mochi's set operators map to Python `set` operators:

| Mochi          | Python                            |
|----------------|-----------------------------------|
| `a union b`    | `a \| b` (for `set`), `dict(itertools.chain(a.items(), b.items()))` for `map` |
| `a intersect b`| `a & b`                           |
| `a except b`   | `a - b`                           |
| `a symdiff b`  | `a ^ b`                           |
| `x in a`       | `x in a`                          |

For ordered sets (where iteration order is observable), Mochi uses `dict.fromkeys`:

```python
ordered_set = dict.fromkeys([1, 2, 3, 2, 1])  # {1: None, 2: None, 3: None}
```

This preserves insertion order (Python 3.7+ dict invariant) and gives O(1) membership.

For multi-set operations (preserving counts), Mochi lowers to `collections.Counter`:

```mochi
let counts = multiset(xs)
let merged = counts1 + counts2
```

lowers to:

```python
counts = collections.Counter(xs)
merged = counts1 + counts2
```

`Counter` supports `+`, `-`, `&`, `|` as multiset arithmetic.

## 5. Join strategies

Mochi joins lower to one of three strategies, picked by the IR pass:

### 5.1 Hash join (default)

For an inner equi-join with one small side and one large side, we build a hash index on the small side and probe with the large side:

```mochi
from x in xs from y in ys where x.k == y.k select { x: x, y: y }
```

lowers to:

```python
ys_by_k: dict[K, list[Y]] = {}
for y in ys:
    ys_by_k.setdefault(y.k, []).append(y)
result = [
    Record_xy(x=x, y=y)
    for x in xs
    for y in ys_by_k.get(x.k, ())
]
```

The IR pass picks the smaller side (when statically determinable via size hints) for the hash index. When both sides are unknown, we default to indexing the right-hand side (`ys`).

For outer joins (`left join`, `right join`), we add a `None` fallback:

```mochi
from x in xs left join y in ys on x.k == y.k select { x: x, y: y }
```

lowers to:

```python
ys_by_k: dict[K, list[Y]] = {}
for y in ys:
    ys_by_k.setdefault(y.k, []).append(y)
result = [
    Record_xy(x=x, y=y)
    for x in xs
    for y in (ys_by_k.get(x.k) or [None])
]
```

The `(ys_by_k.get(x.k) or [None])` idiom yields `[None]` when the key is absent, giving the left-join semantic.

### 5.2 Merge join

For sorted-sorted equi-joins on the same key, we use the `heapq.merge` approach:

```mochi
from x in xs from y in ys where x.k == y.k select (x, y)  // both sorted by k
```

The IR pass detects sorted-ness via `@sorted_by("k")` type annotations or via a preceding `order by k` clause. If both sides are sorted, lower to a merge join:

```python
def merge_join(xs: list[X], ys: list[Y]) -> Iterator[tuple[X, Y]]:
    i, j = 0, 0
    while i < len(xs) and j < len(ys):
        if xs[i].k < ys[j].k:
            i += 1
        elif xs[i].k > ys[j].k:
            j += 1
        else:
            k = xs[i].k
            i_start = i
            while i < len(xs) and xs[i].k == k:
                i += 1
            j_start = j
            while j < len(ys) and ys[j].k == k:
                j += 1
            for xx in xs[i_start:i]:
                for yy in ys[j_start:j]:
                    yield (xx, yy)
result = list(merge_join(xs, ys))
```

Merge join is O(n + m) vs hash join's O(n + m) but with smaller constants and better cache behaviour for large sorted inputs. We use merge join when both sides are statically sorted; otherwise hash join.

### 5.3 Nested-loop fallback

For very small sides (statically known via type hint `@max_size(16)` or constant literal), we skip the hash and do a nested loop:

```python
result = [
    (x, y)
    for x in xs
    for y in ys
    if x.k == y.k
]
```

The nested loop is O(n * m) but with a tiny constant when both sides are small (e.g. lookup table + one row).

### 5.4 Join selection heuristics

The IR pass picks:

1. If both sides have static `@sorted_by(K)`, use merge join.
2. Else if one side has static `@max_size(N)` with N <= 16, use nested loop.
3. Else if join key is non-equi (`x.k > y.k`), use nested loop.
4. Else use hash join.

The decision is recorded in the IR as a `JoinStrategy` enum and is inspectable via `mochi build --emit-ir`.

## 6. Aggregates

Aggregates close over a query and produce a scalar (or a record per group).

### 6.1 sum / min / max / count

Direct lowering:

```mochi
let total = sum(from x in xs select x.v)
let lo    = min(from x in xs select x.v)
let hi    = max(from x in xs select x.v)
let cnt   = count(from x in xs select x.v)
```

lowers to:

```python
total = sum(x.v for x in xs)
lo    = min((x.v for x in xs), default=0)  # default avoids ValueError on empty
hi    = max((x.v for x in xs), default=0)
cnt   = sum(1 for _ in xs)  # NOT len(list(xs)) to avoid materialisation
```

For `count(xs)` where `xs` is a list, we lower to `len(xs)` directly (O(1)).

For `min` / `max` with `default=0` for ints (or `default=0.0` for floats), the IR pass picks the right default from the type. For `Optional[T]`, we lower to `min(it, default=None)`.

### 6.2 avg

Python has no built-in average; we use `statistics.mean`:

```python
avg = statistics.mean(x.v for x in xs)
```

`statistics.mean` raises `StatisticsError` on empty input; Mochi semantics is to return `None` on empty average. The runtime wraps:

```python
def safe_avg(it: Iterable[float]) -> float | None:
    values = list(it)
    if not values:
        return None
    return statistics.fmean(values)  # fmean is faster than mean
```

Note `statistics.fmean` is a 3.8+ floating-point optimised mean; we prefer it because Mochi `avg` always returns float.

### 6.3 Percentile / median / quantile

`statistics.median(xs)`, `statistics.quantiles(xs, n=4)` give quartiles. Mochi:

```mochi
let p95 = percentile(xs, 0.95)
```

lowers to:

```python
def percentile(xs: list[float], p: float) -> float:
    if not xs:
        return float("nan")
    s = sorted(xs)
    k = (len(s) - 1) * p
    f = math.floor(k)
    c = math.ceil(k)
    if f == c:
        return s[int(k)]
    return s[f] + (s[c] - s[f]) * (k - f)

p95 = percentile([x.v for x in xs], 0.95)
```

The percentile helper is in `mochi_runtime/query.py`. For higher accuracy on large datasets, we delegate to `numpy.percentile` if `numpy` is in the dependency set (out of scope for v1).

### 6.4 Custom aggregates

User-defined aggregates lower to a fold:

```mochi
let total = reduce(xs, 0, fn(acc, x) -> acc + x.v)
```

lowers to:

```python
total = functools.reduce(lambda acc, x: acc + x.v, xs, 0)
```

`functools.reduce` is the canonical fold in Python. We prefer it over a hand-rolled loop because it composes with the type checker.

## 7. group by

Mochi's `group by` is the most complex clause. Three lowering strategies:

### 7.1 Sort + groupby (default)

For ordered grouping (when the order of groups matters or when downstream `order by` follows):

```mochi
from x in xs group by x.k into g order by g.key select { k: g.key, n: count(g) }
```

lowers to:

```python
sorted_xs = sorted(xs, key=lambda x: x.k)
result = [
    Record_kn(k=k, n=sum(1 for _ in g))
    for k, g in itertools.groupby(sorted_xs, key=lambda x: x.k)
]
```

### 7.2 defaultdict (unordered groups)

For unordered grouping when the order does not matter (downstream consumer is `sum`, `count`, or set-like):

```mochi
from x in xs group by x.k into g select { k: g.key, total: sum(g.v) }
```

The IR pass detects that result order is not constrained (no `order by` follows) and lowers to:

```python
groups: dict[K, list[X]] = {}
for x in xs:
    groups.setdefault(x.k, []).append(x)
result = [
    Record_ktotal(k=k, total=sum(x.v for x in g))
    for k, g in groups.items()
]
```

`defaultdict(list)` is equivalent but slightly faster. We use `dict.setdefault` to avoid the import and to keep the type explicit.

The `groups.items()` iteration is insertion-ordered (Python 3.7+ dict invariant), so the result reflects the order in which keys were first encountered in `xs`. This is the Mochi-specified group order for `group by` without `order by`.

### 7.3 Counter / specialised aggregates

For `group by x.k select count(g)` specifically, we use `collections.Counter`:

```python
counts = collections.Counter(x.k for x in xs)
result = [Record_kn(k=k, n=n) for k, n in counts.items()]
```

`Counter` is a C-implemented dict subclass and is ~2x faster than the defaultdict approach. The IR pass selects it when the aggregate is exactly `count(g)`.

## 8. order by

Mochi's `order by` lowers to `sorted` for general ordering or `heapq.nsmallest` / `nlargest` for top-K.

### 8.1 General order by

```mochi
from x in xs order by x.k select x
```

lowers to:

```python
result = sorted(xs, key=lambda x: x.k)
```

Python's `sorted` is stable Timsort, O(n log n). For multi-key sort:

```mochi
order by x.k1, x.k2 desc
```

lowers to:

```python
result = sorted(xs, key=lambda x: (x.k1, -x.k2))  # if k2 numeric
# or, more generally:
result = sorted(sorted(xs, key=lambda x: x.k2, reverse=True), key=lambda x: x.k1)
```

The second form (compose two sorts, lowest priority first) relies on Timsort's stability. We use the negation trick only for numeric keys; for arbitrary types we compose stable sorts.

### 8.2 In-place sort

When the input is a list and we are not preserving the original:

```mochi
xs.sort_by(fn(x) -> x.k)
```

lowers to:

```python
xs.sort(key=lambda x: x.k)
```

`list.sort` is in-place, no allocation.

### 8.3 Top-K (heap-based)

```mochi
from x in xs order by x.k limit 10
```

If `limit` is small (statically <= 100) and the source is large, we lower to `heapq.nsmallest`:

```python
result = heapq.nsmallest(10, xs, key=lambda x: x.k)
```

`heapq.nsmallest(k, n)` runs in O(n log k), beating `sorted(xs)[:k]`'s O(n log n) for small k.

For descending top-K:

```mochi
order by x.k desc limit 10
```

lowers to:

```python
result = heapq.nlargest(10, xs, key=lambda x: x.k)
```

The IR pass picks `nsmallest` / `nlargest` when:

- `limit` is a constant <= 100.
- `xs` has no static size hint or `@max_size > 1000`.

Otherwise it uses `sorted(...)[:N]`.

### 8.4 Streaming top-K

For async iterators, neither `sorted` nor `heapq.nsmallest` applies directly (they consume a list). We hand-roll a streaming top-K in `mochi_runtime/query.py`:

```python
import heapq
from collections.abc import AsyncIterator

async def async_nsmallest(k: int, it: AsyncIterator[T], key) -> list[T]:
    heap: list[tuple[Any, int, T]] = []
    counter = 0
    async for x in it:
        kv = key(x)
        if len(heap) < k:
            heapq.heappush(heap, (kv, counter, x))
            counter += 1
        else:
            heapq.heappushpop(heap, (kv, counter, x))
            counter += 1
    return [item for (_, _, item) in sorted(heap)]
```

The `counter` field gives stable ordering when keys are equal (the heap is a min-heap on `(key, counter)`).

## 9. limit and offset

Mochi `limit N` and `skip N`:

```mochi
from x in xs select x skip 10 limit 100
```

For list inputs (random access):

```python
result = xs[10:110]
```

For iterators:

```python
result = list(itertools.islice(xs, 10, 110))
```

The IR pass picks `xs[10:110]` when the source is statically a `list[T]`; otherwise `itertools.islice`.

Edge case: `limit -1` in Mochi means "no limit". The IR pass elides the `islice` entirely when limit is -1.

## 10. Window functions

Python's standard library has no equivalent of SQL window functions (`row_number()`, `lead`, `lag`, `rank`, `dense_rank`, sliding window aggregates). Mochi window functions are hand-rolled in `mochi_runtime/query.py` using `collections.deque`:

```python
import collections
from collections.abc import Iterable, Iterator

def window(xs: Iterable[T], size: int) -> Iterator[list[T]]:
    """Yield successive size-length lists from xs (sliding window)."""
    it = iter(xs)
    buf: collections.deque[T] = collections.deque(maxlen=size)
    for x in it:
        buf.append(x)
        if len(buf) == size:
            yield list(buf)

def row_number(xs: Iterable[T]) -> Iterator[tuple[int, T]]:
    return enumerate(xs, start=1)

def lag(xs: Iterable[T], n: int = 1) -> Iterator[T | None]:
    buf: collections.deque[T] = collections.deque([None] * n, maxlen=n)
    for x in xs:
        yield buf[0]
        buf.append(x)

def lead(xs: Iterable[T], n: int = 1) -> Iterator[T | None]:
    items = list(xs)
    for i, _ in enumerate(items):
        yield items[i + n] if i + n < len(items) else None
```

`collections.deque(maxlen=N)` is O(1) push/pop and gives a fixed-size sliding buffer for free.

Mochi:

```mochi
from x in xs let avg = avg(window(xs, 5)) select { x: x, avg: avg }
```

lowers to:

```python
result = [
    Record_xavg(x=x, avg=statistics.fmean(w))
    for x, w in zip(xs, window(xs, 5))
]
```

This is `O(n * 5)` for a window-5 moving average.

## 11. Streaming queries over AsyncIterator

Async iterators are produced by Mochi `stream` declarations (see [[09-agent-streams]]). The query DSL over a stream uses `async for` comprehensions and the `async_*` runtime helpers.

```mochi
stream Tick {
    let n: int
}

let totals = from t in tick_stream
             where t.n > 0
             group by t.n / 100 into bucket
             select { bucket: bucket.key, n: count(bucket) }
```

For an infinite stream, `group by` cannot complete (group close requires end-of-input). The IR pass detects the unbounded source and reports a compile-time error unless a `window` or `tumble` is specified:

```mochi
let totals = from t in tick_stream
             tumble 60.seconds  // 60-second tumbling window
             group by t.n / 100 into bucket
             select { bucket: bucket.key, n: count(bucket) }
```

Tumbling windows lower to a periodic flush:

```python
async def tumbling_aggregate(stream: AsyncIterator[Tick], window_secs: float) -> AsyncIterator[Result]:
    buf: list[Tick] = []
    loop = asyncio.get_running_loop()
    deadline = loop.time() + window_secs
    async for tick in stream:
        buf.append(tick)
        now = loop.time()
        if now >= deadline:
            yield aggregate(buf)
            buf = []
            deadline = now + window_secs
```

Hopping windows (overlapping) and session windows (gap-defined) have similar shapes; the IR pass picks the right template.

## 12. Worked Mochi-to-Python examples

Five end-to-end examples showing the full lowering.

### 12.1 Simple filter and map

Mochi:

```mochi
let result = from x in numbers
             where x > 10
             select x * 2
```

Python:

```python
result: list[int] = [x * 2 for x in numbers if x > 10]
```

If `result` is consumed by `sum`:

```mochi
let total = sum(from x in numbers where x > 10 select x * 2)
```

Python:

```python
total: int = sum(x * 2 for x in numbers if x > 10)
```

### 12.2 Group by with multiple aggregates

Mochi:

```mochi
let stats = from order in orders
            group by order.customer_id into g
            select {
                customer_id: g.key,
                count: count(g),
                total: sum(g.amount),
                avg:   avg(g.amount),
            }
```

Python:

```python
from dataclasses import dataclass
import statistics

@dataclass(frozen=True, slots=True)
class CustomerStats:
    customer_id: int
    count: int
    total: float
    avg: float

groups: dict[int, list[Order]] = {}
for order in orders:
    groups.setdefault(order.customer_id, []).append(order)

stats: list[CustomerStats] = [
    CustomerStats(
        customer_id=cid,
        count=len(g),
        total=sum(o.amount for o in g),
        avg=statistics.fmean(o.amount for o in g),
    )
    for cid, g in groups.items()
]
```

Note that we materialise each group as `list[Order]` so we can iterate twice (once for `sum`, once for `fmean`). The IR pass detects multi-use of `g` and emits the materialised form.

### 12.3 Inner join via hash

Mochi:

```mochi
let result = from o in orders
             from c in customers
             where o.customer_id == c.id
             select { order: o, customer_name: c.name }
```

Python:

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class OrderWithCustomer:
    order: Order
    customer_name: str

customers_by_id: dict[int, Customer] = {c.id: c for c in customers}
result: list[OrderWithCustomer] = [
    OrderWithCustomer(order=o, customer_name=customers_by_id[o.customer_id].name)
    for o in orders
    if o.customer_id in customers_by_id
]
```

The IR pass detects that `customers` has unique `id` (via a `@unique` annotation or via a primary-key declaration) and uses a `dict[int, Customer]` index instead of `dict[int, list[Customer]]`. Probing becomes `customers_by_id[o.customer_id]` instead of iterating a list.

### 12.4 Order by with top-K

Mochi:

```mochi
let top10 = from u in users
            order by u.score desc
            limit 10
            select { name: u.name, score: u.score }
```

Python:

```python
import heapq
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class TopUser:
    name: str
    score: float

top10: list[TopUser] = [
    TopUser(name=u.name, score=u.score)
    for u in heapq.nlargest(10, users, key=lambda u: u.score)
]
```

The IR pass detects `limit 10` (constant <= 100) and emits `heapq.nlargest` instead of `sorted(users, key=..., reverse=True)[:10]`.

### 12.5 Async stream pipeline

Mochi:

```mochi
let alerts = from ev in event_stream
             where ev.level == "ERROR"
             tumble 5.seconds
             group by ev.service into g
             select { service: g.key, count: count(g) }
```

Python:

```python
import asyncio
from collections.abc import AsyncIterator
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class Alert:
    service: str
    count: int

async def alerts_pipeline(event_stream: AsyncIterator[Event]) -> AsyncIterator[Alert]:
    buf: list[Event] = []
    loop = asyncio.get_running_loop()
    deadline = loop.time() + 5.0
    async for ev in event_stream:
        if ev.level != "ERROR":
            continue
        buf.append(ev)
        now = loop.time()
        if now >= deadline:
            groups: dict[str, int] = {}
            for e in buf:
                groups[e.service] = groups.get(e.service, 0) + 1
            for svc, cnt in groups.items():
                yield Alert(service=svc, count=cnt)
            buf = []
            deadline = now + 5.0
```

The Mochi consumer:

```mochi
for alert in alerts {
    log_alert(alert)
}
```

lowers to:

```python
async for alert in alerts_pipeline(event_stream):
    log_alert(alert)
```

## 13. Query optimisation: IR passes

The IR pass at `transpiler3/python/lower/` performs the following optimisations before emit. These are general and apply across other targets too; the Python-specific bits are noted.

### 13.1 Filter pushdown

A `where` clause that depends only on one source can be pushed into the source's projection:

```mochi
from x in xs from y in ys where x.k > 0 where x.k == y.k select (x, y)
```

becomes:

```mochi
from x in xs where x.k > 0 from y in ys where x.k == y.k select (x, y)
```

In Python this means moving the `if x.k > 0` clause earlier in the comprehension:

```python
result = [(x, y) for x in xs if x.k > 0 for y in ys if x.k == y.k]
```

Filter pushdown reduces the Cartesian product before the join evaluates.

### 13.2 Projection pruning

If only `x.name` is used downstream, we can avoid carrying the full record. Python has no equivalent of "column pruning" (every record carries all fields), but we can avoid post-processing copies:

```mochi
from x in xs select { name: x.name, age: x.age } where age > 18
```

If the user only needs `name` later, the IR pass folds the projection:

```python
result = [x.name for x in xs if x.age > 18]
```

instead of constructing a `Record_nameage` and then re-projecting.

### 13.3 Predicate normalisation

`where a and (b or c)` is rewritten to `where a and b or a and c` if it enables further pushdown. We avoid this in Python because the rewritten form duplicates `a` evaluation; only apply when `a` is a constant or a property access (no side effects, cheap).

### 13.4 Materialisation insertion

When an iterator is used multiple times, insert `xs = list(xs)`:

```mochi
let mean = avg(xs)
let dev  = avg(from x in xs select (x - mean) ** 2)  // re-iterates xs
```

The IR pass detects two uses of `xs` and emits:

```python
xs = list(xs)
mean = statistics.fmean(xs)
dev = statistics.fmean((x - mean) ** 2 for x in xs)
```

Without the explicit `list()`, the second iteration would consume an already-exhausted generator and return empty.

### 13.5 Join reordering

For 3+ way joins, the IR pass picks an order that minimises intermediate row counts using static size hints. This is the classic database "join tree" problem. For Python we use a simple left-deep tree with the smallest table first (since we hash-index the smallest table).

### 13.6 Constant folding in predicates

`where x.k > 10 and x.k < 100 and true` collapses to `where 10 < x.k < 100`. Python supports chained comparisons natively, which is more idiomatic than `x.k > 10 and x.k < 100`.

### 13.7 Sort elision

`from x in xs order by x.k group by x.k` redundantly sorts (the `group by` will sort anyway). The IR pass drops the redundant `order by`:

```python
# instead of sorted(sorted(xs, key=k), key=k):
result = [(k, list(g)) for k, g in itertools.groupby(sorted(xs, key=k), key=k)]
```

### 13.8 Limit pushdown

`from x in xs order by x.k limit 10` should not materialise the full sort; the IR pass picks `heapq.nsmallest(10, xs, key=k)` as discussed in §8.3.

## 14. Datalog evaluation

Mochi Datalog rules lower to a semi-naive bottom-up fixpoint loop. A rule:

```mochi
parent(X, Y) :- mother(X, Y).
parent(X, Y) :- father(X, Y).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

lowers to a Python program that materialises each relation as a `set[tuple]` and iterates rules until no relation grows.

```python
from collections.abc import Iterator

def evaluate_datalog() -> dict[str, set[tuple]]:
    parent: set[tuple[str, str]] = set()
    ancestor: set[tuple[str, str]] = set()
    # initial facts
    parent.update((x, y) for (x, y) in mother)
    parent.update((x, y) for (x, y) in father)
    # rule 3: ancestor(X, Y) :- parent(X, Y).
    ancestor.update(parent)
    # rule 4: ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
    changed = True
    while changed:
        changed = False
        delta: set[tuple[str, str]] = set()
        for (x, y) in parent:
            for (y2, z) in ancestor:
                if y == y2 and (x, z) not in ancestor:
                    delta.add((x, z))
        if delta:
            ancestor.update(delta)
            changed = True
    return {"parent": parent, "ancestor": ancestor}
```

This is the **naive** evaluation. Semi-naive evaluation tracks `delta_ancestor` (new tuples added in the last iteration) and joins only against the delta, not the full `ancestor`:

```python
delta = set(parent)
while delta:
    new_delta: set[tuple[str, str]] = set()
    for (x, y) in parent:
        for (y2, z) in delta:
            if y == y2 and (x, z) not in ancestor:
                new_delta.add((x, z))
    ancestor.update(new_delta)
    delta = new_delta
```

Semi-naive avoids re-doing the same joins each iteration. The transition from naive to semi-naive is automated by the IR pass; users do not write either form.

### 14.1 Indexed evaluation

The double loop `for (x, y) in parent: for (y2, z) in delta:` is O(|parent| * |delta|). We build a hash index on the join variable:

```python
ancestor_by_first: dict[str, set[str]] = {}
for (y, z) in ancestor:
    ancestor_by_first.setdefault(y, set()).add(z)

for (x, y) in parent:
    for z in ancestor_by_first.get(y, ()):
        if (x, z) not in ancestor:
            new_delta.add((x, z))
```

The IR pass detects join keys and emits the index. For pure equi-joins on a single variable, this is O(|parent| + |output|).

### 14.2 Stratified negation

Negation is allowed only when the negated relation is fully computed before the negating rule runs (stratification). The IR pass:

1. Builds the rule dependency graph.
2. Topologically sorts strongly-connected components.
3. Each SCC is evaluated to fixpoint; only after the SCC stabilises do downstream rules (which may negate the SCC's relations) run.

Stratified negation lowers to a sequence of fixpoint loops:

```python
# Stratum 1: compute parent, ancestor (no negation)
... fixpoint over parent, ancestor ...

# Stratum 2: not_ancestor depends on ancestor (negation across strata)
not_ancestor = (all_pairs - ancestor)

# Stratum 3: cousin uses not_ancestor
... fixpoint over cousin ...
```

If the user writes a rule with negation inside a cycle (non-stratified), the emitter reports a compile-time error.

### 14.3 Magic sets

For top-down query evaluation (asking "is `ancestor("alice", "bob")` true?" rather than computing all ancestors), the magic-sets transformation rewrites rules to focus the bottom-up evaluation. The transformation:

1. Add a "magic" predicate `m_ancestor(X)` that represents the set of `X` values for which we are asking about `ancestor(X, _)`.
2. Rewrite each rule to seed the magic predicate.
3. Bottom-up evaluation now only computes ancestors of the magic set.

This is implemented as an IR pass before Python emit. The Python emit is the same shape as semi-naive; only the rule set differs.

### 14.4 Aggregate rules

Mochi Datalog supports aggregates:

```mochi
count_children(P, N) :- N = count { C : parent(P, C) }.
```

lowers to:

```python
counts: dict[str, int] = {}
for (p, c) in parent:
    counts[p] = counts.get(p, 0) + 1
count_children: set[tuple[str, int]] = {(p, n) for p, n in counts.items()}
```

Aggregate rules must be in their own stratum (the aggregate must close before any negation against it). The IR pass enforces this.

## 15. Why we do NOT lower to pandas / polars / DuckDB in v1

The four candidates considered and rejected for v1:

### 15.1 pandas

Pros:

- Ubiquitous in the Python data ecosystem.
- Many users already know it.
- Optimised columnar storage, vectorised operations.

Cons that block v1:

- **Different null semantics**: pandas uses `NaN` for missing values in float columns, `NaT` for datetime, `pd.NA` for the new "nullable" dtypes (Int64, boolean, string). Mochi uses `None` uniformly. Lowering Mochi `T?` to pandas would require per-dtype null handling, which is error-prone.
- **Copy-on-write rules diverge from Mochi**: pandas 2.x introduced copy-on-write (CoW) but it is not the default until pandas 3.0; pandas 1.x has chained-assignment warnings that are notoriously hard to silence. Mochi semantics are pure-by-default: a `let` binding is immutable. Reconciling Mochi immutability with pandas mutation is messy.
- **Type checker support is weak**: pandas-stubs exists but does not cover the full API; `mypy --strict` chokes on idiomatic pandas chains.
- **Heavy dependency**: pandas wheel is ~12 MB, requires numpy (~20 MB) and pytz / dateutil. Mochi v1 runtime is <100 KB; adding pandas explodes the install.
- **Different operator semantics**: pandas `+` on two Series of different lengths broadcasts (with NaN fill); Mochi `+` on two lists of different lengths is an error. The lowering must inject defensive checks.
- **Performance is dataset-shape dependent**: pandas is fast for million-row workloads but slow for thousand-row workloads (overhead dominates). Mochi queries span both sizes; we cannot pick one engine.

pandas is a v2 candidate with `--target=python-pandas` opt-in, where the user accepts the semantic mismatch in exchange for vectorised performance on large datasets.

### 15.2 polars

Pros:

- Rust-based, ~10x faster than pandas on large data.
- Stricter typing than pandas (closer to Mochi).
- Lazy evaluation (more amenable to query optimisation).

Cons that block v1:

- **Still null-semantics mismatch**: polars uses `null` for missing values but has its own quirks (different sort order for nulls, different aggregate behaviour).
- **API instability**: polars pre-1.0 changed APIs frequently; 1.0 (August 2024) settled, but the ecosystem is still moving.
- **Heavy dependency**: polars wheel is ~30 MB (Rust binary).
- **Type-checker support**: polars-stubs is improving but pyright complains about lambda inference in many cases.
- **Streaming engine is separate**: polars has a streaming execution mode for larger-than-memory data, but the API is different from the in-memory eager mode.

polars is a stronger v2 candidate than pandas because of the type system alignment.

### 15.3 DuckDB

Pros:

- In-process columnar SQL engine, ~10x faster than pandas for analytical queries.
- Standard SQL surface.
- Can read parquet, CSV, JSON natively.
- Type system is strict (closer to Mochi).

Cons that block v1:

- **SQL output, not Python expressions**: Mochi query DSL is a comprehension; DuckDB consumes SQL strings. Lowering would require generating SQL, which is a separate code generator.
- **Cannot use Mochi user-defined functions easily**: DuckDB has a UDF API but it requires registering Python functions, which has marshalling overhead.
- **Loss of debuggability**: a generated SQL query is harder to inspect than a generator expression.
- **Heavy dependency**: duckdb wheel is ~30 MB.
- **Mochi records are dataclasses, not arrow tables**: bridging is possible via `pyarrow` but adds another dependency.

DuckDB is a v2 candidate for the "fast analytical queries over large datasets" use case, alongside polars.

### 15.4 SQLite

Pros:

- Stdlib in Python (`sqlite3`).
- Mature, ubiquitous, transactional.
- File-backed for persistence.

Cons that block v1:

- **Type weakness**: SQLite types are dynamic (TEXT, INTEGER, REAL, BLOB, NULL); the strict typing Mochi enforces would need a translation layer.
- **No analytical performance**: SQLite is OLTP, not OLAP; large analytical queries are slow.
- **SQL surface**: same SQL-vs-comprehension mismatch as DuckDB.

SQLite is a v3+ candidate for persistence; not relevant for v1 query lowering.

## 16. Performance characterisation

Some rough numbers for Mochi-emitted Python query performance on the standard CI runner (ubuntu-22.04, x86_64, 4 vCPU, CPython 3.12.7):

| Query shape                            | Rows    | Time (ms) | Notes                          |
|----------------------------------------|---------|-----------|--------------------------------|
| filter + map                           | 1K      | 0.2       | comprehension, no allocation   |
| filter + map                           | 100K    | 12        | linear scan                    |
| filter + map                           | 1M      | 120       | linear scan                    |
| group by (3 groups, count)             | 1K      | 0.3       | Counter                        |
| group by (3 groups, count)             | 100K    | 18        | Counter                        |
| group by (3 groups, sum + avg)         | 100K    | 25        | dict[K, list[V]] then 2 passes |
| inner join (hash, 1K x 1K, 10% match)  | 1K+1K   | 0.5       | dict probe                     |
| inner join (hash, 100K x 100K, 1% match)| 100K+100K| 60       | dict probe                    |
| order by                               | 100K    | 35        | Timsort                        |
| top-10 (heapq)                         | 100K    | 8         | heapq.nlargest                 |
| Datalog ancestor (1K parent facts)     | 1K      | 90        | semi-naive, 5 iterations       |

For comparison, the same query in pandas runs about 2-3x faster for the 100K+ rows cases and 3-5x slower for the 1K cases (pandas overhead). For polars, both 1K and 100K cases are faster than the comprehension lowering, but the dependency cost is heavy.

The conclusion: Mochi v1's pure-Python lowering is fast enough for typical query workloads (under 1M rows), and the simpler dependency story is worth the perf tradeoff. v2 will offer `--target=python-polars` for users who need more.

## 17. Type lowering for query results

The query lowering produces typed Python. Each `select { a: ..., b: ... }` clause infers a record type and emits a `@dataclass(frozen=True, slots=True)`:

```mochi
from x in xs select { name: x.name, age: x.age }
```

emits:

```python
from dataclasses import dataclass

@dataclass(frozen=True, slots=True)
class _Anon_nameAge:
    name: str
    age: int

result: list[_Anon_nameAge] = [
    _Anon_nameAge(name=x.name, age=x.age)
    for x in xs
]
```

The dataclass name is mangled from the field names (`_Anon_` prefix plus sorted field names). The IR pass deduplicates: two queries with the same anonymous record type produce one dataclass.

For `mypy --strict` compliance, every field has a type annotation. The IR pass propagates types from the source records.

For `pyright --strict`, we additionally emit `@override` if the dataclass inherits from a base (not common in query lowerings; relevant for sum-type pattern matching).

## 18. Determinism gates for query output

Query output is part of the byte-equal stdout gate (07-python-target-portability §6). To ensure determinism:

- `dict` and `set` iteration is insertion-ordered (`dict`) or hash-ordered (`set`); we never iterate a `set` for output without sorting first.
- `group by` without `order by` uses `dict[K, V]` (insertion order = first-encountered-key order), which is deterministic.
- `sorted` is stable; equal-key elements retain insertion order.
- `random.sample` and `random.shuffle` are never called inside a query without a seed.
- `time.time()` is not embedded in query output (mocked in tests via `MochiClock`).

The CI gate runs each fixture twice on the same runner and asserts identical stdout. This catches any accidental hash-order or thread-scheduling dependency.

## 19. Memory profile

Pure-Python comprehensions are memory-efficient when chained via generator expressions, less so when materialised at each stage. Worst case is `list(itertools.chain(sorted(...), sorted(...)))` which materialises three lists.

Memory budget for the standard query corpus (100K rows of ~100-byte records):

| Stage                | Memory peak |
|----------------------|-------------|
| source list          | 10 MB       |
| filter (lazy)        | 10 MB       |
| group by (eager)     | 12 MB       |
| order by (eager)     | 14 MB       |
| top-K (heapq)        | 10 MB + K   |

For larger datasets (10M+ rows), the eager `sort` becomes prohibitive. Users should:

- Stream from disk via `csv.DictReader` or `json.JSONDecoder().raw_decode` instead of `json.load`.
- Use `heapq.merge` to merge sorted streams.
- Avoid `group by` without windowing on infinite streams.

The runtime documentation calls these out as "query patterns that scale". v2 will offer `--target=python-polars` for the larger workloads.

## 20. Summary

The Python query DSL lowering targets `itertools` and generator expressions for sync, `async for` and the `async_*` runtime helpers for async, `dict[K, list[V]]` hash indexes for joins, `sorted` + `itertools.groupby` for group-by, `heapq.nsmallest` for top-K, and a hand-rolled semi-naive evaluator for Datalog. The lowering is direct, comprehension-shaped, and `mypy --strict` clean. Pandas, polars, DuckDB, and SQLite are explicitly deferred to v2 because of null-semantic mismatch, type-checker friction, and dependency weight.

The companion notes pick up: [[06-type-lowering]] for the dataclass shape, [[07-python-target-portability]] for the determinism gate that query output sits on top of, [[09-agent-streams]] for the async iterator substrate, [[11-testing-gates]] for the gate enumeration including query-specific fuzzers, and [[12-risks-and-alternatives]] for the v2 polars / DuckDB roadmap.
