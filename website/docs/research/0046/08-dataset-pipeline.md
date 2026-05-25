# MEP-46 research note 08, Mochi query DSL and dataset pipeline on BEAM

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).

This note covers Mochi's LINQ-style query DSL (`from ... in ... where ... select ...`), Datalog facts and rules, group_by, joins (hash and indexed), stream-aware queries, and how each lowers to BEAM constructs.

---

## 1. The Mochi query surface

Mochi inherits a LINQ-style query DSL (see [[01-language-surface]] §3). Examples:

```mochi
let total = from p in people where p.age >= 18 select p.salary sum
let by_age = from p in people group_by p.age into g select {age: g.key, n: count(g)}
let pairs = from p in people from h in p.hobbies select {p.name, h}
let joined = from o in orders join u in users on o.user_id == u.id select {u.name, o.amount}
let top10 = from p in people order_by p.score desc take 10 select p.name
```

The shapes the surface supports:
- `from x in list` (single-source iteration)
- `from x in list from y in ...` (cross product / flatmap)
- `where pred` (filter)
- `select expr` (projection; returns list of expr)
- `group_by key into g` (binds `g` as a group with `.key` and `.values`)
- `order_by k1, k2 desc` (sort)
- `take N`, `skip N` (slicing)
- `join y in list on x.k == y.k` (inner equi-join)
- `left_join y in list on x.k == y.k` (left outer)
- `from x in stream` (subscribes to a stream and yields each event)

All of these compile to BEAM, but the lowering varies by clause.

## 2. Single-source: list comprehension

The simplest case lowers to a BEAM **list comprehension**:

```mochi
from p in people where p.age >= 18 select p.salary
```

becomes Core Erlang for:

```erlang
[maps:get(salary, V_p) || V_p <- V_people, maps:get(age, V_p) >= 18]
```

BEAM's list comprehensions are heavily optimised: the kernel pass desugars them into tail-recursive accumulator loops that the JIT can vectorise reasonably well. There is no allocation per element beyond the cons cell.

**For numeric aggregations** (`sum`, `count`, `max`), do **not** materialise the intermediate list. Use `lists:foldl/3` directly:

```mochi
from p in people where p.age >= 18 select p.salary sum
```

becomes:

```erlang
lists:foldl(fun(V_p, V_acc) ->
    case maps:get(age, V_p) >= 18 of
        true -> V_acc + maps:get(salary, V_p);
        false -> V_acc
    end
end, 0, V_people)
```

This saves the O(N) list allocation. The MEP-46 codegen has a "fusion" pass that recognises `sum`/`count`/`max`/`min` terminals and fuses them with the `select`+`where` into a single fold.

## 3. Multi-source: nested comprehensions

```mochi
from p in people from h in p.hobbies select {p.name, h}
```

becomes:

```erlang
[#{name => V_p1, hobby => V_h} ||
    V_p <- V_people,
    V_p1 <- [maps:get(name, V_p)],
    V_h <- maps:get(hobbies, V_p)]
```

BEAM comprehensions support multi-generator naturally; the nested-fold optimisation still applies via fusion.

## 4. group_by

```mochi
from p in people group_by p.age into g select {age: g.key, n: count(g)}
```

Has no native Erlang equivalent; we lower to `lists:foldl` with a `#{}` accumulator keyed on the group key:

```erlang
V_groups = lists:foldl(fun(V_p, V_acc) ->
    V_key = maps:get(age, V_p),
    maps:update_with(V_key, fun(V_old) -> [V_p | V_old] end, [V_p], V_acc)
end, #{}, V_people),
[#{age => V_k, n => length(V_vs)} || V_k := V_vs <- V_groups]
```

The final comprehension uses **`K := V <-`** generator syntax (OTP 26+) to iterate over the map.

For **large group counts** (>10000 keys), the flat-map representation degrades; consider ETS with `write_concurrency` for the group accumulator. The MEP-46 v0.2 codegen has a heuristic: if the source is annotated `@large` or the compiler can infer >10K groups, use ETS.

## 5. Joins

### 5.1 Hash join (default)

`from o in orders join u in users on o.user_id == u.id select {u.name, o.amount}` lowers to a **hash join**: build a map keyed on the right side's join key, then iterate the left side:

```erlang
V_index = maps:from_list([{maps:get(id, V_u), V_u} || V_u <- V_users]),
[#{name => maps:get(name, V_u), amount => maps:get(amount, V_o)} ||
    V_o <- V_orders,
    V_u <- [maps:get(maps:get(user_id, V_o), V_index, undefined)],
    V_u =/= undefined]
```

Hash join is O(N+M) build-and-probe. The map index allocates O(M) memory; for unique join keys, `maps:from_list/1` runs in O(M log M) due to the flat-map sort-and-merge step. For sorted right sides, `lists:foldl` building the map is faster.

### 5.2 Left join

```mochi
from o in orders left_join u in users on o.user_id == u.id select {u_name: u?.name, amount: o.amount}
```

becomes:

```erlang
V_index = maps:from_list([{maps:get(id, V_u), V_u} || V_u <- V_users]),
[#{u_name => case maps:find(maps:get(user_id, V_o), V_index) of
              {ok, V_u} -> maps:get(name, V_u);
              error -> undefined
           end,
    amount => maps:get(amount, V_o)} ||
    V_o <- V_orders]
```

The `?.` (safe navigation) propagates `undefined` (Mochi's `nil`) cleanly.

### 5.3 Indexed join

If the right side is an **ETS table** (e.g. a Datalog fact relation), we use `ets:lookup/2` instead of building a map:

```erlang
[#{u_name => maps:get(name, V_u), amount => maps:get(amount, V_o)} ||
    V_o <- V_orders,
    [{_, V_u}] <- [ets:lookup(users, maps:get(user_id, V_o))]]
```

ETS lookups are O(1) hash; this is the preferred path for any side that is already an ETS table.

## 6. Sorting

```mochi
from p in people order_by p.score desc take 10 select p.name
```

becomes:

```erlang
V_sorted = lists:sort(fun(V_a, V_b) -> maps:get(score, V_a) >= maps:get(score, V_b) end, V_people),
V_top = lists:sublist(V_sorted, 10),
[maps:get(name, V_p) || V_p <- V_top]
```

`lists:sort/2` is **merge sort** in C (`stdlib`); O(N log N). For very small N, the JIT-emitted Erlang sort is comparable to a C qsort.

For **top-K** specifically (where K << N), a heap-based approach is O(N log K) instead of O(N log N). Mochi has a `top_k` fusion when `order_by ... take K` appears together: it uses a manual bounded heap (a min-heap in a `gb_trees` of size K). The break-even is around K < log2(N), so K < ~13 for N=10000.

## 7. Datalog facts and rules

Mochi `fact` and `rule` declarations are a separate sub-DSL. Example:

```mochi
fact parent("alice", "bob")
fact parent("bob", "charlie")
rule ancestor(X, Y) :- parent(X, Y)
rule ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)
```

The OTP-canonical implementation:
- Each fact relation is an **ETS table**, `ordered_set`, public.
- Rules are compiled to a Datalog **semi-naive evaluator** (the standard bottom-up algorithm; Ullman 1989, Bancilhon & Ramakrishnan 1986).
- The evaluator is the `mochi_datalog` module; it loads rules at boot and provides `mochi_datalog:query(?MODULE, RelName, BindingsList)`.

Facts are added via `mochi_datalog:assert/3`; queries are via comprehension-style:

```mochi
let descendants = ancestor("alice", X)
```

which lowers to:

```erlang
[V_X || {_, V_X} <- mochi_datalog:query(?MODULE, ancestor, [{var, 1}, {bind, <<"alice"/utf8>>}])]
```

The semi-naive evaluator iterates until fixpoint; intermediate fact deltas live in scratch ETS tables that are discarded after each iteration.

For programs with **only ground facts** (no rules), the lowering is direct ETS scans, skipping the evaluator entirely.

## 8. Stream queries

`from x in stream` is a **subscribe** pattern, not a one-shot query. The semantics: for each new event published to the stream, run the rest of the query.

```mochi
from event in click_stream where event.user == "alice" select event.url
```

lowers to a `gen_statem` that subscribes to the stream and runs the filter/projection on each event, then re-emits to a downstream sink (a list buffer, a file, another stream, etc.):

```erlang
% mochi_stream_filter:start_link/2 spawns the processor
mochi_stream_filter:start_link(click_stream, fun(V_event) ->
    case maps:get(user, V_event) == <<"alice"/utf8>> of
        true -> {ok, maps:get(url, V_event)};
        false -> drop
    end
end)
```

The stream processor is supervised by `mochi_stream_sup` (one_for_one); a crash in the processor logs a warning and the stream continues.

For **windowed** stream queries (`from x in stream window 60s ...`), the processor maintains a sliding window in process state and emits a result every window slide.

## 9. The "fusion" optimisation pipeline

The aotir → cerl lowering pass has a small pipeline of fusion rules:

| Pattern                                       | Lowered to                          |
|-----------------------------------------------|-------------------------------------|
| `from x in L where p select e sum/count/max` | `lists:foldl(fun, init, L)`         |
| `from x in L select e`                       | `[E || X <- L]`                     |
| `from x in L where p select e`               | `[E || X <- L, P]`                  |
| `from x in L join y in R on x.k == y.k`      | hash join via `maps:from_list`      |
| `from x in L join y in <ets_rel> on ...`     | ets:lookup-based join               |
| `from x in L order_by k take K` (K small)    | `mochi_query:top_k(K, fun, L)`      |
| `from x in L order_by k`                     | `lists:sort(fun, L)`                |
| `from x in stream where p select e`          | gen_statem stream processor         |

Patterns not in this table fall back to a generic nested-fold lowering, which is correct but suboptimal.

## 10. Cardinality and memory

BEAM is not a database; iterating 10M-row lists in memory is *possible* but not advisable. Mochi `from x in L` materialises the source list in process memory; for streams, items flow through without buffering.

Guidance baked into the MEP:
- Up to 1M elements: list comprehensions are fine.
- 1M-10M elements: use ETS as the source; `from x in ets_table_name` is supported as syntactic sugar for `ets:foldl`.
- >10M elements: use streams (`from x in stream`); never materialise.

The compiler emits a **warning** (Dialyzer-style) if a `from ... in List` is on a source with no upper bound and is followed by a non-fused terminal.

## 11. Pipelining

Mochi query chains:

```mochi
let r = from p in people
    |> where p.age >= 18
    |> group_by p.city into g
    |> select {city: g.key, n: count(g)}
    |> order_by .n desc
    |> take 5
```

Each `|>` stage is a query. The codegen fuses adjacent stages where possible:
- `where` + `select` → single comprehension.
- `group_by` + `select` → single foldl.
- `order_by` + `take K` → top-K.

Non-fusable transitions (group_by → order_by) materialise the intermediate.

## 12. Concurrent queries

Each Mochi query runs on the **calling process**. To parallelise:

```mochi
let totals = par_each cities (fn c -> from p in people where p.city == c select p.salary sum)
```

`par_each` lowers to `pmap` (parallel-map using a `gen_server` worker pool, default `erlang:system_info(schedulers_online)` workers). Each work item runs the inner query in its own process.

For embarrassingly-parallel aggregations (sum, count, max), the partial results combine on the caller side. For non-associative aggregations (order_by globally), `par_each` is incorrect; the compiler rejects this case.

## 13. ETS table lifecycle

Datalog facts and `cache` declarations create ETS tables at module load time, owned by the `mochi_sup` supervisor (`heir` set so the table survives owner restart). Tables are namespaced `mochi_<module>_<rel>`.

Module unload (hot reload) does **not** delete ETS tables; reloading the same module reuses the existing table. The first load creates the table; subsequent loads check existence with `ets:info/2`.

## 14. Persistence (out of scope)

Mochi has no built-in persistence for query results or facts. Users can:
- `mnesia`: replicated, transactional. Call via FFI.
- `dets`: disk-based ETS. Slow, mostly legacy. Skip.
- `rocksdb` via `erocksdb`: third-party NIF binding. Fast, persistent.
- Plain `file:write_file/2` for dumping ETS to JSON.

The MEP-46 does not bless any of these; the FFI surface is the user's responsibility.

## 15. Benchmarks (preliminary)

Targets for v0.1 (vs vm3 baseline):
- 1M-row `from x in L where x.k > 100 select x.v sum`: BEAM target ≤ 3x slower than C target, ≤ 5x slower than vm3 (which does no codegen).
- 100K-row hash join (1:1 cardinality): BEAM ≤ 2x slower than C target.
- Datalog ancestor over 10K parent facts: BEAM ≤ 4x slower than C; within 1.5x of best-effort native (Souffle).

These are aspirational; the gates in [[11-testing-gates]] are differential against vm3 for correctness only, not performance.

---

## Sources

1. Erlang list comprehensions documentation. https://www.erlang.org/doc/system/list_comprehensions.html
2. BEAM list comprehension JIT, Lukas Larsson Code BEAM 2022.
3. `maps:from_list/1` complexity, OTP source `lib/stdlib/src/maps.erl`.
4. Ullman, "Principles of Database and Knowledge-Base Systems Vol. 1." Computer Science Press, 1989.
5. Bancilhon & Ramakrishnan, "An Amateur's Introduction to Recursive Query Processing Strategies." SIGMOD 1986.
6. Souffle Datalog. https://souffle-lang.github.io/
7. OTP 26 map generator syntax. https://www.erlang.org/news/167
8. `pg` documentation. https://www.erlang.org/doc/man/pg.html
9. `ets` documentation. https://www.erlang.org/doc/man/ets.html
