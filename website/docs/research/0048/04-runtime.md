# MEP-48 research note 04, .NET runtime building blocks

Author: research pass for MEP-48.
Date: 2026-05-23 05:51 (GMT+7).
Method: structured research over .NET Foundation release notes for .NET 8, 9, 10, the dotnet/runtime and dotnet/designs repositories, Microsoft Learn API documentation (versions net-8.0 through net-10.0), the OpenTelemetry .NET release notes through 1.15.3 (April 2026), NuGet package metadata for CsvHelper 33.1.0, YamlDotNet 17.1.0, NodaTime 3.3.2, OpenTelemetry 1.15.3, and the BenchmarkDotNet community posts on FrozenCollections and channels.

This note inventories the runtime services Mochi programs need at execution time on the **Common Language Runtime (CoreCLR)** and chooses, for each one, a Base Class Library (BCL) namespace or a vetted NuGet package to lean on. The output of this research is the **namespace layout for the `Mochi.Runtime` assembly** (see section 16 below), which is the runtime DLL that every Mochi-generated `.dll` references.

Companion notes [[01-language-surface]], [[02-design-philosophy]], and [[03-prior-art-transpilers]] establish the language surface Mochi exposes and the philosophy that drives target selection. The sibling note [[../0047/04-runtime]] does the same job for the JVM; readers comparing the two will find a thesis identical at the top level (fat BCL, thin runtime package) but with predictable shape divergence at the bottom (no Loom on CLR, no `synchronized` keyword, persistent collections by way of `System.Collections.Immutable`, etc.).

The .NET baseline for MEP-48 is **.NET 8 LTS** (released November 14 2023), with **.NET 10 LTS** (released November 11 2025, GA) as the recommended target for new deployments. .NET 9 (STS, November 2024) and .NET 11 (STS, expected November 2026) are best-effort: .NET 9 still gets security patches into 2026 because its STS support is 18 months, and .NET 11 will be the next STS during the lifetime of this spec. .NET Framework 4.8.1 is **out of scope** for code generation (it lacks `System.Threading.Channels`, `System.Text.Json` source generators, NativeAOT, file-scoped namespaces, and `Span<T>` performance work).

The thesis, identical to MEP-47 §0: **fat BCL, thin Mochi runtime package**. We do not write a scheduler, a GC, an HTTP stack, a JSON parser, a hash-map, a sort, or a regex engine. We write the glue that turns those into Mochi-shaped APIs. `Mochi.Runtime` is small on purpose.

---

## 1. CLR scheduler and threads

The CLR has **one** kind of thread: the platform thread, owned by the OS scheduler. There is **no virtual-thread equivalent**. This is the single largest mechanical divergence from MEP-47 §1, where Project Loom gives the JVM tens of millions of cheap virtual threads. On .NET, cheap concurrency is delivered by `Task` and the `async` / `await` state-machine rewrite, not by green threads.

`System.Threading.ThreadPool` is the long-standing managed worker pool. It is **work-stealing** since .NET Core 3.0 (the older Hill Climbing algorithm is still in there for thread-count adjustment but the queues are local-then-global with steal). Two queues: a global FIFO queue and per-worker local LIFO queues. Tasks scheduled with `Task.Run` go to the local queue of the current worker if there is one, falling back to the global queue. Continuations follow their antecedent.

`System.Threading.Tasks.Task` and `Task<T>` are the unit of asynchronous work. A `Task` is not a thread; it is a future with attached continuations. The default scheduler is `TaskScheduler.Default`, which submits to `ThreadPool`. `TaskScheduler.FromCurrentSynchronizationContext()` exists for UI frameworks (WPF, WinForms, MAUI, Avalonia) where continuations must run on a specific UI thread.

`async` / `await` is a compiler rewrite: the C# compiler turns an `async` method into a state machine implementing `IAsyncStateMachine`. Each `await` becomes a "suspend, schedule continuation, return" sequence. `ValueTask<T>` is the allocation-free variant for hot paths where the awaited value is usually already available (it is a discriminated union of `T`, `Task<T>`, and `IValueTaskSource<T>`); `System.Threading.Channels` and the `IAsyncEnumerable` infrastructure both use `ValueTask` internally.

`ConfigureAwait(false)` tells the awaiter not to capture the current `SynchronizationContext`, so the continuation runs on the thread pool rather than on the captured context. Library code must call it; application code in a non-UI host (ASP.NET Core, console apps) need not because there is no `SynchronizationContext` in those hosts to capture (ASP.NET Core 5+ removed its `SynchronizationContext`). Mochi-generated code targets headless services and CLI tools, so `Mochi.Runtime` calls `ConfigureAwait(false)` defensively on every internal `await`. This costs nothing and lets users embed `Mochi.Runtime` inside a UI app without deadlocking.

The `async` keyword is **viral / coloured**: a method that awaits must itself be `async`. This is real and unavoidable on .NET. The JVM does not have this problem post-Loom: a virtual thread blocks on `read()` and the runtime unmounts. On .NET, every Mochi function that touches I/O is `async Task<T>`, and the codegen must propagate that colour up every call site (see [[05-codegen-design]] §3 for the colouring algorithm).

**Cancellation.** `CancellationToken` is passed explicitly through async chains. There is no thread-interrupt equivalent for `Task`. `Mochi.Runtime` threads a `CancellationToken` through every blocking primitive; the top-level `Main` registers `Console.CancelKeyPress` (Ctrl-C) and a `PosixSignalRegistration.Create(PosixSignal.SIGTERM, ...)` to cancel it.

**Timers.** `System.Threading.PeriodicTimer` (.NET 6+) is the modern async-friendly timer: `await timer.WaitForNextTickAsync(ct)`. Used by `Mochi.Runtime.Time` for `every`. `System.Threading.Timer` is the classic callback timer; we avoid it because the callback runs on a thread-pool thread without async support.

**For Mochi.Runtime:** every Mochi `agent` is an `async Task` running on `ThreadPool`. Every Mochi `async` block lowers to `async Task<T>`. The runtime never blocks a thread-pool thread with `.Result` or `.Wait()` (these can deadlock on captured `SynchronizationContext` and waste a pool thread regardless). `Mochi.Runtime.Scope.TaskScope` is the equivalent of JVM's `StructuredTaskScope`: a using-disposable that fans out child tasks and joins them on `DisposeAsync`. The .NET BCL does not yet ship a structured-concurrency primitive (a proposal exists; nothing GA), so we provide a thin one.

Compare to [[../0047/04-runtime]] §1: the JVM trades the `async` keyword for the Loom continuation-park machinery. Mochi-on-CLR pays the cost of `async` propagation everywhere; Mochi-on-JVM does not. This is the largest single ergonomic difference at the runtime layer.

## 2. Memory model and GC

The CLR ships **two** garbage collectors, selectable per process:

- **Workstation GC** (default for desktop / CLI). Single GC thread, optimised for low pause and low memory on a single-user machine. The default unless overridden.
- **Server GC** (`<ServerGarbageCollection>true</ServerGarbageCollection>` in csproj, or `DOTNET_gcServer=1`). One heap and one GC thread per logical processor; designed for throughput on server workloads.

**Background GC** is concurrent generation-2 collection: ephemeral collections (gen 0, gen 1) still pause briefly, but the long generation-2 sweep runs on background threads. Background server GC has been the default for server mode since .NET Framework 4.5 (it uses one background thread per logical processor). Background workstation GC uses a single background thread.

**Generations.** The CLR uses a three-generation collector identical in shape to Hotspot's young / old split:

- **Gen 0**: nursery for newly-allocated short-lived objects. Collected on a tight schedule, very fast.
- **Gen 1**: objects that survived one gen-0 collection. Treated as a buffer to filter out medium-lived objects before promoting them.
- **Gen 2**: objects that survived a gen-1 collection. Collected only during full GC.

**Large Object Heap (LOH).** Objects >= 85 000 bytes go straight to the LOH, which is collected only during gen-2 GC. The LOH is not compacted by default (because moving large objects is expensive); `GCSettings.LargeObjectHeapCompactionMode = GCLargeObjectHeapCompactionMode.CompactOnce` triggers a single compaction pass. .NET 4.5.1+.

**Pinned Object Heap (POH).** Added in .NET 5. A separate heap region for objects allocated via `GC.AllocateArray<T>(length, pinned: true)`. The objects are pinned for life (no need to GCHandle-pin them), and the heap is collected like gen 2 but never compacted. The POH is used by `Span<T>` interop with native code, by `System.IO.Pipelines`, and by `SocketsHttpHandler` for socket buffers.

**Region-based GC (.NET 8+).** The GC heap was historically segment-based: 1 to 4 GB contiguous segments per heap. .NET 8 switched both Workstation and Server GC to **region-based** management: small fixed-size regions (4 MB on x64) that the GC can repurpose between generations. This was the prerequisite for **DATAS (Dynamic Adaptation To Application Size)**, which dynamically tunes the heap size to the live data set. DATAS is opt-in in .NET 8, became the default for Server GC in **.NET 9**, and remains the default in .NET 10. DATAS is what makes Server GC reasonable for containers and microservices: the heap shrinks when load drops.

**.NET 10 GC improvements.** .NET 10 sharpens write-barrier elimination (the JIT proves more assignments do not cross generational boundaries and skips the barrier), expands escape analysis so small fixed-size arrays can stack-allocate, and emits richer per-generation collection counters via `System.Diagnostics.Metrics`. See sources for Maoni Stephens' "Preparing for the .NET 10 GC" post.

**NativeAOT bundles Server GC.** A NativeAOT-published binary statically links the Server GC implementation. Workstation GC is reachable only when running on CoreCLR. For NativeAOT-published Mochi binaries (the CLI default for `mochi build --target=dotnet --aot`), Server GC is non-negotiable. This is fine: Server GC with DATAS behaves well on small workloads since DATAS shrinks the heap.

**Memory model.** The C# / CLR memory model is a sequential-consistency-for-data-race-free (SC-DRF) model since C# 11 / .NET 6, with the spec finalised in C# 12. `volatile`, `Interlocked.*`, and `Volatile.Read/Write` are the primitives. `lock` (and the new `System.Threading.Lock`, see §7) is a full fence. Mochi exposes none of this; the runtime uses immutable collections and channels and never relies on data races.

**For Mochi.Runtime:** publish profiles select GC mode:

| Workload | GC mode | DATAS | LOH | Rationale |
| --- | --- | --- | --- | --- |
| `mochi run` (CLI script) | Workstation | n/a | default | Small, predictable, fast warm-up |
| `mochi build` (JIT) | Server (DATAS) | on | default | Industry default since .NET 9 |
| Long-lived agent service | Server (DATAS) | on | compact once monthly | Sub-ms pauses for stream processing |
| NativeAOT publish | Server (DATAS) | on | default | Server is the only choice for NativeAOT |

## 3. Assemblies and the AssemblyLoadContext

The CLR's isolation unit is the **assembly** (a single `.dll` or `.exe` with a manifest). An assembly has a strong name (optional), version, public key, and culture. The runtime resolves dependencies by name through an assembly load context.

`System.Runtime.Loader.AssemblyLoadContext` (ALC) is the .NET Core replacement for `System.AppDomain`. The old AppDomain model from .NET Framework supported multiple isolated managed sub-runtimes within a process; .NET Core ships exactly one process-global AppDomain (`AppDomain.CurrentDomain`), and `AppDomain.CreateDomain` throws `PlatformNotSupportedException` since .NET 5. Multi-AppDomain isolation is gone.

ALCs are lighter: each ALC has its own assembly-name resolution scope, so two assemblies with the same name and version can coexist in the same process under different ALCs. Cross-ALC type identity is by full name + ALC, not by full name alone. The defaults:

- **`AssemblyLoadContext.Default`**: the application's main ALC. Loads everything from the runtime's probing paths.
- **Custom ALCs**: subclass `AssemblyLoadContext` and override `Load(AssemblyName)`. Useful for plugin hosts.
- **Collectible ALCs**: pass `isCollectible: true` to the base constructor. The ALC and all its assemblies can be unloaded by calling `Unload()`. Unload is **cooperative**: it succeeds only when no live references remain (including transitive references through delegates, statics, and reflection handles).

Plugin hosts (PowerShell, Aspire CLI, Visual Studio extensions) all use collectible ALCs since .NET Core 3.0. Hot-reload (`dotnet watch`) uses ALCs too.

The corresponding JVM concept is the class loader hierarchy plus JPMS modules ([[../0047/04-runtime]] §3). The mapping is rough: a JVM class loader provides assembly resolution scope just like an ALC, but JPMS modules also enforce package-level encapsulation. The CLR has no module-level encapsulation; its access modifiers are type-level (`public`, `internal`, `protected internal`, `private protected`, `file`).

`InternalsVisibleToAttribute` is the CLR's equivalent of `exports ... to ...` in JPMS: an assembly can declare another assembly may see its internals. `[ModuleInitializer]` (C# 9+, .NET 5+) is the equivalent of a static initialiser block at the assembly level; we use it in `Mochi.Runtime` to wire up telemetry on first load.

**For Mochi.Runtime:** ship as a single signed assembly `Mochi.Runtime.dll`. NuGet package id `Mochi.Runtime` (see §16). No plugin / collectible-ALC infrastructure in the runtime itself; if Mochi ever wants hot-reload of user code, we will add it as `Mochi.Runtime.Plugins` then. The Mochi user-visible API surface lives under namespaces rooted at `Mochi.Runtime.*`; internals are `internal sealed` with `[InternalsVisibleTo("Mochi.Runtime.Tests")]`.

## 4. Strings

`System.String` is **UTF-16** internally (an immutable sequence of `char`, where `char` is a UTF-16 code unit). This matches the JVM `String` shape ([[../0047/04-runtime]] §4) but **without** the JDK 9 compact-string optimisation: every .NET string is 2 bytes per code unit on the heap, even ASCII-only. (Microsoft has rejected compact strings multiple times because of `Span<char>` and `MemoryMarshal.AsBytes` compatibility breakage. Memory cost stays.)

The Mochi `string` surface is **code-point indexed** (see [[06-type-lowering]]). The mapping uses **`System.Text.Rune`** (.NET Core 3.0+), which represents a single Unicode scalar value (code point):

- `mochi_str_len(s)` -> count of `Rune`s via `s.EnumerateRunes().Count()` (O(N), no allocation thanks to the `RuneEnumerator` struct)
- `mochi_str_at(s, i)` -> walk `s.EnumerateRunes()` skipping i-1 entries
- `mochi_str_slice(s, lo, hi)` -> convert lo/hi from rune indices to UTF-16 indices then `s.Substring`
- `mochi_str_concat(a, b)` -> `string.Concat(a, b)`
- `mochi_str_split(s, sep)` -> `s.Split(sep, StringSplitOptions.None)`

`Rune.TryCreate`, `Rune.GetUnicodeCategory`, and `Rune.IsLetter` are the rune-aware classification APIs. The Mochi `char` type lowers to `Rune` (32-bit scalar) rather than to `char` (16-bit code unit) so surrogate pairs are atomic. See [[06-type-lowering]] §3.

**StringBuilder**. `System.Text.StringBuilder` is the mutable string builder. It uses a linked list of `char[]` chunks to avoid quadratic copy on append. The Mochi `string-builder` library lowers to `StringBuilder` 1-to-1. Hot paths (`mochi.list[string].Join`) use `string.Create<T>(int, T, SpanAction<char, T>)` (.NET Core 2.1+) to write directly into a pre-sized buffer with no intermediate `StringBuilder`.

**Interpolated string handlers** (C# 10, .NET 6+). Each `$"...{x}..."` invokes a custom handler if the target accepts `[InterpolatedStringHandlerArgument]`. The BCL uses this for `string.Create`, logging (`LoggerMessage`), and `ArgumentNullException.ThrowIfNull`. Mochi codegen uses interpolated strings for `print` and `assert`, but defers to `string.Concat` to keep the assembly small (interpolated-handler types add ~200 bytes per arity).

## 5. Collections

The BCL ships a deep collections story; we mostly forward Mochi types into it.

| Mochi type | .NET type | Namespace | Notes |
| --- | --- | --- | --- |
| `list<T>` | `List<T>` | `System.Collections.Generic` | Resizable array, O(1) amortised append |
| `map<K, V>` (insertion-ordered) | `OrderedDictionary<TKey, TValue>` | `System.Collections.Generic` | **New in .NET 9.** See below |
| `set<T>` | `HashSet<T>` | `System.Collections.Generic` | Insertion order NOT preserved |
| `sorted-map<K, V>` | `SortedDictionary<TKey, TValue>` | `System.Collections.Generic` | Red-black tree, O(log N) ops |
| `sorted-set<T>` | `SortedSet<T>` | `System.Collections.Generic` | Red-black tree |
| `queue<T>` | `Queue<T>` | `System.Collections.Generic` | Circular array |
| `deque<T>` | `LinkedList<T>` | `System.Collections.Generic` | Doubly-linked, O(N) memory overhead |
| `concurrent-map<K, V>` | `ConcurrentDictionary<TKey, TValue>` | `System.Collections.Concurrent` | Lock-striped |
| `tuple-array<T>` (read-only) | `ImmutableArray<T>` | `System.Collections.Immutable` | Single-array, no structural sharing |
| `persistent-list<T>` | `ImmutableList<T>` | `System.Collections.Immutable` | AVL tree, log-N updates |
| `persistent-map<K, V>` | `ImmutableDictionary<K, V>` | `System.Collections.Immutable` | HAMT (hash array mapped trie), 32-way |
| `persistent-set<T>` | `ImmutableHashSet<T>` | `System.Collections.Immutable` | HAMT |
| `frozen-map<K, V>` (lookup-tuned) | `FrozenDictionary<TKey, TValue>` | `System.Collections.Frozen` | .NET 8+ |
| `frozen-set<T>` (lookup-tuned) | `FrozenSet<T>` | `System.Collections.Frozen` | .NET 8+ |

### Insertion order divergence

This is the single largest **semantic** divergence from MEP-47 §5 (where `LinkedHashMap` is the JVM workhorse): `System.Collections.Generic.Dictionary<TKey, TValue>` **does not guarantee enumeration order**. It happens to preserve insertion order in current implementations as an undocumented side-effect of its open-addressing scheme, but Microsoft has explicitly reserved the right to break this.

Mochi semantics require insertion order on the `map` type (see [[01-language-surface]] §5). The mitigation is the new `System.Collections.Generic.OrderedDictionary<TKey, TValue>`, shipped in **.NET 9** (November 2024), namespace `System.Collections.Generic` (not the older non-generic `System.Collections.Specialized.OrderedDictionary`). The new generic type implements `IDictionary<TKey, TValue>`, `IList<KeyValuePair<TKey, TValue>>`, and `IReadOnlyList<KeyValuePair<TKey, TValue>>`. Its complexity profile:

- Lookup by key: O(1) amortised (hash table backing)
- Lookup by index: O(1) (parallel array backing)
- Insert: O(1) amortised
- Remove by key: O(N) (must shift the index array)
- Remove by index: O(N)

For Mochi this is acceptable: removal is rare and append-then-lookup is the hot path. On .NET 8 (where `OrderedDictionary<K, V>` does not exist) we ship a polyfill in `Mochi.Runtime.Collections.OrderedDict<K, V>` that uses the same dictionary + list pair internally. The polyfill is a `#if !NET9_0_OR_GREATER` shim that simply delegates to the BCL type on .NET 9+. See [[06-type-lowering]] §5 for the codegen rule.

### Frozen vs immutable

The `System.Collections.Immutable` types (since .NET Standard 1.0, shipped as `System.Collections.Immutable` NuGet, in-box from .NET 5) are **persistent** data structures with structural sharing: `add` returns a new collection sharing most of its tree with the old one. `ImmutableDictionary` is HAMT-based; lookup is O(log_32 N).

`System.Collections.Frozen` (.NET 8+) gives up persistence in exchange for **lookup speed**: `FrozenDictionary<K, V>` is built once via `ToFrozenDictionary()` (expensive, ~10x the cost of a `Dictionary` insert per key) and supports only reads thereafter. The builder analyses the keys and picks one of several specialised implementations: small dictionaries (<=8) use linear search over a sorted array, integer-keyed dictionaries use a perfect hash, string-keyed dictionaries pick the shortest discriminating substring of each key for hashing. Benchmarks show `FrozenDictionary` is 40% to 70% faster than `Dictionary` for lookup at the cost of a much higher build time.

For Mochi: `frozen-map` and `frozen-set` are the right choice for static lookup tables (e.g. compiled regex character classes, keyword sets). The Mochi compiler emits `FrozenDictionary` for **compile-time-known** maps with all-literal keys; runtime-built maps stay `OrderedDictionary` (for ordering) or `Dictionary` (when order does not matter).

### LINQ-aware collection methods

Every BCL collection above implements `IEnumerable<T>`, so all the LINQ operators in §6 apply. `List<T>.AsSpan()` (and the matching `CollectionsMarshal.AsSpan(List<T>)`) exposes the underlying array as a `Span<T>` for zero-copy iteration; we use it in the Datalog engine (§15) for tuple scans.

## 6. LINQ

`System.Linq.Enumerable` is the static class of extension methods that defines LINQ to objects: `Select`, `Where`, `GroupBy`, `OrderBy`, `Join`, `Aggregate`, `ToList`, `ToArray`, `ToDictionary`, `ToHashSet`, `ToFrozenDictionary`, `ToFrozenSet`, `Sum`, `Min`, `Max`, `Count`, `Any`, `All`, etc. Roughly 90 methods.

LINQ is **deferred-evaluation by default**: `Select` and `Where` return an iterator wrapper. The work happens when you enumerate (`foreach`, `ToList`, `Count`). This is the same model as Java Streams and Rust iterators.

**Method syntax vs query syntax.** C# supports both. Query syntax is the LINQ keywords (`from ... in ... where ... select ...`); method syntax is the chain of extension calls. The current style guide (Microsoft's own .NET source) is method syntax for everything except multi-from joins, where query syntax is more readable. Mochi codegen emits method syntax exclusively.

**PLINQ** (`System.Linq.ParallelEnumerable`) is the parallel evaluator: `.AsParallel().WithDegreeOfParallelism(n).Select(...).ToArray()` partitions the source across worker threads. PLINQ pays a real startup cost; below ~10 ms of work per element it loses to sequential LINQ. Mochi `parallel-for` lowers to PLINQ when the body is pure (no I/O); otherwise to `Task.WhenAll` of `Task.Run` invocations.

**Async LINQ.** `System.Linq.Async` (NuGet package `System.Linq.Async` 6.0.1+ as of 2026; **not** BCL but tightly integrated) provides `IAsyncEnumerable<T>` extensions: `SelectAsync`, `WhereAsync`, `Concat`, `Take`. The BCL ships `IAsyncEnumerable<T>` (from `System.Collections.Generic`) since .NET Core 3.0 but only the iterator interface, not the LINQ operators. Microsoft confirmed the LINQ-on-async-enumerable work would land in .NET 10; the `System.Linq.AsyncEnumerable` namespace exists in .NET 10 but coverage is partial (Select, Where, Aggregate, Count, FirstOrDefault, ToList). Mochi's `Mochi.Runtime.Streams` package uses `System.Linq.AsyncEnumerable` on .NET 10 and falls back to the `System.Linq.Async` NuGet on .NET 8 / 9.

LINQ provider extensibility lives in `IQueryable<T>` (`System.Linq.Expressions`). EF Core and LINQ-to-XML use it. Mochi's `query` surface compiles directly to LINQ-to-objects without an `IQueryable` round-trip; for SQL targets we generate parameterised SQL strings (see [[08-dataset-pipeline]]). Reflection on expression trees is a NativeAOT hazard (see §13).

## 7. Concurrency primitives

The BCL ships a rich concurrency toolkit. We pick a subset:

- **`System.Threading.Channels`** (.NET Core 3.0+, BCL since .NET 5). `Channel<T>` is the async-aware producer-consumer queue. `Channel.CreateBounded<T>(BoundedChannelOptions)` and `Channel.CreateUnbounded<T>(UnboundedChannelOptions)`. Bounded channels expose four full-modes: `Wait` (producer awaits), `DropOldest`, `DropNewest`, `DropWrite`. Unbounded channels never block writers. The implementation uses `ValueTask` returned via `IValueTaskSource` for zero-allocation reads when items are available; under the hood a `TaskCompletionSource` is used as the "completion sentinel" task. This is the .NET equivalent of Go channels and JVM `BlockingQueue`. Mochi `chan T` lowers to `Channel<T>`.
- **`TaskCompletionSource<T>`** is the primitive for handing out a `Task<T>` and resolving it from outside. Most async glue uses it under the hood. We rarely expose it directly; it backs the Mochi `Promise<T>` type and the `Mochi.Runtime.Func.Fn` adapters when an interop boundary needs an explicit handle.
- **`CancellationTokenSource`** + **`CancellationToken`**. The CT is the only cooperative cancellation primitive. Every Mochi async function takes a `CancellationToken` (default: `CancellationToken.None`) and forwards it; the runtime's `Mochi.Runtime.Context.RunCtx` holds the root CTS.
- **`SemaphoreSlim`** is the async-friendly counting semaphore. `WaitAsync(CancellationToken)` is the cancellable acquire. The Mochi `bounded-pool` primitive uses it.
- **`ReaderWriterLockSlim`** is the slim reader-writer lock for shared in-memory state. Used by `Mochi.Runtime.Datalog` for table-level locks during bulk loads.
- **`ConcurrentQueue<T>`**, **`ConcurrentStack<T>`**, **`ConcurrentBag<T>`**, **`ConcurrentDictionary<K, V>`** are the lock-free thread-safe collections (the queue/stack use Treiber's linked-list algorithm; the dictionary is lock-striped).
- **`lock` keyword + `Monitor`** is the legacy locking primitive. `lock (obj) { ... }` is sugar for `Monitor.Enter` / `Monitor.Exit` with `try`/`finally`. Object identity is the lock.
- **`System.Threading.Lock`** (.NET 9+) is a **dedicated lock type**: declaring `private readonly System.Threading.Lock _lock = new();` and using `lock (_lock) { ... }` causes the C# 13 compiler to emit calls to `Lock.EnterScope()` / `Lock.Scope.Dispose()` instead of `Monitor.Enter/Exit`. Faster (no header-word lock-bit dance), no risk of accidentally locking a shared boxed value type, and the compiler warns when you cast a `Lock` to `object` and lock it (the cast falls back to `Monitor` silently). Mochi.Runtime targets `System.Threading.Lock` on .NET 9+ and `Monitor`-via-`object` on .NET 8.

**No await inside a lock.** Both `Monitor` and `System.Threading.Lock` are owned by the calling thread, not by the calling task. An `await` inside a `lock` block can resume on a different thread, leaving the lock held by the original thread and the unlock call running on a thread that does not own it. The runtime's lock-using paths therefore never await inside a lock. We use `SemaphoreSlim` when async-safe mutual exclusion is needed.

Compare to MEP-47 §7: Loom solves "no await inside a lock" by simply parking the virtual thread on lock contention (and on JDK 24+ even `synchronized` no longer pins). The CLR has no such fix in flight; `async` and locks remain partitioned.

## 8. HTTP

`System.Net.Http.HttpClient` is the canonical HTTP client. Since .NET Core 2.1, the default underlying handler is **`SocketsHttpHandler`**, a fully-managed implementation that replaced the platform-specific WinHttp/CFNetwork-backed `HttpClientHandler`. `SocketsHttpHandler` exposes:

- `ConnectTimeout`, `PooledConnectionLifetime`, `PooledConnectionIdleTimeout` for connection pool tuning
- `EnableMultipleHttp2Connections` (defaults to false; multiple HTTP/2 connections to the same origin are RFC-disallowed and cause some servers to reject)
- `KeepAlivePingDelay`, `KeepAlivePingTimeout`, `KeepAlivePingPolicy` for HTTP/2 keep-alive
- `Http3Enabled = true` enables HTTP/3 client support (since .NET 6; opt-in even in .NET 10)

**HTTP/3** is supported via the `System.Net.Quic` library (using msquic on Windows / Linux, OpenSSL or Schannel for TLS), shipped in-box since .NET 7 GA. The client still needs `Http3Enabled = true` because msquic distribution is not guaranteed on every platform. ASP.NET Core Kestrel enables HTTP/3 with `listenOptions.Protocols = HttpProtocols.Http1AndHttp2AndHttp3`.

**`IHttpClientFactory`** (`Microsoft.Extensions.Http` NuGet) is the recommended client-factory pattern: it gives you named or typed clients, handles `PooledConnectionLifetime` rotation, and integrates with DI containers. The .NET 10 default `IHttpClientFactory` primary handler is `SocketsHttpHandler` (the breaking change from `HttpClientHandler` landed in .NET 9). Mochi.Runtime.IO uses `HttpClient` directly with a configured `SocketsHttpHandler`; we deliberately do not require Microsoft.Extensions.* (see §17).

**WebSocket**. `System.Net.WebSockets.ClientWebSocket` is the client; `WebSocketAcceptContext` on `HttpListener` is the server. ASP.NET Core has its own WebSocket middleware; we do not depend on it. The Mochi `socket` primitive uses `ClientWebSocket`.

**Server.** We do **not** ship an HTTP server in `Mochi.Runtime`. If Mochi programs need to host an HTTP endpoint, they pull in `Mochi.Server.Kestrel` separately (planned, not in scope for MEP-48 §0 baseline). Headless Mochi services should be enough for the first release.

## 9. JSON

`System.Text.Json` (BCL since .NET Core 3.0, full feature parity with `Newtonsoft.Json` since .NET 7) is the in-box JSON library. The serialiser is `JsonSerializer`; the streaming reader is `Utf8JsonReader`; the writer is `Utf8JsonWriter`. UTF-8 first: reading from a `ReadOnlySpan<byte>` is the zero-copy path; reading from a `string` (UTF-16) costs a transcode.

`JsonSerializer.Serialize` / `JsonSerializer.Deserialize` are the entry points. Reflection-based serialisation is fast (faster than Newtonsoft 10.x by ~2x in BenchmarkDotNet runs) but **forbidden under NativeAOT**: the trimmer cannot prove which types serialise to which JSON shape and removes unused properties.

**Source generators** (`[JsonSerializable(typeof(MyType))]` on a `JsonSerializerContext` subclass) are the NativeAOT-compatible path. The Roslyn source generator emits a `JsonTypeInfo<T>` at compile time with no reflection at runtime. The .NET 10 ASP.NET Core Web API (Native AOT) template enables source generators by default; Mochi codegen does the same for every Mochi record type that is `json-serializable`.

Two source-generation modes: **metadata mode** (default; emits type-info structures and uses them at runtime) and **serialization-optimization mode** (emits a fast-path direct `Utf8JsonWriter.WriteXxx` sequence). Fast-path only covers serialisation, not deserialisation. Mochi emits metadata mode; the runtime cost is comparable and the source surface is simpler.

**Newtonsoft.Json** (Json.NET, NuGet `Newtonsoft.Json` 13.0.4 as of 2026) is still widely deployed because of its `JsonPath` support, custom-converter ecosystem, and `LINQ-to-JSON` (`JObject`, `JArray`, `JToken`). Mochi.Runtime does not depend on it. Users who need JSONPath can install `Newtonsoft.Json` separately and convert; we will not pull a 700 KB legacy library into every Mochi assembly.

The Mochi `json` surface (decode / encode / path / patch) lowers to:

- `decode(s) -> dynamic` -> `JsonDocument.Parse(s).RootElement` wrapped in our `Mochi.Runtime.Json.JsonValue` (a discriminated union over Object / Array / String / Number / Bool / Null)
- `encode(v) -> string` -> `JsonSerializer.Serialize(v, JsonValueContext.Default.JsonValue)`
- `path(v, expr)` -> ad-hoc walker over `JsonValue` (no JSONPath dependency)

## 10. CSV and YAML

Neither is in the BCL. Both have stable, MIT-licensed, widely-used NuGet packages:

- **CsvHelper** (`CsvHelper` NuGet, version **33.1.0** as of April 2026). Streaming reader (`CsvReader`) and writer (`CsvWriter`) on top of `TextReader` / `TextWriter`. Auto-mapping from CSV headers to POCO properties via reflection; explicit `ClassMap<T>` types for control. Dual-licensed MS-PL / Apache 2.0. Targets .NET Standard 2.0 so it works on every reasonable target. The Mochi `csv` surface uses `CsvReader.GetRecords<T>()` for typed reads and `CsvParser` for header-only reads.
- **YamlDotNet** (`YamlDotNet` NuGet, version **17.1.0** as of April 2026). MIT-licensed, ~5 MB ecosystem footprint with no transitive deps. Provides a low-level parser (`Parser`/`Scanner`/`Emitter`) and a high-level object model. The `Deserializer` and `Serializer` builder types are the modern entry points; both accept naming conventions and converter chains. **YamlDotNet source generator** (added in 16.0) generates serializer/deserializer code at compile time for NativeAOT compatibility. The Mochi `yaml` surface uses `Deserializer.Deserialize<T>` with the source generator enabled for AOT publishes.

NativeAOT compatibility is the deciding factor here. CsvHelper currently uses reflection (a source-generator effort is open at <https://github.com/JoshClose/CsvHelper/issues/2080> as of 2026); for AOT, our Mochi-to-CSV codegen emits hand-written column readers that bypass the auto-mapper. YamlDotNet's source generator removes the reflection dependency entirely.

## 11. Time

The BCL ships a layered time story:

- **`DateTime`**: the historical type. 100-nanosecond ticks since year 1, with a `Kind` enum (Unspecified / Utc / Local). Avoid for new code: the `Kind` is easy to lose.
- **`DateTimeOffset`**: instant + UTC offset. The right type for wall-clock + timezone-naive use cases.
- **`TimeSpan`**: signed duration.
- **`DateOnly`** (.NET 6+): calendar date, no time-of-day.
- **`TimeOnly`** (.NET 6+): time of day, no date.
- **`TimeZoneInfo`**: IANA tzdb (since .NET 6, the BCL embeds tzdata 2024a+ on Windows and reads `/usr/share/zoneinfo` on Linux/macOS).
- **`TimeProvider`** (.NET 8+, abstract class): testability abstraction over the clock. `TimeProvider.System` is the default; `Microsoft.Extensions.TimeProvider.Testing.FakeTimeProvider` is the test double. Returns `DateTimeOffset` from `GetUtcNow()` / `GetLocalNow()`.
- **`Stopwatch`** (`System.Diagnostics.Stopwatch`): high-resolution monotonic timer for benchmark-style timing.

**NodaTime** (`NodaTime` NuGet, version **3.3.2** as of April 2026, with TZDB 2026a built in) is the gold-standard external library for strict time modelling: `Instant`, `LocalDateTime`, `ZonedDateTime`, `Duration`, `Period`. Created by Jon Skeet. New in 3.3: `NodaTime.HighPerformance.Duration64` / `Instant64` for high-throughput scenarios where the standard 12-byte `Duration` is too large.

`Mochi.Runtime.Time` design:

- Mochi `time` = `DateTimeOffset` (always UTC; the offset is informational)
- Mochi `duration` = `TimeSpan`
- Mochi `date` = `DateOnly`
- Mochi `time-of-day` = `TimeOnly`
- Mochi `clock` injected via `TimeProvider` (so test code can swap to `FakeTimeProvider`)
- Mochi `tz-aware-time` (when the user opts in) = NodaTime `ZonedDateTime`, pulled in lazily as `Mochi.Runtime.Time.NodaInterop`, optional NuGet dependency

This split mirrors MEP-47 §11: built-in for the common case, NodaTime (CLR) / java.time (JVM) for the strict case. Java.time is in the JVM BCL; NodaTime is not in the CLR BCL.

## 12. Random / PRNG

`System.Random` is the BCL deterministic PRNG. Constructor takes a `int seed`; `Random.Shared` (.NET 6+) is a thread-safe singleton seeded from `Environment.TickCount + Thread.ManagedThreadId`. Algorithm: xoshiro256** since .NET 6 (Microsoft swapped from the legacy Donald Knuth subtractive generator). 

`System.Security.Cryptography.RandomNumberGenerator` is the cryptographically-secure path. `RandomNumberGenerator.Fill(Span<byte>)` and `RandomNumberGenerator.GetInt32(int)` are the static entry points (.NET 6+). Backed by OS CSPRNG (`BCryptGenRandom` on Windows, `getrandom` on Linux, `arc4random` on macOS / BSD).

Mochi `random` lowers to `Random.Shared` for the seeded / fast case and to `RandomNumberGenerator` for the crypto case. Determinism in tests is achieved by injecting a seeded `Random` into `Mochi.Runtime.Context.RunCtx`.

## 13. Reflection

`System.Reflection` is the runtime introspection surface: `Type`, `MethodInfo`, `PropertyInfo`, `FieldInfo`, `Assembly`. Used for serialisation (when not source-generated), DI containers, mocking frameworks, and ORM mapping.

Reflection is **restricted under NativeAOT**. The trimmer (`IL2026`, `IL2070`, `IL3050` warnings) cannot statically prove which types are reachable through `Type.GetType(string)` or `Activator.CreateInstance(Type)`, so it removes types it thinks are unused, and reflection then fails at runtime with `MissingMethodException`.

Two escape hatches:

- `[DynamicallyAccessedMembers(DynamicallyAccessedMemberTypes.All)]` on a `Type` parameter or property tells the trimmer "preserve these members of whatever ends up here at runtime"
- `[DynamicDependency("Method.*", typeof(SomeType))]` on a method tells the trimmer "if this method is reached, also preserve members matching this pattern"

Neither is a substitute for not using reflection. The recommended replacement for the four big use-cases is:

1. Serialisation -> source generators (System.Text.Json, MessagePack-CSharp, YamlDotNet)
2. DI containers -> source-generated DI (Microsoft.Extensions.DependencyInjection has experimental compile-time mode; AutoFac and Lamar are reflection-only)
3. Mocking -> source-generated mocks (NSubstitute analyzers; Moq is reflection-only)
4. ORM mapping -> source generators (EF Core 8+ Compiled Models)

For Mochi, the codegen avoids reflection in the runtime hot path entirely. The two places we use it:

- `Mochi.Runtime.FFI.Dispatch`: when a user `import`s a .NET assembly and calls a method by name. Marked `[RequiresUnreferencedCode]` and `[RequiresDynamicCode]`; the warning surfaces at the Mochi import statement so the user knows that import is not AOT-safe.
- `Mochi.Runtime.AI.ToolReflection`: when an AI tool is described by a Mochi function and we need to enumerate its parameters for the schema JSON. The codegen has a fallback path that emits the schema directly at compile time; reflection is only used as a last resort.

## 14. Telemetry

The CLR ships a tiered telemetry story:

- **`System.Diagnostics.Activity`** (.NET 5+): the W3C trace-context unit. An `Activity` has a `TraceId`, `SpanId`, `ParentSpanId`, `OperationName`, `Tags`, `Events`, and `Links`. `ActivitySource` is the trace producer; `ActivityListener` is the consumer. This is the .NET equivalent of OpenTelemetry's `Tracer` / `Span` API and is directly OpenTelemetry-compatible.
- **`System.Diagnostics.Metrics`** (.NET 6+): the metrics API. `Meter` is the producer; `Counter<T>`, `Histogram<T>`, `ObservableGauge<T>` are the instrument types. `MeterListener` is the consumer. Directly OpenTelemetry-compatible.
- **`System.Diagnostics.Tracing.EventSource`**: the CLR-native event-tracing API. Maps to ETW on Windows, LTTng on Linux, USDT on macOS (the `dtrace`-compatible probe). Roughly comparable to JVM's JFR but lower-level: events are flat structs, no built-in hierarchy. Runtime-internal events (GC, JIT, exception) are all emitted via EventSource.
- **`EventCounter`** (legacy): the .NET Core 3.0 metrics primitive. Superseded by `Metrics` in .NET 6+; we do not use it.

**OpenTelemetry .NET** (`OpenTelemetry` NuGet, version **1.15.3** as of April 2026). The reference OpenTelemetry implementation. Provides `Sdk.CreateTracerProviderBuilder()`, `Sdk.CreateMeterProviderBuilder()`, and OTLP exporters. Subscribes to `ActivitySource` and `Meter` events and bridges them to OTLP. Apache 2.0. The package is stable across all three signals (logs, metrics, traces).

`OpenTelemetry.Exporter.OpenTelemetryProtocol` 1.15.3 is the OTLP exporter. `OpenTelemetry.Extensions.Hosting` 1.15.3 wires it up for `IHostBuilder`-based apps. Mochi.Runtime depends only on `OpenTelemetry` (the core API); the exporter is brought in by the application or by `Mochi.Cli`'s startup configuration.

`Mochi.Runtime.Telemetry` design:

- A single `ActivitySource` per Mochi assembly, named `"Mochi.<AssemblyName>"`. The codegen emits one `internal static readonly ActivitySource Source = new("Mochi.<AssemblyName>", "<version>");` per assembly.
- Mochi `trace` blocks lower to `using var activity = Source.StartActivity("name");` then `activity?.SetTag(...)`.
- Mochi `meter` declarations lower to `Counter<long>` / `Histogram<double>` fields on a per-assembly `Meter`.
- `EventSource` is reserved for **runtime-internal** events (Mochi.Runtime.Datalog scan complete, Mochi.Runtime.AI provider invoked, etc.).

Compare to MEP-47 §14: JVM's JFR is a closer analog to EventSource than to Activity/Metrics. The Mochi runtime mirrors the API shape: trace + metrics for user code, low-level events for runtime internals.

## 15. Datalog tables

Mochi's `query` surface includes Datalog-style fact tables (see [[01-language-surface]] §9). The runtime needs an in-memory tuple-store with index maintenance and a query planner.

`Mochi.Runtime.Datalog` design (mirrors MEP-47 §15):

- `Relation<T0, T1, ..., Tn>` is a small in-memory table parameterised on the tuple type. Backed by `List<(T0, T1, ..., Tn)>` plus zero or more secondary indices (`Dictionary<TKey, List<int>>` of row positions).
- `IndexedRelation<...>` adds compile-time index declarations: the codegen looks at the query's `where` clauses and decides which columns to index.
- The query planner is hand-written: it picks join orders based on relation cardinalities and indexes. Joins are hash joins for equality predicates, nested loops for inequality predicates. No magic-set transformation in v1; if a query needs recursion we fall back to semi-naive evaluation (compute the relation as a least fixed point).
- Persistence: backed by `ImmutableDictionary<TKey, ImmutableList<TValue>>` for the in-memory representation. Read-mostly; we may switch to `FrozenDictionary` for compile-time-known relations in a follow-up.

This is intentionally small. We are not building a production Datalog engine (Soufflé, DuckDB-Datalog, Differential Dataflow); we are building enough engine to make Mochi's `query` block executable. The user-facing API surface is the LINQ method-syntax chain plus the `for ... in ... where` syntactic sugar; the engine is hidden.

## 16. Module layout for `Mochi.Runtime`

Ship as a **single signed NuGet package** `Mochi.Runtime` with a single assembly `Mochi.Runtime.dll`. Apache-2.0 license. Versions track Mochi releases (so Mochi 0.6.0 ships `Mochi.Runtime` 0.6.0). Target framework: `net8.0;net9.0;net10.0`. Multi-target so the same package works on .NET 8 (LTS), .NET 9 (STS), and .NET 10 (LTS). The .NET 8 build polyfills `OrderedDictionary<K, V>` and `System.Threading.Lock`; the .NET 9+ builds delegate to the BCL types.

Namespace tree:

```
Mochi.Runtime
  .Agents                  // agent base class, supervision tree
  .AI                      // LLM dispatch, embedding helpers, tool reflection
  .Collections             // OrderedDict polyfill, Persistent*, FrozenDict helpers
  .Context                 // RunCtx (CancellationToken, TimeProvider, env)
  .Datalog                 // Relation<T..>, query planner
  .Errors                  // MochiError hierarchy, Result<T, E>
  .FFI                     // .NET interop dispatch (reflection-marked unsafe)
  .Func                    // Fn0..Fn9 wrappers; bridges to Func<>/Action<>
  .IO                      // file, http, websocket, stdin/stdout helpers
  .Json                    // JsonValue discriminated union, codec adapters
  .Query                   // HashJoin, NestedLoopJoin, group-by, etc
  .Scope                   // TaskScope (structured concurrency)
  .Streams                 // IAsyncEnumerable helpers, Channel<T> bridges
  .Strings                 // Rune-indexed string helpers
  .Telemetry               // ActivitySource / Meter wrappers
  .Time                    // TimeProvider wrappers, NodaTime interop
```

`Mochi.Runtime.Func` is mostly a no-op on .NET because the BCL already ships `Func<>` (T1 to T16) and `Action<>` (T1 to T16). We expose `Fn0..Fn9` only as **named delegate types** that the Mochi codegen can use as nominal types in generated code without conflicting with arbitrary user-supplied `Func<>` instances. (On JVM these are essential because there is no built-in `Function<...>` arity beyond 1 to 2; on CLR they are mostly cosmetic.)

Internal namespaces (`Mochi.Runtime.Internal.*`) are `internal sealed` with `[InternalsVisibleTo("Mochi.Runtime.Tests")]` and `[InternalsVisibleTo("Mochi.Cli")]`. The user-facing surface is what the `public` types expose.

NuGet metadata:

```xml
<PackageId>Mochi.Runtime</PackageId>
<Authors>Mochilang contributors</Authors>
<License>Apache-2.0</License>
<Description>Runtime support library for Mochi-to-.NET compiled output.</Description>
<RepositoryUrl>https://github.com/mochilang/mochi</RepositoryUrl>
<PackageTags>mochi;runtime;codegen</PackageTags>
<IsAotCompatible>true</IsAotCompatible>
```

`IsAotCompatible=true` is the .NET 8+ flag that enables trim/AOT analyzers at build time. We accept some warnings in `Mochi.Runtime.FFI` (which uses reflection by design) and gate them behind `[RequiresDynamicCode]` so consumer assemblies inherit the warning.

## 17. What we explicitly do NOT need

- **No actor framework.** Akka.NET, Orleans, Proto.Actor are mature actor systems on .NET. Mochi has its own `agent` semantics, and the `Mochi.Runtime.Agents` implementation is a couple hundred lines on top of `Channel<T>` + `Task`. Acknowledged and deferred: if a Mochi user needs distributed actors with cluster sharding and consensus they should reach for Orleans directly; we will not embed it.
- **No ASP.NET Core dependency.** ASP.NET Core is a ~10 MB transitive dependency tree. Mochi's runtime is meant to ship inside CLI tools, batch jobs, agents, and small services where the consumer brings their own HTTP server if needed. If a Mochi user wants to serve HTTP they pull in `Microsoft.AspNetCore.App` (the framework reference) themselves.
- **No EF Core dependency.** EF Core is the Microsoft ORM. Mochi's `query` surface lowers to LINQ-to-objects or to direct ADO.NET; the typed-query story is in [[08-dataset-pipeline]] and is hand-written. EF Core also has poor AOT compatibility (the compiled-models source generator exists but is partial).
- **No Microsoft.Extensions.* meta-package.** Microsoft.Extensions.* gives you DI, logging, configuration, hosting, options. These are excellent for ASP.NET-style apps but viral: they pull in a dozen NuGet packages, and the `IHost` lifecycle is opinionated. Mochi.Runtime depends only on:
  - The runtime itself (no NuGet needed; BCL)
  - `OpenTelemetry` (~ 700 KB on disk; observability is non-negotiable)
  - `YamlDotNet` (optional, only when user code uses `import "yaml"`)
  - `CsvHelper` (optional, only when user code uses `import "csv"`)
  - `NodaTime` (optional, only when user code uses `import "time/zoned"`)

That is the entire dependency closure on a hello-world Mochi program: just the BCL, plus OpenTelemetry. NativeAOT-published Mochi binaries see ~4 MB on disk (see §19).

## 18. Limitations

- **NativeAOT trimming pitfalls.** Reflection-heavy code paths must be marked `[RequiresUnreferencedCode]` so the trimmer can warn at the call site. We have audited every `Type.GetType` and `Activator.CreateInstance` call in `Mochi.Runtime` and either marked it or replaced it. Third-party packages (`Newtonsoft.Json`, EF Core) are not AOT-clean in 2026; we do not depend on them. `YamlDotNet` is AOT-clean via its source generator; `CsvHelper` is partially AOT-clean (the typed-record API uses reflection).
- **async coloring is real.** Every Mochi function that touches I/O becomes `async Task<T>`. This costs one allocation per call (the state machine) unless the value is already available, in which case `ValueTask<T>` cuts the allocation. The codegen prefers `ValueTask<T>` on hot paths (see [[05-codegen-design]] §3.2).
- **No virtual threads.** A Mochi program with 100 000 concurrent agents on the JVM (post-Loom) uses ~100 000 virtual threads at minimal cost. The same program on .NET uses ~100 000 `Task`s on a thread pool of ~8 to 16 threads. The pool can starve if agents do blocking work (sync I/O, lock contention); `Mochi.Runtime` documents this and steers users toward async-only blocking primitives. For pure-async agents the cost is comparable to Loom.
- **JIT vs AOT tradeoff.** JIT-compiled Mochi (`dotnet run`) gets aggressive runtime optimisations (PGO, dynamic devirtualisation, tiered compilation) at the cost of startup time. AOT-compiled Mochi (`mochi publish --aot`) starts in ~12 ms but loses the JIT optimisations; the AOT compiler does its own escape analysis and devirtualisation but is more conservative.
- **No `synchronized` keyword.** Already covered in §7. The `lock` keyword exists but is thread-owned, not task-owned. Async code uses `SemaphoreSlim`.
- **Reflection is the AOT third rail.** Already covered in §13. We accept this and use source generation where the BCL offers it (System.Text.Json, YamlDotNet) and emit static code where it does not (Mochi `query`, AI tool schemas).

## 19. Boot sequence

A NativeAOT-published Mochi binary boots as follows:

1. **OS loader** maps the ELF / PE / Mach-O binary. The CLR is statically linked; there is no JIT initialisation.
2. **CRT init** (`mainCRTStartup` on Windows, `_start` on Linux). The CLR's static init runs here: GC threads start, `Server` GC heap is allocated (region table, gen 0 / 1 / 2 / LOH / POH regions), DATAS initial sizing is computed.
3. **AssemblyLoadContext.Default** is constructed. NativeAOT has only one ALC; collectible ALCs are unsupported.
4. **`Main` entry** is called. C# `Main` may be `static int Main(string[] args)` or `static async Task<int> Main(string[] args)`. The CLR's `Main` is found via the binary's exports table.
5. **Mochi.Runtime ModuleInitializer** runs (via `[ModuleInitializer]` on `Mochi.Runtime.Bootstrap.Init`). This wires up:
   - The root `RunCtx` with `TimeProvider.System` and a fresh `CancellationTokenSource`
   - `Console.CancelKeyPress` and `PosixSignalRegistration` for Ctrl-C / SIGTERM
   - The default `ActivitySource` and `Meter`
6. User code runs from `Main`.

JIT mode is similar but with R2R (ReadyToRun) precompilation: the assembly contains precompiled native code for cold methods, and the JIT lazily compiles hot methods to better quality code (tiered compilation: tier 0 produces unoptimised code fast, tier 1 produces optimised code in the background; PGO data feeds tier 1).

**Cold-start measurements** (Mochi-compatible hello-world, x64 Linux, .NET 8 baseline numbers from Microsoft and from the Aspire CLI shipping AOT in 2026):

| Mode | Startup time | Binary size | Notes |
| --- | --- | --- | --- |
| .NET 8 JIT (R2R, framework-dependent) | ~80 ms | ~150 KB | Plus the framework on disk (~70 MB) |
| .NET 8 JIT (self-contained) | ~90 ms | ~65 MB | Single-file publish |
| .NET 8 NativeAOT | ~15 ms | ~4 MB | Single-file, no runtime, IcuDataMode=Embedded |
| .NET 10 JIT (R2R) | ~70 ms | ~150 KB | Faster R2R thanks to write-barrier elimination |
| .NET 10 NativeAOT | ~12 ms | ~3 MB | Improved trimming + smaller PE headers |
| .NET 10 NativeAOT (no globalisation) | ~9 ms | ~2 MB | `InvariantGlobalization=true` shrinks ICU |

The Microsoft tracking issues ("Bringing down the size of Hello World under 2 MB", dotnet/runtime#80165; "under 1.44 MB", #83069) document the ongoing work; .NET 10 hits the 2 MB floor with globalisation disabled. The 3 MB figure with default globalisation is what Mochi will ship as the AOT default.

For comparison, MEP-47 §19 records JDK 25 GraalVM Native Image hello-world at ~25 ms / ~8 MB (cold) and AppCDS-loaded plain hotspot at ~120 ms / ~14 MB. .NET NativeAOT is currently the smallest and fastest hello-world among the four MEP targets (C: ~5 ms / ~30 KB; CLR: ~12 ms / ~3 MB; BEAM: ~50 ms via release tarball; JVM: ~25 ms via Native Image, ~120 ms via plain JIT).

---

## Sources

- [Microsoft Learn, .NET 10 What's New, https://learn.microsoft.com/en-us/dotnet/core/whats-new/dotnet-10/overview](https://learn.microsoft.com/en-us/dotnet/core/whats-new/dotnet-10/overview)
- [Microsoft Learn, OrderedDictionary<TKey,TValue> Class, https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ordereddictionary-2?view=net-10.0](https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.ordereddictionary-2?view=net-10.0)
- [Microsoft Learn, Lock Class (System.Threading), https://learn.microsoft.com/en-us/dotnet/api/system.threading.lock?view=net-9.0](https://learn.microsoft.com/en-us/dotnet/api/system.threading.lock?view=net-9.0)
- [Microsoft Learn, The lock statement (C# reference), https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/statements/lock](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/statements/lock)
- [Microsoft Learn, Channels, https://learn.microsoft.com/en-us/dotnet/core/extensions/channels](https://learn.microsoft.com/en-us/dotnet/core/extensions/channels)
- [Microsoft Learn, SocketsHttpHandler Class, https://learn.microsoft.com/en-us/dotnet/api/system.net.http.socketshttphandler?view=net-10.0](https://learn.microsoft.com/en-us/dotnet/api/system.net.http.socketshttphandler?view=net-10.0)
- [Microsoft Learn, HttpClient guidelines for .NET, https://learn.microsoft.com/en-us/dotnet/fundamentals/networking/http/httpclient-guidelines](https://learn.microsoft.com/en-us/dotnet/fundamentals/networking/http/httpclient-guidelines)
- [Microsoft Learn, How to use source generation in System.Text.Json, https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/source-generation](https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/source-generation)
- [Microsoft Learn, How to choose reflection or source generation in System.Text.Json, https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/reflection-vs-source-generation](https://learn.microsoft.com/en-us/dotnet/standard/serialization/system-text-json/reflection-vs-source-generation)
- [Microsoft Learn, Background garbage collection, https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/background-gc](https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/background-gc)
- [Microsoft Learn, Fundamentals of garbage collection, https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/fundamentals](https://learn.microsoft.com/en-us/dotnet/standard/garbage-collection/fundamentals)
- [Microsoft Learn, How to use and debug assembly unloadability in .NET, https://learn.microsoft.com/en-us/dotnet/standard/assembly/unloadability](https://learn.microsoft.com/en-us/dotnet/standard/assembly/unloadability)
- [Microsoft Learn, Native AOT deployment overview, https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/](https://learn.microsoft.com/en-us/dotnet/core/deploying/native-aot/)
- [Microsoft Learn, ASP.NET Core support for Native AOT (.NET 10), https://learn.microsoft.com/en-us/aspnet/core/fundamentals/native-aot?view=aspnetcore-10.0](https://learn.microsoft.com/en-us/aspnet/core/fundamentals/native-aot?view=aspnetcore-10.0)
- [Microsoft Learn, FrozenDictionary<TKey,TValue> Class, https://learn.microsoft.com/en-us/dotnet/api/system.collections.frozen.frozendictionary-2?view=net-8.0](https://learn.microsoft.com/en-us/dotnet/api/system.collections.frozen.frozendictionary-2?view=net-8.0)
- [.NET 9 Preview 6 Libraries Release Notes, https://github.com/dotnet/core/blob/main/release-notes/9.0/preview/preview6/libraries.md](https://github.com/dotnet/core/blob/main/release-notes/9.0/preview/preview6/libraries.md)
- [dotnet/designs, .NET Core 3.0 AppDomain Replacement Design, https://github.com/dotnet/designs/blob/main/accepted/2020/AssemblyLoadContext/AppDomainReplacement.md](https://github.com/dotnet/designs/blob/main/accepted/2020/AssemblyLoadContext/AppDomainReplacement.md)
- [dotnet/runtime, Unloadability design doc, https://github.com/dotnet/runtime/blob/main/docs/design/features/unloadability.md](https://github.com/dotnet/runtime/blob/main/docs/design/features/unloadability.md)
- [dotnet/runtime, Bringing down the size of Hello World under 2 MB, https://github.com/dotnet/runtime/issues/80165](https://github.com/dotnet/runtime/issues/80165)
- [dotnet/runtime, Bringing the size of Native AOT Hello World under 1.44 MB, https://github.com/dotnet/runtime/issues/83069](https://github.com/dotnet/runtime/issues/83069)
- [Maoni Stephens, Preparing for the .NET 10 GC, https://maoni0.medium.com/preparing-for-the-net-10-gc-88718b261ef2](https://maoni0.medium.com/preparing-for-the-net-10-gc-88718b261ef2)
- [Soundar Anbalagan, State of Native AOT in .NET 10, https://code.soundaranbu.com/state-of-nativeaot-net10](https://code.soundaranbu.com/state-of-nativeaot-net10)
- [Strathweb, Collectible assemblies in .NET Core 3.0, https://www.strathweb.com/2019/01/collectible-assemblies-in-net-core-3-0/](https://www.strathweb.com/2019/01/collectible-assemblies-in-net-core-3-0/)
- [Steve Gordon, .NET Internals System.Threading.Channels UnboundedChannel Part 2, https://www.stevejgordon.co.uk/dotnet-internals-system-threading-channels-unboundedchannel-part-2](https://www.stevejgordon.co.uk/dotnet-internals-system-threading-channels-unboundedchannel-part-2)
- [OpenTelemetry .NET Releases on GitHub, https://github.com/open-telemetry/opentelemetry-dotnet/releases](https://github.com/open-telemetry/opentelemetry-dotnet/releases)
- [OpenTelemetry 1.15.3 on NuGet, https://www.nuget.org/packages/OpenTelemetry](https://www.nuget.org/packages/OpenTelemetry)
- [CsvHelper 33.1.0 on NuGet, https://www.nuget.org/packages/CsvHelper/](https://www.nuget.org/packages/CsvHelper/)
- [YamlDotNet 17.1.0 on NuGet, https://www.nuget.org/packages/YamlDotNet](https://www.nuget.org/packages/YamlDotNet)
- [NodaTime 3.3.2 on NuGet, https://www.nuget.org/packages/nodatime/](https://www.nuget.org/packages/nodatime/)
- [nodatime/nodatime issue 1751, Plans for TimeProvider in .NET 8, https://github.com/nodatime/nodatime/issues/1751](https://github.com/nodatime/nodatime/issues/1751)
- [Andrew Lock, Using the YamlDotNet source generator for Native AOT, https://andrewlock.net/using-the-yamldotnet-source-generator-for-native-aot/](https://andrewlock.net/using-the-yamldotnet-source-generator-for-native-aot/)
- [Code Corner, .NET 8 FrozenDictionary performance, https://code-corner.dev/2023/11/08/NET-8-%E2%80%94-FrozenDictionary-performance/](https://code-corner.dev/2023/11/08/NET-8-%E2%80%94-FrozenDictionary-performance/)
- [Dave Callan, .NET 8 FrozenDictionary benchmarks, https://davecallan.com/dotnet-8-frozendictionary-benchmarks/](https://davecallan.com/dotnet-8-frozendictionary-benchmarks/)
- [Roxeem, What .NET 10 GC Changes Mean for Developers, https://roxeem.com/2025/09/30/what-net-10-gc-changes-mean-for-developers/](https://roxeem.com/2025/09/30/what-net-10-gc-changes-mean-for-developers/)
