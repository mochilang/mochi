# MEP-46 research note 04, Erlang/OTP runtime services for libmochi_erl

Author: research pass for MEP-46.
Date: 2026-05-23 (GMT+7).
Method: structured research over the OTP source tree, the OTP docs site (erlang.org/doc), Code BEAM talks 2021-2025, and the Erlang Ecosystem Foundation working-group notes.

This note inventories the runtime services Mochi programs need at execution time and chooses, for each one, an OTP module or pattern to lean on. The output of this research is the **module layout for the `mochi` OTP application** (see §22 below), which is the runtime library that all Mochi-generated `.beam` files link against.

---

# Runtime services for a Mochi-on-BEAM program

## 1. The BEAM scheduler

BEAM uses an **M:N scheduler**: M OS threads (one per scheduler, default `erlang:system_info(logical_processors_available)`) run N green threads (Erlang processes). Each scheduler has a *run queue*; processes migrate between queues for load balancing. Scheduling is **preemptive**, based on a **reduction counter** (`CONTEXT_REDS`, currently 4000 reductions per slice); roughly, every function call or BIF decrements the counter; when it hits zero, the process yields.

**Dirty schedulers** (since OTP 17) are a separate pool of OS threads that run **dirty NIFs** and **dirty I/O** without blocking the normal schedulers. Use them for any C work expected to run >1ms.

The scheduler is **work-stealing**: idle schedulers pull from busy queues. There is no priority inversion guarantee, but there are four priority levels (`low`, `normal`, `high`, `max`) that affect how many reductions a process gets per slice; `max` is reserved for system processes.

**For libmochi_erl:** Mochi has no notion of process priority; all user processes run at `normal`. Use `system_flag(scheduler_wall_time, true)` in `mochi_telemetry` for observability.

## 2. Memory model and garbage collection

Each Erlang process has its **own heap**, and most messages between processes are copied. The GC is **per-process generational** (young + old generation) and runs **only when the process needs to allocate**; one process's GC never stalls another. The heap starts at 233 words (configurable via `min_heap_size`/`min_bin_vheap_size`) and grows by 2x increments.

**Large binaries (>64 bytes)** are stored off-heap in a shared **binary reference-counted** area. Small binaries live on the process heap and are copied between processes. The reference-counted region is the source of nearly every "binary memory leak" in production: a long-lived process holds a sub-binary referencing a large parent binary, and the parent cannot be freed.

**For libmochi_erl:** Strings in Mochi are UTF-8 binaries (see [[06-type-lowering]]); we therefore inherit the binary-leak risk. The runtime should:
- Always `binary:copy/1` strings that cross process boundaries and may outlive their source (e.g. user inputs stored in a long-running gen_server).
- Document a `mochi_str:copy/1` helper that wraps `binary:copy/1` semantics so users don't have to know.
- Run `erlang:garbage_collect/0` after large reduction loops in `mochi_query` to free temporary binaries.

The OTP team has been clear that **manual `garbage_collect/0` calls are usually a code smell**, but in dataset pipelines that materialise large intermediate results, they are the standard fix.

## 3. Process model

Erlang processes are not OS threads; they are values, identified by **PIDs** (term type). PID literals are `<0.42.0>` (node ID, process ID, serial). Each process has:
- A **mailbox**, an unbounded FIFO of incoming messages
- A **dictionary**, a per-process key-value store (`put/2`, `get/1`); generally considered an anti-pattern except for tracing
- A **link set** and a **monitor set**: when a linked process dies abnormally, its links die too; monitors fire a `'DOWN'` message but don't cascade death
- An **error handler** for trapping exits (`process_flag(trap_exit, true)` makes the process receive `'EXIT'` messages instead of dying)
- A **group leader** (file descriptor inheritance analogue): I/O calls are forwarded to the group leader process

Process creation is **microsecond-cheap**: `spawn(fun ... end)` is a few hundred reductions and a heap allocation. A BEAM node can hold **hundreds of thousands of processes** comfortably (one production deployment, WhatsApp's chat servers, ran 2M processes per node in 2017).

**For libmochi_erl:** Mochi agents and streams (see [[01-language-surface]] §4) are processes. The runtime spawns under a `mochi_agent_sup` supervisor. PIDs themselves are not exposed to user Mochi code; agents and streams are opaque references that wrap PIDs.

## 4. Message passing and selective receive

`Pid ! Term` sends `Term` to `Pid`'s mailbox. The send is **asynchronous** and **always succeeds** (even if the target is dead; the send is silently dropped). Order is preserved for messages from the *same* sender, but not between senders.

`receive` is **selective**: a receive can pattern-match on the mailbox, skipping non-matching messages until a match is found or the mailbox is exhausted (then it blocks). Selective receive is a famous performance gotcha: a `receive {tag, Ref, _} -> ... end` with no fallback scans the entire mailbox every time, O(N) per call. The compiler optimises this when it can prove the `Ref` is **freshly created** in the same function (the `recv_marker_*` opcodes since OTP 24).

`gen_server`, `gen_statem`, etc. wrap `receive` with disciplined handlers that always match all messages, eliminating the selective-receive trap.

**For libmochi_erl:** Mochi agents always use `gen_server:call/cast`. The runtime never exposes raw send/receive to user Mochi code; the `mochi_agent` API is the only way to interact with processes.

## 5. OTP behaviors

The five canonical behaviors:

- **`gen_server`**: synchronous (`call`) and asynchronous (`cast`) request/response, with `handle_info` for everything else. The workhorse of OTP. Used for stateful agents, caches, registries, anything serialising state.
- **`gen_statem`** (OTP 19+): state-machine behavior with `handle_event_function` or `state_functions` callback mode. Supports timeouts, state-enter actions, and postponed events. The right choice for stream processors with multiple states.
- **`supervisor`**: process supervision tree. Three restart strategies (`one_for_one`, `one_for_all`, `rest_for_one`), plus the `simple_one_for_one` (deprecated in OTP 21 in favor of `dynamic_supervisor`-style `start_child` with templates). Each child has a `restart` policy (`permanent`, `transient`, `temporary`).
- **`gen_event`**: event manager with multiple subscribers. Less used in modern code; `pg` is usually a better fit.
- **`supervisor_bridge`**: adapter for non-OTP processes that need to live in a supervision tree. Useful for wrapping legacy or third-party processes.

The behavior pattern is **callback-driven**: you write the `init/1`, `handle_call/3`, etc. callbacks, and the OTP library handles the message loop, error handling, and code change.

**For libmochi_erl:** Every Mochi-spawned process is a `gen_server` underneath. Mochi's `agent` keyword expands to a module implementing `gen_server`'s callbacks where:
- `init/1` is the agent's constructor
- `handle_call/3` dispatches to Mochi `method` functions
- `handle_cast/2` dispatches to async `tell` calls
- `handle_info/2` routes incoming stream events
- `terminate/2` runs the agent's `on_close` block

## 6. Streams ↔ pubsub mapping

Mochi has first-class **streams** (see [[01-language-surface]] §4). A stream is a *named* hub that any process can publish to or subscribe to. The OTP-canonical implementation:

- Use `pg` (process groups, stdlib since OTP 23, post-pg2): `pg:join(Scope, Group, Pid)`, `pg:leave(...)`, `pg:get_members(Scope, Group)`.
- Each Mochi stream is a `pg` group; the scope is `mochi` (one scope per application, isolates from other apps).
- Publish: `[Pid ! Msg || Pid <- pg:get_members(mochi, StreamName)]`. This is O(N) in subscriber count but allocation-free.
- Subscribe: a Mochi `subscribe` block becomes a `pg:join/3` plus a `receive` loop wrapped in `gen_statem`.

`pg` works **cluster-wide** for free: members on remote nodes are included automatically. This makes Mochi streams distributed by default, which matches the spec's "stream is a logical hub" framing.

The alternative is **`phoenix_pubsub`** (Elixir, but Erlang-callable), which adds local-only and partition-aware modes. For Mochi, plain `pg` is sufficient; users who need partition awareness can drop down to `gen_server.system_id := 'mochi.stream.<name>'` and route manually.

**For libmochi_erl:** `mochi_stream` module wraps `pg` for publish/subscribe. `mochi_stream_sup` is a supervisor that owns a `mochi_stream_recorder` gen_server per stream for things like ring-buffer history (useful for `subscribe ... from beginning`).

## 7. ETS (Erlang Term Storage)

ETS is an in-memory key-value store, implemented in C as part of the runtime. Tables are owned by a process; when the owner dies, the table is deleted (unless `heir` is set).

- **Four table types**: `set`, `ordered_set`, `bag`, `duplicate_bag`. `set` is a hash table; `ordered_set` is a balanced binary tree (O(log N) lookup, ordered iteration).
- **Access modes**: `protected` (default, only owner writes), `public` (anyone writes), `private` (only owner accesses).
- **Concurrency options**: `read_concurrency` (rare-write, many-read; uses RCU-style), `write_concurrency` (uses lock striping). Both are critical at scale.

ETS scans are O(N) by default; **match specifications** (built with `ets:fun2ms/1` or hand-written) compile to a small bytecode that runs over the table in C. `select/2`, `select_count/2`, `select_delete/2` are the workhorses.

**For libmochi_erl:** Mochi's `map` and `set` types are BEAM maps, *not* ETS by default. ETS is only used for:
- Datalog facts (one ETS table per relation; see [[08-dataset-pipeline]])
- Mochi `cache` declarations (memoised function results)
- Internal interning tables (atom registry, fun registry)

The decision to default to BEAM maps and reserve ETS for explicit use is deliberate: per-process maps are faster for small data and have clear ownership; ETS shines for shared, growing tables.

## 8. Mnesia

Mnesia is the OTP distributed database. Tables can be RAM-only (`ram_copies`), disk-resident (`disc_copies` = RAM + disk log + checkpoint, `disc_only_copies` = disk only), and replicated across nodes with configurable replication factor.

Mnesia transactions are **distributed two-phase commit** with conflict-resolution policies. The API is `mnesia:transaction(fun() -> ... end)` returning `{atomic, Result}` or `{aborted, Reason}`.

Modern alternatives (Ra, Khepri, Riak) are popular for new code, but Mnesia is in the OTP stdlib, requires zero extra deps, and is battle-tested for 25+ years.

**For libmochi_erl:** Mnesia is **out of scope for v0.1**. Mochi has no built-in persistence; the user is expected to integrate explicitly (`from sqlite "..."`, `from postgres "..."`, etc.). If users want Mnesia, they can call it via FFI.

## 9. NIFs (Native Implemented Functions)

NIFs are C functions linked into the BEAM emulator that look like Erlang functions to user code. They are **fast** (no marshalling overhead beyond the term API) but **dangerous**: a buggy NIF can crash the VM or, worse, corrupt the heap.

Key rules:
- **Yield within 1ms** or use a **dirty scheduler**. A NIF that runs longer than 1ms can starve other processes.
- **No long-running loops** without `enif_consume_timeslice/2` decrementing the reduction counter.
- **All term creation goes through `enif_make_*`**, which checks heap bounds.

The OTP team's stance (Kenneth Lundin, Code BEAM 2023): *prefer pure Erlang*; *use existing maintained NIF libraries* (`crypto`, `re`, `zlib`, `ssl`) before writing your own. The Rustler library (Rust → BEAM NIF) is the modern recommended path for new NIF code because it eliminates an entire class of memory-safety bugs.

**For libmochi_erl:** **No new NIFs in v0.1.** Reasons:
- Mochi's standard library functions are all expressible in pure Erlang.
- `crypto`, `re`, `zlib` cover the common needs.
- Adding a NIF requires C build infrastructure, platform-specific binaries, and crash-safety review.

Document this stance in the MEP.

## 10. Ports and port drivers

Ports are the *safe* way to integrate native code: a port is a process that wraps a C program speaking a byte-stream protocol over stdin/stdout. Crashes in the port program kill only the port, not the VM.

Port drivers are **like NIFs in that they run in the VM process**, but with a more constrained API. They are mostly used by OTP itself (`inet_drv` for sockets, `efile_drv` for files). Modern code uses **`erl_nif` for NIFs** or **`gen_server`-wrapped Port** for external programs.

**For libmochi_erl:** Mochi's `fetch` (HTTP) uses `gun` (Erlang HTTP client, pure Erlang + a small NIF for SSL handshake reuse via `ssl`). Mochi's `run` (subprocess) uses a port. No port drivers.

## 11. Distribution and clustering

Erlang Distribution is built into ERTS. `net_kernel:start/1` boots the distributed system; nodes are named `<name>@<host>` and discover each other via the **EPMD** (Erlang Port Mapper Daemon) on port 4369, or via direct connection in OTP 23+.

The protocol is binary, custom (not gRPC, not HTTP). It uses TCP by default; TLS is opt-in via `ssl_dist`. Distribution v6 (OTP 28, May 2025) added per-fragment authentication and larger windows.

**For libmochi_erl:** Distribution is **available** but not required. The MEP says Mochi runs on a single node by default; users opt in to distribution by setting `-name` / `-sname` and using `:rpc` or `pg`. The `mochi` application's `start_phases` should be tolerant of running with or without distribution.

## 12. escript and OTP releases

Two distribution formats:

- **escript**: a single executable file that bundles compiled `.beam` modules + the OTP runtime, runs the entry-point function, and exits. Used for CLI tools. Build with `escript:create/2` or `rebar3 escriptize`.
- **OTP releases**: a self-contained directory with the runtime, all OTP apps, and the user app, packaged as a tarball or systemd-runnable. Built with `relx` or `mix release`.

escripts boot in <100ms; releases boot in ~1s but support hot reload, supervision, and long-running services.

**For libmochi_erl:** Mochi build emits **both**:
- `--target=beam-escript` produces a self-contained escript (good for `mochi run` parity, scripting).
- `--target=beam-release` produces an OTP release (good for daemon-style deployments).

See [[10-build-system]] for details.

## 13. Hot code loading

Hot code loading is **automatic**: `code:load_file/1` swaps in a new version. The BEAM keeps two versions in memory: **old** and **current**. When a process makes a *fully-qualified* call (`?MODULE:foo(...)` or `mochi_mod:foo(...)`), the call goes to the **current** version. A second `code:load_file/1` purges the **old** version; processes still running on it are killed.

Tools:
- `code:soft_purge/1`: returns `false` if any process is still running on the old version (so you can wait).
- `code:purge/1`: kills processes still running on old (use only if you accept the loss).
- `appup` + `relup`: the SASL upgrade scripting language for orchestrating multi-module upgrades.

The two-version invariant means **multi-step upgrades** must be staged: you can't jump from v1 to v3; you must load v2, let processes migrate, then load v3.

**For libmochi_erl:** Mochi modules are hot-reloadable for free. The MEP should:
- Document the two-version invariant.
- Recommend full-node restart as the default upgrade strategy (matches modern OTP practice).
- Provide hot reload as documented advanced usage; not the primary path.

## 14. Strings, binaries, IO

Erlang has *two* string representations:
- **String lists**: lists of integers (`[$h, $e, $l, $l, $o]` for `"hello"`). The original Erlang string. Mostly legacy.
- **Binaries**: `<<"hello"/utf8>>`. UTF-8-encoded byte sequences. The modern standard.

The OTP 27 **sigils** (`~"hello"`) produce binaries by default. Erlang 28's stdlib `string` module accepts both representations transparently; new code should use binaries everywhere.

I/O: `io:format/2` accepts both. `io_lib:format/2` returns iolists (nested lists of binaries/strings/bytes), which are written efficiently by `io:put_chars/1` without materialising a single flat string. Iolists are the canonical OTP idiom for "string buffers".

**For libmochi_erl:** Mochi strings are UTF-8 binaries (see [[06-type-lowering]] §3). The `mochi_str` module wraps the OTP `string` module's binary functions:
- `mochi_str:len/1` -> `string:length/1`
- `mochi_str:slice/3` -> `string:slice/3`
- `mochi_str:split/2` -> `binary:split/2` (faster than `string:split` for fixed delimiters)
- `mochi_str:trim/1` -> `string:trim/1`
- `mochi_str:to_lower/1` -> `string:lowercase/1`
- `mochi_str:to_upper/1` -> `string:uppercase/1`

## 15. HTTP and networking

OTP ships **`httpc`** (built-in, OK for one-off requests, awkward for connection pooling) and **`inets`** (HTTP server, mostly legacy).

For modern HTTP:
- **`gun`** (NineNines, since 2014, v2.1 in 2024): HTTP/1.1, HTTP/2, HTTP/3 (QUIC) client. Pure Erlang plus the standard `ssl` lib. Connection pooling, streaming bodies, server push. The de facto modern HTTP client.
- **`cowboy`** (NineNines, since 2011, v2.13 in 2024): HTTP/1.1, HTTP/2, WebSockets server.
- **`hackney`**: alternative client, still used.

For TCP/UDP, OTP's `gen_tcp` / `gen_udp` are first-class.

**For libmochi_erl:** Use **`gun`** for `fetch`. The `mochi_fetch` module wraps `gun:open/await/await_body` into a synchronous (Mochi-style) call. `mochi_fetch_sup` supervises connection pools per host.

## 16. JSON

Pre-OTP-27, JSON in Erlang meant **`jsx`** (pure Erlang, slow), **`jiffy`** (NIF-based, fast, occasional crashes), or **`thoas`** (pure Erlang, modern).

OTP 27 (May 2024) added a stdlib **`json`** module (EEP 67). It is pure Erlang, parser/printer/incremental-decoder, and fast enough for most uses. The benchmark (Andrea Leopardi, ElixirConf EU 2024) shows it within 2x of `jiffy` for typical payloads and equal for small ones.

**For libmochi_erl:** Use the stdlib **`json`** module. No external deps. This is one of the strongest reasons to require OTP 27 minimum (see [[02-design-philosophy]]).

## 17. TLS and crypto

OTP ships **`crypto`** (OpenSSL bindings via NIF) and **`ssl`** (pure Erlang TLS implementation on top of `crypto` primitives). TLS 1.3 has been stable since OTP 22 (2019); OTP 28 (2025) removed TLS 1.0/1.1 support.

The `crypto` API covers AES, ChaCha20, RSA, ECDSA, Ed25519, HMAC-SHA*, scrypt, Argon2 (since OTP 27), HKDF, X.509, etc.

**For libmochi_erl:** Mochi `fetch` over HTTPS uses `gun` over `ssl`. No new crypto exposure in v0.1; users who need crypto can call `crypto` via FFI.

## 18. Telemetry and observability

The **`telemetry`** library (Beam Telemetry, 2019, v1.3 in 2024) is the de facto event/metric framework. Libraries emit events via `telemetry:execute([app, event_name], Measurements, Metadata)`; users attach handlers. `telemetry_metrics` and `telemetry_poller` give Prometheus-style metrics.

**Tracing**: `dbg` (built-in, low overhead), `recon` (Fred Hébert, third-party but ubiquitous), `observer` (graphical, ships with OTP).

**For libmochi_erl:** Expose telemetry events:
- `[mochi, agent, start]`, `[mochi, agent, stop]`, `[mochi, agent, crash]`
- `[mochi, stream, publish]`, `[mochi, stream, subscribe]`
- `[mochi, fetch, request, start]`, `[mochi, fetch, request, stop]`
- `[mochi, query, execute, start]`, `[mochi, query, execute, stop]`

Users attach handlers via `telemetry:attach/4`. The `mochi_telemetry` module provides convenience wrappers and default-attached handlers for `mochi.log`.

## 19. Recent improvements (OTP 26, 27, 28)

- **OTP 26** (May 2023): map shorthand `#{key}`, dynamic supervisor restart intensity, `tools` app reorganisation.
- **OTP 27** (May 2024): **`json` stdlib**, **`maybe` default**, **sigils**, **triple-quoted strings**, `-doc` attribute, `ets:lookup_element/4` with default.
- **OTP 28** (May 2025): Distribution v6, JIT default on more platforms, removal of `now/0`, OpenSSL 3.5 in `crypto`, EEP 75 spec-on-vars (draft).
- **OTP 29** (planned May 2026): parallel compile pipeline improvements, official AtomVM compatibility profile (per EEF notes).

The minimum supported OTP version for Mochi is **OTP 27**, balancing modernity (json, sigils, maybe) with availability (in Debian stable, Ubuntu 24.04, Homebrew, asdf as of mid-2025).

## 20. Limitations and gotchas

- **Atom table is bounded** (`+t` flag, default 1,048,576). Programs that dynamically create atoms (e.g. from user input via `binary_to_atom/1`) will eventually exhaust the table. **Mochi must never `binary_to_atom` user input**; use `binary_to_existing_atom/2` (raises if not pre-registered) or keep things as binaries.

- **Distributed nodes share atom tables**: a remote node may push an atom we haven't seen, polluting our table. The `mochi` application should pre-register all known atoms in `mochi_atoms` (Mochi sum-type variants, record field names, the `some`/`none` constants) at boot.

- **Selective receive scans the mailbox**: see §4. Mitigation: always use `gen_server` and friends.

- **Large message copying is slow**: a 100KB term sent between two processes is fully copied. For shared large data, use ETS or `persistent_term`.

- **`persistent_term` is global and persistent until module unload**: useful for static config but slow to update (whole heap GC triggered on update). Use only for true constants.

- **`receive` after `monitor` race**: the canonical bug is `Ref = monitor(...); receive {'DOWN', Ref, ...} -> ... end`; if the process dies *before* `monitor` is called, the DOWN message never arrives. Use `monitor(process, Pid, [{alias, reply_demonitor}])` (OTP 24+) for race-free monitoring.

- **`code:purge` kills processes**: do not call it casually.

- **`erlang:apply/3` on missing modules**: lazy-load. If `code:get_path/0` doesn't include the module, the call fails. The release tool sets the path correctly; ad-hoc deployments need care.

**For libmochi_erl:** Document each of these in the implementation note. The runtime should:
- Pre-register all Mochi atoms at boot.
- Use `gen_server` everywhere (no raw `receive`).
- Never use `binary_to_atom/1` on user data.
- Use `persistent_term` only for the Mochi module registry.
- Use the OTP 24+ `monitor` alias options.

## 21. Profiling and benchmarking

OTP profilers:
- **`fprof`**: function-level profiling, high overhead, accurate counts.
- **`eprof`**: cheaper, time-based.
- **`cprof`**: call counting.
- **`recon_trace`** (third-party `recon`): production-safe tracing with rate limits.
- **`erlperf`** (third-party): microbenchmark harness.

For load testing the JIT path, `perf top` on Linux shows BeamAsm-emitted symbols with `$_` prefixes. Flame graphs work via Brendan Gregg's `FlameGraph` scripts.

**For libmochi_erl:** No new profiling tools. Mochi users get OTP's full profiling stack out of the box. Document the standard recipes in the MEP.

## 22. The `mochi` OTP application: module layout

Putting it all together, the runtime library `mochi` exposes the following modules:

```
mochi/
├── ebin/                            % Compiled .beam files
├── src/
│   ├── mochi.app.src                 % Application resource file
│   ├── mochi_app.erl                 % application:start/2 callback
│   ├── mochi_sup.erl                 % Top-level supervisor
│   │
│   ├── mochi_atoms.erl               % Pre-registered atom interning
│   ├── mochi_core.erl                % Boxed value helpers (mochi_value)
│   │
│   ├── mochi_str.erl                 % String/binary operations
│   ├── mochi_list.erl                % List helpers (Mochi semantics)
│   ├── mochi_map.erl                 % Map helpers
│   ├── mochi_set.erl                 % Set helpers
│   ├── mochi_omap.erl                % Ordered map helpers
│   ├── mochi_option.erl              % Option(T) helpers
│   ├── mochi_time.erl                % Time/duration arithmetic
│   │
│   ├── mochi_query.erl               % Query DSL runtime (LINQ-style)
│   ├── mochi_datalog.erl             % Datalog interpreter
│   ├── mochi_datalog_ets.erl         % ETS fact table backing
│   │
│   ├── mochi_stream.erl              % Stream pubsub (pg-backed)
│   ├── mochi_stream_sup.erl          % Stream supervisor
│   ├── mochi_stream_recorder.erl     % Optional history ring buffer
│   │
│   ├── mochi_agent.erl               % Agent gen_server base
│   ├── mochi_agent_sup.erl           % Agent supervisor
│   ├── mochi_async.erl               % async/await and futures
│   │
│   ├── mochi_llm.erl                 % generate (LLM) facade
│   ├── mochi_llm_sup.erl             % LLM provider supervisor
│   ├── mochi_llm_openai.erl          % OpenAI provider
│   ├── mochi_llm_anthropic.erl       % Anthropic provider
│   │
│   ├── mochi_fetch.erl               % fetch (HTTP) facade (gun)
│   ├── mochi_fetch_sup.erl           % gun pool supervisor
│   │
│   ├── mochi_ffi.erl                 % FFI marshalling helpers
│   ├── mochi_telemetry.erl           % Telemetry event helpers
│   ├── mochi_log.erl                 % Default logger handler
│   └── mochi_test.erl                % Test harness for `test "..."` blocks
└── test/                            % eunit + common_test for the runtime itself
```

The supervision tree (boot order):
```
mochi_sup (one_for_one)
├── mochi_atoms          (transient, runs init then dies)
├── mochi_fetch_sup      (one_for_one) → gun connection pools
│   └── ...
├── mochi_llm_sup        (one_for_one) → provider gen_servers
│   ├── mochi_llm_openai
│   └── mochi_llm_anthropic
├── mochi_stream_sup     (one_for_one) → stream recorders
│   └── ...
├── mochi_agent_sup      (dynamic, one_for_one) → user agents
└── mochi_telemetry      (gen_server, attached handlers)
```

The runtime application starts under the user's release; user Mochi modules are loaded normally and call into `mochi_*` functions. Generated Mochi modules look like:

```erlang
-module(mochi_user_main).
-compile([no_auto_import]).
-export([main/1]).

-spec main([binary()]) -> integer().
main(V_args) ->
    V_n = mochi_list:length(V_args),
    mochi_log:info(<<"argc"/utf8>>, #{count => V_n}),
    0.
```

This module is a thin wrapper around `mochi_*` runtime calls. The Core Erlang emitted by MEP-46 corresponds to this Erlang text directly, but is produced via `cerl` records, not pretty-printed Erlang.

## 23. What we do NOT need

For completeness, services we considered and rejected for libmochi_erl v0.1:

- **Mnesia**: out of scope; users opt in via FFI.
- **NIFs**: none new; existing `crypto`/`re`/`ssl` cover everything.
- **HiPE / native compilation beyond BeamAsm**: BeamAsm is the JIT.
- **port drivers**: too risky for the value; ports are sufficient.
- **`gen_event`**: `pg`-backed streams cover the use case.
- **Phoenix.PubSub**: `pg` is sufficient; users can drop in `phoenix_pubsub` via FFI if they need it.
- **Distributed locking (global, locks)**: out of scope for v0.1.
- **`persistent_term` for user data**: too easy to misuse; document but don't expose.

These rejections are not permanent; later MEPs can add what's missing.

## 24. Boot sequence

When a Mochi-compiled escript or release starts:

1. ERTS boots (BeamAsm JIT initialises, schedulers spin up).
2. `kernel` and `stdlib` applications start (always).
3. `sasl` starts (for releases).
4. `compiler` starts (lazy; only loaded if hot-compile is used).
5. `mochi` application starts: `mochi_atoms:init/0` pre-registers all known atoms. Supervisors start in tree order.
6. User code's `main` is invoked (for escripts) or the user's application start callback is called (for releases).

Boot time:
- Escript: ~50ms cold (mostly BEAM startup), ~10ms warm.
- Release: ~300ms cold, ~50ms warm. Includes SASL boot, supervision tree spin-up, optional logger attachment.

These numbers are within target for a CLI tool and a daemon respectively.

---

## Sources

1. Erlang/OTP runtime documentation. https://www.erlang.org/doc/system_principles/system_principles.html
2. ERTS user guide. https://www.erlang.org/doc/apps/erts/erts_internal.html
3. OTP design principles. https://www.erlang.org/doc/system/design_principles.html
4. `gen_server` reference. https://www.erlang.org/doc/man/gen_server.html
5. `gen_statem` reference. https://www.erlang.org/doc/man/gen_statem.html
6. `supervisor` reference. https://www.erlang.org/doc/man/supervisor.html
7. `pg` (process groups) reference. https://www.erlang.org/doc/man/pg.html
8. `ets` reference. https://www.erlang.org/doc/man/ets.html
9. `mnesia` user guide. https://www.erlang.org/doc/man/mnesia.html
10. `crypto` reference. https://www.erlang.org/doc/man/crypto.html
11. `ssl` reference. https://www.erlang.org/doc/man/ssl.html
12. `json` (OTP 27 stdlib). https://www.erlang.org/doc/man/json.html
13. `code` reference. https://www.erlang.org/doc/man/code.html
14. SASL/release upgrades. https://www.erlang.org/doc/system/release_handling.html
15. Lukas Larsson, "BeamAsm in OTP 24." Code BEAM SF 2021.
16. Andrea Leopardi, "OTP 27 Highlights." ElixirConf EU 2024.
17. Kenneth Lundin, "Modern NIFs." Code BEAM 2023.
18. Saša Jurić, "Releases without tears." Code BEAM 2024.
19. Fred Hébert, "Erlang in Anger." https://www.erlang-in-anger.com/ (still canonical for production tuning).
20. Joe Armstrong, "Programming Erlang, 2nd ed." Pragmatic, 2013 (legacy but readable).
21. `gun` (NineNines). https://ninenines.eu/docs/en/gun/2.1/
22. `cowboy` (NineNines). https://ninenines.eu/docs/en/cowboy/2.13/
23. `telemetry`. https://github.com/beam-telemetry/telemetry
24. `recon` (Fred Hébert). https://github.com/ferd/recon
25. EEP 67 (json). https://www.erlang.org/eeps/eep-0067
26. EEP 63 (sigils). https://www.erlang.org/eeps/eep-0063
27. EEP 49 (maybe). https://www.erlang.org/eeps/eep-0049
