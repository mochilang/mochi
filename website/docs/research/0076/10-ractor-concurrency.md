---
title: "10. Ractor and Fiber concurrency"
sidebar_position: 11
sidebar_label: "10. Ractor and Fiber"
description: "Ruby 3.x Ractors (parallel execution model), Fibers (cooperative concurrency), how Mochi's async model maps to Ruby's concurrency primitives, and what gem APIs the bridge can bridge across Ractor boundaries."
---

# 10. Ractor and Fiber concurrency

This note documents how MEP-76 positions itself against Ruby's two concurrency primitives: Ractor (true parallelism, Ruby 3.0+) and Fiber (cooperative concurrency / async I/O, Ruby 3.1+). The short answer: the bridge defaults to neither. All gem method invocations go through the main thread (or MEP-56 Thread-based agent threads). Advanced integration is deferred to phase 13.

## Ractor (Ruby 3.0+)

Ractors give Ruby programs true parallelism. Each Ractor has its own Global VM Lock (GVL) partition; two Ractors can run Ruby bytecode simultaneously on separate OS threads without blocking each other.

The catch is severe object-sharing restrictions. Only objects that pass `Ractor.shareable?` can cross Ractor boundaries:

- Frozen objects (deeply frozen, all transitive references also frozen).
- `Ractor::TVar` (transactional variable).
- Objects created inside the receiving Ractor.
- A small set of built-in immutable types (integers, symbols, true/false/nil).

Mutable Ruby objects (Hash, Array, most gem classes) are not shareable. Attempting to pass them across a Ractor boundary raises `Ractor::IsolationError` at runtime.

### Gem Ractor safety (May 2026)

An informal audit of the top-100 most-downloaded gems shows:

| Category | Ractor-safe? | Examples |
|----------|-------------|----------|
| Pure-data gems (no mutable class state) | Yes (if objects frozen) | `base64`, `uri` |
| I/O gems with connection pools | No | `redis`, `pg`, `mongo` |
| Parsing gems with global caches | No | `nokogiri`, `oj` |
| Framework gems | No | `rails`, `sinatra`, `grape` |
| Utility gems with frozen config | Partial | `activesupport` (partial) |

Fewer than 5% of the top-100 gems are fully Ractor-safe without modification.

### MEP-76 default: no Ractor use for gem calls

The bridge does not use Ractors for gem method invocations. All calls happen on the main thread or on MEP-56 Thread-based agent threads (see MEP-56 phase 11). Forcing Ractor use would silently break most gems via `Ractor::IsolationError`. Ractor integration is deferred to phase 13 (see [implementation tracking](/docs/implementation/0076/)).

## GIL/GVL implications

Standard CRuby has one Global VM Lock shared by all threads in the same process. The GVL is released during blocking I/O (network reads, file reads, `sleep`) but held during Ruby bytecode execution.

Consequences for MEP-76:

- **Thread safety for gem calls**: the GVL prevents data races at the VM level for most gem code. Two Mochi agent threads calling the same gem method cannot produce VM-level corruption because only one holds the GVL at a time.
- **CPU-bound gem calls**: gem code that runs pure Ruby computation (e.g., a regex engine) holds the GVL for the duration, blocking other threads. I/O-bound gem calls (network, disk) release the GVL and allow other threads to proceed.
- **Native extension gem calls**: native (C) extension code can release the GVL explicitly via `rb_thread_call_without_gvl`. Most well-written C gems (nokogiri, pg) do this for their I/O paths.

The GVL means the bridge's default (main-thread-only gem calls) is safe even if Mochi agent threads are used, provided the gems are not themselves spawning internal threads that mutate shared state.

## Fiber (Ruby 3.1+ non-blocking Fibers)

Ruby 3.1 added a `Fiber::Scheduler` interface. A scheduler can intercept blocking I/O operations (reads, writes, DNS resolution, `sleep`) and suspend the current Fiber while allowing other Fibers to run. This enables cooperative async I/O without separate threads.

The scheduler interface is a duck-typed contract: any Ruby object that responds to the scheduler callbacks (`io_wait`, `process_wait`, `kernel_sleep`, `block`, `unblock`) can act as a scheduler.

### async gem (github.com/socketry/async)

The `async` gem is the most popular Ruby async framework. It implements `Fiber::Scheduler` and provides an `Async { |task| ... }` block that runs fibers cooperatively:

```ruby
require "async"
require "async/http/internet"

Async do |task|
  internet = Async::HTTP::Internet.new
  response = internet.get("https://example.com/")
  puts response.read
ensure
  internet.close
end
```

From Mochi's perspective, async gem calls are treated as synchronous: the bridge wraps them by calling them on the current thread and blocking until the `Async` block completes. This mirrors the `block_on` approach used in MEP-73's tokio bridge.

### `[ruby.async] framework = "async"` opt-in

Users who want to use the `async` gem's cooperative scheduler at the Mochi level can opt in:

```toml
[ruby.async]
framework = "async"
```

When this is set, the bridge generates Ruby code that wraps the entire Mochi program entry point in an `Async` block:

```ruby
# Generated by MEP-76 bridge with [ruby.async] framework = "async"
require "async"
Async do |_root_task|
  # ... emitted Mochi program body ...
end
```

This allows gem calls that are fiber-aware (e.g., `Async::HTTP`) to execute cooperatively without blocking the Ruby event loop. The trade-off is that every gem call must be fiber-safe; gems that are not fiber-aware behave correctly but do not yield the scheduler.

Without the opt-in, the bridge does not generate an `Async` wrapper. Async-capable gem calls still work but run synchronously (the `Async` block completes inline on the calling fiber).

## TruffleRuby compatibility note

TruffleRuby (one of MEP-56's supported targets, see MEP-56 phase 26) has true multi-thread parallelism with no GVL. Two threads can execute Ruby code simultaneously. Gems that rely on the GVL for implicit thread safety may produce data races on TruffleRuby.

The bridge tracks a `[ruby.capabilities]` database that flags gems known to have GVL-dependent thread safety assumptions. For these gems, the bridge emits a warning comment in the generated Ruby shim:

```ruby
# WARNING: GVL-dependent gem, TruffleRuby behavior may differ.
# nokogiri 1.16.2 uses object-level mutability protected by the CRuby GVL.
# On TruffleRuby, concurrent calls to Nokogiri::HTML5::Document.parse may race.
require "nokogiri"
```

The warning is informational only; the bridge does not refuse to generate code for GVL-dependent gems on TruffleRuby. The user is responsible for ensuring that concurrent gem calls are safe for their target Ruby implementation.

## Concurrency model summary

| Primitive | True parallelism | Gem support | MEP-76 default | Phase |
|-----------|-----------------|-------------|----------------|-------|
| Thread (MEP-56 phase 11) | Yes (I/O-bound; GVL-serialised for CPU) | Full (GVL protects) | Used for agent threads | Existing |
| Fiber / Scheduler | No (cooperative) | Partial (only async-aware gems yield) | Opt-in via `[ruby.async]` | Phase 13 |
| Ractor | Yes (true parallelism) | < 5% of gems | Deferred | Phase 13 |

## Cross-references

- [08. Native C extension gems](08-native-extensions.md) for how native extensions interact with the GVL.
- [09. Bundler and mochi.lock](09-bundler-lockfile.md) for the `vendor/bundle/` layout that supplies gem files to all threads.
- [12. Risks and alternatives](12-risks-and-alternatives.md) for the GVL-assumption risk (R6) and the rejected Ractor-default alternative.
- [MEP-56](/docs/mep/mep-0056) for the Ruby transpiler's thread model (phase 11).
- [MEP-73 research note 08](/docs/research/0073/08-async-bridge) for the analogous tokio `block_on` design in the Rust bridge.
- [Implementation tracking phase 13](/docs/implementation/0076/phase-13-ractor-fiber) for the delivery status.
