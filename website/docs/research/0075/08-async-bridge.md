---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "PHP's async story: PHP 8.1 fibers, ReactPHP event loop, RevoltPHP from amphp/revolt, how PHP cooperative multitasking interacts with MEP-55's sync-wrapper philosophy, when the async opt-in makes sense, how `import php \"react/event-loop\"` integrates with the bridge, and the async-capability flag."
---

# 08. Async bridge

Author: research pass for MEP-75 (Mochi and PHP package bridge). Date: 2026-05-29 22:11 (GMT+7).

This note describes PHP's async ecosystem and the design of MEP-75's optional async bridge.

## 1. PHP's async story

PHP was designed as a synchronous request-response language. Each HTTP request starts a PHP process (or worker), runs to completion, and exits (or returns to the pool in FPM / RoadRunner mode). There is no built-in event loop.

PHP 8.1 (December 2021) introduced **fibers**: cooperative coroutines that can suspend and resume. A fiber is a user-space stack that can be suspended at any `Fiber::suspend()` call and resumed by its caller. Fibers enable cooperative multitasking within a single PHP thread, but they do not provide:

- An event loop (something to decide which fiber runs next).
- Non-blocking I/O (PHP's stream functions block the process unless wrapped with `stream_select` or `uv_*` from libuv).
- Timer scheduling.

The two major event-loop implementations for PHP are **ReactPHP** and **RevoltPHP** (which powers amphp).

## 2. ReactPHP

ReactPHP (`react/event-loop`, first release 2012) is the oldest and most widely-adopted PHP async framework. It provides:

- An event loop (`Loop::run()`) that drives I/O events and timer callbacks.
- Non-blocking HTTP client/server (`react/http`).
- Non-blocking DNS (`react/dns`).
- Promise-based async primitives (`react/promise`).
- Compatibility with PHP 8.1 fibers (since ReactPHP 3.0, released 2023).

ReactPHP 3.0+ uses `revolt/event-loop` internally as the shared fiber-backed scheduler, making ReactPHP and amphp/amp share the same underlying runtime.

Key packages in the ReactPHP ecosystem:

- `react/event-loop ^3.0`: the event loop library.
- `react/http ^1.9`: HTTP client and server.
- `react/socket ^1.14`: TCP/UDP socket abstraction.
- `clue/reactphp-buzz ^2.9`: high-level HTTP client built on ReactPHP.
- `clue/reactphp-ndjson ^1.3`: streaming NDJSON parser.

## 3. RevoltPHP

RevoltPHP (`revolt/event-loop`, extracted from amphp in 2022) is the shared fiber-backed event loop that both ReactPHP 3.0 and amphp/amp use. It provides:

- A fiber-based event loop that runs callbacks on suspended/resumed fibers.
- Integration with `ext-uv` (libuv) for high-performance I/O.
- Compatibility with both ReactPHP and amphp.

The amphp/amp v3 framework (Amphp, 2023) is built entirely on RevoltPHP fibers and is API-incompatible with amphp/amp v2 (which predates fibers and used generator-based coroutines).

**ReactPHP vs RevoltPHP for MEP-75**: both use the same underlying `revolt/event-loop` scheduler. ReactPHP's API is more established and better documented for most use cases. RevoltPHP's API is lower-level and more composable. MEP-75 defaults to ReactPHP (`[php.async] event-loop = "react"`) because:

- ReactPHP has wider ecosystem adoption (more Packagist packages target it).
- ReactPHP's promise model is simpler for the Mochi bridge to wrap.
- The default serves 90%+ of use cases.

RevoltPHP is available via `[php.async] event-loop = "revolt"` for amphp-based packages.

## 4. MEP-55's sync-wrapper philosophy and the async opt-in

MEP-55 Phase 11 (async colouring) decided that the PHP target lowers Mochi's async-coloured functions to synchronous wrappers. The rationale (from the Phase 11 design notes): PHP's `amphp/revolt` was originally listed as a runtime dependency but removed after Phase 11 confirmed that sync-only wrappers are sufficient for the Phase 11 fixture corpus.

MEP-75's async bridge is layered on top of MEP-55's sync model:

- For programs that only import synchronous PHP packages (the majority), the sync model is correct and the async opt-in is irrelevant.
- For programs that import ReactPHP/RevoltPHP-based packages (`react/http`, `amphp/http-client`), the async opt-in injects the event loop and enables the async glue layer.

When `[php.async] enabled = true`, the bridge:

1. Adds `react/event-loop ^3.0` (or `revolt/event-loop ^1.0`) to the vendor sandbox.
2. Emits an `async_glue.php` file in the vendor sandbox that initialises the event loop.
3. Wraps async package methods (methods that return `PromiseInterface` or `React\Promise\PromiseInterface`) in synchronous adapters:
   ```php
   // Generated synchronous adapter for ReactPHP promise-returning method
   function mochi_guzzle_http_get_sync(Client $client, string $url): string {
       $promise = $client->getAsync($url);
       return \React\Async\await($promise);
   }
   ```
4. Emits the corresponding Mochi `extern fn` for the synchronous adapter.

The `react/async` package (provides `React\Async\await()`) is injected alongside `react/event-loop` when `enabled = true`.

## 5. PHP fibers vs the async bridge

PHP 8.1 fibers allow the async bridge to work without blocking the PHP process: `React\Async\await()` uses a fiber to suspend the current coroutine while the promise resolves, then resumes it with the result. From the Mochi extern perspective, the call is synchronous (returns a value); inside the PHP runtime, the fiber yields control to the event loop while waiting.

This model has a cost: every async PHP call goes through a fiber suspend/resume cycle (~2-5 microseconds per cycle on PHP 8.4 on modern hardware). For high-throughput code, this is acceptable; for latency-critical tight loops, it is not. The bridge documents this tradeoff in the extern `from php "..." async` annotation.

PHP fibers are NOT OS threads. All fibers run on a single OS thread; true parallelism requires spawning multiple PHP processes (via `pcntl_fork`, FPM workers, or RoadRunner workers). The MEP-75 async bridge is single-threaded cooperative concurrency, consistent with MEP-55's single-worker model.

## 6. Interaction with MEP-55 targets

| MEP-55 target | Async bridge status |
|---|---|
| `TargetPhpSource` | Supported (event loop initialised in `main.php` if `[php.async] enabled = true`) |
| `TargetPhpRun` | Supported (same as source) |
| `TargetPhpPhar` | Supported (async_glue.php included in Phar) |
| `TargetPhpFrankenPHP` | Supported (FrankenPHP's worker mode runs one PHP instance per worker; each worker has its own event loop) |
| `TargetPhpRoadRunner` | Supported (RoadRunner workers are long-running PHP processes; the event loop persists across requests) |
| `TargetPhpLibrary` | Limited: the library target cannot initialise a global event loop (the consumer application controls the event loop). Async library methods are emitted as promise-returning functions, not blocking adapters. |

## 7. The async-capability flag

`[php.capabilities] net = true` is required when importing async network packages (e.g., `react/http`). The capability flag tracks that the imported package opens network connections; the async opt-in is separate from the capability declaration.

A program with `[php.async] enabled = true` and `[php.capabilities] net = false` fails the capability audit at lock time if any of the injected async packages (e.g., `react/http`) are in the `net` capability set.

## 8. Packages that need the async bridge

From the 24-package fixture corpus, packages that benefit from (or require) the async bridge:

- `guzzlehttp/guzzle ^7.8`: the async methods (`getAsync`, `postAsync`) return promises. Without the async bridge, only the synchronous methods are translated.
- `league/oauth2-server`: the server can be built with ReactPHP for non-blocking token issuance.
- `stripe/stripe-php`: Stripe PHP SDK uses GuzzleHTTP internally; async methods are available via Guzzle's promise interface.

Most of the 24-package corpus is synchronous (Symfony, Doctrine, PHPUnit, Monolog, Carbon, PSR/log, Ramsey UUID, etc.) and works without the async bridge.

## Cross-references

- [[02-design-philosophy]] §7 for the opt-in rationale.
- [[05-type-mapping]] §8 for the callable/Closure type mapping.
- [[11-testing-gates]] §8 for the async bridge test cases.
- [MEP-55 Phase 11](/docs/mep/mep-0055) for the async colouring baseline.
- [MEP-73 research/08](/docs/research/0073/08-async-bridge) for the tokio singleton approach in the Rust bridge.
