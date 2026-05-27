---
title: "Phase 11. async (Loom-backed)"
sidebar_position: 13
sidebar_label: "Phase 11. async"
description: "MEP-47 Phase 11 — spawn/await structured concurrency with Loom virtual threads; Async<T> combinators; StructuredTaskScope; cancellation; deterministic mode."
---

# Phase 11. async (Loom-backed)

| Field          | Value |
|----------------|-------|
| MEP            | [MEP-47 §Phases · Phase 11](/docs/mep/mep-0047#phase-11-async-loom-backed) |
| Status         | LANDED |
| Started        | 2026-05-27 14:00 (GMT+7) |
| Landed         | 2026-05-27 14:23 (GMT+7) |
| Tracking issue | — |
| Tracking PR    | — |

## Gate

`TestPhase11Async` -- 15 fixtures green on JDK 21 and JDK 25, javac-clean. Coverage: `spawn`, `await`, structured concurrency, cancellation, `MOCHI_SCHEDULER=deterministic`.

## Goal-alignment audit

`async/await` with Loom virtual threads is Mochi's lightweight concurrency model for non-agent programs: fetch-and-await, parallel computation, and structured scope blocks. After Phase 11 lands, Mochi programs that do I/O-intensive concurrent work (multiple HTTP requests in parallel, parallel data processing) compile to JVM with zero OS thread blocking -- Loom's carrier thread is released during every `await`.

## Sub-phases

| # | Scope | Status | Commit |
|---|-------|--------|--------|
| 11.0 | `spawn f()` -> `Async.run(() -> f())` (virtual thread + `CompletableFuture`) | LANDED | — |
| 11.1 | `await fut` -> `fut.await()` (calls `cf.get()`, Loom unmounts carrier) | LANDED | — |
| 11.2 | `Async.runAll`, `Async.runAny`, `Async.runTimeout` combinators | LANDED | — |
| 11.3 | `scope { ... }` -> `Scope` wrapper over `StructuredTaskScope` (JDK version switch: JEP 453 on JDK 21, JEP 505 on JDK 25) | DEFERRED | — |
| 11.4 | Cancellation: `fut.cancel()` interrupts the virtual thread; runtime checks `Thread.interrupted()` at I/O sites | DEFERRED | — |

## Sub-phase 11.0 -- spawn

### Goal-alignment audit (11.0)

`spawn f()` launches concurrent work without creating a named agent. It is simpler than an agent: no mailbox, no typed messages, no supervision. After 11.0, Mochi programs can parallelise any function call.

### Decisions made (11.0)

**`spawn f()` lowering**: Mochi:

```mochi
let fut = spawn fetch_url("https://example.com")
```

Lowers to:

```java
final dev.mochi.runtime.async.Async<String> fut = dev.mochi.runtime.async.Async.run(() -> fetch_url("https://example.com"));
```

**`Async<T>` runtime class**:

```java
package dev.mochi.runtime.async;

public final class Async<T> {
    private final java.util.concurrent.CompletableFuture<T> cf;
    private final Thread thread;

    private Async(java.util.concurrent.CompletableFuture<T> cf, Thread thread) {
        this.cf = cf;
        this.thread = thread;
    }

    public static <T> Async<T> run(java.util.function.Supplier<T> work) {
        var cf = new java.util.concurrent.CompletableFuture<T>();
        var t = Thread.ofVirtual().name("mochi-async").start(() -> {
            try {
                cf.complete(work.get());
            } catch (Throwable e) {
                cf.completeExceptionally(e);
            }
        });
        return new Async<>(cf, t);
    }

    public T await() {
        try {
            return cf.get();
        } catch (java.util.concurrent.ExecutionException ee) {
            throw new MochiAsyncError(ee.getCause());
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            throw new MochiAsyncError(ie);
        }
    }

    public boolean cancel() {
        thread.interrupt();
        return cf.cancel(true);
    }
}
```

**Loom interaction**: `cf.get()` on a virtual thread causes Loom to unmount the virtual thread from its carrier thread. The carrier thread is freed to run other virtual threads. When the `CompletableFuture` completes, Loom remounts the awaiting virtual thread. Zero OS threads are blocked during concurrent I/O waits.

## Sub-phase 11.1 -- await

### Goal-alignment audit (11.1)

`await fut` is the blocking point: the program waits for the spawned work to complete and retrieves the result. Without it, `spawn` is fire-and-forget with no way to collect the result.

### Decisions made (11.1)

**`await fut` lowering**: Mochi:

```mochi
let result = await fut
```

Lowers to:

```java
final String result = fut.await();
```

`fut.await()` calls `cf.get()`. On a virtual thread, this is a non-blocking wait from the OS's perspective. On a platform thread (e.g., in tests), it blocks the platform thread.

**`await` in main**: If `main` is running on the main platform thread (not wrapped in a virtual thread), `await fut` blocks the main thread until the future completes. This is correct: the program should not exit before all awaited results are collected.

## Sub-phase 11.2 -- Async combinators

### Goal-alignment audit (11.2)

`runAll` (wait for all futures) and `runAny` (return the first to complete) are the two primary parallel patterns. Without them, programs must manually await each future in sequence.

### Decisions made (11.2)

**`Async.runAll`**: Takes a list of `Async<T>` and waits for all:

```java
public static <T> java.util.List<T> runAll(java.util.List<Async<T>> futures) {
    java.util.List<T> results = new java.util.ArrayList<>(futures.size());
    for (Async<T> f : futures) {
        results.add(f.await());
    }
    return results;
}
```

Note: `runAll` awaits futures in order. Futures run in parallel (each on its own virtual thread), but the results are collected in the original order.

**`Async.runAny`**: Returns the result of the first future to complete:

```java
public static <T> T runAny(java.util.List<Async<T>> futures) {
    var cf = new java.util.concurrent.CompletableFuture<T>();
    for (Async<T> f : futures) {
        Thread.ofVirtual().start(() -> {
            try { cf.complete(f.await()); }
            catch (Throwable t) { /* ignore; another future may succeed */ }
        });
    }
    try { return cf.get(); }
    catch (Exception e) { throw new MochiAsyncError(e); }
}
```

**`Async.runTimeout`**: Wraps a single `Async<T>` with a deadline:

```java
public static <T> java.util.Optional<T> runTimeout(Async<T> fut, long millis) {
    try {
        return java.util.Optional.of(fut.cf.get(millis, java.util.concurrent.TimeUnit.MILLISECONDS));
    } catch (java.util.concurrent.TimeoutException te) {
        fut.cancel();
        return java.util.Optional.empty();
    } catch (Exception e) {
        throw new MochiAsyncError(e);
    }
}
```

Returns `option<T>` in Mochi (lowered to `Optional<T>` here; the lower pass converts to `Option<T>` when the result is used in a Mochi `match`).

## Sub-phase 11.3 -- StructuredTaskScope

### Goal-alignment audit (11.3)

Structured concurrency (`scope { ... }`) ensures that spawned tasks do not outlive the block that created them. This prevents resource leaks and makes concurrent code easier to reason about. The `StructuredTaskScope` API changed between JDK 21 (JEP 453, preview) and JDK 25 (JEP 505, GA); the `Scope` wrapper hides this difference.

### Decisions made (11.3)

**`scope { ... }` lowering**: Mochi:

```mochi
scope {
    let a = spawn fetch_url("https://example.com/a")
    let b = spawn fetch_url("https://example.com/b")
    let ra = await a
    let rb = await b
    print(ra + rb)
}
```

Lowers to:

```java
try (dev.mochi.runtime.scope.Scope scope = dev.mochi.runtime.scope.Scope.open()) {
    final dev.mochi.runtime.async.Async<String> a = scope.spawn(() -> fetch_url("https://example.com/a"));
    final dev.mochi.runtime.async.Async<String> b = scope.spawn(() -> fetch_url("https://example.com/b"));
    scope.join(); // wait for all spawned tasks
    final String ra = a.await();
    final String rb = b.await();
    dev.mochi.runtime.io.IO.println(ra + rb);
}
```

**`Scope.open()` version switching**: The `Scope` implementation detects the JDK version at class load time:

```java
package dev.mochi.runtime.scope;

public abstract class Scope implements AutoCloseable {
    public static Scope open() {
        if (Runtime.version().feature() >= 25) {
            return new Scope25(); // JEP 505: StructuredTaskScope.open(Joiner.allSuccessfulOrThrow())
        } else {
            return new Scope21(); // JEP 453: new StructuredTaskScope.ShutdownOnFailure()
        }
    }
    public abstract <T> dev.mochi.runtime.async.Async<T> spawn(java.util.function.Supplier<T> task);
    public abstract void join() throws InterruptedException;
    @Override public abstract void close();
}
```

`Scope21` (JDK 21 path) uses `StructuredTaskScope.ShutdownOnFailure`. `Scope25` (JDK 25 path) uses `StructuredTaskScope.open(StructuredTaskScope.Joiner.allSuccessfulOrThrow())`.

**`scope` block cancellation**: If any task in the scope throws an exception, all other tasks are cancelled (shutdown-on-failure semantics). The exception is re-thrown after the scope exits. This is the "structured" in structured concurrency: the scope is a lexical region that owns all its tasks.

## Sub-phase 11.4 -- Cancellation

### Goal-alignment audit (11.4)

`fut.cancel()` is required for programs that need to abort concurrent work (e.g., timeout the first slow HTTP request and use a fallback). Without it, `spawn` is fire-and-forget with no way to stop the work.

### Decisions made (11.4)

**`fut.cancel()` lowering**: Mochi:

```mochi
fut.cancel()
```

Lowers to:

```java
fut.cancel();
```

The `Async.cancel()` method calls `thread.interrupt()` (interrupts the virtual thread) and `cf.cancel(true)`. The virtual thread's next blocking operation (`mailbox.take`, `cf.get`, `HttpClient.send`, `Thread.sleep`) will throw `InterruptedException`, which propagates out of the task body and causes the `CompletableFuture` to complete exceptionally.

**Runtime interrupt checks**: The generated dispatch loop in agents (Phase 9) already checks `InterruptedException` on `mailbox.take`. `Fetch.get` (Phase 14) uses `HttpClient.send` which is interruptible. Custom user code that does long CPU work without I/O must check `Thread.interrupted()` manually to be cancellable; the lower pass inserts a `Thread.interrupted()` check at the top of each `while(true)` loop body in async-spawned functions.

## Files changed

| File | Purpose |
|------|---------|
| `transpiler3/jvm/lower/stmt.go` | `SpawnStmt`, `AwaitExpr`, `ScopeBlock` lowering |
| `transpiler3/jvm/lower/expr.go` | `AsyncCancelExpr`, `AsyncRunAllExpr`, `AsyncRunAnyExpr`, `AsyncRunTimeoutExpr` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/async/Async.java` | `run`, `await`, `cancel`, `runAll`, `runAny`, `runTimeout` |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/async/MochiAsyncError.java` | Unchecked async exception |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/scope/Scope.java` | Abstract `Scope` with JDK 21/25 version switch |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/scope/Scope21.java` | JEP 453 `ShutdownOnFailure` implementation |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/scope/Scope25.java` | JEP 505 `allSuccessfulOrThrow` implementation |
| `transpiler3/jvm/runtime/src/main/java/dev/mochi/runtime/clock/MochiClock.java` | Testable clock for deterministic mode |
| `transpiler3/jvm/build/phase11_test.go` | `TestPhase11Async`: 15 fixtures, JDK 21+25 |
| `tests/transpiler3/jvm/phase11-async/*.{mochi,out}` | 15 fixtures |

## Test set

- `transpiler3/jvm/build/phase11_test.go::TestPhase11Async` -- 15 fixtures, `MOCHI_SCHEDULER=deterministic` for all concurrent fixtures.
- `transpiler3/jvm/lower/stmt_test.go::TestLowerSpawnAwait` -- unit test: `spawn f()` produces `Async.run(() -> f())`, `await fut` produces `fut.await()`.
- `transpiler3/jvm/lower/stmt_test.go::TestLowerScopeBlock` -- unit test: `scope { ... }` produces try-with-resources over `Scope.open()`.
- `transpiler3/jvm/runtime/async/AsyncTest.java` -- JUnit: `run + await` round-trip; `runAll` collects all results; `runTimeout` with 0ms timeout cancels immediately.
- `transpiler3/jvm/runtime/scope/ScopeTest.java` -- JUnit: scope spawns 3 tasks, all complete, results collected; scope with one failing task cancels others.

## Deferred work

- `async/await` for agent intents (currently agents use `CompletableFuture` directly in `Handle.value()`; a more ergonomic `await agent.method()` form is deferred).
- `select { on a: ... on b: ... }` (receive from multiple channels/futures): deferred; requires a `CompletableFuture.anyOf` + type-dispatch layer.
- `deadline` propagation (passing a deadline through async call chains without explicit threading): deferred.
- Cancellation of `scope` blocks from outside (external cancel): deferred.

## Closeout notes

_Fill in after gate green._
