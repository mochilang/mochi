---
title: "08. Coroutines bridge"
sidebar_position: 9
sidebar_label: "08. Coroutines bridge"
description: "Kotlin coroutines architecture, the suspend keyword's CPS transformation, the blocking call adapter (runBlocking), the event-loop dispatch adapter, cancellation semantics across the C ABI boundary, and the kotlinx.coroutines.flow.Flow consumer pattern."
---

# 08. Coroutines bridge

Kotlin's coroutines are the primary concurrency mechanism in modern Kotlin code. Most Ktor endpoints, all kotlinx-datetime I/O operations, and the entire Jetpack libraries on Android use `suspend` functions. A Kotlin bridge that cannot call `suspend` functions is unusable for a large fraction of the corpus. This note explains the coroutines architecture, the two dispatch modes, and the cancellation story.

## What `suspend` compiles to

The Kotlin compiler transforms every `suspend` function into a state machine using continuation-passing style (CPS). The JVM bytecode signature of:

```kotlin
suspend fun fetchUser(id: Long): User
```

becomes:

```java
Object fetchUser(long id, Continuation<? super User> continuation)
```

The `Continuation<T>` parameter is a callback: when the coroutine completes (or suspends), it calls `continuation.resumeWith(result)`. The function returns `COROUTINE_SUSPENDED` if it suspended, or the result directly if it completed synchronously.

This means `suspend` functions are not directly callable from C without a `Continuation` implementation that bridges the coroutine lifecycle to the native side.

## The blocking adapter (coroutines-dispatcher = "blocking")

The simplest bridge for `suspend` functions: wrap the call in `kotlinx.coroutines.runBlocking`, which starts a coroutine on the calling thread and blocks until the result is ready.

The bridge generates a JNI wrapper function like:

```java
// Generated Java JNI wrapper
public static String mochi_fetchUser(long id) {
    return (String) RunBlockingKt.runBlocking(
        EmptyCoroutineContext.INSTANCE,
        (scope, continuation) -> UserService.INSTANCE.fetchUser(id, continuation)
    );
}
```

The GraalVM native image compiles this to a C-callable function:

```c
jstring mochi_fetchUser(graal_isolatethread_t* thread, jlong id);
```

The bridge emits:

```mochi
extern fn user_service_fetch_user(id: long): string from kotlin "com.example.UserService.fetchUser"
```

**Trade-offs of blocking mode:**
- Simple to reason about: each Kotlin `suspend` call is a synchronous Mochi call.
- Safe for I/O-bound calls: the calling thread blocks, but the coroutine dispatcher can run other coroutines internally.
- Dangerous for CPU-bound calls or for calls made on the Mochi runtime's own thread pool: nested `runBlocking` can deadlock if the caller's thread is the same dispatcher the coroutine needs.
- Recommended for: HTTP clients, database queries, simple async I/O.

## The event-loop adapter (coroutines-dispatcher = "event-loop")

For Mochi programs with their own event loop (MEP-48 Channel agents, async-coloured functions from MEP-48), the blocking adapter wastes threads. The event-loop mode returns a handle immediately and signals completion asynchronously.

The bridge generates:

```java
// Generated Java JNI wrapper
public static long mochi_fetchUser_async(long id) {
    Deferred<String> deferred = GlobalScope.async(
        MochiEventLoop.dispatcher(),
        (scope, continuation) -> UserService.INSTANCE.fetchUser(id, continuation)
    );
    return MochiHandleRegistry.register(deferred);  // returns a long handle ID
}

public static boolean mochi_fetchUser_poll(long handle, String[] resultOut) {
    Deferred<String> deferred = (Deferred<String>) MochiHandleRegistry.get(handle);
    if (deferred.isCompleted()) {
        resultOut[0] = deferred.getCompleted();
        MochiHandleRegistry.release(handle);
        return true;
    }
    return false;
}
```

The Mochi shim exposes:

```mochi
extern fn user_service_fetch_user_async(id: long): Handle from kotlin "com.example.UserService.fetchUser_async"
extern fn user_service_fetch_user_poll(handle: Handle, out: *string): bool from kotlin "com.example.UserService.fetchUser_poll"
```

**Handle registry:** All in-flight coroutine `Deferred` objects are stored in `MochiHandleRegistry`, a thread-safe `ConcurrentHashMap<Long, Any>` inside the native image. The handle ID is a monotonically incrementing `AtomicLong`. The Mochi runtime polls by calling the `_poll` function from its own event loop.

**Trade-offs of event-loop mode:**
- Non-blocking: the calling thread returns immediately.
- Requires the Mochi runtime to have a polling mechanism (MEP-48 async or a custom event loop).
- Each outstanding coroutine holds a reference in the handle registry; the user must call `_poll` until it returns `true` to avoid leaks.
- Suitable for: Mochi async programs, high-concurrency servers.

## Cancellation

Kotlin coroutines support cooperative cancellation via `cancel()` on the `Job` associated with a coroutine. The bridge exposes cancellation for event-loop handles:

```mochi
extern fn kotlin_handle_cancel(handle: Handle) from kotlin "com.example.MochiHandleRegistry.cancel"
```

The `cancel()` call throws `CancellationException` inside the coroutine, which Kotlin propagates cooperatively (only at `yield` / `delay` / `withContext` points). If the coroutine is in a blocking native call (e.g., a blocking socket read), cancellation does not interrupt it immediately; the next suspension point will detect the cancellation.

Cancellation for blocking-mode calls is not supported: `runBlocking` runs to completion; there is no handle to cancel on. The bridge documents this: blocking-mode `suspend` calls cannot be cancelled.

## Flow<T>

`kotlinx.coroutines.flow.Flow<T>` is Kotlin's cold asynchronous stream. Bridging a `Flow` to the Mochi side requires collecting its values one at a time:

The bridge generates a two-function pattern per `Flow<T>`-returning function:

```mochi
extern fn user_service_watch_users_start(): FlowHandle from kotlin "com.example.UserService.watchUsers.start"
extern fn user_service_watch_users_next(h: FlowHandle, out: *User): FlowResult from kotlin "com.example.UserService.watchUsers.next"
// FlowResult: 0=ok (out written), 1=end of stream, 2=error
```

The `start` function launches the `Flow` collector on a dedicated coroutine. The `next` function returns the next emitted value. This is a pull-based consumer: the Mochi side controls the pace. The underlying Flow runs eagerly; the bridge buffers emitted values in a bounded `kotlinx.coroutines.channels.Channel<T>` (default buffer size: 64; configurable via `[kotlin] flow-buffer = 64`).

`SharedFlow` and `StateFlow` (hot flows) are bridged the same way; the distinction (cold vs. hot) is transparent from the Mochi side.

## The coroutines dispatcher inside the native image

GraalVM Native Image supports `kotlinx.coroutines` and its default dispatcher (`Dispatchers.Default`, a shared ForkJoinPool). The bridge initialises a `kotlinx.coroutines.EventLoop` on a background thread inside the native image during `graal_create_isolate`. This background thread runs for the lifetime of the process.

For `runtime = "graalvm"`, all coroutine dispatch happens inside the native image's isolated heap. The GC, threads, and memory are isolated from the Mochi Go runtime. GC pauses inside the native image do not pause the Go garbage collector or the Mochi main thread; they are isolated to the image's thread pool.

For `runtime = "jvm-embed"`, the `kotlinx.coroutines.Dispatchers.Default` pool shares OS threads with the embedded JVM's other coroutines. The bridge sets the default dispatcher pool size to `GOMAXPROCS` to avoid over-subscribing CPU cores.

## Cross-references

- [[09-jvm-abi-stability]] for the GraalVM native-image threading model.
- [[05-type-mapping]] for the `suspend fun` refusal when no coroutines bridge is configured.
- [MEP-70 §5.2](/docs/mep/mep-0070#52-kotlin) for the `coroutines-dispatcher` manifest key.
- [MEP-48](/docs/mep/mep-0048) for the Mochi async colour model that event-loop mode integrates with.
