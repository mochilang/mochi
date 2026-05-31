---
title: "08. Async bridge"
sidebar_position: 9
sidebar_label: "08. Async bridge"
description: "CompletableFuture<T> call path, JNI callback threads, thread pool sizing, virtual threads (Java 21 Thread.ofVirtual()), RxJava Observable bridge, Project Reactor Mono/Flux bridge, structured concurrency (Java 21 StructuredTaskScope), Mochi's async colour model, the impedance mismatch, and the chosen blocking-get approach."
---

# 08. Async bridge

This note covers how MEP-67 bridges Java's asynchronous programming models (CompletableFuture, RxJava, Project Reactor) to Mochi's `async` keyword.

## 1. Mochi's async model

Mochi's `async` colour (introduced in MEP-48) marks functions that may suspend. An `async fun f(): T` returns a future value that the caller can `await`. In the MEP-53/54 Rust/Go targets, `async` lowers to the host language's native async model (Rust's async fn / Go's goroutine). The bridge does not require the user to understand how the async lowering works; they write `await myJavaMethod()` and the bridge handles the rest.

## 2. Java's async models

Java has multiple async programming models, each bridged differently:

### CompletableFuture (Java 8+)

`java.util.concurrent.CompletableFuture<T>` is Java's primary async primitive. Methods that perform I/O or computation asynchronously return a `CompletableFuture<T>` that completes with a value `T` or an exception.

```java
CompletableFuture<String> future = httpClient.sendAsync(request, BodyHandlers.ofString())
    .thenApply(HttpResponse::body);
String body = future.get(); // blocks until complete
```

### RxJava 3 (io.reactivex.rxjava3)

RxJava provides an event-stream model: `Observable<T>` emits zero or more items, `Single<T>` emits exactly one, `Completable` emits nothing (just completion). Libraries like Retrofit and the AWS SDK offer RxJava adapters.

### Project Reactor (reactor.core)

Reactor provides `Mono<T>` (0 or 1 items) and `Flux<T>` (0 or N items). Spring WebFlux uses Reactor extensively. Reactor operators are lazy and execute when subscribed.

### Java 21 StructuredTaskScope

Java 21 introduces `java.util.concurrent.StructuredTaskScope` for structured concurrency: fork multiple tasks, then join all results at a defined scope boundary.

## 3. The impedance mismatch

Mochi's async model is colour-based (inspired by Rust async and Go goroutines): a function is either async or not, and the `await` at the call site explicitly suspends the caller. Java's async models are callback-based or subscription-based: the result is delivered via a callback, which may run on a different thread.

The cleanest resolution is not to bridge the full async programming model, but to make async Java methods look synchronous to Mochi (by blocking on the result) while ensuring the block does not tie up a platform thread unnecessarily.

## 4. The chosen approach: blocking .get() on a virtual thread

For `CompletableFuture<T>`-returning methods:

1. The JNI bridge submits the Java method call to a thread pool.
2. The Java method runs and returns a `CompletableFuture<T>`.
3. The bridge calls `future.get()` (blocking) on the pool thread.
4. When `get()` returns, the result is converted to the Mochi type and sent back via a Go channel.
5. The Mochi `await` site receives the value from the channel.

On Java 17: the thread pool is a fixed-size `ExecutorService` (default 4 threads; configurable via `[java] async-threads`). A thread is held during `future.get()`. If all 4 threads are blocked on futures, the 5th async call blocks until a thread frees up.

On Java 21: the thread pool is `Executors.newVirtualThreadPerTaskExecutor()`. Virtual threads (Project Loom) are lightweight: the JVM creates thousands of them without exhausting platform threads. `future.get()` on a virtual thread mounts the virtual thread to a platform thread during the blocking phase, but if the future's completion happens on a different thread (which is typical for CompletableFuture), the virtual thread may unmount from the platform thread while waiting. This avoids pinning the platform thread.

**Pinning caveat**: virtual threads pin the underlying platform thread when executing `synchronized` blocks. If the `CompletableFuture` internally uses `synchronized` (which some pre-Java 21 implementations do in `CompletableFuture.join()`), the virtual thread will pin during `.get()`. This is a known limitation of Project Loom's first release. Java 21's `CompletableFuture` has been partially updated to use `ReentrantLock` instead of `synchronized`, but third-party code may still pin.

## 5. Go-side channel bridge

The JNI bridge is implemented in Go via cgo:

```go
// package3/java/jni/async.go

type asyncResult struct {
    value unsafe.Pointer // JNI jobject global ref
    err   error
}

func callJavaMethodAsync(env *C.JNIEnv, obj C.jobject, methodID C.jmethodID, args []C.jvalue) <-chan asyncResult {
    ch := make(chan asyncResult, 1)
    go func() {
        // Submit to the Java ExecutorService via JNI
        future := submitToExecutor(env, obj, methodID, args)
        // Block on future.get() (in the goroutine, not blocking the Mochi main thread)
        result, err := getFutureResult(env, future)
        ch <- asyncResult{result, err}
    }()
    return ch
}
```

The Mochi `await` keyword compiles to a Go channel receive on this channel. The goroutine that calls `getFutureResult` is a Go goroutine backed by a Java virtual thread (on Java 21) or a Java platform thread (on Java 17). The Go scheduler can handle many goroutines; the bottleneck is the Java thread pool.

## 6. RxJava bridge

For `io.reactivex.rxjava3.core.Single<T>`-returning methods:

```java
// JNI wrapper generated by the bridge
public static Object mochi_rx_observable_blockingFirst(io.reactivex.rxjava3.core.Observable<?> obs) {
    return obs.blockingFirst();
}

public static List<?> mochi_rx_observable_blockingList(io.reactivex.rxjava3.core.Observable<?> obs) {
    return obs.toList().blockingGet();
}

public static Object mochi_rx_single_blockingGet(io.reactivex.rxjava3.core.Single<?> single) {
    return single.blockingGet();
}
```

`blockingFirst()` and `blockingGet()` internally block the calling thread (like `CompletableFuture.get()`). The bridge calls them from a virtual/platform thread pool thread, not from the JNI callback thread directly.

## 7. Project Reactor bridge

For `reactor.core.publisher.Mono<T>`-returning methods:

```java
public static Object mochi_reactor_mono_block(reactor.core.publisher.Mono<?> mono) {
    return mono.block(); // blocks indefinitely (no timeout in v1)
}

public static List<?> mochi_reactor_flux_blockList(reactor.core.publisher.Flux<?> flux) {
    return flux.collectList().block();
}
```

`Mono.block()` throws a `java.lang.IllegalStateException` if called from within a Reactor scheduler context (the "block() on a non-blocking thread" error). The bridge ensures it calls `block()` from a non-Reactor thread (the Java pool thread is a plain thread, not a Reactor scheduler thread).

## 8. Java 21 StructuredTaskScope bridge

Java 21's `StructuredTaskScope<T>` is not directly bridged in MEP-67 v1. It represents a structured-concurrency API where multiple tasks are forked within a scope, and the scope does not exit until all tasks complete or the scope fails. Bridging this to Mochi's async model would require Mochi to express structured concurrency scopes, which is a larger design question. SkipReport entries are emitted for methods that return `StructuredTaskScope`-related types.

## 9. Exception propagation from async methods

If the Java async method completes exceptionally:

- `CompletableFuture.get()` throws `ExecutionException` wrapping the original exception.
- `Observable.blockingFirst()` throws the exception directly (unwrapped for unchecked; wrapped in `RuntimeException` for checked).
- `Mono.block()` throws the exception directly.

In all cases, the bridge catches the exception at the JNI boundary, constructs a Mochi `error` value (with the Java exception class name and message), and returns it as the error path of the Mochi `T|error` return type (when the method is declared as throwing checked exceptions) or panics (for unexpected runtime exceptions without a declared error return).

## 10. Timeout configuration

`CompletableFuture.get()` with no timeout blocks indefinitely. The bridge adds a configurable timeout:

```toml
[java]
async-timeout-ms = 30000   # 30 seconds; 0 means no timeout (default 0)
```

When `async-timeout-ms > 0`, the bridge calls `future.get(timeout, TimeUnit.MILLISECONDS)` which throws `TimeoutException` if the future does not complete within the timeout. The bridge translates `TimeoutException` to a Mochi `error` value with message `"Java async call timed out after 30000ms"`.
