---
title: "10. Async CompletableFuture bridge"
sidebar_position: 11
sidebar_label: "10. Async CompletableFuture bridge"
description: "The FutureRuntime callback registry, AsyncPolicy enum, Java thenAccept to JNI-native dispatch chain, and goroutine scheduling."
---

# 10. Async CompletableFuture bridge

## The problem

`CompletableFuture<T>` is the standard Java async primitive. It completes on a Java thread pool thread, not on the calling goroutine. The bridge must deliver the completion value to Go code without blocking a JVM thread indefinitely or requiring Go code to busy-poll.

## FutureRuntime callback registry

`jni.FutureRuntime` is a registry that maps `int64` handles to `FutureCallback` functions. When a Mochi caller invokes a method that returns a `CompletableFuture<T>`, the bridge:

1. Allocates a `FutureCallback` closure that will send the value to a Go channel.
2. Registers the closure in `FutureRuntime`, receiving an `int64` handle.
3. Passes the handle to the wrapper Java method.

## Java-side wiring

The generated wrapper Java method adds a `thenAccept` completion handler:

```java
future.thenAccept(v -> MochiRuntime.callback(cbHandle, v.toString()));
```

`MochiRuntime.callback` is a JNI native method declared in the wrapper class. When the future completes, the Java thread pool invokes this native method.

## AsyncPolicy

`AsyncPolicy` selects the Go-side dispatch strategy:

- `AsyncBlocking`: the calling goroutine blocks on a channel until the callback fires.
- `AsyncGoroutine`: a new goroutine is launched; the callback sends to a buffered channel the caller selects from.

`AsyncBlocking` is simpler but holds a goroutine for the duration of the Java operation. `AsyncGoroutine` is preferred for operations that may complete after a significant delay.

## Goroutine safety

The JNI native callback fires on a JVM thread, which is not a goroutine. CGO runtime handles the transition (`runtime.LockOSThread` is called inside the CGO dispatch). The `FutureRuntime.Invoke` method dispatches the Go callback in a new goroutine to avoid executing Go code on a JVM-owned thread stack beyond what CGO allows.
