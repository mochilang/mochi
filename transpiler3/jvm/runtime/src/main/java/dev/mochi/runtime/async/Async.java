package dev.mochi.runtime.async;

import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Supplier;

/**
 * Async&lt;T&gt; is the JVM runtime backing for Mochi future&lt;T&gt;.
 *
 * spawn expr lowers to Async.run(() -&gt; expr).
 * await fut  lowers to fut.await().
 *
 * Each spawned task runs on a Loom virtual thread; carrier threads are
 * released while the virtual thread blocks on await().
 *
 * Phase 11.0 (run/await/cancel) + 11.2 (runAll/runAny).
 */
public final class Async<T> {

    private final CompletableFuture<T> cf;
    private final Thread thread;

    private Async(CompletableFuture<T> cf, Thread thread) {
        this.cf = cf;
        this.thread = thread;
    }

    /**
     * Spawns a new virtual thread to run {@code work} and returns an Async
     * handle. Phase 11.0.
     */
    public static <T> Async<T> run(Supplier<T> work) {
        var cf = new CompletableFuture<T>();
        var t = Thread.ofVirtual().name("mochi-async").start(() -> {
            try {
                cf.complete(work.get());
            } catch (Throwable e) {
                cf.completeExceptionally(e);
            }
        });
        return new Async<>(cf, t);
    }

    /**
     * Blocks until the async task completes and returns its result.
     * On a virtual thread Loom unmounts the carrier while waiting.
     * Phase 11.1.
     */
    public T await() {
        try {
            return cf.get();
        } catch (ExecutionException ee) {
            throw new MochiAsyncError(ee.getCause());
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            throw new MochiAsyncError(ie);
        }
    }

    /**
     * Cancels the async task by interrupting its virtual thread. Phase 11.4.
     */
    public boolean cancel() {
        thread.interrupt();
        return cf.cancel(true);
    }

    /**
     * Waits for all futures in {@code futures} to complete and returns their
     * results in the same order. Phase 11.2.
     */
    public static <T> List<T> runAll(List<Async<T>> futures) {
        List<T> results = new ArrayList<>(futures.size());
        for (Async<T> f : futures) {
            results.add(f.await());
        }
        return results;
    }

    /**
     * Returns the result of the first future in {@code futures} to complete.
     * Phase 11.2.
     */
    public static <T> T runAny(List<Async<T>> futures) {
        var combined = new CompletableFuture<T>();
        for (Async<T> f : futures) {
            Thread.ofVirtual().start(() -> {
                try {
                    combined.complete(f.await());
                } catch (Throwable t) {
                    // ignore; another future may succeed
                }
            });
        }
        try {
            return combined.get();
        } catch (Exception e) {
            throw new MochiAsyncError(e);
        }
    }
}
