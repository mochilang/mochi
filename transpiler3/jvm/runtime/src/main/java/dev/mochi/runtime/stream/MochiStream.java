package dev.mochi.runtime.stream;

/**
 * MochiStream is the JVM runtime backing for Mochi stream&lt;T&gt;.
 *
 * Internally it wraps a java.util.concurrent.SubmissionPublisher&lt;Object&gt;.
 * Each call to subscribe() or subscribeWithLimit() creates a MochiSub that
 * buffers received items in a LinkedBlockingQueue, so subscribers can call
 * MochiSub.recv() on a virtual thread without blocking a carrier thread.
 *
 * The publisher runs on a virtual-thread-per-task executor, so each onNext
 * dispatch is independent.
 *
 * Phase 10.
 */
public final class MochiStream {

    private final java.util.concurrent.SubmissionPublisher<Object> publisher;

    private MochiStream(int capacity) {
        this.publisher = new java.util.concurrent.SubmissionPublisher<>(
            java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor(),
            capacity
        );
    }

    /**
     * Create a new MochiStream with the given buffer capacity per subscriber.
     */
    public static MochiStream create(int capacity) {
        return new MochiStream(capacity);
    }

    /**
     * Emit (publish) an item to all current subscribers.
     * Blocks the calling virtual thread using SubmissionPublisher.submit()
     * (BLOCK backpressure: waits until all subscribers have buffer space).
     */
    public void emit(Object item) throws InterruptedException {
        publisher.submit(item);
    }

    /**
     * Subscribe to this stream with unbounded demand.
     * Returns a MochiSub from which the caller can call recv() to receive items.
     */
    public MochiSub subscribe() {
        MochiSub sub = new MochiSub(Integer.MAX_VALUE);
        publisher.subscribe(sub);
        return sub;
    }

    /**
     * Subscribe to this stream with a bounded drop threshold.
     * Items are dropped (not buffered) when the subscriber's internal queue
     * already holds {@code limit} items.
     */
    public MochiSub subscribeWithLimit(int limit) {
        MochiSub sub = new MochiSub(limit);
        publisher.subscribe(sub);
        return sub;
    }

    /**
     * Close this stream. Any subsequent emit() calls will be rejected.
     * All subscribed MochiSubs will receive an onComplete signal.
     */
    public void close() {
        publisher.close();
    }
}
