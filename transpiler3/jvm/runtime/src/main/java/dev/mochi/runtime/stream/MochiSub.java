package dev.mochi.runtime.stream;

/**
 * MochiSub is the JVM runtime backing for Mochi sub&lt;T&gt; (subscriber handle).
 *
 * It implements java.util.concurrent.Flow.Subscriber&lt;Object&gt; and buffers
 * incoming items in a LinkedBlockingQueue. The MochiSub.recv() method
 * blocks the calling virtual thread until an item is available.
 *
 * A sentinel object ({@link #DONE}) is placed in the queue when the
 * publisher signals onComplete or onError, causing recv() to return null.
 * Callers should check the return value of recv() when the stream may be
 * closed.
 *
 * Phase 10.
 */
public final class MochiSub implements java.util.concurrent.Flow.Subscriber<Object> {

    /** Sentinel placed in the queue when the stream ends. */
    private static final Object DONE = new Object();

    private final java.util.concurrent.LinkedBlockingQueue<Object> queue;
    private final int limit;
    private volatile java.util.concurrent.Flow.Subscription subscription;

    MochiSub(int limit) {
        this.limit = limit;
        this.queue = new java.util.concurrent.LinkedBlockingQueue<>();
    }

    @Override
    public void onSubscribe(java.util.concurrent.Flow.Subscription s) {
        this.subscription = s;
        s.request(Long.MAX_VALUE); // unbounded demand
    }

    @Override
    public void onNext(Object item) {
        if (limit != Integer.MAX_VALUE && queue.size() >= limit) {
            // Drop the item -- backpressure limit reached.
            return;
        }
        queue.offer(item);
    }

    @Override
    public void onError(Throwable t) {
        queue.offer(DONE); // signal end-of-stream on error
    }

    @Override
    public void onComplete() {
        queue.offer(DONE); // signal clean end-of-stream
    }

    /**
     * Receive the next item from this subscriber, blocking the calling
     * virtual thread until an item is available.
     *
     * Returns null when the stream has ended (onComplete or onError).
     */
    @SuppressWarnings("unchecked")
    public <T> T recv() {
        try {
            Object item = queue.take();
            if (item == DONE) {
                return null;
            }
            return (T) item;
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("stream recv interrupted", ie);
        }
    }
}
