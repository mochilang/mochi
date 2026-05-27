package dev.mochi.runtime.stream;

/**
 * ChanUtil provides blocking channel operations for Mochi channel types.
 *
 * Mochi channels are backed by java.util.concurrent.LinkedBlockingQueue.
 * ChanUtil.take() wraps the InterruptedException so callers on virtual threads
 * do not need to handle it explicitly. The interrupt flag is restored on
 * interruption so the virtual thread scheduler can observe it.
 *
 * Phase 10.
 */
public final class ChanUtil {

    private ChanUtil() {}

    /**
     * Take (receive) one element from the channel, blocking the calling
     * virtual thread until an element is available.
     *
     * @param queue the LinkedBlockingQueue to receive from
     * @return the element taken from the queue
     * @throws RuntimeException wrapping InterruptedException on thread interruption
     */
    @SuppressWarnings("unchecked")
    public static <T> T take(java.util.concurrent.LinkedBlockingQueue<T> queue) {
        try {
            return queue.take();
        } catch (InterruptedException ie) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("channel recv interrupted", ie);
        }
    }
}
