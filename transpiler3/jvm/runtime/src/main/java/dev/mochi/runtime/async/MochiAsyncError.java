package dev.mochi.runtime.async;

/**
 * Unchecked exception wrapping errors that occur inside a spawned async task.
 * Phase 11.0.
 */
public final class MochiAsyncError extends RuntimeException {
    public MochiAsyncError(Throwable cause) {
        super(cause);
    }
}
