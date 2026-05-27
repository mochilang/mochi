package dev.mochi.runtime.error;

/** Unchecked panic exception with Mochi error code. */
public final class MochiPanicException extends RuntimeException {
    public final int code;
    public MochiPanicException(int code, String message) {
        super(message);
        this.code = code;
    }
}
