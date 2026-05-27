package dev.mochi.runtime.math;

import dev.mochi.runtime.error.MochiPanicException;

/** Integer math helpers for Mochi JVM. */
public final class IntMath {
    private IntMath() {}

    public static long div(long a, long b) {
        try {
            return a / b;
        } catch (ArithmeticException e) {
            throw new MochiPanicException(5, "integer divide by zero");
        }
    }

    public static long mod(long a, long b) {
        try {
            return a % b;
        } catch (ArithmeticException e) {
            throw new MochiPanicException(5, "integer divide by zero");
        }
    }
}
