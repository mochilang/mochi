package dev.mochi.runtime.str;

/** String helpers for Mochi JVM. */
public final class StringOps {
    private StringOps() {}

    /** Returns the number of Unicode code points in s. */
    public static long len(String s) {
        return s.codePointCount(0, s.length());
    }

    /** Returns the i-th Unicode code point as a long. */
    public static long charAt(String s, long i) {
        int idx = s.offsetByCodePoints(0, (int) i);
        return s.codePointAt(idx);
    }

    /** Returns true if s contains sub as a substring. */
    public static boolean contains(String s, String sub) {
        return s.contains(sub);
    }
}
