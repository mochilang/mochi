package dev.mochi.runtime.coll;

/**
 * ListUtil provides helper methods for Mochi list and set operations.
 */
public final class ListUtil {
    private ListUtil() {}

    /**
     * Functional append: returns a new ArrayList that is a copy of xs
     * with elem appended. Preserves the original list unchanged.
     */
    public static <T> java.util.ArrayList<T> append(java.util.List<T> xs, T elem) {
        java.util.ArrayList<T> result = new java.util.ArrayList<>(xs);
        result.add(elem);
        return result;
    }

    /**
     * Mutable set-add: adds elem to the set in place and returns the same set.
     * Used when lowering Mochi's `s = add(s, x)` pattern where the assignment
     * rebinds to the same (mutated) set reference.
     */
    public static <T> java.util.LinkedHashSet<T> setAdd(java.util.LinkedHashSet<T> s, T elem) {
        s.add(elem);
        return s;
    }

    /**
     * Slice: returns a new ArrayList containing elements xs[start..end).
     * Indices are clamped to [0, xs.size()] so out-of-range values are safe.
     * Used for Mochi query `skip N / take N` lowering.
     */
    public static <T> java.util.ArrayList<T> slice(java.util.List<T> xs, long start, long end) {
        int size = xs.size();
        int from = (int) Math.max(0L, Math.min(start, (long) size));
        int to   = (int) Math.max(0L, Math.min(end,   (long) size));
        if (from >= to) return new java.util.ArrayList<>();
        return new java.util.ArrayList<>(xs.subList(from, to));
    }
}
