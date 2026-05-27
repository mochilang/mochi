package dev.mochi.runtime.coll;

/**
 * MapUtil provides factory helpers for Mochi map literals.
 * Maps are backed by LinkedHashMap so insertion order is preserved.
 */
public final class MapUtil {
    private MapUtil() {}

    /**
     * Creates a LinkedHashMap from interleaved key-value pairs.
     * MapUtil.of(k1, v1, k2, v2, ...) builds a map with those entries
     * in insertion order.
     */
    @SuppressWarnings("unchecked")
    public static <K, V> java.util.LinkedHashMap<K, V> of(Object... kvs) {
        java.util.LinkedHashMap<K, V> m = new java.util.LinkedHashMap<>(kvs.length / 2);
        for (int i = 0; i < kvs.length; i += 2) {
            m.put((K) kvs[i], (V) kvs[i + 1]);
        }
        return m;
    }

    /** Creates an empty LinkedHashMap. */
    public static <K, V> java.util.LinkedHashMap<K, V> empty() {
        return new java.util.LinkedHashMap<>();
    }
}
