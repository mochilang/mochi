public class Main {
    static class LRUCache {
        long max_capacity;
        String[] store;
        LRUCache(long max_capacity, String[] store) {
            this.max_capacity = max_capacity;
            this.store = store;
        }
        LRUCache() {}
        @Override public String toString() {
            return String.format("{'max_capacity': %s, 'store': %s}", String.valueOf(max_capacity), String.valueOf(store));
        }
    }

    static LRUCache lru = null;
    static String r = null;

    static LRUCache new_cache(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("n should be an integer greater than 0."));
        }
        long cap_1 = (long)((long)(n) == 0L ? 2147483647 : n);
        return new LRUCache(cap_1, new String[]{});
    }

    static String[] remove_element(String[] xs, String x) {
        String[] res = ((String[])(new String[]{}));
        boolean removed_1 = false;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs.length)) {
            String v_1 = xs[(int)((long)(i_1))];
            if ((removed_1 == false) && (v_1.equals(x))) {
                removed_1 = true;
            } else {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new String[]{v_1})).toArray(String[]::new)));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static LRUCache refer(LRUCache cache, String x) {
        String[] store = ((String[])(cache.store));
        boolean exists_1 = false;
        long i_3 = 0L;
        while ((long)(i_3) < (long)(store.length)) {
            if ((store[(int)((long)(i_3))].equals(x))) {
                exists_1 = true;
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        if (exists_1) {
            store = ((String[])(remove_element(((String[])(store)), x)));
        } else         if ((long)(store.length) == (long)(cache.max_capacity)) {
            String[] new_store_1 = ((String[])(new String[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)((long)(store.length) - 1L)) {
                new_store_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_store_1), java.util.Arrays.stream(new String[]{store[(int)((long)(j_1))]})).toArray(String[]::new)));
                j_1 = (long)((long)(j_1) + 1L);
            }
            store = ((String[])(new_store_1));
        }
        store = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new String[]{x}), java.util.Arrays.stream(store)).toArray(String[]::new)));
        return new LRUCache(cache.max_capacity, store);
    }

    static void display(LRUCache cache) {
        long i_4 = 0L;
        while ((long)(i_4) < (long)(cache.store.length)) {
            System.out.println(cache.store[(int)((long)(i_4))]);
            i_4 = (long)((long)(i_4) + 1L);
        }
    }

    static String repr_item(String s) {
        boolean all_digits = true;
        long i_6 = 0L;
        while ((long)(i_6) < (long)(_runeLen(s))) {
            String ch_1 = s.substring((int)((long)(i_6)), (int)((long)(i_6))+1);
            if ((ch_1.compareTo("0") < 0) || (ch_1.compareTo("9") > 0)) {
                all_digits = false;
            }
            i_6 = (long)((long)(i_6) + 1L);
        }
        if (all_digits) {
            return s;
        }
        return "'" + s + "'";
    }

    static String cache_repr(LRUCache cache) {
        String res_1 = "LRUCache(" + _p(cache.max_capacity) + ") => [";
        long i_8 = 0L;
        while ((long)(i_8) < (long)(cache.store.length)) {
            res_1 = res_1 + String.valueOf(repr_item(cache.store[(int)((long)(i_8))]));
            if ((long)(i_8) < (long)((long)(cache.store.length) - 1L)) {
                res_1 = res_1 + ", ";
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
        res_1 = res_1 + "]";
        return res_1;
    }
    public static void main(String[] args) {
        lru = new_cache(4L);
        lru = refer(lru, "A");
        lru = refer(lru, "2");
        lru = refer(lru, "3");
        lru = refer(lru, "A");
        lru = refer(lru, "4");
        lru = refer(lru, "5");
        r = String.valueOf(cache_repr(lru));
        System.out.println(r);
        if (!(r.equals("LRUCache(4) => [5, 4, 'A', 3]"))) {
            throw new RuntimeException(String.valueOf("Assertion error"));
        }
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
