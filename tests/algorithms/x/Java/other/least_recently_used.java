public class Main {
    static class LRUCache {
        int max_capacity;
        String[] store;
        LRUCache(int max_capacity, String[] store) {
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

    static LRUCache new_cache(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("n should be an integer greater than 0."));
        }
        int cap = n == 0 ? 2147483647 : n;
        return new LRUCache(cap, new String[]{});
    }

    static String[] remove_element(String[] xs, String x) {
        String[] res = ((String[])(new String[]{}));
        boolean removed = false;
        int i = 0;
        while (i < xs.length) {
            String v = xs[i];
            if (removed == false && (v.equals(x))) {
                removed = true;
            } else {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new String[]{v})).toArray(String[]::new)));
            }
            i = i + 1;
        }
        return res;
    }

    static LRUCache refer(LRUCache cache, String x) {
        String[] store = ((String[])(cache.store));
        boolean exists = false;
        int i_1 = 0;
        while (i_1 < store.length) {
            if ((store[i_1].equals(x))) {
                exists = true;
            }
            i_1 = i_1 + 1;
        }
        if (exists) {
            store = ((String[])(remove_element(((String[])(store)), x)));
        } else         if (store.length == cache.max_capacity) {
            String[] new_store = ((String[])(new String[]{}));
            int j = 0;
            while (j < store.length - 1) {
                new_store = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_store), java.util.Arrays.stream(new String[]{store[j]})).toArray(String[]::new)));
                j = j + 1;
            }
            store = ((String[])(new_store));
        }
        store = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new String[]{x}), java.util.Arrays.stream(store)).toArray(String[]::new)));
        return new LRUCache(cache.max_capacity, store);
    }

    static void display(LRUCache cache) {
        int i_2 = 0;
        while (i_2 < cache.store.length) {
            System.out.println(cache.store[i_2]);
            i_2 = i_2 + 1;
        }
    }

    static String repr_item(String s) {
        boolean all_digits = true;
        int i_3 = 0;
        while (i_3 < _runeLen(s)) {
            String ch = s.substring(i_3, i_3+1);
            if ((ch.compareTo("0") < 0) || (ch.compareTo("9") > 0)) {
                all_digits = false;
            }
            i_3 = i_3 + 1;
        }
        if (all_digits) {
            return s;
        }
        return "'" + s + "'";
    }

    static String cache_repr(LRUCache cache) {
        String res_1 = "LRUCache(" + _p(cache.max_capacity) + ") => [";
        int i_4 = 0;
        while (i_4 < cache.store.length) {
            res_1 = res_1 + String.valueOf(repr_item(cache.store[i_4]));
            if (i_4 < cache.store.length - 1) {
                res_1 = res_1 + ", ";
            }
            i_4 = i_4 + 1;
        }
        res_1 = res_1 + "]";
        return res_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            lru = new_cache(4);
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
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
        return String.valueOf(v);
    }
}
