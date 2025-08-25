public class Main {
    static class LRUCache {
        java.math.BigInteger max_capacity;
        String[] store;
        LRUCache(java.math.BigInteger max_capacity, String[] store) {
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

    static LRUCache new_cache(java.math.BigInteger n) {
        if (n.compareTo(java.math.BigInteger.valueOf(0)) < 0) {
            throw new RuntimeException(String.valueOf("n should be an integer greater than 0."));
        }
        java.math.BigInteger cap_1 = new java.math.BigInteger(String.valueOf(n.compareTo(java.math.BigInteger.valueOf(0)) == 0 ? 2147483647 : n));
        return new LRUCache(new java.math.BigInteger(String.valueOf(cap_1)), ((String[])(new String[]{})));
    }

    static String[] remove_element(String[] xs, String x) {
        String[] res = ((String[])(new String[]{}));
        boolean removed_1 = false;
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(xs.length))) < 0) {
            String v_1 = xs[_idx((xs).length, ((java.math.BigInteger)(i_1)).longValue())];
            if ((removed_1 == false) && (v_1.equals(x))) {
                removed_1 = true;
            } else {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new String[]{v_1})).toArray(String[]::new)));
            }
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return ((String[])(res));
    }

    static LRUCache refer(LRUCache cache, String x) {
        String[] store = ((String[])(cache.store));
        boolean exists_1 = false;
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(store.length))) < 0) {
            if ((store[_idx((store).length, ((java.math.BigInteger)(i_3)).longValue())].equals(x))) {
                exists_1 = true;
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        if (exists_1) {
            store = ((String[])(remove_element(((String[])(store)), x)));
        } else         if (new java.math.BigInteger(String.valueOf(store.length)).compareTo(cache.max_capacity) == 0) {
            String[] new_store_1 = ((String[])(new String[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(store.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                new_store_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_store_1), java.util.Arrays.stream(new String[]{store[_idx((store).length, ((java.math.BigInteger)(j_1)).longValue())]})).toArray(String[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            store = ((String[])(new_store_1));
        }
        store = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new String[]{x}), java.util.Arrays.stream(store)).toArray(String[]::new)));
        return new LRUCache(new java.math.BigInteger(String.valueOf(cache.max_capacity)), ((String[])(store)));
    }

    static void display(LRUCache cache) {
        java.math.BigInteger i_4 = java.math.BigInteger.valueOf(0);
        while (i_4.compareTo(new java.math.BigInteger(String.valueOf(cache.store.length))) < 0) {
            System.out.println(cache.store[_idx((cache.store).length, ((java.math.BigInteger)(i_4)).longValue())]);
            i_4 = new java.math.BigInteger(String.valueOf(i_4.add(java.math.BigInteger.valueOf(1))));
        }
    }

    static String repr_item(String s) {
        boolean all_digits = true;
        java.math.BigInteger i_6 = java.math.BigInteger.valueOf(0);
        while (i_6.compareTo(new java.math.BigInteger(String.valueOf(_runeLen(s)))) < 0) {
            String ch_1 = s.substring((int)(((java.math.BigInteger)(i_6)).longValue()), (int)(((java.math.BigInteger)(i_6)).longValue())+1);
            if ((ch_1.compareTo("0") < 0) || (ch_1.compareTo("9") > 0)) {
                all_digits = false;
            }
            i_6 = new java.math.BigInteger(String.valueOf(i_6.add(java.math.BigInteger.valueOf(1))));
        }
        if (all_digits) {
            return s;
        }
        return "'" + s + "'";
    }

    static String cache_repr(LRUCache cache) {
        String res_1 = "LRUCache(" + _p(cache.max_capacity) + ") => [";
        java.math.BigInteger i_8 = java.math.BigInteger.valueOf(0);
        while (i_8.compareTo(new java.math.BigInteger(String.valueOf(cache.store.length))) < 0) {
            res_1 = res_1 + String.valueOf(repr_item(cache.store[_idx((cache.store).length, ((java.math.BigInteger)(i_8)).longValue())]));
            if (i_8.compareTo(new java.math.BigInteger(String.valueOf(cache.store.length)).subtract(java.math.BigInteger.valueOf(1))) < 0) {
                res_1 = res_1 + ", ";
            }
            i_8 = new java.math.BigInteger(String.valueOf(i_8.add(java.math.BigInteger.valueOf(1))));
        }
        res_1 = res_1 + "]";
        return res_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            lru = new_cache(java.math.BigInteger.valueOf(4));
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
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
