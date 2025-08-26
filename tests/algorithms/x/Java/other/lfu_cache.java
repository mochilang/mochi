public class Main {
    static class Entry {
        java.math.BigInteger key;
        java.math.BigInteger val;
        java.math.BigInteger freq;
        java.math.BigInteger order;
        Entry(java.math.BigInteger key, java.math.BigInteger val, java.math.BigInteger freq, java.math.BigInteger order) {
            this.key = key;
            this.val = val;
            this.freq = freq;
            this.order = order;
        }
        Entry() {}
        @Override public String toString() {
            return String.format("{'key': %s, 'val': %s, 'freq': %s, 'order': %s}", String.valueOf(key), String.valueOf(val), String.valueOf(freq), String.valueOf(order));
        }
    }

    static class LFUCache {
        Entry[] entries;
        java.math.BigInteger capacity;
        java.math.BigInteger hits;
        java.math.BigInteger miss;
        java.math.BigInteger tick;
        LFUCache(Entry[] entries, java.math.BigInteger capacity, java.math.BigInteger hits, java.math.BigInteger miss, java.math.BigInteger tick) {
            this.entries = entries;
            this.capacity = capacity;
            this.hits = hits;
            this.miss = miss;
            this.tick = tick;
        }
        LFUCache() {}
        @Override public String toString() {
            return String.format("{'entries': %s, 'capacity': %s, 'hits': %s, 'miss': %s, 'tick': %s}", String.valueOf(entries), String.valueOf(capacity), String.valueOf(hits), String.valueOf(miss), String.valueOf(tick));
        }
    }

    static class GetResult {
        LFUCache cache;
        java.math.BigInteger value;
        boolean ok;
        GetResult(LFUCache cache, java.math.BigInteger value, boolean ok) {
            this.cache = cache;
            this.value = value;
            this.ok = ok;
        }
        GetResult() {}
        @Override public String toString() {
            return String.format("{'cache': %s, 'value': %s, 'ok': %s}", String.valueOf(cache), String.valueOf(value), String.valueOf(ok));
        }
    }


    static LFUCache lfu_new(java.math.BigInteger cap) {
        return new LFUCache(((Entry[])(new Entry[]{})), cap, java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0));
    }

    static java.math.BigInteger find_entry(Entry[] entries, java.math.BigInteger key) {
        java.math.BigInteger i = java.math.BigInteger.valueOf(0);
        while (i.compareTo(new java.math.BigInteger(String.valueOf(entries.length))) < 0) {
            Entry e_1 = entries[_idx((entries).length, ((java.math.BigInteger)(i)).longValue())];
            if (e_1.key.compareTo(key) == 0) {
                return i;
            }
            i = i.add(java.math.BigInteger.valueOf(1));
        }
        return (java.math.BigInteger.valueOf(1)).negate();
    }

    static GetResult lfu_get(LFUCache cache, java.math.BigInteger key) {
        java.math.BigInteger idx = find_entry(((Entry[])(cache.entries)), key);
        if (idx.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            LFUCache new_cache_1 = new LFUCache(((Entry[])(cache.entries)), cache.capacity, cache.hits, cache.miss.add(java.math.BigInteger.valueOf(1)), cache.tick);
            return new GetResult(new_cache_1, java.math.BigInteger.valueOf(0), false);
        }
        Entry[] entries_1 = ((Entry[])(cache.entries));
        Entry e_3 = entries_1[_idx((entries_1).length, ((java.math.BigInteger)(idx)).longValue())];
e_3.freq = e_3.freq.add(java.math.BigInteger.valueOf(1));
        java.math.BigInteger new_tick_1 = cache.tick.add(java.math.BigInteger.valueOf(1));
e_3.order = new_tick_1;
entries_1[(int)(((java.math.BigInteger)(idx)).longValue())] = e_3;
        LFUCache new_cache_3 = new LFUCache(((Entry[])(entries_1)), cache.capacity, cache.hits.add(java.math.BigInteger.valueOf(1)), cache.miss, new_tick_1);
        return new GetResult(new_cache_3, e_3.val, true);
    }

    static Entry[] remove_lfu(Entry[] entries) {
        if (new java.math.BigInteger(String.valueOf(entries.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return ((Entry[])(entries));
        }
        java.math.BigInteger min_idx_1 = java.math.BigInteger.valueOf(0);
        java.math.BigInteger i_2 = java.math.BigInteger.valueOf(1);
        while (i_2.compareTo(new java.math.BigInteger(String.valueOf(entries.length))) < 0) {
            Entry e_5 = entries[_idx((entries).length, ((java.math.BigInteger)(i_2)).longValue())];
            Entry m_1 = entries[_idx((entries).length, ((java.math.BigInteger)(min_idx_1)).longValue())];
            if (e_5.freq.compareTo(m_1.freq) < 0 || (e_5.freq.compareTo(m_1.freq) == 0 && e_5.order.compareTo(m_1.order) < 0)) {
                min_idx_1 = i_2;
            }
            i_2 = i_2.add(java.math.BigInteger.valueOf(1));
        }
        Entry[] res_1 = ((Entry[])(new Entry[]{}));
        java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
        while (j_1.compareTo(new java.math.BigInteger(String.valueOf(entries.length))) < 0) {
            if (j_1.compareTo(min_idx_1) != 0) {
                res_1 = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(entries[_idx((entries).length, ((java.math.BigInteger)(j_1)).longValue())])).toArray(Entry[]::new)));
            }
            j_1 = j_1.add(java.math.BigInteger.valueOf(1));
        }
        return ((Entry[])(res_1));
    }

    static LFUCache lfu_put(LFUCache cache, java.math.BigInteger key, java.math.BigInteger value) {
        Entry[] entries_2 = ((Entry[])(cache.entries));
        java.math.BigInteger idx_2 = find_entry(((Entry[])(entries_2)), key);
        if (idx_2.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0) {
            Entry e_7 = entries_2[_idx((entries_2).length, ((java.math.BigInteger)(idx_2)).longValue())];
e_7.val = value;
e_7.freq = e_7.freq.add(java.math.BigInteger.valueOf(1));
            java.math.BigInteger new_tick_3 = cache.tick.add(java.math.BigInteger.valueOf(1));
e_7.order = new_tick_3;
entries_2[(int)(((java.math.BigInteger)(idx_2)).longValue())] = e_7;
            return new LFUCache(((Entry[])(entries_2)), cache.capacity, cache.hits, cache.miss, new_tick_3);
        }
        if (new java.math.BigInteger(String.valueOf(entries_2.length)).compareTo(cache.capacity) >= 0) {
            entries_2 = ((Entry[])(remove_lfu(((Entry[])(entries_2)))));
        }
        java.math.BigInteger new_tick_5 = cache.tick.add(java.math.BigInteger.valueOf(1));
        Entry new_entry_1 = new Entry(key, value, java.math.BigInteger.valueOf(1), new_tick_5);
        entries_2 = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(entries_2), java.util.stream.Stream.of(new_entry_1)).toArray(Entry[]::new)));
        return new LFUCache(((Entry[])(entries_2)), cache.capacity, cache.hits, cache.miss, new_tick_5);
    }

    static String cache_info(LFUCache cache) {
        return "CacheInfo(hits=" + _p(cache.hits) + ", misses=" + _p(cache.miss) + ", capacity=" + _p(cache.capacity) + ", current_size=" + _p(cache.entries.length) + ")";
    }

    static void main() {
        LFUCache cache = lfu_new(java.math.BigInteger.valueOf(2));
        cache = lfu_put(cache, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1));
        cache = lfu_put(cache, java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2));
        GetResult r_1 = lfu_get(cache, java.math.BigInteger.valueOf(1));
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        cache = lfu_put(cache, java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(3));
        r_1 = lfu_get(cache, java.math.BigInteger.valueOf(2));
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        cache = lfu_put(cache, java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(4));
        r_1 = lfu_get(cache, java.math.BigInteger.valueOf(1));
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        r_1 = lfu_get(cache, java.math.BigInteger.valueOf(3));
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        r_1 = lfu_get(cache, java.math.BigInteger.valueOf(4));
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        System.out.println(cache_info(cache));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
