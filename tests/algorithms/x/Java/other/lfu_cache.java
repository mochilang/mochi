public class Main {
    static class Entry {
        long key;
        long val;
        long freq;
        long order;
        Entry(long key, long val, long freq, long order) {
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
        long capacity;
        long hits;
        long miss;
        long tick;
        LFUCache(Entry[] entries, long capacity, long hits, long miss, long tick) {
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
        long value;
        boolean ok;
        GetResult(LFUCache cache, long value, boolean ok) {
            this.cache = cache;
            this.value = value;
            this.ok = ok;
        }
        GetResult() {}
        @Override public String toString() {
            return String.format("{'cache': %s, 'value': %s, 'ok': %s}", String.valueOf(cache), String.valueOf(value), String.valueOf(ok));
        }
    }


    static LFUCache lfu_new(long cap) {
        return new LFUCache(new Entry[]{}, cap, 0, 0, 0);
    }

    static long find_entry(Entry[] entries, long key) {
        long i = 0L;
        while ((long)(i) < (long)(entries.length)) {
            Entry e_1 = entries[(int)((long)(i))];
            if ((long)(e_1.key) == key) {
                return i;
            }
            i = (long)((long)(i) + (long)(1));
        }
        return (long)(0) - (long)(1);
    }

    static GetResult lfu_get(LFUCache cache, long key) {
        long idx = find_entry(((Entry[])(cache.entries)), key);
        if (idx == (long)((long)(0) - (long)(1))) {
            LFUCache new_cache_1 = new LFUCache(cache.entries, cache.capacity, cache.hits, (long)(cache.miss) + (long)(1), cache.tick);
            return new GetResult(new_cache_1, 0, false);
        }
        Entry[] entries_1 = ((Entry[])(cache.entries));
        Entry e_3 = entries_1[(int)((long)(idx))];
e_3.freq = (long)(e_3.freq) + (long)(1);
        long new_tick_1 = (long)((long)(cache.tick) + (long)(1));
e_3.order = new_tick_1;
entries_1[(int)((long)(idx))] = e_3;
        LFUCache new_cache_3 = new LFUCache(entries_1, cache.capacity, (long)(cache.hits) + (long)(1), cache.miss, new_tick_1);
        return new GetResult(new_cache_3, e_3.val, true);
    }

    static Entry[] remove_lfu(Entry[] entries) {
        if ((long)(entries.length) == (long)(0)) {
            return entries;
        }
        long min_idx_1 = 0L;
        long i_2 = 1L;
        while ((long)(i_2) < (long)(entries.length)) {
            Entry e_5 = entries[(int)((long)(i_2))];
            Entry m_1 = entries[(int)((long)(min_idx_1))];
            if ((long)(e_5.freq) < (long)(m_1.freq) || ((long)(e_5.freq) == (long)(m_1.freq) && (long)(e_5.order) < (long)(m_1.order))) {
                min_idx_1 = (long)(i_2);
            }
            i_2 = (long)((long)(i_2) + (long)(1));
        }
        Entry[] res_1 = ((Entry[])(new Entry[]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(entries.length)) {
            if ((long)(j_1) != (long)(min_idx_1)) {
                res_1 = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(entries[(int)((long)(j_1))])).toArray(Entry[]::new)));
            }
            j_1 = (long)((long)(j_1) + (long)(1));
        }
        return res_1;
    }

    static LFUCache lfu_put(LFUCache cache, long key, long value) {
        Entry[] entries_2 = ((Entry[])(cache.entries));
        long idx_2 = find_entry(((Entry[])(entries_2)), key);
        if (idx_2 != (long)((long)(0) - (long)(1))) {
            Entry e_7 = entries_2[(int)((long)(idx_2))];
e_7.val = value;
e_7.freq = (long)(e_7.freq) + (long)(1);
            long new_tick_3 = (long)((long)(cache.tick) + (long)(1));
e_7.order = new_tick_3;
entries_2[(int)((long)(idx_2))] = e_7;
            return new LFUCache(entries_2, cache.capacity, cache.hits, cache.miss, new_tick_3);
        }
        if ((long)(entries_2.length) >= (long)(cache.capacity)) {
            entries_2 = ((Entry[])(remove_lfu(((Entry[])(entries_2)))));
        }
        long new_tick_5 = (long)((long)(cache.tick) + (long)(1));
        Entry new_entry_1 = new Entry(key, value, 1, new_tick_5);
        entries_2 = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(entries_2), java.util.stream.Stream.of(new_entry_1)).toArray(Entry[]::new)));
        return new LFUCache(entries_2, cache.capacity, cache.hits, cache.miss, new_tick_5);
    }

    static String cache_info(LFUCache cache) {
        return "CacheInfo(hits=" + _p(cache.hits) + ", misses=" + _p(cache.miss) + ", capacity=" + _p(cache.capacity) + ", current_size=" + _p(cache.entries.length) + ")";
    }

    static void main() {
        LFUCache cache = lfu_new(2L);
        cache = lfu_put(cache, 1L, 1L);
        cache = lfu_put(cache, 2L, 2L);
        GetResult r_1 = lfu_get(cache, 1L);
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        cache = lfu_put(cache, 3L, 3L);
        r_1 = lfu_get(cache, 2L);
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        cache = lfu_put(cache, 4L, 4L);
        r_1 = lfu_get(cache, 1L);
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        r_1 = lfu_get(cache, 3L);
        cache = r_1.cache;
        if (r_1.ok) {
            System.out.println(_p(r_1.value));
        } else {
            System.out.println("None");
        }
        r_1 = lfu_get(cache, 4L);
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
