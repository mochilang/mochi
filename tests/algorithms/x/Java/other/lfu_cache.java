public class Main {
    static class Entry {
        int key;
        int val;
        int freq;
        int order;
        Entry(int key, int val, int freq, int order) {
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
        int capacity;
        int hits;
        int miss;
        int tick;
        LFUCache(Entry[] entries, int capacity, int hits, int miss, int tick) {
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
        int value;
        boolean ok;
        GetResult(LFUCache cache, int value, boolean ok) {
            this.cache = cache;
            this.value = value;
            this.ok = ok;
        }
        GetResult() {}
        @Override public String toString() {
            return String.format("{'cache': %s, 'value': %s, 'ok': %s}", String.valueOf(cache), String.valueOf(value), String.valueOf(ok));
        }
    }


    static LFUCache lfu_new(int cap) {
        return new LFUCache(new Entry[]{}, cap, 0, 0, 0);
    }

    static int find_entry(Entry[] entries, int key) {
        int i = 0;
        while (i < entries.length) {
            Entry e = entries[i];
            if (e.key == key) {
                return i;
            }
            i = i + 1;
        }
        return 0 - 1;
    }

    static GetResult lfu_get(LFUCache cache, int key) {
        int idx = find_entry(((Entry[])(cache.entries)), key);
        if (idx == 0 - 1) {
            LFUCache new_cache = new LFUCache(cache.entries, cache.capacity, cache.hits, cache.miss + 1, cache.tick);
            return new GetResult(new_cache, 0, false);
        }
        Entry[] entries = ((Entry[])(cache.entries));
        Entry e_1 = entries[idx];
e_1.freq = e_1.freq + 1;
        int new_tick = cache.tick + 1;
e_1.order = new_tick;
entries[idx] = e_1;
        LFUCache new_cache_1 = new LFUCache(entries, cache.capacity, cache.hits + 1, cache.miss, new_tick);
        return new GetResult(new_cache_1, e_1.val, true);
    }

    static Entry[] remove_lfu(Entry[] entries) {
        if (entries.length == 0) {
            return entries;
        }
        int min_idx = 0;
        int i_1 = 1;
        while (i_1 < entries.length) {
            Entry e_2 = entries[i_1];
            Entry m = entries[min_idx];
            if (e_2.freq < m.freq || (e_2.freq == m.freq && e_2.order < m.order)) {
                min_idx = i_1;
            }
            i_1 = i_1 + 1;
        }
        Entry[] res = ((Entry[])(new Entry[]{}));
        int j = 0;
        while (j < entries.length) {
            if (j != min_idx) {
                res = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(entries[j])).toArray(Entry[]::new)));
            }
            j = j + 1;
        }
        return res;
    }

    static LFUCache lfu_put(LFUCache cache, int key, int value) {
        Entry[] entries_1 = ((Entry[])(cache.entries));
        int idx_1 = find_entry(((Entry[])(entries_1)), key);
        if (idx_1 != 0 - 1) {
            Entry e_3 = entries_1[idx_1];
e_3.val = value;
e_3.freq = e_3.freq + 1;
            int new_tick_1 = cache.tick + 1;
e_3.order = new_tick_1;
entries_1[idx_1] = e_3;
            return new LFUCache(entries_1, cache.capacity, cache.hits, cache.miss, new_tick_1);
        }
        if (entries_1.length >= cache.capacity) {
            entries_1 = ((Entry[])(remove_lfu(((Entry[])(entries_1)))));
        }
        int new_tick_2 = cache.tick + 1;
        Entry new_entry = new Entry(key, value, 1, new_tick_2);
        entries_1 = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(entries_1), java.util.stream.Stream.of(new_entry)).toArray(Entry[]::new)));
        return new LFUCache(entries_1, cache.capacity, cache.hits, cache.miss, new_tick_2);
    }

    static String cache_info(LFUCache cache) {
        return "CacheInfo(hits=" + _p(cache.hits) + ", misses=" + _p(cache.miss) + ", capacity=" + _p(cache.capacity) + ", current_size=" + _p(cache.entries.length) + ")";
    }

    static void main() {
        LFUCache cache = lfu_new(2);
        cache = lfu_put(cache, 1, 1);
        cache = lfu_put(cache, 2, 2);
        GetResult r = lfu_get(cache, 1);
        cache = r.cache;
        if (r.ok) {
            System.out.println(_p(r.value));
        } else {
            System.out.println("None");
        }
        cache = lfu_put(cache, 3, 3);
        r = lfu_get(cache, 2);
        cache = r.cache;
        if (r.ok) {
            System.out.println(_p(r.value));
        } else {
            System.out.println("None");
        }
        cache = lfu_put(cache, 4, 4);
        r = lfu_get(cache, 1);
        cache = r.cache;
        if (r.ok) {
            System.out.println(_p(r.value));
        } else {
            System.out.println("None");
        }
        r = lfu_get(cache, 3);
        cache = r.cache;
        if (r.ok) {
            System.out.println(_p(r.value));
        } else {
            System.out.println("None");
        }
        r = lfu_get(cache, 4);
        cache = r.cache;
        if (r.ok) {
            System.out.println(_p(r.value));
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
        return String.valueOf(v);
    }
}
