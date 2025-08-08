public class Main {
    static class Entry {
        String key;
        String value;
        Entry(String key, String value) {
            this.key = key;
            this.value = value;
        }
        Entry() {}
        @Override public String toString() {
            return String.format("{'key': '%s', 'value': '%s'}", String.valueOf(key), String.valueOf(value));
        }
    }

    static class HashMap {
        Entry[] entries;
        HashMap(Entry[] entries) {
            this.entries = entries;
        }
        HashMap() {}
        @Override public String toString() {
            return String.format("{'entries': %s}", String.valueOf(entries));
        }
    }

    static class GetResult {
        boolean found;
        String value;
        GetResult(boolean found, String value) {
            this.found = found;
            this.value = value;
        }
        GetResult() {}
        @Override public String toString() {
            return String.format("{'found': %s, 'value': '%s'}", String.valueOf(found), String.valueOf(value));
        }
    }

    static class DelResult {
        HashMap map;
        boolean ok;
        DelResult(HashMap map, boolean ok) {
            this.map = map;
            this.ok = ok;
        }
        DelResult() {}
        @Override public String toString() {
            return String.format("{'map': %s, 'ok': %s}", String.valueOf(map), String.valueOf(ok));
        }
    }


    static HashMap make_hash_map() {
        return new HashMap(new Entry[]{});
    }

    static int hm_len(HashMap m) {
        return ((Entry[]) (m.get("entries"))).length;
    }

    static HashMap hm_set(HashMap m, String key, String value) {
        Entry[] entries = (Entry[])(((Entry[]) (m.get("entries"))));
        boolean updated = false;
        Entry[] new_entries = ((Entry[])(new Entry[]{}));
        int i = 0;
        while (i < entries.length) {
            Entry e = entries[i];
            if ((e.key.equals(key))) {
                new_entries = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_entries), java.util.stream.Stream.of(new Entry(key, value))).toArray(Entry[]::new)));
                updated = true;
            } else {
                new_entries = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_entries), java.util.stream.Stream.of(e)).toArray(Entry[]::new)));
            }
            i = i + 1;
        }
        if (!(Boolean)updated) {
            new_entries = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_entries), java.util.stream.Stream.of(new Entry(key, value))).toArray(Entry[]::new)));
        }
        return new HashMap(new_entries);
    }

    static GetResult hm_get(HashMap m, String key) {
        int i_1 = 0;
        while (i_1 < ((Entry[]) (m.get("entries"))).length) {
            Entry e_1 = ((Entry[]) (m.get("entries")))[i_1];
            if ((e_1.key.equals(key))) {
                return new GetResult(true, e_1.value);
            }
            i_1 = i_1 + 1;
        }
        return new GetResult(false, "");
    }

    static DelResult hm_del(HashMap m, String key) {
        Entry[] entries_1 = (Entry[])(((Entry[]) (m.get("entries"))));
        Entry[] new_entries_1 = ((Entry[])(new Entry[]{}));
        boolean removed = false;
        int i_2 = 0;
        while (i_2 < entries_1.length) {
            Entry e_2 = entries_1[i_2];
            if ((e_2.key.equals(key))) {
                removed = true;
            } else {
                new_entries_1 = ((Entry[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_entries_1), java.util.stream.Stream.of(e_2)).toArray(Entry[]::new)));
            }
            i_2 = i_2 + 1;
        }
        if (((Boolean)(removed))) {
            return new DelResult(new HashMap(new_entries_1), true);
        }
        return new DelResult(m, false);
    }

    static boolean test_add_items() {
        HashMap h = make_hash_map();
        h = hm_set(h, "key_a", "val_a");
        h = hm_set(h, "key_b", "val_b");
        GetResult a = hm_get(h, "key_a");
        GetResult b = hm_get(h, "key_b");
        return hm_len(h) == 2 && a.found && b.found && (a.value.equals("val_a")) && (b.value.equals("val_b"));
    }

    static boolean test_overwrite_items() {
        HashMap h_1 = make_hash_map();
        h_1 = hm_set(h_1, "key_a", "val_a");
        h_1 = hm_set(h_1, "key_a", "val_b");
        GetResult a_1 = hm_get(h_1, "key_a");
        return hm_len(h_1) == 1 && a_1.found && (a_1.value.equals("val_b"));
    }

    static boolean test_delete_items() {
        HashMap h_2 = make_hash_map();
        h_2 = hm_set(h_2, "key_a", "val_a");
        h_2 = hm_set(h_2, "key_b", "val_b");
        DelResult d1 = hm_del(h_2, "key_a");
        h_2 = d1.map;
        DelResult d2 = hm_del(h_2, "key_b");
        h_2 = d2.map;
        h_2 = hm_set(h_2, "key_a", "val_a");
        DelResult d3 = hm_del(h_2, "key_a");
        h_2 = d3.map;
        return hm_len(h_2) == 0;
    }

    static boolean test_access_absent_items() {
        HashMap h_3 = make_hash_map();
        GetResult g1 = hm_get(h_3, "key_a");
        DelResult d1_1 = hm_del(h_3, "key_a");
        h_3 = d1_1.map;
        h_3 = hm_set(h_3, "key_a", "val_a");
        DelResult d2_1 = hm_del(h_3, "key_a");
        h_3 = d2_1.map;
        DelResult d3_1 = hm_del(h_3, "key_a");
        h_3 = d3_1.map;
        GetResult g2 = hm_get(h_3, "key_a");
        return g1.found == false && d1_1.ok == false && d2_1.ok && d3_1.ok == false && g2.found == false && hm_len(h_3) == 0;
    }

    static boolean test_add_with_resize_up() {
        HashMap h_4 = make_hash_map();
        int i_3 = 0;
        while (i_3 < 5) {
            String s = _p(i_3);
            h_4 = hm_set(h_4, s, s);
            i_3 = i_3 + 1;
        }
        return hm_len(h_4) == 5;
    }

    static boolean test_add_with_resize_down() {
        HashMap h_5 = make_hash_map();
        int i_4 = 0;
        while (i_4 < 5) {
            String s_1 = _p(i_4);
            h_5 = hm_set(h_5, s_1, s_1);
            i_4 = i_4 + 1;
        }
        int j = 0;
        while (j < 5) {
            String s_2 = _p(j);
            DelResult d = hm_del(h_5, s_2);
            h_5 = d.map;
            j = j + 1;
        }
        h_5 = hm_set(h_5, "key_a", "val_b");
        GetResult a_2 = hm_get(h_5, "key_a");
        return hm_len(h_5) == 1 && a_2.found && (a_2.value.equals("val_b"));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(test_add_items());
            System.out.println(test_overwrite_items());
            System.out.println(test_delete_items());
            System.out.println(test_access_absent_items());
            System.out.println(test_add_with_resize_up());
            System.out.println(test_add_with_resize_down());
            System.out.println(true ? "True" : "False");
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
