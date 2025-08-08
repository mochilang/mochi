public class Main {
    static class HashTableWithLinkedList {
        int size_table;
        int charge_factor;
        int[][] values;
        java.util.Map<Integer,int[]> keys;
        HashTableWithLinkedList(int size_table, int charge_factor, int[][] values, java.util.Map<Integer,int[]> keys) {
            this.size_table = size_table;
            this.charge_factor = charge_factor;
            this.values = values;
            this.keys = keys;
        }
        HashTableWithLinkedList() {}
        @Override public String toString() {
            return String.format("{'size_table': %s, 'charge_factor': %s, 'values': %s, 'keys': %s}", String.valueOf(size_table), String.valueOf(charge_factor), String.valueOf(values), String.valueOf(keys));
        }
    }


    static HashTableWithLinkedList make_table(int size_table, int charge_factor) {
        int[][] vals = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < size_table) {
            vals = ((int[][])(appendObj((int[][])vals, new int[]{})));
            i = i + 1;
        }
        return new HashTableWithLinkedList(size_table, charge_factor, vals, new java.util.LinkedHashMap<Integer, int[]>());
    }

    static int hash_function(HashTableWithLinkedList ht, int key) {
        int res = Math.floorMod(key, ht.size_table);
        if (res < 0) {
            res = res + ht.size_table;
        }
        return res;
    }

    static int[] prepend(int[] lst, int value) {
        int[] result = ((int[])(new int[]{value}));
        int i_1 = 0;
        while (i_1 < lst.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(lst[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void set_value(HashTableWithLinkedList ht, int key, int data) {
        int[] current = ((int[])(ht.values[key]));
        int[] updated = ((int[])(prepend(((int[])(current)), data)));
        int[][] vals_1 = ((int[][])(ht.values));
vals_1[key] = ((int[])(updated));
ht.values = vals_1;
        java.util.Map<Integer,int[]> ks = ht.keys;
ks.put(key, ((int[])(updated)));
ht.keys = ks;
    }

    static int count_empty(HashTableWithLinkedList ht) {
        int count = 0;
        int i_2 = 0;
        while (i_2 < ht.values.length) {
            if (ht.values[i_2].length == 0) {
                count = count + 1;
            }
            i_2 = i_2 + 1;
        }
        return count;
    }

    static double balanced_factor(HashTableWithLinkedList ht) {
        int total = 0;
        int i_3 = 0;
        while (i_3 < ht.values.length) {
            total = total + (ht.charge_factor - ht.values[i_3].length);
            i_3 = i_3 + 1;
        }
        return (((Number)(total)).doubleValue()) / (((Number)(ht.size_table)).doubleValue()) * (((Number)(ht.charge_factor)).doubleValue());
    }

    static int collision_resolution(HashTableWithLinkedList ht, int key) {
        if (!(ht.values[key].length == ht.charge_factor && count_empty(ht) == 0)) {
            return key;
        }
        int new_key = Math.floorMod((key + 1), ht.size_table);
        int steps = 0;
        while (ht.values[new_key].length == ht.charge_factor && steps < ht.size_table - 1) {
            new_key = Math.floorMod((new_key + 1), ht.size_table);
            steps = steps + 1;
        }
        if (ht.values[new_key].length < ht.charge_factor) {
            return new_key;
        }
        return -1;
    }

    static void insert(HashTableWithLinkedList ht, int data) {
        int key = hash_function(ht, data);
        if (ht.values[key].length == 0 || ht.values[key].length < ht.charge_factor) {
            set_value(ht, key, data);
            return;
        }
        int dest = collision_resolution(ht, key);
        if (dest >= 0) {
            set_value(ht, dest, data);
        } else {
            System.out.println("table full");
        }
    }

    static void main() {
        HashTableWithLinkedList ht = make_table(3, 2);
        insert(ht, 10);
        insert(ht, 20);
        insert(ht, 30);
        insert(ht, 40);
        insert(ht, 50);
        System.out.println(_p(ht.values));
        System.out.println(_p(balanced_factor(ht)));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
