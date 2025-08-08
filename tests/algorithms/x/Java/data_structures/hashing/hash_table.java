public class Main {
    static class HashTable {
        int size_table;
        int[] values;
        boolean[] filled;
        int charge_factor;
        double lim_charge;
        HashTable(int size_table, int[] values, boolean[] filled, int charge_factor, double lim_charge) {
            this.size_table = size_table;
            this.values = values;
            this.filled = filled;
            this.charge_factor = charge_factor;
            this.lim_charge = lim_charge;
        }
        HashTable() {}
        @Override public String toString() {
            return String.format("{'size_table': %s, 'values': %s, 'filled': %s, 'charge_factor': %s, 'lim_charge': %s}", String.valueOf(size_table), String.valueOf(values), String.valueOf(filled), String.valueOf(charge_factor), String.valueOf(lim_charge));
        }
    }


    static int[] repeat_int(int n, int val) {
        int[] res = new int[0];
        int i = 0;
        while (i < n) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(val)).toArray()));
            i = i + 1;
        }
        return res;
    }

    static boolean[] repeat_bool(int n, boolean val) {
        boolean[] res_1 = new boolean[0];
        int i_1 = 0;
        while (i_1 < n) {
            res_1 = ((boolean[])(appendBool(res_1, ((Boolean)(val)))));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static int[] set_int(int[] xs, int idx, int value) {
        int[] res_2 = new int[0];
        int i_2 = 0;
        while (i_2 < xs.length) {
            if (i_2 == idx) {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(value)).toArray()));
            } else {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(xs[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return res_2;
    }

    static boolean[] set_bool(boolean[] xs, int idx, boolean value) {
        boolean[] res_3 = new boolean[0];
        int i_3 = 0;
        while (i_3 < xs.length) {
            if (i_3 == idx) {
                res_3 = ((boolean[])(appendBool(res_3, ((Boolean)(value)))));
            } else {
                res_3 = ((boolean[])(appendBool(res_3, ((Boolean)(xs[i_3])))));
            }
            i_3 = i_3 + 1;
        }
        return res_3;
    }

    static HashTable create_table(int size_table, int charge_factor, double lim_charge) {
        return new HashTable(size_table, repeat_int(size_table, 0), repeat_bool(size_table, false), charge_factor, lim_charge);
    }

    static int hash_function(HashTable ht, int key) {
        int k = Math.floorMod(key, ht.size_table);
        if (k < 0) {
            k = k + ht.size_table;
        }
        return k;
    }

    static boolean is_prime(int n) {
        if (n < 2) {
            return false;
        }
        if (Math.floorMod(n, 2) == 0) {
            return n == 2;
        }
        int i_4 = 3;
        while (i_4 * i_4 <= n) {
            if (Math.floorMod(n, i_4) == 0) {
                return false;
            }
            i_4 = i_4 + 2;
        }
        return true;
    }

    static int next_prime(int value, int factor) {
        int candidate = value * factor + 1;
        while (!(Boolean)is_prime(candidate)) {
            candidate = candidate + 1;
        }
        return candidate;
    }

    static HashTable set_value(HashTable ht, int key, int data) {
        int[] new_values = ((int[])(set_int(((int[])(ht.values)), key, data)));
        boolean[] new_filled = ((boolean[])(set_bool(((boolean[])(ht.filled)), key, true)));
        return new HashTable(ht.size_table, new_values, new_filled, ht.charge_factor, ht.lim_charge);
    }

    static int collision_resolution(HashTable ht, int key) {
        int new_key = hash_function(ht, key + 1);
        int steps = 0;
        while (ht.filled[new_key]) {
            new_key = hash_function(ht, new_key + 1);
            steps = steps + 1;
            if (steps >= ht.size_table) {
                return -1;
            }
        }
        return new_key;
    }

    static HashTable rehashing(HashTable ht) {
        int[] survivors = new int[0];
        int i_5 = 0;
        while (i_5 < ht.values.length) {
            if (ht.filled[i_5]) {
                survivors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(survivors), java.util.stream.IntStream.of(ht.values[i_5])).toArray()));
            }
            i_5 = i_5 + 1;
        }
        int new_size = next_prime(ht.size_table, 2);
        HashTable new_ht = create_table(new_size, ht.charge_factor, ht.lim_charge);
        i_5 = 0;
        while (i_5 < survivors.length) {
            new_ht = insert_data(new_ht, survivors[i_5]);
            i_5 = i_5 + 1;
        }
        return new_ht;
    }

    static HashTable insert_data(HashTable ht, int data) {
        int key = hash_function(ht, data);
        if (!ht.filled[key]) {
            return set_value(ht, key, data);
        }
        if (ht.values[key] == data) {
            return ht;
        }
        int new_key_1 = collision_resolution(ht, key);
        if (new_key_1 >= 0) {
            return set_value(ht, new_key_1, data);
        }
        HashTable resized = rehashing(ht);
        return insert_data(resized, data);
    }

    static int[][] keys(HashTable ht) {
        int[][] res_4 = new int[0][];
        int i_6 = 0;
        while (i_6 < ht.values.length) {
            if (ht.filled[i_6]) {
                res_4 = ((int[][])(appendObj((int[][])res_4, new int[]{i_6, ht.values[i_6]})));
            }
            i_6 = i_6 + 1;
        }
        return res_4;
    }

    static void main() {
        HashTable ht = create_table(3, 1, 0.75);
        ht = insert_data(ht, 17);
        ht = insert_data(ht, 18);
        ht = insert_data(ht, 99);
        System.out.println(keys(ht));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
