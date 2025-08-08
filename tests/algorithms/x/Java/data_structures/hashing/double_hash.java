public class Main {

    static boolean is_prime(int n) {
        if (n < 2) {
            return false;
        }
        int i = 2;
        while (i * i <= n) {
            if (Math.floorMod(n, i) == 0) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int prev_prime(int n) {
        int p = n - 1;
        while (p >= 2) {
            if (((Boolean)(is_prime(p)))) {
                return p;
            }
            p = p - 1;
        }
        return 1;
    }

    static int[] create_table(int size) {
        int[] vals = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < size) {
            vals = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vals), java.util.stream.IntStream.of((-1))).toArray()));
            i_1 = i_1 + 1;
        }
        return vals;
    }

    static int hash1(int size, int key) {
        return Math.floorMod(key, size);
    }

    static int hash2(int prime, int key) {
        return prime - (Math.floorMod(key, prime));
    }

    static int[] insert_double_hash(int[] values, int size, int prime, int value) {
        int[] vals_1 = ((int[])(values));
        int idx = hash1(size, value);
        int step = hash2(prime, value);
        int count = 0;
        while (vals_1[idx] != (-1) && count < size) {
            idx = Math.floorMod((idx + step), size);
            count = count + 1;
        }
        if (vals_1[idx] == (-1)) {
vals_1[idx] = value;
        }
        return vals_1;
    }

    static java.util.Map<Integer,Integer> table_keys(int[] values) {
        java.util.Map<Integer,Integer> res = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        int i_2 = 0;
        while (i_2 < values.length) {
            if (values[i_2] != (-1)) {
res.put(i_2, values[i_2]);
            }
            i_2 = i_2 + 1;
        }
        return res;
    }

    static void run_example(int size, int[] data) {
        int prime = prev_prime(size);
        int[] table = ((int[])(create_table(size)));
        int i_3 = 0;
        while (i_3 < data.length) {
            table = ((int[])(insert_double_hash(((int[])(table)), size, prime, data[i_3])));
            i_3 = i_3 + 1;
        }
        System.out.println(_p(table_keys(((int[])(table)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            run_example(3, ((int[])(new int[]{10, 20, 30})));
            run_example(4, ((int[])(new int[]{10, 20, 30})));
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
