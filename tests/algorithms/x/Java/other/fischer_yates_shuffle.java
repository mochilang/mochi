public class Main {
    static long seed = 0;
    static long[] integers;
    static String[] strings;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * (long)(1103515245)) + (long)(12345)))), 2147483648L))));
        return ((long)(Math.floorDiv(seed, 65536)));
    }

    static long randint(long a, long b) {
        long r = rand();
        return a + Math.floorMod(r, ((long)(b - a) + (long)(1)));
    }

    static long[] fisher_yates_shuffle_int(long[] data) {
        long[] res = ((long[])(data));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(res.length)) {
            long a_1 = randint(0L, (long)((long)(res.length) - (long)(1)));
            long b_1 = randint(0L, (long)((long)(res.length) - (long)(1)));
            long temp_1 = res[(int)((long)(a_1))];
res[(int)((long)(a_1))] = res[(int)((long)(b_1))];
res[(int)((long)(b_1))] = temp_1;
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return res;
    }

    static String[] fisher_yates_shuffle_str(String[] data) {
        String[] res_1 = ((String[])(data));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(res_1.length)) {
            long a_3 = randint(0L, (long)((long)(res_1.length) - (long)(1)));
            long b_3 = randint(0L, (long)((long)(res_1.length) - (long)(1)));
            String temp_3 = res_1[(int)((long)(a_3))];
res_1[(int)((long)(a_3))] = res_1[(int)((long)(b_3))];
res_1[(int)((long)(b_3))] = temp_3;
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return res_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = (long)(1);
            integers = ((long[])(new long[]{0, 1, 2, 3, 4, 5, 6, 7}));
            strings = ((String[])(new String[]{"python", "says", "hello", "!"}));
            System.out.println("Fisher-Yates Shuffle:");
            System.out.println("List " + _p(integers) + " " + _p(strings));
            System.out.println("FY Shuffle " + _p(fisher_yates_shuffle_int(((long[])(integers)))) + " " + _p(fisher_yates_shuffle_str(((String[])(strings)))));
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
