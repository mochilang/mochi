public class Main {

    static long max_sum_sliding_window(long[] arr, long k) {
        if (k < 0 || arr.length < k) {
            throw new RuntimeException(String.valueOf("Invalid Input"));
        }
        long idx_1 = 0;
        long current_sum_1 = 0;
        while (idx_1 < k) {
            current_sum_1 = current_sum_1 + arr[(int)(idx_1)];
            idx_1 = idx_1 + 1;
        }
        long max_sum_1 = current_sum_1;
        long i_1 = 0;
        while (i_1 < arr.length - k) {
            current_sum_1 = current_sum_1 - arr[(int)(i_1)] + arr[(int)(i_1 + k)];
            if (current_sum_1 > max_sum_1) {
                max_sum_1 = current_sum_1;
            }
            i_1 = i_1 + 1;
        }
        return max_sum_1;
    }

    static void test_max_sum_sliding_window() {
        long[] arr1 = ((long[])(new long[]{1, 4, 2, 10, 2, 3, 1, 0, 20}));
        if (max_sum_sliding_window(((long[])(arr1)), 4) != 24) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        long[] arr2_1 = ((long[])(new long[]{1, 4, 2, 10, 2, 13, 1, 0, 2}));
        if (max_sum_sliding_window(((long[])(arr2_1)), 4) != 27) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
    }

    static void main() {
        test_max_sum_sliding_window();
        long[] sample_1 = ((long[])(new long[]{1, 4, 2, 10, 2, 3, 1, 0, 20}));
        System.out.println(_p(max_sum_sliding_window(((long[])(sample_1)), 4)));
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
