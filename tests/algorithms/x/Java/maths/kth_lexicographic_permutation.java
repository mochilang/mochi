public class Main {

    static long[] remove_at(long[] xs, long idx) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = 0;
        while (i_1 < xs.length) {
            if (i_1 != idx) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(xs[(int)(i_1)])).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static long[] kth_permutation(long k, long n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("n must be positive"));
        }
        long[] factorials_1 = ((long[])(new long[]{1}));
        long i_3 = 2;
        while (i_3 < n) {
            factorials_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(factorials_1), java.util.stream.LongStream.of(factorials_1[(int)(factorials_1.length - 1)] * i_3)).toArray()));
            i_3 = i_3 + 1;
        }
        long total_1 = factorials_1[(int)(factorials_1.length - 1)] * n;
        if ((k < 0) || (k >= total_1)) {
            throw new RuntimeException(String.valueOf("k out of bounds"));
        }
        long[] elements_1 = ((long[])(new long[]{}));
        long e_1 = 0;
        while (e_1 < n) {
            elements_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(elements_1), java.util.stream.LongStream.of(e_1)).toArray()));
            e_1 = e_1 + 1;
        }
        long[] permutation_1 = ((long[])(new long[]{}));
        long idx_1 = factorials_1.length - 1;
        while (idx_1 >= 0) {
            long factorial_1 = factorials_1[(int)(idx_1)];
            long number_1 = Math.floorDiv(k, factorial_1);
            k = Math.floorMod(k, factorial_1);
            permutation_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(permutation_1), java.util.stream.LongStream.of(elements_1[(int)(number_1)])).toArray()));
            elements_1 = ((long[])(remove_at(((long[])(elements_1)), number_1)));
            idx_1 = idx_1 - 1;
        }
        permutation_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(permutation_1), java.util.stream.LongStream.of(elements_1[(int)(0)])).toArray()));
        return permutation_1;
    }

    static boolean list_equal(long[] a, long[] b) {
        if (a.length != b.length) {
            return false;
        }
        long i_5 = 0;
        while (i_5 < a.length) {
            if (a[(int)(i_5)] != b[(int)(i_5)]) {
                return false;
            }
            i_5 = i_5 + 1;
        }
        return true;
    }

    static String list_to_string(long[] xs) {
        if (xs.length == 0) {
            return "[]";
        }
        String s_1 = "[" + _p(_geti(xs, ((Number)(0)).intValue()));
        long i_7 = 1;
        while (i_7 < xs.length) {
            s_1 = s_1 + ", " + _p(_geti(xs, ((Number)(i_7)).intValue()));
            i_7 = i_7 + 1;
        }
        s_1 = s_1 + "]";
        return s_1;
    }

    static void test_kth_permutation() {
        long[] expected1 = ((long[])(new long[]{0, 1, 2, 3, 4}));
        long[] res1_1 = ((long[])(kth_permutation(0, 5)));
        if (!(Boolean)list_equal(((long[])(res1_1)), ((long[])(expected1)))) {
            throw new RuntimeException(String.valueOf("test case 1 failed"));
        }
        long[] expected2_1 = ((long[])(new long[]{1, 3, 0, 2}));
        long[] res2_1 = ((long[])(kth_permutation(10, 4)));
        if (!(Boolean)list_equal(((long[])(res2_1)), ((long[])(expected2_1)))) {
            throw new RuntimeException(String.valueOf("test case 2 failed"));
        }
    }

    static void main() {
        test_kth_permutation();
        long[] res_2 = ((long[])(kth_permutation(10, 4)));
        System.out.println(list_to_string(((long[])(res_2))));
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

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
