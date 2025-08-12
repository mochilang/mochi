public class Main {

    static long binary_search(long[] arr, long lower_bound, long upper_bound, long value) {
        long r = Math.floorDiv((lower_bound + upper_bound), 2);
        if (_geti(arr, (int)((long)(r))) == value) {
            return r;
        }
        if (lower_bound >= upper_bound) {
            return -1;
        }
        if (_geti(arr, (int)((long)(r))) < value) {
            return binary_search(((long[])(arr)), (long)((long)(r) + (long)(1)), upper_bound, value);
        }
        return binary_search(((long[])(arr)), lower_bound, (long)((long)(r) - (long)(1)), value);
    }

    static long[] mat_bin_search(long value, long[][] matrix) {
        long index = 0L;
        if (_geti(((long[])_geto(matrix, (int)((long)(index)))), (int)((long)(0))) == value) {
            return new long[]{index, 0};
        }
        while (index < (long)(matrix.length) && _geti(((long[])_geto(matrix, (int)((long)(index)))), (int)((long)(0))) < value) {
            long r_2 = binary_search(((long[])(((long[])_geto(matrix, (int)((long)(index)))))), 0L, (long)((long)(((long[])_geto(matrix, (int)((long)(index)))).length) - (long)(1)), value);
            if (r_2 != (long)((-1))) {
                return new long[]{index, r_2};
            }
            index = (long)(index + (long)(1));
        }
        return new long[]{-1, -1};
    }

    static void main() {
        long[] row = ((long[])(new long[]{1, 4, 7, 11, 15}));
        System.out.println(_p(binary_search(((long[])(row)), 0L, (long)((long)(row.length) - (long)(1)), 1L)));
        System.out.println(_p(binary_search(((long[])(row)), 0L, (long)((long)(row.length) - (long)(1)), 23L)));
        long[][] matrix_1 = ((long[][])(new long[][]{new long[]{1, 4, 7, 11, 15}, new long[]{2, 5, 8, 12, 19}, new long[]{3, 6, 9, 16, 22}, new long[]{10, 13, 14, 17, 24}, new long[]{18, 21, 23, 26, 30}}));
        System.out.println(_p(mat_bin_search(1L, ((long[][])(matrix_1)))));
        System.out.println(_p(mat_bin_search(34L, ((long[][])(matrix_1)))));
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

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }

    static Object _geto(Object[] a, int i) {
        if (a == null) return null;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return null;
        return a[i];
    }
}
