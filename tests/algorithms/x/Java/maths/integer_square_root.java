public class Main {

    static long integer_square_root(long num) {
        if (num < 0) {
            throw new RuntimeException(String.valueOf("num must be non-negative integer"));
        }
        if (num < 2) {
            return num;
        }
        long left_bound_1 = 0;
        long right_bound_1 = Math.floorDiv(num, 2);
        while (left_bound_1 <= right_bound_1) {
            long mid_1 = left_bound_1 + Math.floorDiv((right_bound_1 - left_bound_1), 2);
            long mid_squared_1 = mid_1 * mid_1;
            if (mid_squared_1 == num) {
                return mid_1;
            }
            if (mid_squared_1 < num) {
                left_bound_1 = mid_1 + 1;
            } else {
                right_bound_1 = mid_1 - 1;
            }
        }
        return right_bound_1;
    }

    static void test_integer_square_root() {
        long[] expected = ((long[])(new long[]{0, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4}));
        long i_1 = 0;
        while (i_1 < expected.length) {
            long result_1 = integer_square_root(i_1);
            if (result_1 != expected[(int)(i_1)]) {
                throw new RuntimeException(String.valueOf("test failed at index " + _p(i_1)));
            }
            i_1 = i_1 + 1;
        }
        if (integer_square_root(625) != 25) {
            throw new RuntimeException(String.valueOf("sqrt of 625 incorrect"));
        }
        if (integer_square_root(2147483647) != 46340) {
            throw new RuntimeException(String.valueOf("sqrt of max int incorrect"));
        }
    }

    static void main() {
        test_integer_square_root();
        System.out.println(_p(integer_square_root(625)));
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
