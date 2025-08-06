public class Main {

    static int integer_square_root(int num) {
        if (num < 0) {
            throw new RuntimeException(String.valueOf("num must be non-negative integer"));
        }
        if (num < 2) {
            return num;
        }
        int left_bound = 0;
        int right_bound = num / 2;
        while (left_bound <= right_bound) {
            int mid = left_bound + (right_bound - left_bound) / 2;
            int mid_squared = mid * mid;
            if (mid_squared == num) {
                return mid;
            }
            if (mid_squared < num) {
                left_bound = mid + 1;
            } else {
                right_bound = mid - 1;
            }
        }
        return right_bound;
    }

    static void test_integer_square_root() {
        int[] expected = ((int[])(new int[]{0, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4}));
        int i = 0;
        while (i < expected.length) {
            int result = integer_square_root(i);
            if (result != expected[i]) {
                throw new RuntimeException(String.valueOf("test failed at index " + _p(i)));
            }
            i = i + 1;
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
        return String.valueOf(v);
    }
}
