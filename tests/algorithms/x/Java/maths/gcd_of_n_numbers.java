public class Main {

    static long gcd(long a, long b) {
        long x = a;
        long y_1 = b;
        while (y_1 != 0) {
            long r_1 = Math.floorMod(x, y_1);
            x = y_1;
            y_1 = r_1;
        }
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static long get_greatest_common_divisor(long[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("at least one number is required"));
        }
        long g_1 = nums[(int)(0)];
        if (g_1 <= 0) {
            throw new RuntimeException(String.valueOf("numbers must be integer and greater than zero"));
        }
        long i_1 = 1;
        while (i_1 < nums.length) {
            long n_1 = nums[(int)(i_1)];
            if (n_1 <= 0) {
                throw new RuntimeException(String.valueOf("numbers must be integer and greater than zero"));
            }
            g_1 = gcd(g_1, n_1);
            i_1 = i_1 + 1;
        }
        return g_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{18, 45})))));
            System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{23, 37})))));
            System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{2520, 8350})))));
            System.out.println(_p(get_greatest_common_divisor(((long[])(new long[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))));
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
