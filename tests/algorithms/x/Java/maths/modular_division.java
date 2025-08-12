public class Main {

    static long mod(long a, long n) {
        long r = Math.floorMod(a, n);
        if (r < 0) {
            return r + n;
        }
        return r;
    }

    static long greatest_common_divisor(long a, long b) {
        long x = a < 0 ? -a : a;
        long y_1 = b < 0 ? -b : b;
        while (y_1 != 0) {
            long t_1 = Math.floorMod(x, y_1);
            x = y_1;
            y_1 = t_1;
        }
        return x;
    }

    static long[] extended_gcd(long a, long b) {
        if (b == 0) {
            return new long[]{a, 1, 0};
        }
        long[] res_1 = ((long[])(extended_gcd(b, Math.floorMod(a, b))));
        long d_1 = res_1[(int)(0)];
        long p_1 = res_1[(int)(1)];
        long q_1 = res_1[(int)(2)];
        long x_2 = q_1;
        long y_3 = p_1 - q_1 * ((Number)((Math.floorDiv(a, b)))).intValue();
        return new long[]{d_1, x_2, y_3};
    }

    static long[] extended_euclid(long a, long b) {
        if (b == 0) {
            return new long[]{1, 0};
        }
        long[] res_3 = ((long[])(extended_euclid(b, Math.floorMod(a, b))));
        long x_4 = res_3[(int)(1)];
        long y_5 = res_3[(int)(0)] - ((Number)((Math.floorDiv(a, b)))).intValue() * res_3[(int)(1)];
        return new long[]{x_4, y_5};
    }

    static long invert_modulo(long a, long n) {
        long[] res_4 = ((long[])(extended_euclid(a, n)));
        long inv_1 = res_4[(int)(0)];
        return mod(inv_1, n);
    }

    static long modular_division(long a, long b, long n) {
        if (n <= 1) {
            throw new RuntimeException(String.valueOf("n must be > 1"));
        }
        if (a <= 0) {
            throw new RuntimeException(String.valueOf("a must be > 0"));
        }
        if (greatest_common_divisor(a, n) != 1) {
            throw new RuntimeException(String.valueOf("gcd(a,n) != 1"));
        }
        long[] eg_1 = ((long[])(extended_gcd(n, a)));
        long s_1 = eg_1[(int)(2)];
        return mod(b * s_1, n);
    }

    static long modular_division2(long a, long b, long n) {
        long s_2 = invert_modulo(a, n);
        return mod(b * s_2, n);
    }

    static void tests() {
        if (modular_division(4, 8, 5) != 2) {
            throw new RuntimeException(String.valueOf("md1"));
        }
        if (modular_division(3, 8, 5) != 1) {
            throw new RuntimeException(String.valueOf("md2"));
        }
        if (modular_division(4, 11, 5) != 4) {
            throw new RuntimeException(String.valueOf("md3"));
        }
        if (modular_division2(4, 8, 5) != 2) {
            throw new RuntimeException(String.valueOf("md21"));
        }
        if (modular_division2(3, 8, 5) != 1) {
            throw new RuntimeException(String.valueOf("md22"));
        }
        if (modular_division2(4, 11, 5) != 4) {
            throw new RuntimeException(String.valueOf("md23"));
        }
        if (invert_modulo(2, 5) != 3) {
            throw new RuntimeException(String.valueOf("inv"));
        }
        long[] eg_3 = ((long[])(extended_gcd(10, 6)));
        if (eg_3[(int)(0)] != 2 || eg_3[(int)(1)] != (-1) || eg_3[(int)(2)] != 2) {
            throw new RuntimeException(String.valueOf("eg"));
        }
        long[] eu_1 = ((long[])(extended_euclid(10, 6)));
        if (eu_1[(int)(0)] != (-1) || eu_1[(int)(1)] != 2) {
            throw new RuntimeException(String.valueOf("eu"));
        }
        if (greatest_common_divisor(121, 11) != 11) {
            throw new RuntimeException(String.valueOf("gcd"));
        }
    }

    static void main() {
        tests();
        System.out.println(_p(modular_division(4, 8, 5)));
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
