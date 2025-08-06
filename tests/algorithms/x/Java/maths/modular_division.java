public class Main {

    static int mod(int a, int n) {
        int r = Math.floorMod(a, n);
        if (r < 0) {
            return r + n;
        }
        return r;
    }

    static int greatest_common_divisor(int a, int b) {
        int x = a < 0 ? -a : a;
        int y = b < 0 ? -b : b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        return x;
    }

    static int[] extended_gcd(int a, int b) {
        if (b == 0) {
            return new int[]{a, 1, 0};
        }
        int[] res = ((int[])(extended_gcd(b, Math.floorMod(a, b))));
        int d = res[0];
        int p = res[1];
        int q = res[2];
        int x_1 = q;
        int y_1 = p - q * (a / b);
        return new int[]{d, x_1, y_1};
    }

    static int[] extended_euclid(int a, int b) {
        if (b == 0) {
            return new int[]{1, 0};
        }
        int[] res_1 = ((int[])(extended_euclid(b, Math.floorMod(a, b))));
        int x_2 = res_1[1];
        int y_2 = res_1[0] - (a / b) * res_1[1];
        return new int[]{x_2, y_2};
    }

    static int invert_modulo(int a, int n) {
        int[] res_2 = ((int[])(extended_euclid(a, n)));
        int inv = res_2[0];
        return mod(inv, n);
    }

    static int modular_division(int a, int b, int n) {
        if (n <= 1) {
            throw new RuntimeException(String.valueOf("n must be > 1"));
        }
        if (a <= 0) {
            throw new RuntimeException(String.valueOf("a must be > 0"));
        }
        if (greatest_common_divisor(a, n) != 1) {
            throw new RuntimeException(String.valueOf("gcd(a,n) != 1"));
        }
        int[] eg = ((int[])(extended_gcd(n, a)));
        int s = eg[2];
        return mod(b * s, n);
    }

    static int modular_division2(int a, int b, int n) {
        int s_1 = invert_modulo(a, n);
        return mod(b * s_1, n);
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
        int[] eg_1 = ((int[])(extended_gcd(10, 6)));
        if (eg_1[0] != 2 || eg_1[1] != (-1) || eg_1[2] != 2) {
            throw new RuntimeException(String.valueOf("eg"));
        }
        int[] eu = ((int[])(extended_euclid(10, 6)));
        if (eu[0] != (-1) || eu[1] != 2) {
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
        return String.valueOf(v);
    }
}
