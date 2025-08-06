public class Main {

    static int abs_int(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static int greatest_common_divisor(int a, int b) {
        int x = abs_int(a);
        int y = abs_int(b);
        if (x == 0) {
            return y;
        }
        return greatest_common_divisor(Math.floorMod(y, x), x);
    }

    static int gcd_by_iterative(int x, int y) {
        int a = abs_int(x);
        int b = abs_int(y);
        while (b != 0) {
            int temp = b;
            b = Math.floorMod(a, b);
            a = temp;
        }
        return a;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(greatest_common_divisor(24, 40)));
            System.out.println(_p(greatest_common_divisor(1, 1)));
            System.out.println(_p(greatest_common_divisor(1, 800)));
            System.out.println(_p(greatest_common_divisor(11, 37)));
            System.out.println(_p(greatest_common_divisor(3, 5)));
            System.out.println(_p(greatest_common_divisor(16, 4)));
            System.out.println(_p(greatest_common_divisor(-3, 9)));
            System.out.println(_p(greatest_common_divisor(9, -3)));
            System.out.println(_p(greatest_common_divisor(3, -9)));
            System.out.println(_p(greatest_common_divisor(-3, -9)));
            System.out.println(_p(gcd_by_iterative(24, 40)));
            System.out.println(_p(greatest_common_divisor(24, 40) == gcd_by_iterative(24, 40)));
            System.out.println(_p(gcd_by_iterative(-3, -9)));
            System.out.println(_p(gcd_by_iterative(3, -9)));
            System.out.println(_p(gcd_by_iterative(1, -800)));
            System.out.println(_p(gcd_by_iterative(11, 37)));
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
