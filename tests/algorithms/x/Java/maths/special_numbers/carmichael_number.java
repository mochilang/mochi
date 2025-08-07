public class Main {

    static int abs_int(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int gcd(int a, int b) {
        if (a == 0) {
            return abs_int(b);
        }
        return gcd(Math.floorMod(b, a), a);
    }

    static int power(int x, int y, int m) {
        if (y == 0) {
            return Math.floorMod(1, m);
        }
        int temp = Math.floorMod(power(x, Math.floorDiv(y, 2), m), m);
        temp = Math.floorMod((temp * temp), m);
        if (Math.floorMod(y, 2) == 1) {
            temp = Math.floorMod((temp * x), m);
        }
        return temp;
    }

    static boolean is_carmichael_number(int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("Number must be positive"));
        }
        int b = 2;
        while (b < n) {
            if (gcd(b, n) == 1) {
                if (power(b, n - 1, n) != 1) {
                    return false;
                }
            }
            b = b + 1;
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(power(2, 15, 3)));
            System.out.println(_p(power(5, 1, 30)));
            System.out.println(_p(is_carmichael_number(4)));
            System.out.println(_p(is_carmichael_number(561)));
            System.out.println(_p(is_carmichael_number(562)));
            System.out.println(_p(is_carmichael_number(1105)));
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
