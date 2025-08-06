public class Main {
    static class EuclidResult {
        int x;
        int y;
        EuclidResult(int x, int y) {
            this.x = x;
            this.y = y;
        }
        EuclidResult() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static EuclidResult e1;
    static EuclidResult e2;

    static EuclidResult extended_euclid(int a, int b) {
        if (b == 0) {
            return new EuclidResult(1, 0);
        }
        EuclidResult res = extended_euclid(b, Math.floorMod(a, b));
        int k = a / b;
        return new EuclidResult(res.y, res.x - k * res.y);
    }

    static int chinese_remainder_theorem(int n1, int r1, int n2, int r2) {
        EuclidResult res_1 = extended_euclid(n1, n2);
        int x = res_1.x;
        int y = res_1.y;
        int m = n1 * n2;
        int n = r2 * x * n1 + r1 * y * n2;
        return Math.floorMod(((Math.floorMod(n, m)) + m), m);
    }

    static int invert_modulo(int a, int n) {
        EuclidResult res_2 = extended_euclid(a, n);
        int b = res_2.x;
        if (b < 0) {
            b = Math.floorMod((Math.floorMod(b, n) + n), n);
        }
        return b;
    }

    static int chinese_remainder_theorem2(int n1, int r1, int n2, int r2) {
        int x_1 = invert_modulo(n1, n2);
        int y_1 = invert_modulo(n2, n1);
        int m_1 = n1 * n2;
        int n_1 = r2 * x_1 * n1 + r1 * y_1 * n2;
        return Math.floorMod(((Math.floorMod(n_1, m_1)) + m_1), m_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            e1 = extended_euclid(10, 6);
            System.out.println(_p(e1.x) + "," + _p(e1.y));
            e2 = extended_euclid(7, 5);
            System.out.println(_p(e2.x) + "," + _p(e2.y));
            System.out.println(_p(chinese_remainder_theorem(5, 1, 7, 3)));
            System.out.println(_p(chinese_remainder_theorem(6, 1, 4, 3)));
            System.out.println(_p(invert_modulo(2, 5)));
            System.out.println(_p(invert_modulo(8, 7)));
            System.out.println(_p(chinese_remainder_theorem2(5, 1, 7, 3)));
            System.out.println(_p(chinese_remainder_theorem2(6, 1, 4, 3)));
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
