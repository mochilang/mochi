public class Main {

    static double ln(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_1 = t;
        double sum_1 = 0.0;
        long k_1 = 1;
        while (k_1 <= 99) {
            sum_1 = sum_1 + term_1 / (((Number)(k_1)).doubleValue());
            term_1 = term_1 * t * t;
            k_1 = k_1 + 2;
        }
        return 2.0 * sum_1;
    }

    static double log10(double x) {
        return ln(x) / ln(10.0);
    }

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double res(long x, long y) {
        if (x == 0) {
            return 0.0;
        }
        if (y == 0) {
            return 1.0;
        }
        if (x < 0) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        return (((Number)(y)).doubleValue()) * log10(((Number)(x)).doubleValue());
    }

    static void test_res() {
        if (absf(res(5, 7) - 4.892790030352132) > 1e-07) {
            throw new RuntimeException(String.valueOf("res(5,7) failed"));
        }
        if (res(0, 5) != 0.0) {
            throw new RuntimeException(String.valueOf("res(0,5) failed"));
        }
        if (res(3, 0) != 1.0) {
            throw new RuntimeException(String.valueOf("res(3,0) failed"));
        }
    }

    static String compare(long x1, long y1, long x2, long y2) {
        double r1 = res(x1, y1);
        double r2_1 = res(x2, y2);
        if (r1 > r2_1) {
            return "Largest number is " + _p(x1) + " ^ " + _p(y1);
        }
        if (r2_1 > r1) {
            return "Largest number is " + _p(x2) + " ^ " + _p(y2);
        }
        return "Both are equal";
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            test_res();
            System.out.println(compare(5, 7, 4, 8));
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
