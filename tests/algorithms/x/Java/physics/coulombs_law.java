public class Main {
    static double K;

    static String format2(double x) {
        String sign = String.valueOf((double)(x) < 0.0 ? "-" : "");
        double y_1 = (double)((double)(x) < 0.0 ? -x : x);
        double m_1 = 100.0;
        double scaled_1 = y_1 * m_1;
        long i_1 = (long)(((Number)(scaled_1)).intValue());
        if (scaled_1 - (((Number)(i_1)).doubleValue()) >= 0.5) {
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        long int_part_1 = Math.floorDiv(i_1, 100);
        long frac_part_1 = Math.floorMod(i_1, 100);
        String frac_str_1 = _p(frac_part_1);
        if ((long)(frac_part_1) < (long)(10)) {
            frac_str_1 = "0" + frac_str_1;
        }
        return sign + _p(int_part_1) + "." + frac_str_1;
    }

    static double coulombs_law(double q1, double q2, double radius) {
        if ((double)(radius) <= 0.0) {
            throw new RuntimeException(String.valueOf("radius must be positive"));
        }
        double force_1 = (double)(K) * (double)(q1) * (double)(q2) / ((double)(radius) * (double)(radius));
        return force_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            K = 8.9875517923e+09;
            System.out.println(format2((double)(coulombs_law(15.5, 20.0, 15.0))));
            System.out.println(format2((double)(coulombs_law(1.0, 15.0, 5.0))));
            System.out.println(format2((double)(coulombs_law(20.0, -50.0, 15.0))));
            System.out.println(format2((double)(coulombs_law(-5.0, -8.0, 10.0))));
            System.out.println(format2((double)(coulombs_law(50.0, 100.0, 50.0))));
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
