public class Main {
    static double PI;

    static double floor(double x) {
        long i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow(double x, long n) {
        double result = 1.0;
        long i_2 = 0;
        while (i_2 < n) {
            result = result * x;
            i_2 = i_2 + 1;
        }
        return result;
    }

    static double factorial(long n) {
        double result_1 = 1.0;
        long i_4 = 2;
        while (i_4 <= n) {
            result_1 = result_1 * (((Number)(i_4)).doubleValue());
            i_4 = i_4 + 1;
        }
        return result_1;
    }

    static double maclaurin_sin(double theta, long accuracy) {
        double t = theta;
        double div_1 = floor(t / (2.0 * PI));
        t = t - 2.0 * div_1 * PI;
        double sum_1 = 0.0;
        long r_1 = 0;
        while (r_1 < accuracy) {
            long power_1 = 2 * r_1 + 1;
            double sign_1 = Math.floorMod(r_1, 2) == 0 ? 1.0 : -1.0;
            sum_1 = sum_1 + sign_1 * pow(t, power_1) / factorial(power_1);
            r_1 = r_1 + 1;
        }
        return sum_1;
    }

    static double maclaurin_cos(double theta, long accuracy) {
        double t_1 = theta;
        double div_3 = floor(t_1 / (2.0 * PI));
        t_1 = t_1 - 2.0 * div_3 * PI;
        double sum_3 = 0.0;
        long r_3 = 0;
        while (r_3 < accuracy) {
            long power_3 = 2 * r_3;
            double sign_3 = Math.floorMod(r_3, 2) == 0 ? 1.0 : -1.0;
            sum_3 = sum_3 + sign_3 * pow(t_1, power_3) / factorial(power_3);
            r_3 = r_3 + 1;
        }
        return sum_3;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            System.out.println(_p(maclaurin_sin(10.0, 30)));
            System.out.println(_p(maclaurin_sin(-10.0, 30)));
            System.out.println(_p(maclaurin_sin(10.0, 15)));
            System.out.println(_p(maclaurin_sin(-10.0, 15)));
            System.out.println(_p(maclaurin_cos(5.0, 30)));
            System.out.println(_p(maclaurin_cos(-5.0, 30)));
            System.out.println(_p(maclaurin_cos(10.0, 15)));
            System.out.println(_p(maclaurin_cos(-10.0, 15)));
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
