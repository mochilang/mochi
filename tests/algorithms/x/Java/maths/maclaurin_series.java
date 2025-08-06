public class Main {
    static double PI;

    static double floor(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow(double x, int n) {
        double result = 1.0;
        int i_1 = 0;
        while (i_1 < n) {
            result = result * x;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double factorial(int n) {
        double result_1 = 1.0;
        int i_2 = 2;
        while (i_2 <= n) {
            result_1 = result_1 * (((Number)(i_2)).doubleValue());
            i_2 = i_2 + 1;
        }
        return result_1;
    }

    static double maclaurin_sin(double theta, int accuracy) {
        double t = theta;
        double div = floor(t / (2.0 * PI));
        t = t - 2.0 * div * PI;
        double sum = 0.0;
        int r = 0;
        while (r < accuracy) {
            int power = 2 * r + 1;
            double sign = Math.floorMod(r, 2) == 0 ? 1.0 : -1.0;
            sum = sum + sign * pow(t, power) / factorial(power);
            r = r + 1;
        }
        return sum;
    }

    static double maclaurin_cos(double theta, int accuracy) {
        double t_1 = theta;
        double div_1 = floor(t_1 / (2.0 * PI));
        t_1 = t_1 - 2.0 * div_1 * PI;
        double sum_1 = 0.0;
        int r_1 = 0;
        while (r_1 < accuracy) {
            int power_1 = 2 * r_1;
            double sign_1 = Math.floorMod(r_1, 2) == 0 ? 1.0 : -1.0;
            sum_1 = sum_1 + sign_1 * pow(t_1, power_1) / factorial(power_1);
            r_1 = r_1 + 1;
        }
        return sum_1;
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
        return String.valueOf(v);
    }
}
