public class Main {
    static double PI;

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double to_radians(double deg) {
        return deg * PI / 180.0;
    }

    static double sin_taylor(double x) {
        double term = x;
        double sum = x;
        int i = 1;
        while (i < 10) {
            double k1 = 2.0 * (((Number)(i)).doubleValue());
            double k2 = k1 + 1.0;
            term = -term * x * x / (k1 * k2);
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double cos_taylor(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int i_1 = 1;
        while (i_1 < 10) {
            double k1_1 = 2.0 * (((Number)(i_1)).doubleValue()) - 1.0;
            double k2_1 = 2.0 * (((Number)(i_1)).doubleValue());
            term_1 = -term_1 * x * x / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static double[] rect(double mag, double angle) {
        double c = cos_taylor(angle);
        double s = sin_taylor(angle);
        return new double[]{mag * c, mag * s};
    }

    static double[] multiply(double[] a, double[] b) {
        return new double[]{a[0] * b[0] - a[1] * b[1], a[0] * b[1] + a[1] * b[0]};
    }

    static double[] apparent_power(double voltage, double current, double voltage_angle, double current_angle) {
        double vrad = to_radians(voltage_angle);
        double irad = to_radians(current_angle);
        double[] vrect = ((double[])(rect(voltage, vrad)));
        double[] irect = ((double[])(rect(current, irad)));
        double[] result = ((double[])(multiply(((double[])(vrect)), ((double[])(irect)))));
        return result;
    }

    static boolean approx_equal(double[] a, double[] b, double eps) {
        return Math.abs(a[0] - b[0]) < eps && Math.abs(a[1] - b[1]) < eps;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
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
}
