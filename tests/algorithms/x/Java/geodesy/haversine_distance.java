public class Main {
    static double PI;
    static double AXIS_A;
    static double AXIS_B;
    static double RADIUS;
    static double[] SAN_FRANCISCO;
    static double[] YOSEMITE;

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

    static double tan_approx(double x) {
        return sin_taylor(x) / cos_taylor(x);
    }

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i_2 = 0;
        while (i_2 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_2 = i_2 + 1;
        }
        return guess;
    }

    static double atanApprox(double x) {
        if (x > 1.0) {
            return PI / 2.0 - x / (x * x + 0.28);
        }
        if (x < (-1.0)) {
            return -PI / 2.0 - x / (x * x + 0.28);
        }
        return x / (1.0 + 0.28 * x * x);
    }

    static double atan2Approx(double y, double x) {
        if (x > 0.0) {
            double val = atanApprox(y / x);
            return val;
        }
        if (x < 0.0) {
            if (y >= 0.0) {
                return atanApprox(y / x) + PI;
            }
            return atanApprox(y / x) - PI;
        }
        if (y > 0.0) {
            return PI / 2.0;
        }
        if (y < 0.0) {
            return -PI / 2.0;
        }
        return 0.0;
    }

    static double asinApprox(double x) {
        double denom = sqrtApprox(1.0 - x * x);
        double res = atan2Approx(x, denom);
        return res;
    }

    static double haversine_distance(double lat1, double lon1, double lat2, double lon2) {
        double flattening = (AXIS_A - AXIS_B) / AXIS_A;
        double phi_1 = atanApprox((1.0 - flattening) * tan_approx(to_radians(lat1)));
        double phi_2 = atanApprox((1.0 - flattening) * tan_approx(to_radians(lat2)));
        double lambda_1 = to_radians(lon1);
        double lambda_2 = to_radians(lon2);
        double sin_sq_phi = sin_taylor((phi_2 - phi_1) / 2.0);
        double sin_sq_lambda = sin_taylor((lambda_2 - lambda_1) / 2.0);
        sin_sq_phi = sin_sq_phi * sin_sq_phi;
        sin_sq_lambda = sin_sq_lambda * sin_sq_lambda;
        double h_value = sqrtApprox(sin_sq_phi + cos_taylor(phi_1) * cos_taylor(phi_2) * sin_sq_lambda);
        return 2.0 * RADIUS * asinApprox(h_value);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            AXIS_A = 6378137.0;
            AXIS_B = 6.356752314245e+06;
            RADIUS = 6378137.0;
            SAN_FRANCISCO = ((double[])(new double[]{37.774856, -122.424227}));
            YOSEMITE = ((double[])(new double[]{37.864742, -119.537521}));
            System.out.println(_p(haversine_distance(SAN_FRANCISCO[0], SAN_FRANCISCO[1], YOSEMITE[0], YOSEMITE[1])));
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
