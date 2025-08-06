public class Main {
    static double PI;
    static double EQUATORIAL_RADIUS;

    static double to_radians(double deg) {
        return deg * PI / 180.0;
    }

    static double sin_approx(double x) {
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

    static double cos_approx(double x) {
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

    static double sqrt_approx(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i_2 = 0;
        while (i_2 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_2 = i_2 + 1;
        }
        return guess;
    }

    static double lamberts_ellipsoidal_distance(double lat1, double lon1, double lat2, double lon2) {
        double phi1 = to_radians(lat1);
        double phi2 = to_radians(lat2);
        double lambda1 = to_radians(lon1);
        double lambda2 = to_radians(lon2);
        double x = (lambda2 - lambda1) * cos_approx((phi1 + phi2) / 2.0);
        double y = phi2 - phi1;
        return EQUATORIAL_RADIUS * sqrt_approx(x * x + y * y);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            EQUATORIAL_RADIUS = 6378137.0;
            System.out.println(lamberts_ellipsoidal_distance(37.774856, -122.424227, 37.864742, -119.537521));
            System.out.println(lamberts_ellipsoidal_distance(37.774856, -122.424227, 40.713019, -74.012647));
            System.out.println(lamberts_ellipsoidal_distance(37.774856, -122.424227, 45.443012, 12.313071));
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
