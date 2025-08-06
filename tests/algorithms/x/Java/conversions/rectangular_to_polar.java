public class Main {
    static double PI;

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
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
            double r = atanApprox(y / x);
            return r;
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

    static double deg(double rad) {
        return rad * 180.0 / PI;
    }

    static double floor(double x) {
        int i_1 = ((Number)(x)).intValue();
        if ((((Number)(i_1)).doubleValue()) > x) {
            i_1 = i_1 - 1;
        }
        return ((Number)(i_1)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_2 = 0;
        while (i_2 < n) {
            p = p * 10.0;
            i_2 = i_2 + 1;
        }
        return p;
    }

    static double round(double x, int n) {
        double m = pow10(n);
        return floor(x * m + 0.5) / m;
    }

    static double[] rectangular_to_polar(double real, double img) {
        double mod = round(sqrtApprox(real * real + img * img), 2);
        double ang = round(deg(atan2Approx(img, real)), 2);
        return new double[]{mod, ang};
    }

    static void show(double real, double img) {
        double[] r_1 = ((double[])(rectangular_to_polar(real, img)));
        System.out.println(_p(r_1));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        show(5.0, -5.0);
        show(-1.0, 1.0);
        show(-1.0, -1.0);
        show(1e-10, 1e-10);
        show(-1e-10, 1e-10);
        show(9.75, 5.93);
        show(10000.0, 99999.0);
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
