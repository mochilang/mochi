public class Main {
    static double r1;
    static double r2;
    static double r3;

    static double exp_approx(double x) {
        double y = x;
        boolean is_neg = false;
        if (x < 0.0) {
            is_neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (((Boolean)(is_neg))) {
            return 1.0 / sum;
        }
        return sum;
    }

    static double ln_series(double x) {
        double t = (x - 1.0) / (x + 1.0);
        double term_1 = t;
        double sum_1 = 0.0;
        int n_1 = 1;
        while (n_1 <= 19) {
            sum_1 = sum_1 + term_1 / (((Number)(n_1)).doubleValue());
            term_1 = term_1 * t * t;
            n_1 = n_1 + 2;
        }
        return 2.0 * sum_1;
    }

    static double ln(double x) {
        double y_1 = x;
        int k = 0;
        while (y_1 >= 10.0) {
            y_1 = y_1 / 10.0;
            k = k + 1;
        }
        while (y_1 < 1.0) {
            y_1 = y_1 * 10.0;
            k = k - 1;
        }
        return ln_series(y_1) + (((Number)(k)).doubleValue()) * ln_series(10.0);
    }

    static double powf(double base, double exponent) {
        return exp_approx(exponent * ln(base));
    }

    static double rainfall_intensity(double coefficient_k, double coefficient_a, double coefficient_b, double coefficient_c, double return_period, double duration) {
        if (coefficient_k <= 0.0) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if (coefficient_a <= 0.0) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if (coefficient_b <= 0.0) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if (coefficient_c <= 0.0) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if (return_period <= 0.0) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        if (duration <= 0.0) {
            throw new RuntimeException(String.valueOf("All parameters must be positive."));
        }
        double numerator = coefficient_k * powf(return_period, coefficient_a);
        double denominator = powf(duration + coefficient_b, coefficient_c);
        return numerator / denominator;
    }
    public static void main(String[] args) {
        r1 = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 10.0, 60.0);
        System.out.println(_p(r1));
        r2 = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 10.0, 30.0);
        System.out.println(_p(r2));
        r3 = rainfall_intensity(1000.0, 0.2, 11.6, 0.81, 5.0, 60.0);
        System.out.println(_p(r3));
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
