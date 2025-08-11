public class Main {
    static long n;

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double factorial_float(long n) {
        double result = 1.0;
        long i_3 = 2;
        while (i_3 <= n) {
            result = result * (((Number)(i_3)).doubleValue());
            i_3 = i_3 + 1;
        }
        return result;
    }

    static double pi(long n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("Undefined for non-natural numbers"));
        }
        long iterations_1 = Math.floorDiv((n + 13), 14);
        double constant_term_1 = 426880.0 * sqrtApprox(10005.0);
        double exponential_term_1 = 1.0;
        double linear_term_1 = 13591409.0;
        double partial_sum_1 = linear_term_1;
        long k_1 = 1;
        while (k_1 < iterations_1) {
            long k6_1 = 6 * k_1;
            long k3_1 = 3 * k_1;
            double fact6k_1 = factorial_float(k6_1);
            double fact3k_1 = factorial_float(k3_1);
            double factk_1 = factorial_float(k_1);
            double multinomial_1 = fact6k_1 / (fact3k_1 * factk_1 * factk_1 * factk_1);
            linear_term_1 = linear_term_1 + 545140134.0;
            exponential_term_1 = exponential_term_1 * (-262537412640768000.0);
            partial_sum_1 = partial_sum_1 + multinomial_1 * linear_term_1 / exponential_term_1;
            k_1 = k_1 + 1;
        }
        return constant_term_1 / partial_sum_1;
    }
    public static void main(String[] args) {
        n = 50;
        System.out.println("The first " + _p(n) + " digits of pi is: " + _p(pi(n)));
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
