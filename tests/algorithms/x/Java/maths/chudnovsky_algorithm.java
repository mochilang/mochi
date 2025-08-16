public class Main {
    static long n = 50L;

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double factorial_float(long n) {
        double result = (double)(1.0);
        long i_3 = 2L;
        while ((long)(i_3) <= (long)(n)) {
            result = (double)((double)(result) * (double)((((Number)(i_3)).doubleValue())));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result;
    }

    static double pi(long n) {
        if ((long)(n) < 1L) {
            throw new RuntimeException(String.valueOf("Undefined for non-natural numbers"));
        }
        long iterations_1 = Math.floorDiv(((long)(((long)(n) + 13L))), ((long)(14)));
        double constant_term_1 = (double)((double)(426880.0) * (double)(sqrtApprox((double)(10005.0))));
        double exponential_term_1 = (double)(1.0);
        double linear_term_1 = (double)(13591409.0);
        double partial_sum_1 = (double)(linear_term_1);
        long k_1 = 1L;
        while ((long)(k_1) < (long)(iterations_1)) {
            long k6_1 = (long)(6L * (long)(k_1));
            long k3_1 = (long)(3L * (long)(k_1));
            double fact6k_1 = (double)(factorial_float((long)(k6_1)));
            double fact3k_1 = (double)(factorial_float((long)(k3_1)));
            double factk_1 = (double)(factorial_float((long)(k_1)));
            double multinomial_1 = (double)((double)(fact6k_1) / (double)(((double)((double)((double)(fact3k_1) * (double)(factk_1)) * (double)(factk_1)) * (double)(factk_1))));
            linear_term_1 = (double)((double)(linear_term_1) + (double)(545140134.0));
            exponential_term_1 = (double)((double)(exponential_term_1) * (double)((-262537412640768000.0)));
            partial_sum_1 = (double)((double)(partial_sum_1) + (double)((double)((double)(multinomial_1) * (double)(linear_term_1)) / (double)(exponential_term_1)));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return (double)(constant_term_1) / (double)(partial_sum_1);
    }
    public static void main(String[] args) {
        System.out.println("The first " + _p(n) + " digits of pi is: " + _p(pi((long)(n))));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
