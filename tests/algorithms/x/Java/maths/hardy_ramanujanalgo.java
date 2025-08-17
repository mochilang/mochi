public class Main {

    static long exact_prime_factor_count(long n) {
        long count = 0L;
        long num_1 = (long)(n);
        if (Math.floorMod(num_1, 2) == 0L) {
            count = (long)((long)(count) + 1L);
            while (Math.floorMod(num_1, 2) == 0L) {
                num_1 = Math.floorDiv(num_1, 2);
            }
        }
        long i_1 = 3L;
        while ((long)((long)(i_1) * (long)(i_1)) <= (long)(num_1)) {
            if (Math.floorMod(num_1, i_1) == 0L) {
                count = (long)((long)(count) + 1L);
                while (Math.floorMod(num_1, i_1) == 0L) {
                    num_1 = Math.floorDiv(num_1, i_1);
                }
            }
            i_1 = (long)((long)(i_1) + 2L);
        }
        if ((long)(num_1) > 2L) {
            count = (long)((long)(count) + 1L);
        }
        return count;
    }

    static double ln(double x) {
        double ln2 = (double)(0.6931471805599453);
        double y_1 = (double)(x);
        double k_1 = (double)(0.0);
        while ((double)(y_1) > (double)(2.0)) {
            y_1 = (double)((double)(y_1) / (double)(2.0));
            k_1 = (double)((double)(k_1) + (double)(ln2));
        }
        while ((double)(y_1) < (double)(1.0)) {
            y_1 = (double)((double)(y_1) * (double)(2.0));
            k_1 = (double)((double)(k_1) - (double)(ln2));
        }
        double t_1 = (double)((double)(((double)(y_1) - (double)(1.0))) / (double)(((double)(y_1) + (double)(1.0))));
        double term_1 = (double)(t_1);
        double sum_1 = (double)(0.0);
        long n_1 = 1L;
        while ((long)(n_1) <= 19L) {
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)((((Number)(n_1)).doubleValue()))));
            term_1 = (double)((double)((double)(term_1) * (double)(t_1)) * (double)(t_1));
            n_1 = (long)((long)(n_1) + 2L);
        }
        return (double)(k_1) + (double)((double)(2.0) * (double)(sum_1));
    }

    static double floor(double x) {
        long i_2 = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i_2)).doubleValue())) > (double)(x)) {
            i_2 = (long)((long)(i_2) - 1L);
        }
        return ((Number)(i_2)).doubleValue();
    }

    static double round4(double x) {
        double m = (double)(10000.0);
        return Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m);
    }

    static void main() {
        long n_2 = 51242183L;
        long count_2 = (long)(exact_prime_factor_count((long)(n_2)));
        System.out.println("The number of distinct prime factors is/are " + _p(count_2));
        double loglog_1 = (double)(ln((double)(ln((double)(((Number)(n_2)).doubleValue())))));
        System.out.println("The value of log(log(n)) is " + _p(round4((double)(loglog_1))));
    }
    public static void main(String[] args) {
        main();
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
