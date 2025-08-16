public class Main {
    static double PI = (double)(3.141592653589793);

    static double absf(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double sqrt(double x) {
        if ((double)(x) < (double)(0.0)) {
            throw new RuntimeException(String.valueOf("sqrt domain error"));
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double ln(double x) {
        if ((double)(x) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = (double)((double)(((double)(x) - (double)(1.0))) / (double)(((double)(x) + (double)(1.0))));
        double y2_1 = (double)((double)(y_1) * (double)(y_1));
        double term_1 = (double)(y_1);
        double sum_1 = (double)(0.0);
        long k_1 = 0L;
        while ((long)(k_1) < 10L) {
            double denom_1 = (double)(((Number)(((long)(2L * (long)(k_1)) + 1L))).doubleValue());
            sum_1 = (double)((double)(sum_1) + (double)((double)(term_1) / (double)(denom_1)));
            term_1 = (double)((double)(term_1) * (double)(y2_1));
            k_1 = (long)((long)(k_1) + 1L);
        }
        return (double)(2.0) * (double)(sum_1);
    }

    static double exp_series(double x) {
        double term_2 = (double)(1.0);
        double sum_3 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 20L) {
            term_2 = (double)((double)((double)(term_2) * (double)(x)) / (double)((((Number)(n_1)).doubleValue())));
            sum_3 = (double)((double)(sum_3) + (double)(term_2));
            n_1 = (long)((long)(n_1) + 1L);
        }
        return sum_3;
    }

    static double powf(double base, double exponent) {
        if ((double)(base) <= (double)(0.0)) {
            return 0.0;
        }
        return exp_series((double)((double)(exponent) * (double)(ln((double)(base)))));
    }

    static double integrand(double x, double z) {
        return (double)(powf((double)(x), (double)((double)(z) - (double)(1.0)))) * (double)(exp_series((double)(-x)));
    }

    static double gamma_iterative(double num) {
        if ((double)(num) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        double step_1 = (double)(0.001);
        double limit_1 = (double)(100.0);
        double x_1 = (double)(step_1);
        double total_1 = (double)(0.0);
        while ((double)(x_1) < (double)(limit_1)) {
            total_1 = (double)((double)(total_1) + (double)((double)(integrand((double)(x_1), (double)(num))) * (double)(step_1)));
            x_1 = (double)((double)(x_1) + (double)(step_1));
        }
        return total_1;
    }

    static double gamma_recursive(double num) {
        if ((double)(num) <= (double)(0.0)) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        if ((double)(num) > (double)(171.5)) {
            throw new RuntimeException(String.valueOf("math range error"));
        }
        long int_part_1 = (long)(((Number)(num)).intValue());
        double frac_1 = (double)((double)(num) - (double)((((Number)(int_part_1)).doubleValue())));
        if (!((double)(absf((double)(frac_1))) < (double)(1e-06) || (double)(absf((double)((double)(frac_1) - (double)(0.5)))) < (double)(1e-06))) {
            throw new RuntimeException(String.valueOf("num must be an integer or a half-integer"));
        }
        if ((double)(absf((double)((double)(num) - (double)(0.5)))) < (double)(1e-06)) {
            return sqrt((double)(PI));
        }
        if ((double)(absf((double)((double)(num) - (double)(1.0)))) < (double)(1e-06)) {
            return 1.0;
        }
        return (double)(((double)(num) - (double)(1.0))) * (double)(gamma_recursive((double)((double)(num) - (double)(1.0))));
    }

    static void main() {
        System.out.println(gamma_iterative((double)(5.0)));
        System.out.println(gamma_recursive((double)(5.0)));
        System.out.println(gamma_recursive((double)(0.5)));
    }
    public static void main(String[] args) {
        main();
    }
}
