public class Main {
    static double PI = (double)(3.141592653589793);

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static double expApprox(double x) {
        boolean is_neg = false;
        double y_1 = (double)(x);
        if ((double)(x) < (double)(0.0)) {
            is_neg = true;
            y_1 = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 30L) {
            term_1 = (double)((double)((double)(term_1) * (double)(y_1)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = (long)((long)(n_1) + 1L);
        }
        if (is_neg) {
            return (double)(1.0) / (double)(sum_1);
        }
        return sum_1;
    }

    static double gaussian(double x, double mu, double sigma) {
        double coeff = (double)((double)(1.0) / (double)(sqrtApprox((double)((double)((double)((double)(2.0) * (double)(PI)) * (double)(sigma)) * (double)(sigma)))));
        double exponent_1 = (double)((double)(-((double)(((double)(x) - (double)(mu))) * (double)(((double)(x) - (double)(mu))))) / (double)(((double)((double)(2.0) * (double)(sigma)) * (double)(sigma))));
        return (double)(coeff) * (double)(expApprox((double)(exponent_1)));
    }

    static void main() {
        double result = (double)(gaussian((double)(1.0), (double)(0.0), (double)(1.0)));
        System.out.println(result);
    }
    public static void main(String[] args) {
        main();
    }
}
