public class Main {

    static double pow(double base, long exp) {
        double result = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result = (double)((double)(result) * (double)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static double sqrt_approx(double x) {
        if ((double)(x) == (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        long i_3 = 0L;
        while ((long)(i_3) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return guess_1;
    }

    static double hubble_parameter(double hubble_constant, double radiation_density, double matter_density, double dark_energy, double redshift) {
        double[] parameters = ((double[])(new double[]{redshift, radiation_density, matter_density, dark_energy}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(parameters.length)) {
            if ((double)(parameters[(int)((long)(i_5))]) < (double)(0.0)) {
                throw new RuntimeException(String.valueOf("All input parameters must be positive"));
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        i_5 = 1L;
        while ((long)(i_5) < 4L) {
            if ((double)(parameters[(int)((long)(i_5))]) > (double)(1.0)) {
                throw new RuntimeException(String.valueOf("Relative densities cannot be greater than one"));
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        double curvature_1 = (double)((double)(1.0) - (double)(((double)((double)(matter_density) + (double)(radiation_density)) + (double)(dark_energy))));
        double zp1_1 = (double)((double)(redshift) + (double)(1.0));
        double e2_1 = (double)((double)((double)((double)((double)(radiation_density) * (double)(pow((double)(zp1_1), 4L))) + (double)((double)(matter_density) * (double)(pow((double)(zp1_1), 3L)))) + (double)((double)(curvature_1) * (double)(pow((double)(zp1_1), 2L)))) + (double)(dark_energy));
        return (double)(hubble_constant) * (double)(sqrt_approx((double)(e2_1)));
    }

    static void test_hubble_parameter() {
        double h = (double)(hubble_parameter((double)(68.3), (double)(0.0001), (double)(0.3), (double)(0.7), (double)(0.0)));
        if ((double)(h) < (double)(68.2999) || (double)(h) > (double)(68.3001)) {
            throw new RuntimeException(String.valueOf("hubble_parameter test failed"));
        }
    }

    static void main() {
        test_hubble_parameter();
        System.out.println(hubble_parameter((double)(68.3), (double)(0.0001), (double)(0.3), (double)(0.7), (double)(0.0)));
    }
    public static void main(String[] args) {
        main();
    }
}
