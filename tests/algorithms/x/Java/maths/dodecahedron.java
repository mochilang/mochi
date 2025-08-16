public class Main {

    static double sqrtApprox(double x) {
        double guess = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(((double)(guess) + (double)((double)(x) / (double)(guess)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static double abs_val(double num) {
        if ((double)(num) < (double)(0.0)) {
            return -num;
        }
        return num;
    }

    static boolean approx_equal(double a, double b, double eps) {
        return (double)(abs_val((double)((double)(a) - (double)(b)))) < (double)(eps);
    }

    static double dodecahedron_surface_area(long edge) {
        if ((long)(edge) <= 0L) {
            throw new RuntimeException(String.valueOf("Length must be a positive."));
        }
        double term_1 = (double)(sqrtApprox((double)((double)(25.0) + (double)((double)(10.0) * (double)(sqrtApprox((double)(5.0)))))));
        double e_1 = (double)(((Number)(edge)).doubleValue());
        return (double)((double)((double)(3.0) * (double)(term_1)) * (double)(e_1)) * (double)(e_1);
    }

    static double dodecahedron_volume(long edge) {
        if ((long)(edge) <= 0L) {
            throw new RuntimeException(String.valueOf("Length must be a positive."));
        }
        double term_3 = (double)((double)(((double)(15.0) + (double)((double)(7.0) * (double)(sqrtApprox((double)(5.0)))))) / (double)(4.0));
        double e_3 = (double)(((Number)(edge)).doubleValue());
        return (double)((double)((double)(term_3) * (double)(e_3)) * (double)(e_3)) * (double)(e_3);
    }

    static void test_dodecahedron() {
        if (!(Boolean)approx_equal((double)(dodecahedron_surface_area(5L)), (double)(516.1432201766901), (double)(0.0001))) {
            throw new RuntimeException(String.valueOf("surface area 5 failed"));
        }
        if (!(Boolean)approx_equal((double)(dodecahedron_surface_area(10L)), (double)(2064.5728807067603), (double)(0.0001))) {
            throw new RuntimeException(String.valueOf("surface area 10 failed"));
        }
        if (!(Boolean)approx_equal((double)(dodecahedron_volume(5L)), (double)(957.8898700780791), (double)(0.0001))) {
            throw new RuntimeException(String.valueOf("volume 5 failed"));
        }
        if (!(Boolean)approx_equal((double)(dodecahedron_volume(10L)), (double)(7663.118960624633), (double)(0.0001))) {
            throw new RuntimeException(String.valueOf("volume 10 failed"));
        }
    }

    static void main() {
        test_dodecahedron();
        System.out.println(dodecahedron_surface_area(5L));
        System.out.println(dodecahedron_volume(5L));
    }
    public static void main(String[] args) {
        main();
    }
}
