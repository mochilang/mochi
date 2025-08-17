public class Main {

    static double pow_float(double base, long exponent) {
        long exp = (long)(exponent);
        double result_1 = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result_1 = (double)((double)(result_1) * (double)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result_1;
    }

    static double evaluate_poly(double[] poly, double x) {
        double total = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(poly.length)) {
            total = (double)((double)(total) + (double)((double)(poly[(int)((long)(i_3))]) * (double)(pow_float((double)(x), (long)(i_3)))));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return total;
    }

    static double horner(double[] poly, double x) {
        double result_2 = (double)(0.0);
        long i_5 = (long)((long)(poly.length) - 1L);
        while ((long)(i_5) >= 0L) {
            result_2 = (double)((double)((double)(result_2) * (double)(x)) + (double)(poly[(int)((long)(i_5))]));
            i_5 = (long)((long)(i_5) - 1L);
        }
        return result_2;
    }

    static void test_polynomial_evaluation() {
        double[] poly = ((double[])(new double[]{0.0, 0.0, 5.0, 9.3, 7.0}));
        double x_1 = (double)(10.0);
        if ((double)(evaluate_poly(((double[])(poly)), (double)(x_1))) != (double)(79800.0)) {
            throw new RuntimeException(String.valueOf("evaluate_poly failed"));
        }
        if ((double)(horner(((double[])(poly)), (double)(x_1))) != (double)(79800.0)) {
            throw new RuntimeException(String.valueOf("horner failed"));
        }
    }

    static void main() {
        test_polynomial_evaluation();
        double[] poly_2 = ((double[])(new double[]{0.0, 0.0, 5.0, 9.3, 7.0}));
        double x_3 = (double)(10.0);
        System.out.println(evaluate_poly(((double[])(poly_2)), (double)(x_3)));
        System.out.println(horner(((double[])(poly_2)), (double)(x_3)));
    }
    public static void main(String[] args) {
        main();
    }
}
