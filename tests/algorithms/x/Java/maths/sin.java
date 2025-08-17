public class Main {
    static double PI = (double)(3.141592653589793);

    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static double floor(double x) {
        long i = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i)).doubleValue())) > (double)(x)) {
            i = (long)((long)(i) - 1L);
        }
        return ((Number)(i)).doubleValue();
    }

    static double pow(double x, long n) {
        double result = (double)(1.0);
        long i_2 = 0L;
        while ((long)(i_2) < (long)(n)) {
            result = (double)((double)(result) * (double)(x));
            i_2 = (long)((long)(i_2) + 1L);
        }
        return result;
    }

    static double factorial(long n) {
        double result_1 = (double)(1.0);
        long i_4 = 2L;
        while ((long)(i_4) <= (long)(n)) {
            result_1 = (double)((double)(result_1) * (double)((((Number)(i_4)).doubleValue())));
            i_4 = (long)((long)(i_4) + 1L);
        }
        return result_1;
    }

    static double radians(double deg) {
        return (double)((double)(deg) * (double)(PI)) / (double)(180.0);
    }

    static double taylor_sin(double angle_in_degrees, long accuracy, long rounded_values_count) {
        double k = ((Number)(Math.floor((double)(angle_in_degrees) / (double)(360.0)))).doubleValue();
        double angle_1 = (double)((double)(angle_in_degrees) - (double)(((double)(k) * (double)(360.0))));
        double angle_in_radians_1 = (double)(radians((double)(angle_1)));
        double result_3 = (double)(angle_in_radians_1);
        long a_1 = 3L;
        double sign_1 = (double)(-1.0);
        long i_6 = 0L;
        while ((long)(i_6) < (long)(accuracy)) {
            result_3 = (double)((double)(result_3) + (double)((double)(((double)(sign_1) * (double)(pow((double)(angle_in_radians_1), (long)(a_1))))) / (double)(factorial((long)(a_1)))));
            sign_1 = (double)(-sign_1);
            a_1 = (long)((long)(a_1) + 2L);
            i_6 = (long)((long)(i_6) + 1L);
        }
        return result_3;
    }

    static void test_sin() {
        double eps = (double)(1e-07);
        if (Math.abs((double)(taylor_sin((double)(0.0), 18L, 10L)) - (double)(0.0)) > (double)(eps)) {
            throw new RuntimeException(String.valueOf("sin(0) failed"));
        }
        if (Math.abs((double)(taylor_sin((double)(90.0), 18L, 10L)) - (double)(1.0)) > (double)(eps)) {
            throw new RuntimeException(String.valueOf("sin(90) failed"));
        }
        if (Math.abs((double)(taylor_sin((double)(180.0), 18L, 10L)) - (double)(0.0)) > (double)(eps)) {
            throw new RuntimeException(String.valueOf("sin(180) failed"));
        }
        if (Math.abs((double)(taylor_sin((double)(270.0), 18L, 10L)) - (double)((-1.0))) > (double)(eps)) {
            throw new RuntimeException(String.valueOf("sin(270) failed"));
        }
    }

    static void main() {
        test_sin();
        double res_1 = (double)(taylor_sin((double)(64.0), 18L, 10L));
        System.out.println(res_1);
    }
    public static void main(String[] args) {
        main();
    }
}
