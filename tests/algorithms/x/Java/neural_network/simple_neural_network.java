public class Main {
    static long seed = 1L;
    static double INITIAL_VALUE = (double)(0.02);
    static double result;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return seed;
    }

    static long randint(long low, long high) {
        return (long)((Math.floorMod(rand(), ((long)((long)(high) - (long)(low)) + 1L)))) + (long)(low);
    }

    static double expApprox(double x) {
        double y = (double)(x);
        boolean is_neg_1 = false;
        if ((double)(x) < (double)(0.0)) {
            is_neg_1 = true;
            y = (double)(-x);
        }
        double term_1 = (double)(1.0);
        double sum_1 = (double)(1.0);
        long n_1 = 1L;
        while ((long)(n_1) < 30L) {
            term_1 = (double)((double)((double)(term_1) * (double)(y)) / (double)((((Number)(n_1)).doubleValue())));
            sum_1 = (double)((double)(sum_1) + (double)(term_1));
            n_1 = (long)((long)(n_1) + 1L);
        }
        if (is_neg_1) {
            return (double)(1.0) / (double)(sum_1);
        }
        return sum_1;
    }

    static double sigmoid(double x) {
        return (double)(1.0) / (double)(((double)(1.0) + (double)(expApprox((double)(-x)))));
    }

    static double sigmoid_derivative(double sig_val) {
        return (double)(sig_val) * (double)(((double)(1.0) - (double)(sig_val)));
    }

    static double forward_propagation(long expected, long number_propagations) {
        double weight = (double)((double)((double)(2.0) * (double)((((Number)(randint(1L, 100L))).doubleValue()))) - (double)(1.0));
        double layer_1_1 = (double)(0.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(number_propagations)) {
            layer_1_1 = (double)(sigmoid((double)((double)(INITIAL_VALUE) * (double)(weight))));
            double layer_1_error_1 = (double)((double)(((double)(((Number)(expected)).doubleValue()) / (double)(100.0))) - (double)(layer_1_1));
            double layer_1_delta_1 = (double)((double)(layer_1_error_1) * (double)(sigmoid_derivative((double)(layer_1_1))));
            weight = (double)((double)(weight) + (double)((double)(INITIAL_VALUE) * (double)(layer_1_delta_1)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return (double)(layer_1_1) * (double)(100.0);
    }
    public static void main(String[] args) {
        seed = 1L;
        result = (double)(forward_propagation(32L, 450000L));
        System.out.println(result);
    }
}
