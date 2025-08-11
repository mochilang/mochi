public class Main {
    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static long factorial(long n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("factorial is undefined for negative numbers"));
        }
        long result_1 = 1;
        long i_1 = 2;
        while (i_1 <= n) {
            result_1 = result_1 * i_1;
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static double pow_float(double base, long exp) {
        double result_2 = 1.0;
        long i_3 = 0;
        while (i_3 < exp) {
            result_2 = result_2 * base;
            i_3 = i_3 + 1;
        }
        return result_2;
    }

    static double binomial_distribution(long successes, long trials, double prob) {
        if (successes > trials) {
            throw new RuntimeException(String.valueOf("successes must be lower or equal to trials"));
        }
        if (trials < 0 || successes < 0) {
            throw new RuntimeException(String.valueOf("the function is defined for non-negative integers"));
        }
        if (!(0.0 < prob && prob < 1.0)) {
            throw new RuntimeException(String.valueOf("prob has to be in range of 1 - 0"));
        }
        double probability_1 = pow_float(prob, successes) * pow_float(1.0 - prob, trials - successes);
        double numerator_1 = ((Number)(factorial(trials))).doubleValue();
        double denominator_1 = ((Number)((factorial(successes) * factorial(trials - successes)))).doubleValue();
        double coefficient_1 = numerator_1 / denominator_1;
        return probability_1 * coefficient_1;
    }
    public static void main(String[] args) {
    }
}
