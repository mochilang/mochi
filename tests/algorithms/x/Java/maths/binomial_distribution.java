public class Main {
    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static int factorial(int n) {
        if (n < 0) {
            throw new RuntimeException(String.valueOf("factorial is undefined for negative numbers"));
        }
        int result = 1;
        int i = 2;
        while (i <= n) {
            result = result * i;
            i = i + 1;
        }
        return result;
    }

    static double pow_float(double base, int exp) {
        double result_1 = 1.0;
        int i_1 = 0;
        while (i_1 < exp) {
            result_1 = result_1 * base;
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static double binomial_distribution(int successes, int trials, double prob) {
        if (successes > trials) {
            throw new RuntimeException(String.valueOf("successes must be lower or equal to trials"));
        }
        if (trials < 0 || successes < 0) {
            throw new RuntimeException(String.valueOf("the function is defined for non-negative integers"));
        }
        if (!(0.0 < prob && prob < 1.0)) {
            throw new RuntimeException(String.valueOf("prob has to be in range of 1 - 0"));
        }
        double probability = pow_float(prob, successes) * pow_float(1.0 - prob, trials - successes);
        double numerator = ((Number)(factorial(trials))).doubleValue();
        double denominator = ((Number)((factorial(successes) * factorial(trials - successes)))).doubleValue();
        double coefficient = numerator / denominator;
        return probability * coefficient;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
