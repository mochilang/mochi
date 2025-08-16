public class Main {
    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return -x;
        }
        return x;
    }

    static long factorial(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("factorial is undefined for negative numbers"));
        }
        long result_1 = 1L;
        long i_1 = 2L;
        while ((long)(i_1) <= (long)(n)) {
            result_1 = (long)((long)(result_1) * (long)(i_1));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result_1;
    }

    static double pow_float(double base, long exp) {
        double result_2 = (double)(1.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(exp)) {
            result_2 = (double)((double)(result_2) * (double)(base));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return result_2;
    }

    static double binomial_distribution(long successes, long trials, double prob) {
        if ((long)(successes) > (long)(trials)) {
            throw new RuntimeException(String.valueOf("successes must be lower or equal to trials"));
        }
        if ((long)(trials) < 0L || (long)(successes) < 0L) {
            throw new RuntimeException(String.valueOf("the function is defined for non-negative integers"));
        }
        if (!((double)(0.0) < (double)(prob) && (double)(prob) < (double)(1.0))) {
            throw new RuntimeException(String.valueOf("prob has to be in range of 1 - 0"));
        }
        double probability_1 = (double)((double)(pow_float((double)(prob), (long)(successes))) * (double)(pow_float((double)((double)(1.0) - (double)(prob)), (long)((long)(trials) - (long)(successes)))));
        double numerator_1 = (double)(((Number)(factorial((long)(trials)))).doubleValue());
        double denominator_1 = (double)(((Number)(((long)(factorial((long)(successes))) * (long)(factorial((long)((long)(trials) - (long)(successes))))))).doubleValue());
        double coefficient_1 = (double)((double)(numerator_1) / (double)(denominator_1));
        return (double)(probability_1) * (double)(coefficient_1);
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
