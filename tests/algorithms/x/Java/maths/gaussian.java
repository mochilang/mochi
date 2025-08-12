public class Main {
    static double PI;

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        long i_1 = 0;
        while (i_1 < 20) {
            guess = (guess + x / guess) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess;
    }

    static double expApprox(double x) {
        boolean is_neg = false;
        double y_1 = x;
        if (x < 0.0) {
            is_neg = true;
            y_1 = -x;
        }
        double term_1 = 1.0;
        double sum_1 = 1.0;
        long n_1 = 1;
        while (n_1 < 30) {
            term_1 = term_1 * y_1 / (((Number)(n_1)).doubleValue());
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        if (is_neg) {
            return 1.0 / sum_1;
        }
        return sum_1;
    }

    static double gaussian(double x, double mu, double sigma) {
        double coeff = 1.0 / sqrtApprox(2.0 * PI * sigma * sigma);
        double exponent_1 = -((x - mu) * (x - mu)) / (2.0 * sigma * sigma);
        return coeff * expApprox(exponent_1);
    }

    static void main() {
        double result = gaussian(1.0, 0.0, 1.0);
        System.out.println(result);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            main();
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
