public class Main {
    static double PI;

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrt(double x) {
        if (x < 0.0) {
            throw new RuntimeException(String.valueOf("sqrt domain error"));
        }
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double ln(double x) {
        if (x <= 0.0) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y = (x - 1.0) / (x + 1.0);
        double y2 = y * y;
        double term = y;
        double sum = 0.0;
        int k = 0;
        while (k < 10) {
            double denom = ((Number)((2 * k + 1))).doubleValue();
            sum = sum + term / denom;
            term = term * y2;
            k = k + 1;
        }
        return 2.0 * sum;
    }

    static double exp_series(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int n = 1;
        while (n < 20) {
            term_1 = term_1 * x / (((Number)(n)).doubleValue());
            sum_1 = sum_1 + term_1;
            n = n + 1;
        }
        return sum_1;
    }

    static double powf(double base, double exponent) {
        if (base <= 0.0) {
            return 0.0;
        }
        return exp_series(exponent * ln(base));
    }

    static double integrand(double x, double z) {
        return powf(x, z - 1.0) * exp_series(-x);
    }

    static double gamma_iterative(double num) {
        if (num <= 0.0) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        double step = 0.001;
        double limit = 100.0;
        double x = step;
        double total = 0.0;
        while (x < limit) {
            total = total + integrand(x, num) * step;
            x = x + step;
        }
        return total;
    }

    static double gamma_recursive(double num) {
        if (num <= 0.0) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        if (num > 171.5) {
            throw new RuntimeException(String.valueOf("math range error"));
        }
        int int_part = ((Number)(num)).intValue();
        double frac = num - (((Number)(int_part)).doubleValue());
        if (!(absf(frac) < 1e-06 || absf(frac - 0.5) < 1e-06)) {
            throw new RuntimeException(String.valueOf("num must be an integer or a half-integer"));
        }
        if (absf(num - 0.5) < 1e-06) {
            return sqrt(PI);
        }
        if (absf(num - 1.0) < 1e-06) {
            return 1.0;
        }
        return (num - 1.0) * gamma_recursive(num - 1.0);
    }

    static void main() {
        System.out.println(gamma_iterative(5.0));
        System.out.println(gamma_recursive(5.0));
        System.out.println(gamma_recursive(0.5));
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
