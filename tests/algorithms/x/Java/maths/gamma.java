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
        double guess_1 = x / 2.0;
        long i_1 = 0;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double ln(double x) {
        if (x <= 0.0) {
            throw new RuntimeException(String.valueOf("ln domain error"));
        }
        double y_1 = (x - 1.0) / (x + 1.0);
        double y2_1 = y_1 * y_1;
        double term_1 = y_1;
        double sum_1 = 0.0;
        long k_1 = 0;
        while (k_1 < 10) {
            double denom_1 = ((Number)((2 * k_1 + 1))).doubleValue();
            sum_1 = sum_1 + term_1 / denom_1;
            term_1 = term_1 * y2_1;
            k_1 = k_1 + 1;
        }
        return 2.0 * sum_1;
    }

    static double exp_series(double x) {
        double term_2 = 1.0;
        double sum_3 = 1.0;
        long n_1 = 1;
        while (n_1 < 20) {
            term_2 = term_2 * x / (((Number)(n_1)).doubleValue());
            sum_3 = sum_3 + term_2;
            n_1 = n_1 + 1;
        }
        return sum_3;
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
        double step_1 = 0.001;
        double limit_1 = 100.0;
        double x_1 = step_1;
        double total_1 = 0.0;
        while (x_1 < limit_1) {
            total_1 = total_1 + integrand(x_1, num) * step_1;
            x_1 = x_1 + step_1;
        }
        return total_1;
    }

    static double gamma_recursive(double num) {
        if (num <= 0.0) {
            throw new RuntimeException(String.valueOf("math domain error"));
        }
        if (num > 171.5) {
            throw new RuntimeException(String.valueOf("math range error"));
        }
        long int_part_1 = ((Number)(num)).intValue();
        double frac_1 = num - (((Number)(int_part_1)).doubleValue());
        if (!(absf(frac_1) < 1e-06 || absf(frac_1 - 0.5) < 1e-06)) {
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
