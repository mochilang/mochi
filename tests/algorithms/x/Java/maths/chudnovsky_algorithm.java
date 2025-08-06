public class Main {
    static int n;

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double factorial_float(int n) {
        double result = 1.0;
        int i_1 = 2;
        while (i_1 <= n) {
            result = result * (((Number)(i_1)).doubleValue());
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double pi(int n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("Undefined for non-natural numbers"));
        }
        int iterations = (n + 13) / 14;
        double constant_term = 426880.0 * sqrtApprox(10005.0);
        double exponential_term = 1.0;
        double linear_term = 13591409.0;
        double partial_sum = linear_term;
        int k = 1;
        while (k < iterations) {
            int k6 = 6 * k;
            int k3 = 3 * k;
            double fact6k = factorial_float(k6);
            double fact3k = factorial_float(k3);
            double factk = factorial_float(k);
            double multinomial = fact6k / (fact3k * factk * factk * factk);
            linear_term = linear_term + 545140134.0;
            exponential_term = exponential_term * (-262537412640768000.0);
            partial_sum = partial_sum + multinomial * linear_term / exponential_term;
            k = k + 1;
        }
        return constant_term / partial_sum;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            n = 50;
            System.out.println("The first " + _p(n) + " digits of pi is: " + _p(pi(n)));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
