public class Main {

    static double pow_float(double base, int exp) {
        double result = 1.0;
        int exponent = exp;
        if (exponent < 0) {
            exponent = -exponent;
            int i = 0;
            while (i < exponent) {
                result = result * base;
                i = i + 1;
            }
            return 1.0 / result;
        }
        int i_1 = 0;
        while (i_1 < exponent) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double sum_of_geometric_progression(int first_term, int common_ratio, int num_of_terms) {
        if (common_ratio == 1) {
            return ((Number)((num_of_terms * first_term))).doubleValue();
        }
        double a = ((Number)(first_term)).doubleValue();
        double r = ((Number)(common_ratio)).doubleValue();
        return (a / (1.0 - r)) * (1.0 - pow_float(r, num_of_terms));
    }

    static void test_sum() {
        if (sum_of_geometric_progression(1, 2, 10) != 1023.0) {
            throw new RuntimeException(String.valueOf("example1 failed"));
        }
        if (sum_of_geometric_progression(1, 10, 5) != 11111.0) {
            throw new RuntimeException(String.valueOf("example2 failed"));
        }
        if (sum_of_geometric_progression(-1, 2, 10) != (-1023.0)) {
            throw new RuntimeException(String.valueOf("example3 failed"));
        }
    }

    static void main() {
        test_sum();
        System.out.println(sum_of_geometric_progression(1, 2, 10));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
