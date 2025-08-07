public class Main {

    static double sum_of_harmonic_progression(double first_term, double common_difference, int number_of_terms) {
        double[] arithmetic_progression = ((double[])(new double[]{1.0 / first_term}));
        double term = 1.0 / first_term;
        int i = 0;
        while (i < number_of_terms - 1) {
            term = term + common_difference;
            arithmetic_progression = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(arithmetic_progression), java.util.stream.DoubleStream.of(term)).toArray()));
            i = i + 1;
        }
        double total = 0.0;
        int j = 0;
        while (j < arithmetic_progression.length) {
            total = total + (1.0 / arithmetic_progression[j]);
            j = j + 1;
        }
        return total;
    }

    static double abs_val(double num) {
        if (num < 0.0) {
            return -num;
        }
        return num;
    }

    static void test_sum_of_harmonic_progression() {
        double result1 = sum_of_harmonic_progression(0.5, 2.0, 2);
        if (abs_val(result1 - 0.75) > 1e-07) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        double result2 = sum_of_harmonic_progression(0.2, 5.0, 5);
        if (abs_val(result2 - 0.45666666666666667) > 1e-07) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
    }

    static void main() {
        test_sum_of_harmonic_progression();
        System.out.println(sum_of_harmonic_progression(0.5, 2.0, 2));
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
