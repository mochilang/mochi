public class Main {

    static double pow_float(double base, int exponent) {
        int exp = exponent;
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double evaluate_poly(double[] poly, double x) {
        double total = 0.0;
        int i_1 = 0;
        while (i_1 < poly.length) {
            total = total + poly[i_1] * pow_float(x, i_1);
            i_1 = i_1 + 1;
        }
        return total;
    }

    static double horner(double[] poly, double x) {
        double result_1 = 0.0;
        int i_2 = poly.length - 1;
        while (i_2 >= 0) {
            result_1 = result_1 * x + poly[i_2];
            i_2 = i_2 - 1;
        }
        return result_1;
    }

    static void test_polynomial_evaluation() {
        double[] poly = ((double[])(new double[]{0.0, 0.0, 5.0, 9.3, 7.0}));
        double x = 10.0;
        if (evaluate_poly(((double[])(poly)), x) != 79800.0) {
            throw new RuntimeException(String.valueOf("evaluate_poly failed"));
        }
        if (horner(((double[])(poly)), x) != 79800.0) {
            throw new RuntimeException(String.valueOf("horner failed"));
        }
    }

    static void main() {
        test_polynomial_evaluation();
        double[] poly_1 = ((double[])(new double[]{0.0, 0.0, 5.0, 9.3, 7.0}));
        double x_1 = 10.0;
        System.out.println(evaluate_poly(((double[])(poly_1)), x_1));
        System.out.println(horner(((double[])(poly_1)), x_1));
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
