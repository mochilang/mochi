public class Main {

    static double[] cramers_rule_2x2(double[] eq1, double[] eq2) {
        if ((long)(eq1.length) != 3L || (long)(eq2.length) != 3L) {
            throw new RuntimeException(String.valueOf("Please enter a valid equation."));
        }
        if ((double)(eq1[(int)(0L)]) == (double)(0.0) && (double)(eq1[(int)(1L)]) == (double)(0.0) && (double)(eq2[(int)(0L)]) == (double)(0.0) && (double)(eq2[(int)(1L)]) == (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Both a & b of two equations can't be zero."));
        }
        double a1_1 = (double)(eq1[(int)(0L)]);
        double b1_1 = (double)(eq1[(int)(1L)]);
        double c1_1 = (double)(eq1[(int)(2L)]);
        double a2_1 = (double)(eq2[(int)(0L)]);
        double b2_1 = (double)(eq2[(int)(1L)]);
        double c2_1 = (double)(eq2[(int)(2L)]);
        double determinant_1 = (double)((double)((double)(a1_1) * (double)(b2_1)) - (double)((double)(a2_1) * (double)(b1_1)));
        double determinant_x_1 = (double)((double)((double)(c1_1) * (double)(b2_1)) - (double)((double)(c2_1) * (double)(b1_1)));
        double determinant_y_1 = (double)((double)((double)(a1_1) * (double)(c2_1)) - (double)((double)(a2_1) * (double)(c1_1)));
        if ((double)(determinant_1) == (double)(0.0)) {
            if ((double)(determinant_x_1) == (double)(0.0) && (double)(determinant_y_1) == (double)(0.0)) {
                throw new RuntimeException(String.valueOf("Infinite solutions. (Consistent system)"));
            }
            throw new RuntimeException(String.valueOf("No solution. (Inconsistent system)"));
        }
        if ((double)(determinant_x_1) == (double)(0.0) && (double)(determinant_y_1) == (double)(0.0)) {
            return new double[]{0.0, 0.0};
        }
        double x_1 = (double)((double)(determinant_x_1) / (double)(determinant_1));
        double y_1 = (double)((double)(determinant_y_1) / (double)(determinant_1));
        return new double[]{x_1, y_1};
    }

    static void test_cramers_rule_2x2() {
        double[] r1 = ((double[])(cramers_rule_2x2(((double[])(new double[]{2.0, 3.0, 0.0})), ((double[])(new double[]{5.0, 1.0, 0.0})))));
        if ((double)(r1[(int)(0L)]) != (double)(0.0) || (double)(r1[(int)(1L)]) != (double)(0.0)) {
            throw new RuntimeException(String.valueOf("Test1 failed"));
        }
        double[] r2_1 = ((double[])(cramers_rule_2x2(((double[])(new double[]{0.0, 4.0, 50.0})), ((double[])(new double[]{2.0, 0.0, 26.0})))));
        if ((double)(r2_1[(int)(0L)]) != (double)(13.0) || (double)(r2_1[(int)(1L)]) != (double)(12.5)) {
            throw new RuntimeException(String.valueOf("Test2 failed"));
        }
    }

    static void main() {
        test_cramers_rule_2x2();
        System.out.println(cramers_rule_2x2(((double[])(new double[]{11.0, 2.0, 30.0})), ((double[])(new double[]{1.0, 0.0, 4.0}))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
