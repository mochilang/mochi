public class Main {

    static double[] cramers_rule_2x2(double[] eq1, double[] eq2) {
        if (eq1.length != 3 || eq2.length != 3) {
            throw new RuntimeException(String.valueOf("Please enter a valid equation."));
        }
        if (eq1[0] == 0.0 && eq1[1] == 0.0 && eq2[0] == 0.0 && eq2[1] == 0.0) {
            throw new RuntimeException(String.valueOf("Both a & b of two equations can't be zero."));
        }
        double a1 = eq1[0];
        double b1 = eq1[1];
        double c1 = eq1[2];
        double a2 = eq2[0];
        double b2 = eq2[1];
        double c2 = eq2[2];
        double determinant = a1 * b2 - a2 * b1;
        double determinant_x = c1 * b2 - c2 * b1;
        double determinant_y = a1 * c2 - a2 * c1;
        if (determinant == 0.0) {
            if (determinant_x == 0.0 && determinant_y == 0.0) {
                throw new RuntimeException(String.valueOf("Infinite solutions. (Consistent system)"));
            }
            throw new RuntimeException(String.valueOf("No solution. (Inconsistent system)"));
        }
        if (determinant_x == 0.0 && determinant_y == 0.0) {
            return new double[]{0.0, 0.0};
        }
        double x = determinant_x / determinant;
        double y = determinant_y / determinant;
        return new double[]{x, y};
    }

    static void test_cramers_rule_2x2() {
        double[] r1 = ((double[])(cramers_rule_2x2(((double[])(new double[]{2.0, 3.0, 0.0})), ((double[])(new double[]{5.0, 1.0, 0.0})))));
        if (r1[0] != 0.0 || r1[1] != 0.0) {
            throw new RuntimeException(String.valueOf("Test1 failed"));
        }
        double[] r2 = ((double[])(cramers_rule_2x2(((double[])(new double[]{0.0, 4.0, 50.0})), ((double[])(new double[]{2.0, 0.0, 26.0})))));
        if (r2[0] != 13.0 || r2[1] != 12.5) {
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
