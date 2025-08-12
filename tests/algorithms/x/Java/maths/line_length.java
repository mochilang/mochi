public class Main {

    static double sqrt_newton(double n) {
        if (n == 0.0) {
            return 0.0;
        }
        double x_1 = n;
        long i_1 = 0;
        while (i_1 < 20) {
            x_1 = (x_1 + n / x_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return x_1;
    }

    static double hypot(double a, double b) {
        return sqrt_newton(a * a + b * b);
    }

    static double line_length(java.util.function.Function<Double,Double> fnc, double x_start, double x_end, long steps) {
        double x1 = x_start;
        double fx1_1 = fnc.apply(x_start);
        double length_1 = 0.0;
        long i_3 = 0;
        double step_1 = (x_end - x_start) / (1.0 * steps);
        while (i_3 < steps) {
            double x2_1 = step_1 + x1;
            double fx2_1 = fnc.apply(x2_1);
            length_1 = length_1 + hypot(x2_1 - x1, fx2_1 - fx1_1);
            x1 = x2_1;
            fx1_1 = fx2_1;
            i_3 = i_3 + 1;
        }
        return length_1;
    }

    static double f1(double x) {
        return x;
    }

    static double f2(double x) {
        return 1.0;
    }

    static double f3(double x) {
        return (x * x) / 10.0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(line_length(Main::f1, 0.0, 1.0, 10));
            System.out.println(line_length(Main::f2, -5.5, 4.5, 100));
            System.out.println(line_length(Main::f3, 0.0, 10.0, 1000));
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
