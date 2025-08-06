public class Main {

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double abs_val(double num) {
        if (num < 0.0) {
            return -num;
        }
        return num;
    }

    static boolean approx_equal(double a, double b, double eps) {
        return abs_val(a - b) < eps;
    }

    static double dodecahedron_surface_area(int edge) {
        if (edge <= 0) {
            throw new RuntimeException(String.valueOf("Length must be a positive."));
        }
        double term = sqrtApprox(25.0 + 10.0 * sqrtApprox(5.0));
        double e = ((Number)(edge)).doubleValue();
        return 3.0 * term * e * e;
    }

    static double dodecahedron_volume(int edge) {
        if (edge <= 0) {
            throw new RuntimeException(String.valueOf("Length must be a positive."));
        }
        double term_1 = (15.0 + 7.0 * sqrtApprox(5.0)) / 4.0;
        double e_1 = ((Number)(edge)).doubleValue();
        return term_1 * e_1 * e_1 * e_1;
    }

    static void test_dodecahedron() {
        if (!(Boolean)approx_equal(dodecahedron_surface_area(5), 516.1432201766901, 0.0001)) {
            throw new RuntimeException(String.valueOf("surface area 5 failed"));
        }
        if (!(Boolean)approx_equal(dodecahedron_surface_area(10), 2064.5728807067603, 0.0001)) {
            throw new RuntimeException(String.valueOf("surface area 10 failed"));
        }
        if (!(Boolean)approx_equal(dodecahedron_volume(5), 957.8898700780791, 0.0001)) {
            throw new RuntimeException(String.valueOf("volume 5 failed"));
        }
        if (!(Boolean)approx_equal(dodecahedron_volume(10), 7663.118960624633, 0.0001)) {
            throw new RuntimeException(String.valueOf("volume 10 failed"));
        }
    }

    static void main() {
        test_dodecahedron();
        System.out.println(dodecahedron_surface_area(5));
        System.out.println(dodecahedron_volume(5));
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
