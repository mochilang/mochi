public class Main {
    static double PI;
    static int seed = 0;
    static class Point {
        double x;
        double y;
        Point(double x, double y) {
            this.x = x;
            this.y = y;
        }
        Point() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static int next_seed(int x) {
        return ((int)(Math.floorMod(((long)((x * 1103515245 + 12345))), 2147483648L)));
    }

    static double rand_unit() {
        seed = next_seed(seed);
        return (((Number)(seed)).doubleValue()) / 2147483648.0;
    }

    static boolean is_in_unit_circle(Point p) {
        return p.x * p.x + p.y * p.y <= 1.0;
    }

    static Point random_unit_square() {
        return new Point(rand_unit(), rand_unit());
    }

    static double estimate_pi(int simulations) {
        if (simulations < 1) {
            throw new RuntimeException(String.valueOf("At least one simulation is necessary to estimate PI."));
        }
        int inside = 0;
        int i = 0;
        while (i < simulations) {
            Point p = random_unit_square();
            if (((Boolean)(is_in_unit_circle(p)))) {
                inside = inside + 1;
            }
            i = i + 1;
        }
        return 4.0 * (((Number)(inside)).doubleValue()) / (((Number)(simulations)).doubleValue());
    }

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static void main() {
        int n = 10000;
        double my_pi = estimate_pi(n);
        double error = abs_float(my_pi - PI);
        System.out.println("An estimate of PI is " + _p(my_pi) + " with an error of " + _p(error));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            seed = 1;
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
