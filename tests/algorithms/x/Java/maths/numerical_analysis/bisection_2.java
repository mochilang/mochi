public class Main {

    static double equation(double x) {
        return 10.0 - x * x;
    }

    static double bisection(double a, double b) {
        if (equation(a) * equation(b) >= 0.0) {
            throw new RuntimeException(String.valueOf("Wrong space!"));
        }
        double left = a;
        double right = b;
        double c = left;
        while ((right - left) >= 0.01) {
            c = (left + right) / 2.0;
            if (equation(c) == 0.0) {
                break;
            }
            if (equation(c) * equation(left) < 0.0) {
                right = c;
            } else {
                left = c;
            }
        }
        return c;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(bisection(-2.0, 5.0));
            System.out.println(bisection(0.0, 6.0));
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
