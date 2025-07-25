public class Main {

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double maxf(double a, double b) {
        if (a > b) {
            return a;
        }
        return b;
    }

    static boolean isClose(double a, double b) {
        double relTol = 1e-09;
        double t = Math.abs(a - b);
        double u = relTol * maxf(Math.abs(a), Math.abs(b));
        return t <= u;
    }

    static double sqrtApprox(double x) {
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static void main() {
        double root2 = sqrtApprox(2.0);
        double[][] pairs = new double[][]{new double[]{1.0000000000000002e+14, 1.0000000000000002e+14}, new double[]{100.01, 100.011}, new double[]{1.0000000000000002e+13 / 10000.0, 1.0000000000000001e+09}, new double[]{0.001, 0.0010000001}, new double[]{1.01e-22, 0.0}, new double[]{root2 * root2, 2.0}, new double[]{(-root2) * root2, -2.0}, new double[]{100000000000000000.0, 100000000000000000.0}, new double[]{3.141592653589793, 3.141592653589793}};
        for (double[] pair : pairs) {
            double a = pair[0];
            double b = pair[1];
            String s = String.valueOf(isClose(a, b) ? "≈" : "≉");
            System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf(a) + " ") + s) + " ") + String.valueOf(b));
        }
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
