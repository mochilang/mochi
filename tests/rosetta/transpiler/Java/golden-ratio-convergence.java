public class Main {

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double g = x;
        int i = 0;
        while (i < 20) {
            g = (g + x / g) / 2.0;
            i = i + 1;
        }
        return g;
    }

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static void main() {
        double oldPhi = 1.0;
        double phi = 0.0;
        int iters = 0;
        double limit = 1e-05;
        while (true) {
            phi = 1.0 + 1.0 / oldPhi;
            iters = iters + 1;
            if (Math.abs(phi - oldPhi) <= limit) {
                break;
            }
            oldPhi = phi;
        }
        double actual = (1.0 + sqrtApprox(5.0)) / 2.0;
        System.out.println("Final value of phi : " + _p(phi));
        System.out.println("Number of iterations : " + _p(iters));
        System.out.println("Error (approx) : " + _p(phi - actual));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
