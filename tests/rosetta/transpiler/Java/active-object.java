public class Main {
    static double PI = 3.141592653589793;
    static double dt = 0.01;
    static double s = 0.0;
    static double t1 = 0.0;
    static double k1 = sinApprox(0.0);
    static int i = 1;
    static int i2 = 1;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 12) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            while (i <= 200) {
                double t2 = (((Number)(i)).doubleValue()) * dt;
                double k2 = sinApprox(t2 * PI);
                s = s + (k1 + k2) * 0.5 * (t2 - t1);
                t1 = t2;
                k1 = k2;
                i = i + 1;
            }
            while (i2 <= 50) {
                double t2 = 2.0 + (((Number)(i2)).doubleValue()) * dt;
                double k2 = 0.0;
                s = s + (k1 + k2) * 0.5 * (t2 - t1);
                t1 = t2;
                k1 = k2;
                i2 = i2 + 1;
            }
            System.out.println(s);
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
        return rt.totalMemory() - rt.freeMemory();
    }
}
