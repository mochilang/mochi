public class Main {
    static double PI = 3.141592653589793;
    static double degreesIncr = 0.1 * PI / 180.0;
    static double turns = 2.0;
    static double stop = 360.0 * turns * 10.0 * degreesIncr;
    static double width = 600.0;
    static double centre = width / 2.0;
    static double a = 1.0;
    static double b = 20.0;
    static double theta = 0.0;
    static int count = 0;

    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 10) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double cosApprox(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n <= 10) {
            double denom = ((Number)(((2 * n - 1) * (2 * n)))).doubleValue();
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
            while (theta < stop) {
                double r = a + b * theta;
                double x = r * cosApprox(theta);
                double y = r * sinApprox(theta);
                if (count % 100 == 0) {
                    System.out.println(String.valueOf(String.valueOf(centre + x) + ",") + String.valueOf(centre - y));
                }
                theta = theta + degreesIncr;
                count = count + 1;
            }
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
