public class Main {
    static double PI;
    static int nframes;
    static int w;
    static int h;
    static int total;
    static int f;

    static double floorf(double x) {
        int i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double frac(double x) {
        return x - floorf(x);
    }

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

    static double sqrtApprox(double x) {
        if (x <= 0) {
            return 0.0;
        }
        double guess = x;
        int i_1 = 0;
        while (i_1 < 10) {
            guess = (guess + x / guess) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            nframes = 10;
            w = 32;
            h = 32;
            total = 0;
            f = 1;
            while (f <= nframes) {
                int y = 0;
                while (y < h) {
                    int x = 0;
                    while (x < w) {
                        double fx = ((Number)(x)).doubleValue();
                        double fy = ((Number)(y)).doubleValue();
                        double value = sinApprox(fx / 16.0);
                        value = value + sinApprox(fy / 8.0);
                        value = value + sinApprox((fx + fy) / 16.0);
                        value = value + sinApprox(sqrtApprox(fx * fx + fy * fy) / 8.0);
                        value = value + 4.0;
                        value = value / 8.0;
                        double rem = frac(value + (((Number)(f)).doubleValue()) / (((Number)(nframes)).doubleValue()));
                        int ci = ((Number)(((((Number)(nframes)).doubleValue()) * rem))).intValue() + 1;
                        total = total + ci;
                        x = x + 1;
                    }
                    y = y + 1;
                }
                f = f + 1;
            }
            System.out.println(total);
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
