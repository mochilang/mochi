public class Main {

    static double powf(double base, int exp) {
        double result = 1.0;
        int i = 0;
        while (i < exp) {
            result = result * base;
            i = i + 1;
        }
        return result;
    }

    static double nthRoot(double x, int n) {
        double low = 0.0;
        double high = x;
        int i = 0;
        while (i < 60) {
            double mid = (low + high) / 2.0;
            if (powf(mid, n) > x) {
                high = mid;
            } else {
                low = mid;
            }
            i = i + 1;
        }
        return low;
    }

    static void main() {
        double sum = 0.0;
        double sumRecip = 0.0;
        double prod = 1.0;
        int n = 1;
        while (n <= 10) {
            double f = ((Number)(n)).doubleValue();
            sum = sum + f;
            sumRecip = sumRecip + 1.0 / f;
            prod = prod * f;
            n = n + 1;
        }
        double count = 10.0;
        double a = sum / count;
        double g = nthRoot(prod, 10);
        double h = count / sumRecip;
        System.out.println(String.valueOf(String.valueOf(String.valueOf(String.valueOf("A: " + String.valueOf(a)) + " G: ") + String.valueOf(g)) + " H: ") + String.valueOf(h));
        System.out.println("A >= G >= H: " + String.valueOf(a >= g && g >= h));
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
