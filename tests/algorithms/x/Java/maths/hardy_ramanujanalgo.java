public class Main {

    static int exact_prime_factor_count(int n) {
        int count = 0;
        int num = n;
        if (Math.floorMod(num, 2) == 0) {
            count = count + 1;
            while (Math.floorMod(num, 2) == 0) {
                num = num / 2;
            }
        }
        int i = 3;
        while (i * i <= num) {
            if (Math.floorMod(num, i) == 0) {
                count = count + 1;
                while (Math.floorMod(num, i) == 0) {
                    num = num / i;
                }
            }
            i = i + 2;
        }
        if (num > 2) {
            count = count + 1;
        }
        return count;
    }

    static double ln(double x) {
        double ln2 = 0.6931471805599453;
        double y = x;
        double k = 0.0;
        while (y > 2.0) {
            y = y / 2.0;
            k = k + ln2;
        }
        while (y < 1.0) {
            y = y * 2.0;
            k = k - ln2;
        }
        double t = (y - 1.0) / (y + 1.0);
        double term = t;
        double sum = 0.0;
        int n = 1;
        while (n <= 19) {
            sum = sum + term / (((Number)(n)).doubleValue());
            term = term * t * t;
            n = n + 2;
        }
        return k + 2.0 * sum;
    }

    static double floor(double x) {
        int i_1 = ((Number)(x)).intValue();
        if ((((Number)(i_1)).doubleValue()) > x) {
            i_1 = i_1 - 1;
        }
        return ((Number)(i_1)).doubleValue();
    }

    static double round4(double x) {
        double m = 10000.0;
        return floor(x * m + 0.5) / m;
    }

    static void main() {
        int n_1 = 51242183;
        int count_1 = exact_prime_factor_count(n_1);
        System.out.println("The number of distinct prime factors is/are " + _p(count_1));
        double loglog = ln(ln(((Number)(n_1)).doubleValue()));
        System.out.println("The value of log(log(n)) is " + _p(round4(loglog)));
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
