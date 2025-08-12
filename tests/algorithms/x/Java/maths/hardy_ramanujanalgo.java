public class Main {

    static long exact_prime_factor_count(long n) {
        long count = 0;
        long num_1 = n;
        if (Math.floorMod(num_1, 2) == 0) {
            count = count + 1;
            while (Math.floorMod(num_1, 2) == 0) {
                num_1 = Math.floorDiv(num_1, 2);
            }
        }
        long i_1 = 3;
        while (i_1 * i_1 <= num_1) {
            if (Math.floorMod(num_1, i_1) == 0) {
                count = count + 1;
                while (Math.floorMod(num_1, i_1) == 0) {
                    num_1 = Math.floorDiv(num_1, i_1);
                }
            }
            i_1 = i_1 + 2;
        }
        if (num_1 > 2) {
            count = count + 1;
        }
        return count;
    }

    static double ln(double x) {
        double ln2 = 0.6931471805599453;
        double y_1 = x;
        double k_1 = 0.0;
        while (y_1 > 2.0) {
            y_1 = y_1 / 2.0;
            k_1 = k_1 + ln2;
        }
        while (y_1 < 1.0) {
            y_1 = y_1 * 2.0;
            k_1 = k_1 - ln2;
        }
        double t_1 = (y_1 - 1.0) / (y_1 + 1.0);
        double term_1 = t_1;
        double sum_1 = 0.0;
        long n_1 = 1;
        while (n_1 <= 19) {
            sum_1 = sum_1 + term_1 / (((Number)(n_1)).doubleValue());
            term_1 = term_1 * t_1 * t_1;
            n_1 = n_1 + 2;
        }
        return k_1 + 2.0 * sum_1;
    }

    static double floor(double x) {
        long i_2 = ((Number)(x)).intValue();
        if ((((Number)(i_2)).doubleValue()) > x) {
            i_2 = i_2 - 1;
        }
        return ((Number)(i_2)).doubleValue();
    }

    static double round4(double x) {
        double m = 10000.0;
        return floor(x * m + 0.5) / m;
    }

    static void main() {
        long n_2 = 51242183;
        long count_2 = exact_prime_factor_count(n_2);
        System.out.println("The number of distinct prime factors is/are " + _p(count_2));
        double loglog_1 = ln(ln(((Number)(n_2)).doubleValue()));
        System.out.println("The value of log(log(n)) is " + _p(round4(loglog_1)));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
