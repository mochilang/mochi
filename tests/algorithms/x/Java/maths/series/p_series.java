public class Main {

    static String pow_string(int base, int exp) {
        if (exp >= 0) {
            int res = 1;
            int i = 0;
            while (i < exp) {
                res = res * base;
                i = i + 1;
            }
            return _p(res);
        }
        int e = -exp;
        double res_1 = 1.0;
        double b = base * 1.0;
        int i_1 = 0;
        while (i_1 < e) {
            res_1 = res_1 * b;
            i_1 = i_1 + 1;
        }
        double value = 1.0 / res_1;
        return _p(value);
    }

    static String[] p_series(int nth_term, int power) {
        String[] series = ((String[])(new String[]{}));
        if (nth_term <= 0) {
            return series;
        }
        int i_2 = 1;
        while (i_2 <= nth_term) {
            if (i_2 == 1) {
                series = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(series), java.util.stream.Stream.of("1")).toArray(String[]::new)));
            } else {
                series = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(series), java.util.stream.Stream.of("1 / " + String.valueOf(pow_string(i_2, power)))).toArray(String[]::new)));
            }
            i_2 = i_2 + 1;
        }
        return series;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(p_series(5, 2));
            System.out.println(p_series(-5, 2));
            System.out.println(p_series(5, -2));
            System.out.println(p_series(0, 0));
            System.out.println(p_series(1, 1));
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
