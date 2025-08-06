public class Main {

    static int gcd(int a, int b) {
        int x = a;
        int y = b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static String[] proper_fractions(int den) {
        if (den < 0) {
            throw new RuntimeException(String.valueOf("The Denominator Cannot be less than 0"));
        }
        String[] res = ((String[])(new String[]{}));
        int n = 1;
        while (n < den) {
            if (gcd(n, den) == 1) {
                res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(_p(n) + "/" + _p(den))).toArray(String[]::new)));
            }
            n = n + 1;
        }
        return res;
    }

    static void test_proper_fractions() {
        String[] a = ((String[])(proper_fractions(10)));
        if (a != new String[]{"1/10", "3/10", "7/10", "9/10"}) {
            throw new RuntimeException(String.valueOf("test 10 failed"));
        }
        String[] b = ((String[])(proper_fractions(5)));
        if (b != new String[]{"1/5", "2/5", "3/5", "4/5"}) {
            throw new RuntimeException(String.valueOf("test 5 failed"));
        }
        String[] c = ((String[])(proper_fractions(0)));
        if (c != ((Number)(new Object[]{})).intValue()) {
            throw new RuntimeException(String.valueOf("test 0 failed"));
        }
    }

    static void main() {
        test_proper_fractions();
        System.out.println(_p(proper_fractions(10)));
        System.out.println(_p(proper_fractions(5)));
        System.out.println(_p(proper_fractions(0)));
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
