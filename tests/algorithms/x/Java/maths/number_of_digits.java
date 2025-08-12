public class Main {

    static long abs_int(long n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static long num_digits(long n) {
        long x = abs_int(n);
        long digits_1 = 1;
        while (x >= 10) {
            x = Math.floorDiv(x, 10);
            digits_1 = digits_1 + 1;
        }
        return digits_1;
    }

    static long num_digits_fast(long n) {
        long x_1 = abs_int(n);
        long digits_3 = 1;
        long power_1 = 10;
        while (x_1 >= power_1) {
            power_1 = power_1 * 10;
            digits_3 = digits_3 + 1;
        }
        return digits_3;
    }

    static long num_digits_faster(long n) {
        String s = _p(abs_int(n));
        return _runeLen(s);
    }

    static void test_num_digits() {
        if (num_digits(12345) != 5) {
            throw new RuntimeException(String.valueOf("num_digits 12345 failed"));
        }
        if (num_digits(123) != 3) {
            throw new RuntimeException(String.valueOf("num_digits 123 failed"));
        }
        if (num_digits(0) != 1) {
            throw new RuntimeException(String.valueOf("num_digits 0 failed"));
        }
        if (num_digits(-1) != 1) {
            throw new RuntimeException(String.valueOf("num_digits -1 failed"));
        }
        if (num_digits(-123456) != 6) {
            throw new RuntimeException(String.valueOf("num_digits -123456 failed"));
        }
        if (num_digits_fast(12345) != 5) {
            throw new RuntimeException(String.valueOf("num_digits_fast 12345 failed"));
        }
        if (num_digits_fast(123) != 3) {
            throw new RuntimeException(String.valueOf("num_digits_fast 123 failed"));
        }
        if (num_digits_fast(0) != 1) {
            throw new RuntimeException(String.valueOf("num_digits_fast 0 failed"));
        }
        if (num_digits_fast(-1) != 1) {
            throw new RuntimeException(String.valueOf("num_digits_fast -1 failed"));
        }
        if (num_digits_fast(-123456) != 6) {
            throw new RuntimeException(String.valueOf("num_digits_fast -123456 failed"));
        }
        if (num_digits_faster(12345) != 5) {
            throw new RuntimeException(String.valueOf("num_digits_faster 12345 failed"));
        }
        if (num_digits_faster(123) != 3) {
            throw new RuntimeException(String.valueOf("num_digits_faster 123 failed"));
        }
        if (num_digits_faster(0) != 1) {
            throw new RuntimeException(String.valueOf("num_digits_faster 0 failed"));
        }
        if (num_digits_faster(-1) != 1) {
            throw new RuntimeException(String.valueOf("num_digits_faster -1 failed"));
        }
        if (num_digits_faster(-123456) != 6) {
            throw new RuntimeException(String.valueOf("num_digits_faster -123456 failed"));
        }
    }

    static void main() {
        test_num_digits();
        System.out.println(_p(num_digits(12345)));
        System.out.println(_p(num_digits_fast(12345)));
        System.out.println(_p(num_digits_faster(12345)));
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
