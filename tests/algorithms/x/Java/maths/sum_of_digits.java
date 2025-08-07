public class Main {

    static long abs_int(long n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static long sum_of_digits(long n) {
        long m = abs_int(n);
        long res = 0;
        while (m > 0) {
            res = res + (Math.floorMod(m, 10));
            m = Math.floorDiv(m, 10);
        }
        return res;
    }

    static long sum_of_digits_recursion(long n) {
        long m_1 = abs_int(n);
        if (m_1 < 10) {
            return m_1;
        }
        return (Math.floorMod(m_1, 10)) + sum_of_digits_recursion(Math.floorDiv(m_1, 10));
    }

    static long sum_of_digits_compact(long n) {
        String s = _p(abs_int(n));
        long res_1 = 0;
        long i = 0;
        while (i < _runeLen(s)) {
            res_1 = res_1 + (s.substring(i, i+1));
            i = i + 1;
        }
        return res_1;
    }

    static void test_sum_of_digits() {
        if (sum_of_digits(12345) != 15) {
            throw new RuntimeException(String.valueOf("sum_of_digits 12345 failed"));
        }
        if (sum_of_digits(123) != 6) {
            throw new RuntimeException(String.valueOf("sum_of_digits 123 failed"));
        }
        if (sum_of_digits(-123) != 6) {
            throw new RuntimeException(String.valueOf("sum_of_digits -123 failed"));
        }
        if (sum_of_digits(0) != 0) {
            throw new RuntimeException(String.valueOf("sum_of_digits 0 failed"));
        }
        if (sum_of_digits_recursion(12345) != 15) {
            throw new RuntimeException(String.valueOf("recursion 12345 failed"));
        }
        if (sum_of_digits_recursion(123) != 6) {
            throw new RuntimeException(String.valueOf("recursion 123 failed"));
        }
        if (sum_of_digits_recursion(-123) != 6) {
            throw new RuntimeException(String.valueOf("recursion -123 failed"));
        }
        if (sum_of_digits_recursion(0) != 0) {
            throw new RuntimeException(String.valueOf("recursion 0 failed"));
        }
        if (sum_of_digits_compact(12345) != 15) {
            throw new RuntimeException(String.valueOf("compact 12345 failed"));
        }
        if (sum_of_digits_compact(123) != 6) {
            throw new RuntimeException(String.valueOf("compact 123 failed"));
        }
        if (sum_of_digits_compact(-123) != 6) {
            throw new RuntimeException(String.valueOf("compact -123 failed"));
        }
        if (sum_of_digits_compact(0) != 0) {
            throw new RuntimeException(String.valueOf("compact 0 failed"));
        }
    }

    static void main() {
        test_sum_of_digits();
        System.out.println(_p(sum_of_digits(12345)));
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
    static long _nowSeed;
    static long _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Long.parseLong(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (_nowSeed * 1664525L + 1013904223) % 2147483647L;
            return _nowSeed;
        }
        return System.nanoTime() / 1000;
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
        return String.valueOf(v);
    }
}
