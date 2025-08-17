public class Main {

    static long abs_int(long n) {
        if ((long)(n) < 0L) {
            return -n;
        }
        return n;
    }

    static long sum_of_digits(long n) {
        long m = (long)(abs_int((long)(n)));
        long res_1 = 0L;
        while ((long)(m) > 0L) {
            res_1 = (long)((long)(res_1) + (long)((Math.floorMod(m, 10))));
            m = Math.floorDiv(m, 10);
        }
        return res_1;
    }

    static long sum_of_digits_recursion(long n) {
        long m_1 = (long)(abs_int((long)(n)));
        if ((long)(m_1) < 10L) {
            return m_1;
        }
        return (long)((Math.floorMod(m_1, 10))) + (long)(sum_of_digits_recursion(Math.floorDiv(((long)(m_1)), ((long)(10)))));
    }

    static long sum_of_digits_compact(long n) {
        String s = _p(abs_int((long)(n)));
        long res_3 = 0L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(_runeLen(s))) {
            res_3 = (long)((long)(res_3) + (long)((Long.parseLong(s.substring((int)((long)(i_1)), (int)((long)(i_1))+1)))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res_3;
    }

    static void test_sum_of_digits() {
        if ((long)(sum_of_digits(12345L)) != 15L) {
            throw new RuntimeException(String.valueOf("sum_of_digits 12345 failed"));
        }
        if ((long)(sum_of_digits(123L)) != 6L) {
            throw new RuntimeException(String.valueOf("sum_of_digits 123 failed"));
        }
        if ((long)(sum_of_digits((long)(-123))) != 6L) {
            throw new RuntimeException(String.valueOf("sum_of_digits -123 failed"));
        }
        if ((long)(sum_of_digits(0L)) != 0L) {
            throw new RuntimeException(String.valueOf("sum_of_digits 0 failed"));
        }
        if ((long)(sum_of_digits_recursion(12345L)) != 15L) {
            throw new RuntimeException(String.valueOf("recursion 12345 failed"));
        }
        if ((long)(sum_of_digits_recursion(123L)) != 6L) {
            throw new RuntimeException(String.valueOf("recursion 123 failed"));
        }
        if ((long)(sum_of_digits_recursion((long)(-123))) != 6L) {
            throw new RuntimeException(String.valueOf("recursion -123 failed"));
        }
        if ((long)(sum_of_digits_recursion(0L)) != 0L) {
            throw new RuntimeException(String.valueOf("recursion 0 failed"));
        }
        if ((long)(sum_of_digits_compact(12345L)) != 15L) {
            throw new RuntimeException(String.valueOf("compact 12345 failed"));
        }
        if ((long)(sum_of_digits_compact(123L)) != 6L) {
            throw new RuntimeException(String.valueOf("compact 123 failed"));
        }
        if ((long)(sum_of_digits_compact((long)(-123))) != 6L) {
            throw new RuntimeException(String.valueOf("compact -123 failed"));
        }
        if ((long)(sum_of_digits_compact(0L)) != 0L) {
            throw new RuntimeException(String.valueOf("compact 0 failed"));
        }
    }

    static void main() {
        test_sum_of_digits();
        System.out.println(_p(sum_of_digits(12345L)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
