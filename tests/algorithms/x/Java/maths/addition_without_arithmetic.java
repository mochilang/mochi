public class Main {
    static long MAX = 4294967296L;
    static long HALF = 2147483648L;

    static long to_unsigned(long n) {
        if ((long)(n) < 0L) {
            return (long)(MAX) + (long)(n);
        }
        return n;
    }

    static long from_unsigned(long n) {
        if ((long)(n) >= (long)(HALF)) {
            return (long)(n) - (long)(MAX);
        }
        return n;
    }

    static long bit_and(long a, long b) {
        long x = (long)(a);
        long y_1 = (long)(b);
        long res_1 = 0L;
        long bit_1 = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < 32L) {
            if ((Math.floorMod(x, 2) == 1L) && (Math.floorMod(y_1, 2) == 1L)) {
                res_1 = (long)((long)(res_1) + (long)(bit_1));
            }
            x = (long)((long)(x) / 2L);
            y_1 = (long)((long)(y_1) / 2L);
            bit_1 = (long)((long)(bit_1) * 2L);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res_1;
    }

    static long bit_xor(long a, long b) {
        long x_1 = (long)(a);
        long y_3 = (long)(b);
        long res_3 = 0L;
        long bit_3 = 1L;
        long i_3 = 0L;
        while ((long)(i_3) < 32L) {
            long abit_1 = Math.floorMod(x_1, 2);
            long bbit_1 = Math.floorMod(y_3, 2);
            if (Math.floorMod(((long)(abit_1) + (long)(bbit_1)), 2) == 1L) {
                res_3 = (long)((long)(res_3) + (long)(bit_3));
            }
            x_1 = (long)((long)(x_1) / 2L);
            y_3 = (long)((long)(y_3) / 2L);
            bit_3 = (long)((long)(bit_3) * 2L);
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res_3;
    }

    static long lshift1(long num) {
        return Math.floorMod(((long)(num) * 2L), MAX);
    }

    static long add(long a, long b) {
        long first = (long)(to_unsigned((long)(a)));
        long second_1 = (long)(to_unsigned((long)(b)));
        while ((long)(second_1) != 0L) {
            long carry_1 = (long)(bit_and((long)(first), (long)(second_1)));
            first = (long)(bit_xor((long)(first), (long)(second_1)));
            second_1 = (long)(lshift1((long)(carry_1)));
        }
        long result_1 = (long)(from_unsigned((long)(first)));
        return result_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(add(3L, 5L)));
            System.out.println(_p(add(13L, 5L)));
            System.out.println(_p(add((long)(-7), 2L)));
            System.out.println(_p(add(0L, (long)(-7))));
            System.out.println(_p(add((long)(-321), 0L)));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
