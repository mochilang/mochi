public class Main {

    static long int_pow(long base, long exp) {
        long result = 1;
        long i_1 = 0;
        while (i_1 < exp) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static long karatsuba(long a, long b) {
        if (_runeLen(_p(a)) == 1 || _runeLen(_p(b)) == 1) {
            return a * b;
        }
        long m1_1 = _runeLen(_p(a));
        long lb_1 = _runeLen(_p(b));
        if (lb_1 > m1_1) {
            m1_1 = lb_1;
        }
        long m2_1 = Math.floorDiv(m1_1, 2);
        long power_1 = int_pow(10, m2_1);
        long a1_1 = Math.floorDiv(a, power_1);
        long a2_1 = Math.floorMod(a, power_1);
        long b1_1 = Math.floorDiv(b, power_1);
        long b2_1 = Math.floorMod(b, power_1);
        long x_1 = karatsuba(a2_1, b2_1);
        long y_1 = karatsuba(a1_1 + a2_1, b1_1 + b2_1);
        long z_1 = karatsuba(a1_1, b1_1);
        long result_2 = z_1 * int_pow(10, 2 * m2_1) + (y_1 - z_1 - x_1) * power_1 + x_1;
        return result_2;
    }

    static void main() {
        System.out.println(_p(karatsuba(15463, 23489)));
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
