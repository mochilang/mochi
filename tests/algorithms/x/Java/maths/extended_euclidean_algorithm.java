public class Main {
    static class Coeffs {
        long x;
        long y;
        Coeffs(long x, long y) {
            this.x = x;
            this.y = y;
        }
        Coeffs() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static long abs_val(long n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static Coeffs extended_euclidean_algorithm(long a, long b) {
        if (abs_val(a) == 1) {
            return new Coeffs(a, 0);
        }
        if (abs_val(b) == 1) {
            return new Coeffs(0, b);
        }
        long old_remainder_1 = a;
        long remainder_1 = b;
        long old_coeff_a_1 = 1;
        long coeff_a_1 = 0;
        long old_coeff_b_1 = 0;
        long coeff_b_1 = 1;
        while (remainder_1 != 0) {
            long quotient_1 = Math.floorDiv(old_remainder_1, remainder_1);
            long temp_remainder_1 = old_remainder_1 - quotient_1 * remainder_1;
            old_remainder_1 = remainder_1;
            remainder_1 = temp_remainder_1;
            long temp_a_1 = old_coeff_a_1 - quotient_1 * coeff_a_1;
            old_coeff_a_1 = coeff_a_1;
            coeff_a_1 = temp_a_1;
            long temp_b_1 = old_coeff_b_1 - quotient_1 * coeff_b_1;
            old_coeff_b_1 = coeff_b_1;
            coeff_b_1 = temp_b_1;
        }
        if (a < 0) {
            old_coeff_a_1 = -old_coeff_a_1;
        }
        if (b < 0) {
            old_coeff_b_1 = -old_coeff_b_1;
        }
        return new Coeffs(old_coeff_a_1, old_coeff_b_1);
    }

    static void test_extended_euclidean_algorithm() {
        Coeffs r1 = extended_euclidean_algorithm(1, 24);
        if ((r1.x != 1) || (r1.y != 0)) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        Coeffs r2_1 = extended_euclidean_algorithm(8, 14);
        if ((r2_1.x != 2) || (r2_1.y != (-1))) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        Coeffs r3_1 = extended_euclidean_algorithm(240, 46);
        if ((r3_1.x != (-9)) || (r3_1.y != 47)) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        Coeffs r4_1 = extended_euclidean_algorithm(1, -4);
        if ((r4_1.x != 1) || (r4_1.y != 0)) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        Coeffs r5_1 = extended_euclidean_algorithm(-2, -4);
        if ((r5_1.x != (-1)) || (r5_1.y != 0)) {
            throw new RuntimeException(String.valueOf("test5 failed"));
        }
        Coeffs r6_1 = extended_euclidean_algorithm(0, -4);
        if ((r6_1.x != 0) || (r6_1.y != (-1))) {
            throw new RuntimeException(String.valueOf("test6 failed"));
        }
        Coeffs r7_1 = extended_euclidean_algorithm(2, 0);
        if ((r7_1.x != 1) || (r7_1.y != 0)) {
            throw new RuntimeException(String.valueOf("test7 failed"));
        }
    }

    static void main() {
        test_extended_euclidean_algorithm();
        Coeffs res_1 = extended_euclidean_algorithm(240, 46);
        System.out.println("(" + _p(res_1.x) + ", " + _p(res_1.y) + ")");
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
