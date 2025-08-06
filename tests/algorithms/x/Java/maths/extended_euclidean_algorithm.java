public class Main {
    static class Coeffs {
        int x;
        int y;
        Coeffs(int x, int y) {
            this.x = x;
            this.y = y;
        }
        Coeffs() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }


    static int abs_val(int n) {
        if (n < 0) {
            return -n;
        }
        return n;
    }

    static Coeffs extended_euclidean_algorithm(int a, int b) {
        if (abs_val(a) == 1) {
            return new Coeffs(a, 0);
        }
        if (abs_val(b) == 1) {
            return new Coeffs(0, b);
        }
        int old_remainder = a;
        int remainder = b;
        int old_coeff_a = 1;
        int coeff_a = 0;
        int old_coeff_b = 0;
        int coeff_b = 1;
        while (remainder != 0) {
            int quotient = old_remainder / remainder;
            int temp_remainder = old_remainder - quotient * remainder;
            old_remainder = remainder;
            remainder = temp_remainder;
            int temp_a = old_coeff_a - quotient * coeff_a;
            old_coeff_a = coeff_a;
            coeff_a = temp_a;
            int temp_b = old_coeff_b - quotient * coeff_b;
            old_coeff_b = coeff_b;
            coeff_b = temp_b;
        }
        if (a < 0) {
            old_coeff_a = -old_coeff_a;
        }
        if (b < 0) {
            old_coeff_b = -old_coeff_b;
        }
        return new Coeffs(old_coeff_a, old_coeff_b);
    }

    static void test_extended_euclidean_algorithm() {
        Coeffs r1 = extended_euclidean_algorithm(1, 24);
        if ((r1.x != 1) || (r1.y != 0)) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        Coeffs r2 = extended_euclidean_algorithm(8, 14);
        if ((r2.x != 2) || (r2.y != (-1))) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        Coeffs r3 = extended_euclidean_algorithm(240, 46);
        if ((r3.x != (-9)) || (r3.y != 47)) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        Coeffs r4 = extended_euclidean_algorithm(1, -4);
        if ((r4.x != 1) || (r4.y != 0)) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        Coeffs r5 = extended_euclidean_algorithm(-2, -4);
        if ((r5.x != (-1)) || (r5.y != 0)) {
            throw new RuntimeException(String.valueOf("test5 failed"));
        }
        Coeffs r6 = extended_euclidean_algorithm(0, -4);
        if ((r6.x != 0) || (r6.y != (-1))) {
            throw new RuntimeException(String.valueOf("test6 failed"));
        }
        Coeffs r7 = extended_euclidean_algorithm(2, 0);
        if ((r7.x != 1) || (r7.y != 0)) {
            throw new RuntimeException(String.valueOf("test7 failed"));
        }
    }

    static void main() {
        test_extended_euclidean_algorithm();
        Coeffs res = extended_euclidean_algorithm(240, 46);
        System.out.println("(" + _p(res.x) + ", " + _p(res.y) + ")");
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
