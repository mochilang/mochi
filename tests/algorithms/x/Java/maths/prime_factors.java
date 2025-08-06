public class Main {

    static int[] prime_factors(int n) {
        if (n < 2) {
            return new int[]{};
        }
        int num = n;
        int i = 2;
        int[] factors = ((int[])(new int[]{}));
        while (i * i <= num) {
            if (Math.floorMod(num, i) == 0) {
                factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(i)).toArray()));
                num = num / i;
            } else {
                i = i + 1;
            }
        }
        if (num > 1) {
            factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(num)).toArray()));
        }
        return factors;
    }

    static boolean list_eq(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_1 = 0;
        while (i_1 < a.length) {
            if (a[i_1] != b[i_1]) {
                return false;
            }
            i_1 = i_1 + 1;
        }
        return true;
    }

    static void test_prime_factors() {
        if (!(Boolean)list_eq(((int[])(prime_factors(0))), ((int[])(new int[]{})))) {
            throw new RuntimeException(String.valueOf("prime_factors(0) failed"));
        }
        if (!(Boolean)list_eq(((int[])(prime_factors(100))), ((int[])(new int[]{2, 2, 5, 5})))) {
            throw new RuntimeException(String.valueOf("prime_factors(100) failed"));
        }
        if (!(Boolean)list_eq(((int[])(prime_factors(2560))), ((int[])(new int[]{2, 2, 2, 2, 2, 2, 2, 2, 2, 5})))) {
            throw new RuntimeException(String.valueOf("prime_factors(2560) failed"));
        }
        if (!(Boolean)list_eq(((int[])(prime_factors(97))), ((int[])(new int[]{97})))) {
            throw new RuntimeException(String.valueOf("prime_factors(97) failed"));
        }
    }

    static void main() {
        test_prime_factors();
        System.out.println(_p(prime_factors(100)));
        System.out.println(_p(prime_factors(2560)));
        System.out.println(_p(prime_factors(97)));
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
