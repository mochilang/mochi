public class Main {

    static int[] prime_sieve_eratosthenes(int num) {
        if (num <= 0) {
            throw new RuntimeException(String.valueOf("Input must be a positive integer"));
        }
        boolean[] primes = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i <= num) {
            primes = ((boolean[])(appendBool(primes, true)));
            i = i + 1;
        }
        int p = 2;
        while (p * p <= num) {
            if (((Boolean)(primes[p]))) {
                int j = p * p;
                while (j <= num) {
primes[j] = false;
                    j = j + p;
                }
            }
            p = p + 1;
        }
        int[] result = ((int[])(new int[]{}));
        int k = 2;
        while (k <= num) {
            if (((Boolean)(primes[k]))) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(k)).toArray()));
            }
            k = k + 1;
        }
        return result;
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

    static void test_prime_sieve_eratosthenes() {
        if (!(Boolean)list_eq(((int[])(prime_sieve_eratosthenes(10))), ((int[])(new int[]{2, 3, 5, 7})))) {
            throw new RuntimeException(String.valueOf("test 10 failed"));
        }
        if (!(Boolean)list_eq(((int[])(prime_sieve_eratosthenes(20))), ((int[])(new int[]{2, 3, 5, 7, 11, 13, 17, 19})))) {
            throw new RuntimeException(String.valueOf("test 20 failed"));
        }
        if (!(Boolean)list_eq(((int[])(prime_sieve_eratosthenes(2))), ((int[])(new int[]{2})))) {
            throw new RuntimeException(String.valueOf("test 2 failed"));
        }
        if (prime_sieve_eratosthenes(1).length != 0) {
            throw new RuntimeException(String.valueOf("test 1 failed"));
        }
    }

    static void main() {
        test_prime_sieve_eratosthenes();
        System.out.println(_p(prime_sieve_eratosthenes(20)));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
