public class Main {

    static int[] totient(int n) {
        boolean[] is_prime = ((boolean[])(new boolean[]{}));
        int[] totients = ((int[])(new int[]{}));
        int[] primes = ((int[])(new int[]{}));
        int i = 0;
        while (i <= n) {
            is_prime = ((boolean[])(appendBool(is_prime, true)));
            totients = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(totients), java.util.stream.IntStream.of(i - 1)).toArray()));
            i = i + 1;
        }
        i = 2;
        while (i <= n) {
            if (((Boolean)(is_prime[i]))) {
                primes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(i)).toArray()));
            }
            int j = 0;
            while (j < primes.length) {
                int p = primes[j];
                if (i * p >= n) {
                    break;
                }
is_prime[i * p] = false;
                if (Math.floorMod(i, p) == 0) {
totients[i * p] = totients[i] * p;
                    break;
                }
totients[i * p] = totients[i] * (p - 1);
                j = j + 1;
            }
            i = i + 1;
        }
        return totients;
    }

    static void test_totient() {
        int[] expected = ((int[])(new int[]{-1, 0, 1, 2, 2, 4, 2, 6, 4, 6, 9}));
        int[] res = ((int[])(totient(10)));
        int idx = 0;
        while (idx < expected.length) {
            if (res[idx] != expected[idx]) {
                throw new RuntimeException(String.valueOf("totient mismatch at " + _p(idx)));
            }
            idx = idx + 1;
        }
    }

    static void main() {
        test_totient();
        int n = 10;
        int[] res_1 = ((int[])(totient(n)));
        int i_1 = 1;
        while (i_1 < n) {
            System.out.println(_p(i_1) + " has " + _p(_geti(res_1, i_1)) + " relative primes.");
            i_1 = i_1 + 1;
        }
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
