public class Main {

    static long[] totient(long n) {
        boolean[] is_prime = ((boolean[])(new boolean[]{}));
        long[] totients_1 = ((long[])(new long[]{}));
        long[] primes_1 = ((long[])(new long[]{}));
        long i_1 = 0;
        while (i_1 <= n) {
            is_prime = ((boolean[])(appendBool(is_prime, true)));
            totients_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(totients_1), java.util.stream.LongStream.of(i_1 - 1)).toArray()));
            i_1 = i_1 + 1;
        }
        i_1 = 2;
        while (i_1 <= n) {
            if (((Boolean)(is_prime[(int)(i_1)]))) {
                primes_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(primes_1), java.util.stream.LongStream.of(i_1)).toArray()));
            }
            long j_1 = 0;
            while (j_1 < primes_1.length) {
                long p_1 = primes_1[(int)(j_1)];
                if (i_1 * p_1 >= n) {
                    break;
                }
is_prime[(int)(i_1 * p_1)] = false;
                if (Math.floorMod(i_1, p_1) == 0) {
totients_1[(int)(i_1 * p_1)] = totients_1[(int)(i_1)] * p_1;
                    break;
                }
totients_1[(int)(i_1 * p_1)] = totients_1[(int)(i_1)] * (p_1 - 1);
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return totients_1;
    }

    static void test_totient() {
        long[] expected = ((long[])(new long[]{-1, 0, 1, 2, 2, 4, 2, 6, 4, 6, 9}));
        long[] res_1 = ((long[])(totient(10)));
        long idx_1 = 0;
        while (idx_1 < expected.length) {
            if (res_1[(int)(idx_1)] != expected[(int)(idx_1)]) {
                throw new RuntimeException(String.valueOf("totient mismatch at " + _p(idx_1)));
            }
            idx_1 = idx_1 + 1;
        }
    }

    static void main() {
        test_totient();
        long n_1 = 10;
        long[] res_3 = ((long[])(totient(n_1)));
        long i_3 = 1;
        while (i_3 < n_1) {
            System.out.println(_p(i_3) + " has " + _p(_geti(res_3, ((Number)(i_3)).intValue())) + " relative primes.");
            i_3 = i_3 + 1;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
