public class Main {

    static int[] slow_primes(int max_n) {
        int[] result = ((int[])(new int[]{}));
        int i = 2;
        while (i <= max_n) {
            int j = 2;
            boolean is_prime = true;
            while (j < i) {
                if (Math.floorMod(i, j) == 0) {
                    is_prime = false;
                    break;
                }
                j = j + 1;
            }
            if (((Boolean)(is_prime))) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(i)).toArray()));
            }
            i = i + 1;
        }
        return result;
    }

    static int[] primes(int max_n) {
        int[] result_1 = ((int[])(new int[]{}));
        int i_1 = 2;
        while (i_1 <= max_n) {
            int j_1 = 2;
            boolean is_prime_1 = true;
            while (j_1 * j_1 <= i_1) {
                if (Math.floorMod(i_1, j_1) == 0) {
                    is_prime_1 = false;
                    break;
                }
                j_1 = j_1 + 1;
            }
            if (((Boolean)(is_prime_1))) {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(i_1)).toArray()));
            }
            i_1 = i_1 + 1;
        }
        return result_1;
    }

    static int[] fast_primes(int max_n) {
        int[] result_2 = ((int[])(new int[]{}));
        if (max_n >= 2) {
            result_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_2), java.util.stream.IntStream.of(2)).toArray()));
        }
        int i_2 = 3;
        while (i_2 <= max_n) {
            int j_2 = 3;
            boolean is_prime_2 = true;
            while (j_2 * j_2 <= i_2) {
                if (Math.floorMod(i_2, j_2) == 0) {
                    is_prime_2 = false;
                    break;
                }
                j_2 = j_2 + 2;
            }
            if (((Boolean)(is_prime_2))) {
                result_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_2), java.util.stream.IntStream.of(i_2)).toArray()));
            }
            i_2 = i_2 + 2;
        }
        return result_2;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(slow_primes(25)));
            System.out.println(_p(primes(25)));
            System.out.println(_p(fast_primes(25)));
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
