public class Main {

    static int isqrt(int n) {
        int r = 0;
        while ((r + 1) * (r + 1) <= n) {
            r = r + 1;
        }
        return r;
    }

    static int[] prime_sieve(int num) {
        if (num <= 0) {
            throw new RuntimeException(String.valueOf("Invalid input, please enter a positive integer."));
        }
        boolean[] sieve = ((boolean[])(new boolean[]{}));
        int i = 0;
        while (i <= num) {
            sieve = ((boolean[])(appendBool(sieve, true)));
            i = i + 1;
        }
        int[] prime = ((int[])(new int[]{}));
        int start = 2;
        int end = isqrt(num);
        while (start <= end) {
            if (((Boolean)(sieve[start]))) {
                prime = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prime), java.util.stream.IntStream.of(start)).toArray()));
                int j = start * start;
                while (j <= num) {
                    if (((Boolean)(sieve[j]))) {
sieve[j] = false;
                    }
                    j = j + start;
                }
            }
            start = start + 1;
        }
        int k = end + 1;
        while (k <= num) {
            if (((Boolean)(sieve[k]))) {
                prime = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prime), java.util.stream.IntStream.of(k)).toArray()));
            }
            k = k + 1;
        }
        return prime;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(prime_sieve(50)));
            System.out.println(_p(prime_sieve(25)));
            System.out.println(_p(prime_sieve(10)));
            System.out.println(_p(prime_sieve(9)));
            System.out.println(_p(prime_sieve(2)));
            System.out.println(_p(prime_sieve(1)));
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
