public class Main {

    static int[] odd_sieve(int num) {
        if (num <= 2) {
            return new int[]{};
        }
        if (num == 3) {
            return new int[]{2};
        }
        int size = num / 2 - 1;
        boolean[] sieve = ((boolean[])(new boolean[]{}));
        int idx = 0;
        while (idx < size) {
            sieve = ((boolean[])(appendBool(sieve, true)));
            idx = idx + 1;
        }
        int i = 3;
        while (i * i <= num) {
            int s_idx = i / 2 - 1;
            if (((Boolean)(sieve[s_idx]))) {
                int j = i * i;
                while (j < num) {
                    int j_idx = j / 2 - 1;
sieve[j_idx] = false;
                    j = j + 2 * i;
                }
            }
            i = i + 2;
        }
        int[] primes = ((int[])(new int[]{2}));
        int n = 3;
        int k = 0;
        while (n < num) {
            if (((Boolean)(sieve[k]))) {
                primes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(primes), java.util.stream.IntStream.of(n)).toArray()));
            }
            n = n + 2;
            k = k + 1;
        }
        return primes;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(odd_sieve(2));
            System.out.println(odd_sieve(3));
            System.out.println(odd_sieve(10));
            System.out.println(odd_sieve(20));
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
}
