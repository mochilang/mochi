public class Main {

    static int[] prime_factors(int n) {
        int i = 2;
        int x = n;
        int[] factors = ((int[])(new int[]{}));
        while (i * i <= x) {
            if (Math.floorMod(x, i) == 0) {
                factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(i)).toArray()));
                x = ((Number)((x / i))).intValue();
            } else {
                i = i + 1;
            }
        }
        if (x > 1) {
            factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(x)).toArray()));
        }
        return factors;
    }

    static int liouville_lambda(int n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("Input must be a positive integer"));
        }
        int cnt = prime_factors(n).length;
        if (Math.floorMod(cnt, 2) == 0) {
            return 1;
        }
        return 0 - 1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(liouville_lambda(10));
            System.out.println(liouville_lambda(11));
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
}
