public class Main {

    static long[] prime_factors(long n) {
        long i = 2;
        long x_1 = n;
        long[] factors_1 = ((long[])(new long[]{}));
        while (i * i <= x_1) {
            if (Math.floorMod(x_1, i) == 0) {
                factors_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(factors_1), java.util.stream.LongStream.of(i)).toArray()));
                x_1 = ((Number)((Math.floorDiv(x_1, i)))).intValue();
            } else {
                i = i + 1;
            }
        }
        if (x_1 > 1) {
            factors_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(factors_1), java.util.stream.LongStream.of(x_1)).toArray()));
        }
        return factors_1;
    }

    static long liouville_lambda(long n) {
        if (n < 1) {
            throw new RuntimeException(String.valueOf("Input must be a positive integer"));
        }
        long cnt_1 = prime_factors(n).length;
        if (Math.floorMod(cnt_1, 2) == 0) {
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
