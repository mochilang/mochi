public class Main {

    static long[] primeFactors(long n) {
        long i = 2;
        long[] factors_1 = ((long[])(new long[]{}));
        while (((i * i) <= n)) {
            if (((Math.floorMod(n, i)) == 0)) {
                factors_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(factors_1), java.util.stream.LongStream.of(i)).toArray()));
                n = ((Number)((Math.floorDiv(n, i)))).longValue();
            } else {
                i = (i + 1);
            }
        }
        if ((n > 1)) {
            factors_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(factors_1), java.util.stream.LongStream.of(n)).toArray()));
        }
        return factors_1;
    }

    static boolean isSquareFree(long[] factors) {
        java.util.Map<Long,Boolean> seen = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
        for (long f : factors) {
            if ((seen.containsKey(f))) {
                return false;
            }
seen.put(f, true);
        }
        return true;
    }

    static long mobius(long n) {
        long[] factors_2 = ((long[])(primeFactors(n)));
        if (((Boolean)((isSquareFree(((long[])(factors_2))))))) {
            return ((Math.floorMod(factors_2.length, 2)) == 0) ? 1 : (-1);
        }
        return 0;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(mobius(24));
            System.out.println(mobius(-1));
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
