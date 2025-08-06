public class Main {

    static int[] primeFactors(int n) {
        int i = 2;
        int[] factors = ((int[])(new int[]{}));
        while (((i * i) <= n)) {
            if (((Math.floorMod(n, i)) == 0)) {
                factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(i)).toArray()));
                n = (n / i);
            } else {
                i = (i + 1);
            }
        }
        if ((n > 1)) {
            factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(n)).toArray()));
        }
        return factors;
    }

    static boolean isSquareFree(int[] factors) {
        java.util.Map<Integer,Boolean> seen = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
        for (int f : factors) {
            if (((Boolean)((seen.containsKey(f))))) {
                return false;
            }
seen.put(f, true);
        }
        return true;
    }

    static int mobius(int n) {
        int[] factors_1 = ((int[])(primeFactors(n)));
        if (((Boolean)((isSquareFree(((int[])(factors_1))))))) {
            return ((Math.floorMod(factors_1.length, 2)) == 0) ? 1 : (-1);
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
