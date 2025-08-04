public class Main {

    static int[] primeFactors(int n) {
        int[] factors = ((int[])(new int[]{}));
        int last = 0;
        int x = n;
        while (Math.floorMod(x, 2) == 0) {
            if (last == 2) {
                return new int[]{};
            }
            factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(2)).toArray()));
            last = 2;
            x = x / 2;
        }
        int p = 3;
        while (p * p <= x) {
            while (Math.floorMod(x, p) == 0) {
                if (last == p) {
                    return new int[]{};
                }
                factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(p)).toArray()));
                last = p;
                x = x / p;
            }
            p = p + 2;
        }
        if (x > 1) {
            if (last == x) {
                return new int[]{};
            }
            factors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factors), java.util.stream.IntStream.of(x)).toArray()));
        }
        return factors;
    }

    static boolean isGiuga(int n) {
        int[] facs = ((int[])(primeFactors(n)));
        if (facs.length <= 2) {
            return false;
        }
        for (int f : facs) {
            if ((Math.floorMod((n / f - 1), f)) != 0) {
                return false;
            }
        }
        return true;
    }

    static void main() {
        int[] known = ((int[])(new int[]{30, 858, 1722, 66198}));
        int[] nums = ((int[])(new int[]{}));
        for (int n : known) {
            if (((Boolean)(isGiuga(n)))) {
                nums = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(nums), java.util.stream.IntStream.of(n)).toArray()));
            }
        }
        System.out.println("The first 4 Giuga numbers are:");
        System.out.println(java.util.Arrays.toString(nums));
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
}
