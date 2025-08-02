public class Main {
    static int seed;

    static int randN(int n) {
        seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
        return Math.floorMod(seed, n);
    }

    static int[] eqIndices(int[] xs) {
        int r = 0;
        int i = 0;
        while (i < xs.length) {
            r = r + xs[i];
            i = i + 1;
        }
        int l = 0;
        int[] eq = new int[]{};
        i = 0;
        while (i < xs.length) {
            r = r - xs[i];
            if (l == r) {
                eq = java.util.stream.IntStream.concat(java.util.Arrays.stream(eq), java.util.stream.IntStream.of(i)).toArray();
            }
            l = l + xs[i];
            i = i + 1;
        }
        return eq;
    }

    static void main() {
        System.out.println(eqIndices(new int[]{-7, 1, 5, 2, -4, 3, 0}));
        int[] verylong = new int[]{};
        int i_1 = 0;
        while (i_1 < 10000) {
            seed = Math.floorMod((seed * 1664525 + 1013904223), 2147483647);
            verylong = java.util.stream.IntStream.concat(java.util.Arrays.stream(verylong), java.util.stream.IntStream.of(Math.floorMod(seed, 1001) - 500)).toArray();
            i_1 = i_1 + 1;
        }
        System.out.println(eqIndices(verylong));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = Math.floorMod(_now(), 2147483647);
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
