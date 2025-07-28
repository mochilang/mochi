public class Main {
    static int amount = 1000;

    static int countChange(int amount) {
        int[] ways = new int[]{};
        int i = 0;
        while (i <= amount) {
            ways = java.util.stream.IntStream.concat(java.util.Arrays.stream(ways), java.util.stream.IntStream.of(0)).toArray();
            i = i + 1;
        }
ways[0] = 1;
        for (int coin : new int[]{100, 50, 25, 10, 5, 1}) {
            int j = coin;
            while (j <= amount) {
ways[j] = ways[j] + ways[j - coin];
                j = j + 1;
            }
        }
        return ways[amount];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("amount, ways to make change: " + String.valueOf(amount) + " " + String.valueOf(countChange(amount)));
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
