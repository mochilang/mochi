public class Main {

    static int dp_count(int[] s, int n) {
        if (n < 0) {
            return 0;
        }
        int[] table = ((int[])(new int[]{}));
        int i = 0;
        while (i <= n) {
            table = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(table), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
table[0] = 1;
        int idx = 0;
        while (idx < s.length) {
            int coin_val = s[idx];
            int j = coin_val;
            while (j <= n) {
table[j] = table[j] + table[j - coin_val];
                j = j + 1;
            }
            idx = idx + 1;
        }
        return table[n];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(dp_count(((int[])(new int[]{1, 2, 3})), 4));
            System.out.println(dp_count(((int[])(new int[]{1, 2, 3})), 7));
            System.out.println(dp_count(((int[])(new int[]{2, 5, 3, 6})), 10));
            System.out.println(dp_count(((int[])(new int[]{10})), 99));
            System.out.println(dp_count(((int[])(new int[]{4, 5, 6})), 0));
            System.out.println(dp_count(((int[])(new int[]{1, 2, 3})), -5));
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
