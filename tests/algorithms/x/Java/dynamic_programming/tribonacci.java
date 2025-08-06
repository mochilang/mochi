public class Main {

    static int[] tribonacci(int num) {
        int[] dp = ((int[])(new int[]{}));
        int i = 0;
        while (i < num) {
            if (i == 0 || i == 1) {
                dp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp), java.util.stream.IntStream.of(0)).toArray()));
            } else             if (i == 2) {
                dp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp), java.util.stream.IntStream.of(1)).toArray()));
            } else {
                int t = dp[i - 1] + dp[i - 2] + dp[i - 3];
                dp = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(dp), java.util.stream.IntStream.of(t)).toArray()));
            }
            i = i + 1;
        }
        return dp;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(tribonacci(8));
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
