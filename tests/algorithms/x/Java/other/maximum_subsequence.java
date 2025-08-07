public class Main {

    static int max_int(int a, int b) {
        if (a >= b) {
            return a;
        } else {
            return b;
        }
    }

    static int max_subsequence_sum(int[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("input sequence should not be empty"));
        }
        int ans = nums[0];
        int i = 1;
        while (i < nums.length) {
            int num = nums[i];
            int extended = ans + num;
            ans = max_int(max_int(ans, extended), num);
            i = i + 1;
        }
        return ans;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(max_subsequence_sum(((int[])(new int[]{1, 2, 3, 4, -2}))));
            System.out.println(max_subsequence_sum(((int[])(new int[]{-2, -3, -1, -4, -6}))));
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
