public class Main {

    static int maximum_non_adjacent_sum(int[] nums) {
        if (nums.length == 0) {
            return 0;
        }
        int max_including = nums[0];
        int max_excluding = 0;
        int i = 1;
        while (i < nums.length) {
            int num = nums[i];
            int new_including = max_excluding + num;
            int new_excluding = max_including > max_excluding ? max_including : max_excluding;
            max_including = new_including;
            max_excluding = new_excluding;
            i = i + 1;
        }
        if (max_including > max_excluding) {
            return max_including;
        }
        return max_excluding;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(maximum_non_adjacent_sum(((int[])(new int[]{1, 2, 3})))));
            System.out.println(_p(maximum_non_adjacent_sum(((int[])(new int[]{1, 5, 3, 7, 2, 2, 6})))));
            System.out.println(_p(maximum_non_adjacent_sum(((int[])(new int[]{-1, -5, -3, -7, -2, -2, -6})))));
            System.out.println(_p(maximum_non_adjacent_sum(((int[])(new int[]{499, 500, -3, -7, -2, -2, -6})))));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
