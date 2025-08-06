public class Main {

    static int minimum_subarray_sum(int target, int[] numbers) {
        int n = numbers.length;
        if (n == 0) {
            return 0;
        }
        if (target == 0) {
            int i = 0;
            while (i < n) {
                if (numbers[i] == 0) {
                    return 0;
                }
                i = i + 1;
            }
        }
        int left = 0;
        int right = 0;
        int curr_sum = 0;
        int min_len = n + 1;
        while (right < n) {
            curr_sum = curr_sum + numbers[right];
            while (curr_sum >= target && left <= right) {
                int current_len = right - left + 1;
                if (current_len < min_len) {
                    min_len = current_len;
                }
                curr_sum = curr_sum - numbers[left];
                left = left + 1;
            }
            right = right + 1;
        }
        if (min_len == n + 1) {
            return 0;
        }
        return min_len;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(minimum_subarray_sum(7, ((int[])(new int[]{2, 3, 1, 2, 4, 3})))));
            System.out.println(_p(minimum_subarray_sum(7, ((int[])(new int[]{2, 3, -1, 2, 4, -3})))));
            System.out.println(_p(minimum_subarray_sum(11, ((int[])(new int[]{1, 1, 1, 1, 1, 1, 1, 1})))));
            System.out.println(_p(minimum_subarray_sum(0, ((int[])(new int[]{1, 2, 3})))));
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
