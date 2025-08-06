public class Main {

    static int max_sum_sliding_window(int[] arr, int k) {
        if (k < 0 || arr.length < k) {
            throw new RuntimeException(String.valueOf("Invalid Input"));
        }
        int idx = 0;
        int current_sum = 0;
        while (idx < k) {
            current_sum = current_sum + arr[idx];
            idx = idx + 1;
        }
        int max_sum = current_sum;
        int i = 0;
        while (i < arr.length - k) {
            current_sum = current_sum - arr[i] + arr[i + k];
            if (current_sum > max_sum) {
                max_sum = current_sum;
            }
            i = i + 1;
        }
        return max_sum;
    }

    static void test_max_sum_sliding_window() {
        int[] arr1 = ((int[])(new int[]{1, 4, 2, 10, 2, 3, 1, 0, 20}));
        if (max_sum_sliding_window(((int[])(arr1)), 4) != 24) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        int[] arr2 = ((int[])(new int[]{1, 4, 2, 10, 2, 13, 1, 0, 2}));
        if (max_sum_sliding_window(((int[])(arr2)), 4) != 27) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
    }

    static void main() {
        test_max_sum_sliding_window();
        int[] sample = ((int[])(new int[]{1, 4, 2, 10, 2, 3, 1, 0, 20}));
        System.out.println(_p(max_sum_sliding_window(((int[])(sample)), 4)));
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
