public class Main {

    static double find_min_iterative(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("find_min_iterative() arg is an empty sequence"));
        }
        double min_num = nums[0];
        int i = 0;
        while (i < nums.length) {
            double num = nums[i];
            if (num < min_num) {
                min_num = num;
            }
            i = i + 1;
        }
        return min_num;
    }

    static double find_min_recursive(double[] nums, int left, int right) {
        int n = nums.length;
        if (n == 0) {
            throw new RuntimeException(String.valueOf("find_min_recursive() arg is an empty sequence"));
        }
        if (left >= n || left < (0 - n) || right >= n || right < (0 - n)) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        int l = left;
        int r = right;
        if (l < 0) {
            l = n + l;
        }
        if (r < 0) {
            r = n + r;
        }
        if (l == r) {
            return nums[l];
        }
        int mid = (l + r) / 2;
        double left_min = find_min_recursive(((double[])(nums)), l, mid);
        double right_min = find_min_recursive(((double[])(nums)), mid + 1, r);
        if (left_min <= right_min) {
            return left_min;
        }
        return right_min;
    }

    static void test_find_min() {
        double[] a = ((double[])(new double[]{3.0, 2.0, 1.0}));
        if (find_min_iterative(((double[])(a))) != 1.0) {
            throw new RuntimeException(String.valueOf("iterative test1 failed"));
        }
        if (find_min_recursive(((double[])(a)), 0, a.length - 1) != 1.0) {
            throw new RuntimeException(String.valueOf("recursive test1 failed"));
        }
        double[] b = ((double[])(new double[]{-3.0, -2.0, -1.0}));
        if (find_min_iterative(((double[])(b))) != (-3.0)) {
            throw new RuntimeException(String.valueOf("iterative test2 failed"));
        }
        if (find_min_recursive(((double[])(b)), 0, b.length - 1) != (-3.0)) {
            throw new RuntimeException(String.valueOf("recursive test2 failed"));
        }
        double[] c = ((double[])(new double[]{3.0, -3.0, 0.0}));
        if (find_min_iterative(((double[])(c))) != (-3.0)) {
            throw new RuntimeException(String.valueOf("iterative test3 failed"));
        }
        if (find_min_recursive(((double[])(c)), 0, c.length - 1) != (-3.0)) {
            throw new RuntimeException(String.valueOf("recursive test3 failed"));
        }
        double[] d = ((double[])(new double[]{1.0, 3.0, 5.0, 7.0, 9.0, 2.0, 4.0, 6.0, 8.0, 10.0}));
        if (find_min_recursive(((double[])(d)), (0 - d.length), (0 - 1)) != 1.0) {
            throw new RuntimeException(String.valueOf("negative index test failed"));
        }
    }

    static void main() {
        test_find_min();
        double[] sample = ((double[])(new double[]{0.0, 1.0, 2.0, 3.0, 4.0, 5.0, -3.0, 24.0, -56.0}));
        System.out.println(_p(find_min_iterative(((double[])(sample)))));
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
