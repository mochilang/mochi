public class Main {

    static int normalize_index(int index, int n) {
        if (index < 0) {
            return n + index;
        }
        return index;
    }

    static double find_max_iterative(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("find_max_iterative() arg is an empty sequence"));
        }
        double max_num = nums[0];
        int i = 0;
        while (i < nums.length) {
            double x = nums[i];
            if (x > max_num) {
                max_num = x;
            }
            i = i + 1;
        }
        return max_num;
    }

    static double find_max_recursive(double[] nums, int left, int right) {
        int n = nums.length;
        if (n == 0) {
            throw new RuntimeException(String.valueOf("find_max_recursive() arg is an empty sequence"));
        }
        if (left >= n || left < (0 - n) || right >= n || right < (0 - n)) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        int l = normalize_index(left, n);
        int r = normalize_index(right, n);
        if (l == r) {
            return nums[l];
        }
        int mid = (l + r) / 2;
        double left_max = find_max_recursive(((double[])(nums)), l, mid);
        double right_max = find_max_recursive(((double[])(nums)), mid + 1, r);
        if (left_max >= right_max) {
            return left_max;
        }
        return right_max;
    }

    static void test_find_max() {
        double[] arr = ((double[])(new double[]{2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0}));
        if (find_max_iterative(((double[])(arr))) != 94.0) {
            throw new RuntimeException(String.valueOf("find_max_iterative failed"));
        }
        if (find_max_recursive(((double[])(arr)), 0, arr.length - 1) != 94.0) {
            throw new RuntimeException(String.valueOf("find_max_recursive failed"));
        }
        if (find_max_recursive(((double[])(arr)), -arr.length, -1) != 94.0) {
            throw new RuntimeException(String.valueOf("negative index handling failed"));
        }
    }

    static void main() {
        test_find_max();
        double[] nums = ((double[])(new double[]{2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0}));
        System.out.println(find_max_iterative(((double[])(nums))));
        System.out.println(find_max_recursive(((double[])(nums)), 0, nums.length - 1));
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
