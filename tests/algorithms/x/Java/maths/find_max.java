public class Main {

    static long normalize_index(long index, long n) {
        if (index < 0) {
            return n + index;
        }
        return index;
    }

    static double find_max_iterative(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("find_max_iterative() arg is an empty sequence"));
        }
        double max_num_1 = nums[(int)(0)];
        long i_1 = 0;
        while (i_1 < nums.length) {
            double x_1 = nums[(int)(i_1)];
            if (x_1 > max_num_1) {
                max_num_1 = x_1;
            }
            i_1 = i_1 + 1;
        }
        return max_num_1;
    }

    static double find_max_recursive(double[] nums, long left, long right) {
        long n = nums.length;
        if (n == 0) {
            throw new RuntimeException(String.valueOf("find_max_recursive() arg is an empty sequence"));
        }
        if (left >= n || left < (0 - n) || right >= n || right < (0 - n)) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        long l_1 = normalize_index(left, n);
        long r_1 = normalize_index(right, n);
        if (l_1 == r_1) {
            return nums[(int)(l_1)];
        }
        long mid_1 = Math.floorDiv((l_1 + r_1), 2);
        double left_max_1 = find_max_recursive(((double[])(nums)), l_1, mid_1);
        double right_max_1 = find_max_recursive(((double[])(nums)), mid_1 + 1, r_1);
        if (left_max_1 >= right_max_1) {
            return left_max_1;
        }
        return right_max_1;
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
        double[] nums_1 = ((double[])(new double[]{2.0, 4.0, 9.0, 7.0, 19.0, 94.0, 5.0}));
        System.out.println(find_max_iterative(((double[])(nums_1))));
        System.out.println(find_max_recursive(((double[])(nums_1)), 0, nums_1.length - 1));
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
