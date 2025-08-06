public class Main {

    static double[] bubble_sort(double[] nums) {
        double[] arr = ((double[])(new double[]{}));
        int i = 0;
        while (i < nums.length) {
            arr = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(arr), java.util.stream.DoubleStream.of(nums[i])).toArray()));
            i = i + 1;
        }
        int n = arr.length;
        int a = 0;
        while (a < n) {
            int b = 0;
            while (b < n - a - 1) {
                if (arr[b] > arr[b + 1]) {
                    double temp = arr[b];
arr[b] = arr[b + 1];
arr[b + 1] = temp;
                }
                b = b + 1;
            }
            a = a + 1;
        }
        return arr;
    }

    static double find_median(double[] nums) {
        int length = nums.length;
        int div = length / 2;
        int mod = Math.floorMod(length, 2);
        if (mod != 0) {
            return nums[div];
        }
        return (nums[div] + nums[div - 1]) / 2.0;
    }

    static double interquartile_range(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("The list is empty. Provide a non-empty list."));
        }
        double[] sorted = ((double[])(bubble_sort(((double[])(nums)))));
        int length_1 = sorted.length;
        int div_1 = length_1 / 2;
        int mod_1 = Math.floorMod(length_1, 2);
        double[] lower = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < div_1) {
            lower = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(lower), java.util.stream.DoubleStream.of(sorted[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        double[] upper = ((double[])(new double[]{}));
        int j = div_1 + mod_1;
        while (j < length_1) {
            upper = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(upper), java.util.stream.DoubleStream.of(sorted[j])).toArray()));
            j = j + 1;
        }
        double q1 = find_median(((double[])(lower)));
        double q3 = find_median(((double[])(upper)));
        return q3 - q1;
    }

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static boolean float_equal(double a, double b) {
        double diff = absf(a - b);
        return diff < 1e-07;
    }

    static void test_interquartile_range() {
        if (!(Boolean)float_equal(interquartile_range(((double[])(new double[]{4.0, 1.0, 2.0, 3.0, 2.0}))), 2.0)) {
            throw new RuntimeException(String.valueOf("interquartile_range case1 failed"));
        }
        if (!(Boolean)float_equal(interquartile_range(((double[])(new double[]{-2.0, -7.0, -10.0, 9.0, 8.0, 4.0, -67.0, 45.0}))), 17.0)) {
            throw new RuntimeException(String.valueOf("interquartile_range case2 failed"));
        }
        if (!(Boolean)float_equal(interquartile_range(((double[])(new double[]{-2.1, -7.1, -10.1, 9.1, 8.1, 4.1, -67.1, 45.1}))), 17.2)) {
            throw new RuntimeException(String.valueOf("interquartile_range case3 failed"));
        }
        if (!(Boolean)float_equal(interquartile_range(((double[])(new double[]{0.0, 0.0, 0.0, 0.0, 0.0}))), 0.0)) {
            throw new RuntimeException(String.valueOf("interquartile_range case4 failed"));
        }
    }

    static void main() {
        test_interquartile_range();
        System.out.println(_p(interquartile_range(((double[])(new double[]{4.0, 1.0, 2.0, 3.0, 2.0})))));
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
