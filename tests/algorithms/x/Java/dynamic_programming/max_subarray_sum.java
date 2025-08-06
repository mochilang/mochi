public class Main {
    static double[] empty = new double[0];

    static double max_subarray_sum(double[] nums, boolean allow_empty) {
        if (nums.length == 0) {
            return 0.0;
        }
        double max_sum = 0.0;
        double curr_sum = 0.0;
        if (((Boolean)(allow_empty))) {
            max_sum = 0.0;
            curr_sum = 0.0;
            int i = 0;
            while (i < nums.length) {
                double num = nums[i];
                double temp = curr_sum + num;
                curr_sum = temp > 0.0 ? temp : 0.0;
                if (curr_sum > max_sum) {
                    max_sum = curr_sum;
                }
                i = i + 1;
            }
        } else {
            max_sum = nums[0];
            curr_sum = nums[0];
            int i_1 = 1;
            while (i_1 < nums.length) {
                double num_1 = nums[i_1];
                double temp_1 = curr_sum + num_1;
                curr_sum = temp_1 > num_1 ? temp_1 : num_1;
                if (curr_sum > max_sum) {
                    max_sum = curr_sum;
                }
                i_1 = i_1 + 1;
            }
        }
        return max_sum;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{2.0, 8.0, 9.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{0.0, 0.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{-1.0, 0.0, 1.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{1.0, 2.0, 3.0, 4.0, -2.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{-2.0, 1.0, -3.0, 4.0, -1.0, 2.0, 1.0, -5.0, 4.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{2.0, 3.0, -9.0, 8.0, -2.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{-2.0, -3.0, -1.0, -4.0, -6.0})), false)));
            System.out.println(_p(max_subarray_sum(((double[])(new double[]{-2.0, -3.0, -1.0, -4.0, -6.0})), true)));
            empty = ((double[])(new double[]{}));
            System.out.println(_p(max_subarray_sum(((double[])(empty)), false)));
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
