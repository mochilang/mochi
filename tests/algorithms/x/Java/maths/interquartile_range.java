public class Main {

    static double[] bubble_sort(double[] nums) {
        double[] arr = ((double[])(new double[]{}));
        long i_1 = 0;
        while (i_1 < nums.length) {
            arr = ((double[])(appendDouble(arr, nums[(int)(i_1)])));
            i_1 = i_1 + 1;
        }
        long n_1 = arr.length;
        long a_1 = 0;
        while (a_1 < n_1) {
            long b_1 = 0;
            while (b_1 < n_1 - a_1 - 1) {
                if (arr[(int)(b_1)] > arr[(int)(b_1 + 1)]) {
                    double temp_1 = arr[(int)(b_1)];
arr[(int)(b_1)] = arr[(int)(b_1 + 1)];
arr[(int)(b_1 + 1)] = temp_1;
                }
                b_1 = b_1 + 1;
            }
            a_1 = a_1 + 1;
        }
        return arr;
    }

    static double find_median(double[] nums) {
        long length = nums.length;
        long div_1 = Math.floorDiv(length, 2);
        long mod_1 = Math.floorMod(length, 2);
        if (mod_1 != 0) {
            return nums[(int)(div_1)];
        }
        return (nums[(int)(div_1)] + nums[(int)(div_1 - 1)]) / 2.0;
    }

    static double interquartile_range(double[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("The list is empty. Provide a non-empty list."));
        }
        double[] sorted_1 = ((double[])(bubble_sort(((double[])(nums)))));
        long length_2 = sorted_1.length;
        Object div_3 = Math.floorDiv(length_2, 2);
        long mod_3 = Math.floorMod(length_2, 2);
        double[] lower_1 = ((double[])(new double[]{}));
        long i_3 = 0;
        while (i_3 < ((Number)(div_3)).intValue()) {
            lower_1 = ((double[])(appendDouble(lower_1, sorted_1[(int)(i_3)])));
            i_3 = i_3 + 1;
        }
        double[] upper_1 = ((double[])(new double[]{}));
        long j_1 = ((Number)(div_3)).intValue() + mod_3;
        while (j_1 < length_2) {
            upper_1 = ((double[])(appendDouble(upper_1, sorted_1[(int)(j_1)])));
            j_1 = j_1 + 1;
        }
        double q1_1 = find_median(((double[])(lower_1)));
        double q3_1 = find_median(((double[])(upper_1)));
        return q3_1 - q1_1;
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
