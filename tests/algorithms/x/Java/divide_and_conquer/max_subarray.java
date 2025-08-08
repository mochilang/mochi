public class Main {
    static class Result {
        int start;
        int end;
        double sum;
        Result(int start, int end, double sum) {
            this.start = start;
            this.end = end;
            this.sum = sum;
        }
        Result() {}
        @Override public String toString() {
            return String.format("{'start': %s, 'end': %s, 'sum': %s}", String.valueOf(start), String.valueOf(end), String.valueOf(sum));
        }
    }


    static Result max_cross_sum(double[] arr, int low, int mid, int high) {
        double left_sum = -1000000000000000000.0;
        int max_left = -1;
        double sum = 0.0;
        int i = mid;
        while (i >= low) {
            sum = sum + arr[i];
            if (sum > left_sum) {
                left_sum = sum;
                max_left = i;
            }
            i = i - 1;
        }
        double right_sum = -1000000000000000000.0;
        int max_right = -1;
        sum = 0.0;
        i = mid + 1;
        while (i <= high) {
            sum = sum + arr[i];
            if (sum > right_sum) {
                right_sum = sum;
                max_right = i;
            }
            i = i + 1;
        }
        return new Result(max_left, max_right, left_sum + right_sum);
    }

    static Result max_subarray(double[] arr, int low, int high) {
        if (arr.length == 0) {
            return new Result(-1, -1, 0.0);
        }
        if (low == high) {
            return new Result(low, high, arr[low]);
        }
        int mid = Math.floorDiv((low + high), 2);
        Result left = max_subarray(((double[])(arr)), low, mid);
        Result right = max_subarray(((double[])(arr)), mid + 1, high);
        Result cross = max_cross_sum(((double[])(arr)), low, mid, high);
        if (left.sum >= right.sum && left.sum >= cross.sum) {
            return left;
        }
        if (right.sum >= left.sum && right.sum >= cross.sum) {
            return right;
        }
        return cross;
    }

    static void show(Result res) {
        System.out.println("[" + _p(res.start) + ", " + _p(res.end) + ", " + _p(res.sum) + "]");
    }

    static void main() {
        double[] nums1 = ((double[])(new double[]{-2.0, 1.0, -3.0, 4.0, -1.0, 2.0, 1.0, -5.0, 4.0}));
        Result res1 = max_subarray(((double[])(nums1)), 0, nums1.length - 1);
        show(res1);
        double[] nums2 = ((double[])(new double[]{2.0, 8.0, 9.0}));
        Result res2 = max_subarray(((double[])(nums2)), 0, nums2.length - 1);
        show(res2);
        double[] nums3 = ((double[])(new double[]{0.0, 0.0}));
        Result res3 = max_subarray(((double[])(nums3)), 0, nums3.length - 1);
        show(res3);
        double[] nums4 = ((double[])(new double[]{-1.0, 0.0, 1.0}));
        Result res4 = max_subarray(((double[])(nums4)), 0, nums4.length - 1);
        show(res4);
        double[] nums5 = ((double[])(new double[]{-2.0, -3.0, -1.0, -4.0, -6.0}));
        Result res5 = max_subarray(((double[])(nums5)), 0, nums5.length - 1);
        show(res5);
        double[] nums6 = ((double[])(new double[]{}));
        Result res6 = max_subarray(((double[])(nums6)), 0, 0);
        show(res6);
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
