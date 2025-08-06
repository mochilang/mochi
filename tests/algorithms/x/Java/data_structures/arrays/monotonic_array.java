public class Main {

    static boolean is_monotonic(int[] nums) {
        if (nums.length <= 2) {
            return true;
        }
        boolean increasing = true;
        boolean decreasing = true;
        int i = 0;
        while (i < nums.length - 1) {
            if (nums[i] > nums[i + 1]) {
                increasing = false;
            }
            if (nums[i] < nums[i + 1]) {
                decreasing = false;
            }
            i = i + 1;
        }
        return increasing || decreasing;
    }
    public static void main(String[] args) {
        System.out.println(_p(is_monotonic(((int[])(new int[]{1, 2, 2, 3})))));
        System.out.println(_p(is_monotonic(((int[])(new int[]{6, 5, 4, 4})))));
        System.out.println(_p(is_monotonic(((int[])(new int[]{1, 3, 2})))));
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
