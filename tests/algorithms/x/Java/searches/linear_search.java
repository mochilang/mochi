public class Main {

    static int linear_search(int[] sequence, int target) {
        int i = 0;
        while (i < sequence.length) {
            if (sequence[i] == target) {
                return i;
            }
            i = i + 1;
        }
        return -1;
    }

    static int rec_linear_search(int[] sequence, int low, int high, int target) {
        if (!(0 <= high && high < sequence.length && 0 <= low && low < sequence.length)) {
            throw new RuntimeException(String.valueOf("Invalid upper or lower bound!"));
        }
        if (high < low) {
            return -1;
        }
        if (sequence[low] == target) {
            return low;
        }
        if (sequence[high] == target) {
            return high;
        }
        return rec_linear_search(((int[])(sequence)), low + 1, high - 1, target);
    }
    public static void main(String[] args) {
        System.out.println(_p(linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 0)));
        System.out.println(_p(linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 15)));
        System.out.println(_p(linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 5)));
        System.out.println(_p(linear_search(((int[])(new int[]{0, 5, 7, 10, 15})), 6)));
        System.out.println(_p(rec_linear_search(((int[])(new int[]{0, 30, 500, 100, 700})), 0, 4, 0)));
        System.out.println(_p(rec_linear_search(((int[])(new int[]{0, 30, 500, 100, 700})), 0, 4, 700)));
        System.out.println(_p(rec_linear_search(((int[])(new int[]{0, 30, 500, 100, 700})), 0, 4, 30)));
        System.out.println(_p(rec_linear_search(((int[])(new int[]{0, 30, 500, 100, 700})), 0, 4, -6)));
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
