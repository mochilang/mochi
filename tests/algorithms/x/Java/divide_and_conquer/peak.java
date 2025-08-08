public class Main {

    static int peak(int[] lst) {
        int low = 0;
        int high = lst.length - 1;
        while (low < high) {
            int mid = Math.floorDiv((low + high), 2);
            if (lst[mid] < lst[mid + 1]) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return lst[low];
    }

    static void main() {
        System.out.println(_p(peak(((int[])(new int[]{1, 2, 3, 4, 5, 4, 3, 2, 1})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 10, 9, 8, 7, 6, 5, 4})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 9, 8, 7})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 2, 3, 4, 5, 6, 7, 0})))));
        System.out.println(_p(peak(((int[])(new int[]{1, 2, 3, 4, 3, 2, 1, 0, -1, -2})))));
    }
    public static void main(String[] args) {
        main();
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
