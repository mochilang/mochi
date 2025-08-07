public class Main {

    static int[] binary_insertion_sort(int[] arr) {
        int i = 1;
        while (i < arr.length) {
            int value = arr[i];
            int low = 0;
            int high = i - 1;
            while (low <= high) {
                int mid = Math.floorDiv((low + high), 2);
                if (value < arr[mid]) {
                    high = mid - 1;
                } else {
                    low = mid + 1;
                }
            }
            int j = i;
            while (j > low) {
arr[j] = arr[j - 1];
                j = j - 1;
            }
arr[low] = value;
            i = i + 1;
        }
        return arr;
    }

    static void main() {
        int[] example1 = ((int[])(new int[]{5, 2, 4, 6, 1, 3}));
        System.out.println(_p(binary_insertion_sort(((int[])(example1)))));
        int[] example2 = ((int[])(new int[]{}));
        System.out.println(_p(binary_insertion_sort(((int[])(example2)))));
        int[] example3 = ((int[])(new int[]{4, 2, 4, 1, 3}));
        System.out.println(_p(binary_insertion_sort(((int[])(example3)))));
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
