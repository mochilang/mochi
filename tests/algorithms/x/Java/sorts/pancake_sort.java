public class Main {

    static int[] flip(int[] arr, int k) {
        int start = 0;
        int end = k;
        while (start < end) {
            int temp = arr[start];
arr[start] = arr[end];
arr[end] = temp;
            start = start + 1;
            end = end - 1;
        }
        return arr;
    }

    static int find_max_index(int[] arr, int n) {
        int mi = 0;
        int i = 1;
        while (i < n) {
            if (arr[i] > arr[mi]) {
                mi = i;
            }
            i = i + 1;
        }
        return mi;
    }

    static int[] pancake_sort(int[] arr) {
        int cur = arr.length;
        while (cur > 1) {
            int mi_1 = find_max_index(((int[])(arr)), cur);
            arr = ((int[])(flip(((int[])(arr)), mi_1)));
            arr = ((int[])(flip(((int[])(arr)), cur - 1)));
            cur = cur - 1;
        }
        return arr;
    }

    static void main() {
        int[] data = ((int[])(new int[]{3, 6, 1, 10, 2}));
        int[] sorted = ((int[])(pancake_sort(((int[])(data)))));
        System.out.println(_p(sorted));
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
