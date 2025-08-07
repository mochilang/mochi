public class Main {

    static int[] cocktail_shaker_sort(int[] arr) {
        int start = 0;
        int end = arr.length - 1;
        while (start < end) {
            boolean swapped = false;
            int i = start;
            while (i < end) {
                if (arr[i] > arr[i + 1]) {
                    int temp = arr[i];
arr[i] = arr[i + 1];
arr[i + 1] = temp;
                    swapped = true;
                }
                i = i + 1;
            }
            if (!swapped) {
                break;
            }
            end = end - 1;
            i = end;
            while (i > start) {
                if (arr[i] < arr[i - 1]) {
                    int temp2 = arr[i];
arr[i] = arr[i - 1];
arr[i - 1] = temp2;
                    swapped = true;
                }
                i = i - 1;
            }
            if (!swapped) {
                break;
            }
            start = start + 1;
        }
        return arr;
    }
    public static void main(String[] args) {
        System.out.println(_p(cocktail_shaker_sort(((int[])(new int[]{4, 5, 2, 1, 2})))));
        System.out.println(_p(cocktail_shaker_sort(((int[])(new int[]{-4, 5, 0, 1, 2, 11})))));
        System.out.println(_p(cocktail_shaker_sort(((int[])(new int[]{1, 2, 3, 4, 5})))));
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
