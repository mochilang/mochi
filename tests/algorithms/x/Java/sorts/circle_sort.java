public class Main {

    static boolean circle_sort_util(int[] collection, int low, int high) {
        boolean swapped = false;
        if (low == high) {
            return swapped;
        }
        int left = low;
        int right = high;
        while (left < right) {
            if (collection[left] > collection[right]) {
                int tmp = collection[left];
collection[left] = collection[right];
collection[right] = tmp;
                swapped = true;
            }
            left = left + 1;
            right = right - 1;
        }
        if (left == right && collection[left] > collection[right + 1]) {
            int tmp2 = collection[left];
collection[left] = collection[right + 1];
collection[right + 1] = tmp2;
            swapped = true;
        }
        int mid = low + Math.floorDiv((high - low), 2);
        boolean left_swap = circle_sort_util(((int[])(collection)), low, mid);
        boolean right_swap = circle_sort_util(((int[])(collection)), mid + 1, high);
        if (swapped || ((Boolean)(left_swap)) || ((Boolean)(right_swap))) {
            return true;
        } else {
            return false;
        }
    }

    static int[] circle_sort(int[] collection) {
        if (collection.length < 2) {
            return collection;
        }
        boolean is_not_sorted = true;
        while (is_not_sorted) {
            is_not_sorted = ((Boolean)(circle_sort_util(((int[])(collection)), 0, collection.length - 1)));
        }
        return collection;
    }
    public static void main(String[] args) {
        System.out.println(_p(circle_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
        System.out.println(_p(circle_sort(((int[])(new int[]{})))));
        System.out.println(_p(circle_sort(((int[])(new int[]{-2, 5, 0, -45})))));
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
