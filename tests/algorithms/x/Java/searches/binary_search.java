public class Main {

    static boolean is_sorted(int[] arr) {
        int i = 1;
        while (i < arr.length) {
            if (arr[i - 1] > arr[i]) {
                return false;
            }
            i = i + 1;
        }
        return true;
    }

    static int binary_search(int[] sorted_collection, int item) {
        if (!(Boolean)is_sorted(((int[])(sorted_collection)))) {
            return -1;
        }
        int left = 0;
        int right = sorted_collection.length - 1;
        while (left <= right) {
            int midpoint = left + Math.floorDiv((right - left), 2);
            int current_item = sorted_collection[midpoint];
            if (current_item == item) {
                return midpoint;
            }
            if (item < current_item) {
                right = midpoint - 1;
            } else {
                left = midpoint + 1;
            }
        }
        return -1;
    }

    static int binary_search_by_recursion(int[] sorted_collection, int item, int left, int right) {
        if (right < left) {
            return -1;
        }
        int midpoint_1 = left + Math.floorDiv((right - left), 2);
        if (sorted_collection[midpoint_1] == item) {
            return midpoint_1;
        }
        if (sorted_collection[midpoint_1] > item) {
            return binary_search_by_recursion(((int[])(sorted_collection)), item, left, midpoint_1 - 1);
        }
        return binary_search_by_recursion(((int[])(sorted_collection)), item, midpoint_1 + 1, right);
    }

    static int exponential_search(int[] sorted_collection, int item) {
        if (!(Boolean)is_sorted(((int[])(sorted_collection)))) {
            return -1;
        }
        if (sorted_collection.length == 0) {
            return -1;
        }
        int bound = 1;
        while (bound < sorted_collection.length && sorted_collection[bound] < item) {
            bound = bound * 2;
        }
        int left_1 = Math.floorDiv(bound, 2);
        int right_1 = _min(new int[]{bound, sorted_collection.length - 1});
        return binary_search_by_recursion(((int[])(sorted_collection)), item, left_1, right_1);
    }

    static void main() {
        int[] data = ((int[])(new int[]{0, 5, 7, 10, 15}));
        System.out.println(_p(binary_search(((int[])(data)), 0)));
        System.out.println(_p(binary_search(((int[])(data)), 15)));
        System.out.println(_p(binary_search(((int[])(data)), 5)));
        System.out.println(_p(binary_search(((int[])(data)), 6)));
        System.out.println(_p(binary_search_by_recursion(((int[])(data)), 0, 0, data.length - 1)));
        System.out.println(_p(binary_search_by_recursion(((int[])(data)), 15, 0, data.length - 1)));
        System.out.println(_p(binary_search_by_recursion(((int[])(data)), 5, 0, data.length - 1)));
        System.out.println(_p(binary_search_by_recursion(((int[])(data)), 6, 0, data.length - 1)));
        System.out.println(_p(exponential_search(((int[])(data)), 0)));
        System.out.println(_p(exponential_search(((int[])(data)), 15)));
        System.out.println(_p(exponential_search(((int[])(data)), 5)));
        System.out.println(_p(exponential_search(((int[])(data)), 6)));
    }
    public static void main(String[] args) {
        main();
    }

    static int _min(int[] a) {
        int m = a[0];
        for (int i = 1; i < a.length; i++) if (a[i] < m) m = a[i];
        return m;
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
