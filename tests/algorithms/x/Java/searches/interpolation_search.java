public class Main {

    static int interpolation_search(int[] arr, int item) {
        int left = 0;
        int right = arr.length - 1;
        while (left <= right) {
            if (arr[left] == arr[right]) {
                if (arr[left] == item) {
                    return left;
                }
                return -1;
            }
            int point = left + Math.floorDiv(((item - arr[left]) * (right - left)), (arr[right] - arr[left]));
            if (point < 0 || point >= arr.length) {
                return -1;
            }
            int current = arr[point];
            if (current == item) {
                return point;
            }
            if (point < left) {
                right = left;
                left = point;
            } else             if (point > right) {
                left = right;
                right = point;
            } else             if (item < current) {
                right = point - 1;
            } else {
                left = point + 1;
            }
        }
        return -1;
    }

    static int interpolation_search_recursive(int[] arr, int item, int left, int right) {
        if (left > right) {
            return -1;
        }
        if (arr[left] == arr[right]) {
            if (arr[left] == item) {
                return left;
            }
            return -1;
        }
        int point_1 = left + Math.floorDiv(((item - arr[left]) * (right - left)), (arr[right] - arr[left]));
        if (point_1 < 0 || point_1 >= arr.length) {
            return -1;
        }
        if (arr[point_1] == item) {
            return point_1;
        }
        if (point_1 < left) {
            return interpolation_search_recursive(((int[])(arr)), item, point_1, left);
        }
        if (point_1 > right) {
            return interpolation_search_recursive(((int[])(arr)), item, right, left);
        }
        if (arr[point_1] > item) {
            return interpolation_search_recursive(((int[])(arr)), item, left, point_1 - 1);
        }
        return interpolation_search_recursive(((int[])(arr)), item, point_1 + 1, right);
    }

    static int interpolation_search_by_recursion(int[] arr, int item) {
        return interpolation_search_recursive(((int[])(arr)), item, 0, arr.length - 1);
    }
    public static void main(String[] args) {
        System.out.println(_p(interpolation_search(((int[])(new int[]{1, 2, 3, 4, 5})), 2)));
        System.out.println(_p(interpolation_search(((int[])(new int[]{1, 2, 3, 4, 5})), 6)));
        System.out.println(_p(interpolation_search_by_recursion(((int[])(new int[]{0, 5, 7, 10, 15})), 5)));
        System.out.println(_p(interpolation_search_by_recursion(((int[])(new int[]{0, 5, 7, 10, 15})), 100)));
        System.out.println(_p(interpolation_search_by_recursion(((int[])(new int[]{5, 5, 5, 5, 5})), 3)));
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
