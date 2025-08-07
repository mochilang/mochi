public class Main {
    static int[] array1 = new int[0];
    static int[] array2 = new int[0];
    static int[] array3 = new int[0];
    static int[] nums1 = new int[0];
    static int[] nums2 = new int[0];
    static int[] nums3 = new int[0];

    static int[] quick_sort_3partition(int[] arr, int left, int right) {
        if (right <= left) {
            return arr;
        }
        int a = left;
        int i = left;
        int b = right;
        int pivot = arr[left];
        while (i <= b) {
            if (arr[i] < pivot) {
                int temp = arr[a];
arr[a] = arr[i];
arr[i] = temp;
                a = a + 1;
                i = i + 1;
            } else             if (arr[i] > pivot) {
                int temp_1 = arr[b];
arr[b] = arr[i];
arr[i] = temp_1;
                b = b - 1;
            } else {
                i = i + 1;
            }
        }
        arr = ((int[])(quick_sort_3partition(((int[])(arr)), left, a - 1)));
        arr = ((int[])(quick_sort_3partition(((int[])(arr)), b + 1, right)));
        return arr;
    }

    static int[] quick_sort_lomuto_partition(int[] arr, int left, int right) {
        if (left < right) {
            int pivot_index = lomuto_partition(((int[])(arr)), left, right);
            arr = ((int[])(quick_sort_lomuto_partition(((int[])(arr)), left, pivot_index - 1)));
            arr = ((int[])(quick_sort_lomuto_partition(((int[])(arr)), pivot_index + 1, right)));
        }
        return arr;
    }

    static int lomuto_partition(int[] arr, int left, int right) {
        int pivot_1 = arr[right];
        int store_index = left;
        int i_1 = left;
        while (i_1 < right) {
            if (arr[i_1] < pivot_1) {
                int temp_2 = arr[store_index];
arr[store_index] = arr[i_1];
arr[i_1] = temp_2;
                store_index = store_index + 1;
            }
            i_1 = i_1 + 1;
        }
        int temp_3 = arr[right];
arr[right] = arr[store_index];
arr[store_index] = temp_3;
        return store_index;
    }

    static int[] three_way_radix_quicksort(int[] arr) {
        if (arr.length <= 1) {
            return arr;
        }
        int pivot_2 = arr[0];
        int[] less = ((int[])(new int[]{}));
        int[] equal = ((int[])(new int[]{}));
        int[] greater = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < arr.length) {
            int val = arr[i_2];
            if (val < pivot_2) {
                less = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(less), java.util.stream.IntStream.of(val)).toArray()));
            } else             if (val > pivot_2) {
                greater = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(greater), java.util.stream.IntStream.of(val)).toArray()));
            } else {
                equal = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(equal), java.util.stream.IntStream.of(val)).toArray()));
            }
            i_2 = i_2 + 1;
        }
        int[] sorted_less = ((int[])(three_way_radix_quicksort(((int[])(less)))));
        int[] sorted_greater = ((int[])(three_way_radix_quicksort(((int[])(greater)))));
        int[] result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(sorted_less), java.util.Arrays.stream(equal)).toArray()));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.Arrays.stream(sorted_greater)).toArray()));
        return result;
    }
    public static void main(String[] args) {
        array1 = ((int[])(new int[]{5, -1, -1, 5, 5, 24, 0}));
        array1 = ((int[])(quick_sort_3partition(((int[])(array1)), 0, array1.length - 1)));
        System.out.println(_p(array1));
        array2 = ((int[])(new int[]{9, 0, 2, 6}));
        array2 = ((int[])(quick_sort_3partition(((int[])(array2)), 0, array2.length - 1)));
        System.out.println(_p(array2));
        array3 = ((int[])(new int[]{}));
        array3 = ((int[])(quick_sort_3partition(((int[])(array3)), 0, array3.length - 1)));
        System.out.println(_p(array3));
        nums1 = ((int[])(new int[]{0, 5, 3, 1, 2}));
        nums1 = ((int[])(quick_sort_lomuto_partition(((int[])(nums1)), 0, nums1.length - 1)));
        System.out.println(_p(nums1));
        nums2 = ((int[])(new int[]{}));
        nums2 = ((int[])(quick_sort_lomuto_partition(((int[])(nums2)), 0, nums2.length - 1)));
        System.out.println(_p(nums2));
        nums3 = ((int[])(new int[]{-2, 5, 0, -4}));
        nums3 = ((int[])(quick_sort_lomuto_partition(((int[])(nums3)), 0, nums3.length - 1)));
        System.out.println(_p(nums3));
        System.out.println(_p(three_way_radix_quicksort(((int[])(new int[]{})))));
        System.out.println(_p(three_way_radix_quicksort(((int[])(new int[]{1})))));
        System.out.println(_p(three_way_radix_quicksort(((int[])(new int[]{-5, -2, 1, -2, 0, 1})))));
        System.out.println(_p(three_way_radix_quicksort(((int[])(new int[]{1, 2, 5, 1, 2, 0, 0, 5, 2, -1})))));
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
