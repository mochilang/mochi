public class Main {

    static int[] binary_search_insertion_from(int[] sorted_list, int item, int start) {
        int left = start;
        int right = sorted_list.length - 1;
        while (left <= right) {
            int middle = Math.floorDiv((left + right), 2);
            if (left == right) {
                if (sorted_list[middle] < item) {
                    left = middle + 1;
                }
                break;
            } else             if (sorted_list[middle] < item) {
                left = middle + 1;
            } else {
                right = middle - 1;
            }
        }
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        while (i < left) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(sorted_list[i])).toArray()));
            i = i + 1;
        }
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(item)).toArray()));
        while (i < sorted_list.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(sorted_list[i])).toArray()));
            i = i + 1;
        }
        return result;
    }

    static int[] binary_search_insertion(int[] sorted_list, int item) {
        return binary_search_insertion_from(((int[])(sorted_list)), item, 0);
    }

    static int[][] merge(int[][] left, int[][] right) {
        int[][] result_1 = ((int[][])(new int[][]{}));
        int i_1 = 0;
        int j = 0;
        while (i_1 < left.length && j < right.length) {
            if (left[i_1][0] < right[j][0]) {
                result_1 = ((int[][])(appendObj(result_1, left[i_1])));
                i_1 = i_1 + 1;
            } else {
                result_1 = ((int[][])(appendObj(result_1, right[j])));
                j = j + 1;
            }
        }
        while (i_1 < left.length) {
            result_1 = ((int[][])(appendObj(result_1, left[i_1])));
            i_1 = i_1 + 1;
        }
        while (j < right.length) {
            result_1 = ((int[][])(appendObj(result_1, right[j])));
            j = j + 1;
        }
        return result_1;
    }

    static int[][] sortlist_2d(int[][] list_2d) {
        int length = list_2d.length;
        if (length <= 1) {
            return list_2d;
        }
        int middle_1 = Math.floorDiv(length, 2);
        int[][] left_1 = ((int[][])(new int[][]{}));
        int i_2 = 0;
        while (i_2 < middle_1) {
            left_1 = ((int[][])(appendObj(left_1, list_2d[i_2])));
            i_2 = i_2 + 1;
        }
        int[][] right_1 = ((int[][])(new int[][]{}));
        int j_1 = middle_1;
        while (j_1 < length) {
            right_1 = ((int[][])(appendObj(right_1, list_2d[j_1])));
            j_1 = j_1 + 1;
        }
        return merge(((int[][])(sortlist_2d(((int[][])(left_1))))), ((int[][])(sortlist_2d(((int[][])(right_1))))));
    }

    static int[] merge_insertion_sort(int[] collection) {
        if (collection.length <= 1) {
            return collection;
        }
        int[][] two_paired_list = ((int[][])(new int[][]{}));
        boolean has_last_odd_item = false;
        int i_3 = 0;
        while (i_3 < collection.length) {
            if (i_3 == collection.length - 1) {
                has_last_odd_item = true;
            } else {
                int a = collection[i_3];
                int b = collection[i_3 + 1];
                if (a < b) {
                    two_paired_list = ((int[][])(appendObj(two_paired_list, new int[]{a, b})));
                } else {
                    two_paired_list = ((int[][])(appendObj(two_paired_list, new int[]{b, a})));
                }
            }
            i_3 = i_3 + 2;
        }
        int[][] sorted_list_2d = ((int[][])(sortlist_2d(((int[][])(two_paired_list)))));
        int[] result_2 = ((int[])(new int[]{}));
        i_3 = 0;
        while (i_3 < sorted_list_2d.length) {
            result_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_2), java.util.stream.IntStream.of(sorted_list_2d[i_3][0])).toArray()));
            i_3 = i_3 + 1;
        }
        result_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_2), java.util.stream.IntStream.of(sorted_list_2d[sorted_list_2d.length - 1][1])).toArray()));
        if (has_last_odd_item) {
            result_2 = ((int[])(binary_search_insertion(((int[])(result_2)), collection[collection.length - 1])));
        }
        boolean inserted_before = false;
        int idx = 0;
        while (idx < sorted_list_2d.length - 1) {
            if (has_last_odd_item && result_2[idx] == collection[collection.length - 1]) {
                inserted_before = true;
            }
            int pivot = sorted_list_2d[idx][1];
            if (inserted_before) {
                result_2 = ((int[])(binary_search_insertion_from(((int[])(result_2)), pivot, idx + 2)));
            } else {
                result_2 = ((int[])(binary_search_insertion_from(((int[])(result_2)), pivot, idx + 1)));
            }
            idx = idx + 1;
        }
        return result_2;
    }

    static void main() {
        int[] example1 = ((int[])(new int[]{0, 5, 3, 2, 2}));
        int[] example2 = ((int[])(new int[]{99}));
        int[] example3 = ((int[])(new int[]{-2, -5, -45}));
        System.out.println(_p(merge_insertion_sort(((int[])(example1)))));
        System.out.println(_p(merge_insertion_sort(((int[])(example2)))));
        System.out.println(_p(merge_insertion_sort(((int[])(example3)))));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
