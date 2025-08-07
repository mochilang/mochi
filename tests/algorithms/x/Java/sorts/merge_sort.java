public class Main {

    static int[] subarray(int[] xs, int start, int end) {
        int[] result = ((int[])(new int[]{}));
        int i = start;
        while (i < end) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return result;
    }

    static int[] merge(int[] left, int[] right) {
        int[] result_1 = ((int[])(new int[]{}));
        int i_1 = 0;
        int j = 0;
        while (i_1 < left.length && j < right.length) {
            if (left[i_1] <= right[j]) {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(left[i_1])).toArray()));
                i_1 = i_1 + 1;
            } else {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(right[j])).toArray()));
                j = j + 1;
            }
        }
        while (i_1 < left.length) {
            result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(left[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        while (j < right.length) {
            result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(right[j])).toArray()));
            j = j + 1;
        }
        return result_1;
    }

    static int[] merge_sort(int[] collection) {
        if (collection.length <= 1) {
            return collection;
        }
        int mid_index = Math.floorDiv(collection.length, 2);
        int[] left = ((int[])(subarray(((int[])(collection)), 0, mid_index)));
        int[] right = ((int[])(subarray(((int[])(collection)), mid_index, collection.length)));
        int[] sorted_left = ((int[])(merge_sort(((int[])(left)))));
        int[] sorted_right = ((int[])(merge_sort(((int[])(right)))));
        return merge(((int[])(sorted_left)), ((int[])(sorted_right)));
    }
    public static void main(String[] args) {
        System.out.println(_p(merge_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
        System.out.println(_p(merge_sort(((int[])(new int[]{})))));
        System.out.println(_p(merge_sort(((int[])(new int[]{-2, -5, -45})))));
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
