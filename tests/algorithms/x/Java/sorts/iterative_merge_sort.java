public class Main {

    static int[] merge(int[] a, int low, int mid, int high) {
        int[] left = ((int[])(java.util.Arrays.copyOfRange(a, low, mid)));
        int[] right = ((int[])(java.util.Arrays.copyOfRange(a, mid, high + 1)));
        int[] result = ((int[])(new int[]{}));
        while (left.length > 0 && right.length > 0) {
            if (left[0] <= right[0]) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(left[0])).toArray()));
                left = ((int[])(java.util.Arrays.copyOfRange(left, 1, left.length)));
            } else {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(right[0])).toArray()));
                right = ((int[])(java.util.Arrays.copyOfRange(right, 1, right.length)));
            }
        }
        int i = 0;
        while (i < left.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(left[i])).toArray()));
            i = i + 1;
        }
        i = 0;
        while (i < right.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(right[i])).toArray()));
            i = i + 1;
        }
        i = 0;
        while (i < result.length) {
a[low + i] = result[i];
            i = i + 1;
        }
        return a;
    }

    static int[] iter_merge_sort(int[] items) {
        int n = items.length;
        if (n <= 1) {
            return items;
        }
        int[] arr = ((int[])(java.util.Arrays.copyOfRange(items, 0, items.length)));
        int p = 2;
        while (p <= n) {
            int i_1 = 0;
            while (i_1 < n) {
                int high = i_1 + p - 1;
                if (high >= n) {
                    high = n - 1;
                }
                int low = i_1;
                int mid = Math.floorDiv((low + high + 1), 2);
                arr = ((int[])(merge(((int[])(arr)), low, mid, high)));
                i_1 = i_1 + p;
            }
            if (p * 2 >= n) {
                int mid2 = i_1 - p;
                arr = ((int[])(merge(((int[])(arr)), 0, mid2, n - 1)));
                break;
            }
            p = p * 2;
        }
        return arr;
    }

    static String list_to_string(int[] arr) {
        String s = "[";
        int i_2 = 0;
        while (i_2 < arr.length) {
            s = s + _p(_geti(arr, i_2));
            if (i_2 < arr.length - 1) {
                s = s + ", ";
            }
            i_2 = i_2 + 1;
        }
        return s + "]";
    }
    public static void main(String[] args) {
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{5, 9, 8, 7, 1, 2, 7})))))));
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{1})))))));
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{2, 1})))))));
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{4, 3, 2, 1})))))));
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{5, 4, 3, 2, 1})))))));
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{-2, -9, -1, -4})))))));
        System.out.println(list_to_string(((int[])(iter_merge_sort(((int[])(new int[]{})))))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
