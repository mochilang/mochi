public class Main {

    static int[] bubble_sort_iterative(int[] collection) {
        int n = collection.length;
        while (n > 0) {
            boolean swapped = false;
            int j = 0;
            while (j < n - 1) {
                if (collection[j] > collection[j + 1]) {
                    int temp = collection[j];
collection[j] = collection[j + 1];
collection[j + 1] = temp;
                    swapped = true;
                }
                j = j + 1;
            }
            if (!swapped) {
                break;
            }
            n = n - 1;
        }
        return collection;
    }

    static int[] bubble_sort_recursive(int[] collection) {
        int n_1 = collection.length;
        boolean swapped_1 = false;
        int i = 0;
        while (i < n_1 - 1) {
            if (collection[i] > collection[i + 1]) {
                int temp_1 = collection[i];
collection[i] = collection[i + 1];
collection[i + 1] = temp_1;
                swapped_1 = true;
            }
            i = i + 1;
        }
        if (swapped_1) {
            return bubble_sort_recursive(((int[])(collection)));
        }
        return collection;
    }

    static int[] copy_list(int[] xs) {
        int[] out = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length) {
            out = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(out), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return out;
    }

    static boolean list_eq(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int k = 0;
        while (k < a.length) {
            if (a[k] != b[k]) {
                return false;
            }
            k = k + 1;
        }
        return true;
    }

    static void test_bubble_sort() {
        int[] example = ((int[])(new int[]{0, 5, 2, 3, 2}));
        int[] expected = ((int[])(new int[]{0, 2, 2, 3, 5}));
        if (!(Boolean)list_eq(((int[])(bubble_sort_iterative(((int[])(copy_list(((int[])(example)))))))), ((int[])(expected)))) {
            throw new RuntimeException(String.valueOf("iterative failed"));
        }
        if (!(Boolean)list_eq(((int[])(bubble_sort_recursive(((int[])(copy_list(((int[])(example)))))))), ((int[])(expected)))) {
            throw new RuntimeException(String.valueOf("recursive failed"));
        }
        int[] empty = ((int[])(new int[]{}));
        if (bubble_sort_iterative(((int[])(copy_list(((int[])(empty)))))).length != 0) {
            throw new RuntimeException(String.valueOf("empty iterative failed"));
        }
        if (bubble_sort_recursive(((int[])(copy_list(((int[])(empty)))))).length != 0) {
            throw new RuntimeException(String.valueOf("empty recursive failed"));
        }
    }

    static void main() {
        test_bubble_sort();
        int[] arr = ((int[])(new int[]{5, 1, 4, 2, 8}));
        System.out.println(_p(bubble_sort_iterative(((int[])(copy_list(((int[])(arr))))))));
        System.out.println(_p(bubble_sort_recursive(((int[])(copy_list(((int[])(arr))))))));
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
