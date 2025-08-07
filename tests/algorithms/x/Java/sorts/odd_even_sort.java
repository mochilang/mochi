public class Main {

    static int[] odd_even_sort(int[] xs) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        int n = arr.length;
        boolean sorted = false;
        while (sorted == false) {
            sorted = true;
            int j = 0;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    int tmp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp;
                    sorted = false;
                }
                j = j + 2;
            }
            j = 1;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    int tmp_1 = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = tmp_1;
                    sorted = false;
                }
                j = j + 2;
            }
        }
        return arr;
    }

    static void print_list(int[] xs) {
        int i_1 = 0;
        String out = "";
        while (i_1 < xs.length) {
            if (i_1 > 0) {
                out = out + " ";
            }
            out = out + _p(_geti(xs, i_1));
            i_1 = i_1 + 1;
        }
        System.out.println(out);
    }

    static void test_odd_even_sort() {
        int[] a = ((int[])(new int[]{5, 4, 3, 2, 1}));
        int[] r1 = ((int[])(odd_even_sort(((int[])(a)))));
        if (r1[0] != 1 || r1[1] != 2 || r1[2] != 3 || r1[3] != 4 || r1[4] != 5) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        int[] b = ((int[])(new int[]{}));
        int[] r2 = ((int[])(odd_even_sort(((int[])(b)))));
        if (r2.length != 0) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        int[] c = ((int[])(new int[]{-10, -1, 10, 2}));
        int[] r3 = ((int[])(odd_even_sort(((int[])(c)))));
        if (r3[0] != (-10) || r3[1] != (-1) || r3[2] != 2 || r3[3] != 10) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
        int[] d = ((int[])(new int[]{1, 2, 3, 4}));
        int[] r4 = ((int[])(odd_even_sort(((int[])(d)))));
        if (r4[0] != 1 || r4[1] != 2 || r4[2] != 3 || r4[3] != 4) {
            throw new RuntimeException(String.valueOf("case4 failed"));
        }
    }

    static void main() {
        test_odd_even_sort();
        int[] sample = ((int[])(new int[]{5, 4, 3, 2, 1}));
        int[] sorted_1 = ((int[])(odd_even_sort(((int[])(sample)))));
        print_list(((int[])(sorted_1)));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
