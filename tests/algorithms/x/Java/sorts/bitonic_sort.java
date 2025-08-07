public class Main {

    static int[] set_at_int(int[] xs, int idx, int value) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length) {
            if (i == idx) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(value)).toArray()));
            } else {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            }
            i = i + 1;
        }
        return res;
    }

    static int[] comp_and_swap(int[] arr, int i, int j, int dir) {
        int[] res_1 = ((int[])(arr));
        int xi = arr[i];
        int xj = arr[j];
        if ((dir == 1 && xi > xj) || (dir == 0 && xi < xj)) {
            res_1 = ((int[])(set_at_int(((int[])(res_1)), i, xj)));
            res_1 = ((int[])(set_at_int(((int[])(res_1)), j, xi)));
        }
        return res_1;
    }

    static int[] bitonic_merge(int[] arr, int low, int length, int dir) {
        int[] res_2 = ((int[])(arr));
        if (length > 1) {
            int mid = Math.floorDiv(length, 2);
            int k = low;
            while (k < low + mid) {
                res_2 = ((int[])(comp_and_swap(((int[])(res_2)), k, k + mid, dir)));
                k = k + 1;
            }
            res_2 = ((int[])(bitonic_merge(((int[])(res_2)), low, mid, dir)));
            res_2 = ((int[])(bitonic_merge(((int[])(res_2)), low + mid, mid, dir)));
        }
        return res_2;
    }

    static int[] bitonic_sort(int[] arr, int low, int length, int dir) {
        int[] res_3 = ((int[])(arr));
        if (length > 1) {
            int mid_1 = Math.floorDiv(length, 2);
            res_3 = ((int[])(bitonic_sort(((int[])(res_3)), low, mid_1, 1)));
            res_3 = ((int[])(bitonic_sort(((int[])(res_3)), low + mid_1, mid_1, 0)));
            res_3 = ((int[])(bitonic_merge(((int[])(res_3)), low, length, dir)));
        }
        return res_3;
    }

    static void main() {
        int[] data = ((int[])(new int[]{12, 34, 92, -23, 0, -121, -167, 145}));
        int[] asc = ((int[])(bitonic_sort(((int[])(data)), 0, data.length, 1)));
        System.out.println(_p(asc));
        int[] desc = ((int[])(bitonic_merge(((int[])(asc)), 0, asc.length, 0)));
        System.out.println(_p(desc));
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
