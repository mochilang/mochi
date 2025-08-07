public class Main {

    static int[] set_at_int(int[] xs, int idx, int value) {
        int i = 0;
        int[] res = ((int[])(new int[]{}));
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

    static int[] sort_int(int[] xs) {
        int[] res_1 = ((int[])(xs));
        int i_1 = 1;
        while (i_1 < res_1.length) {
            int key = res_1[i_1];
            int j = i_1 - 1;
            while (j >= 0 && res_1[j] > key) {
                res_1 = ((int[])(set_at_int(((int[])(res_1)), j + 1, res_1[j])));
                j = j - 1;
            }
            res_1 = ((int[])(set_at_int(((int[])(res_1)), j + 1, key)));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static int median_of_five(int[] arr) {
        int[] sorted = ((int[])(sort_int(((int[])(arr)))));
        return sorted[Math.floorDiv(sorted.length, 2)];
    }

    static int median_of_medians(int[] arr) {
        if (arr.length <= 5) {
            return median_of_five(((int[])(arr)));
        }
        int[] medians = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < arr.length) {
            if (i_2 + 5 <= arr.length) {
                medians = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(medians), java.util.stream.IntStream.of(median_of_five(((int[])(java.util.Arrays.copyOfRange(arr, i_2, i_2 + 5)))))).toArray()));
            } else {
                medians = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(medians), java.util.stream.IntStream.of(median_of_five(((int[])(java.util.Arrays.copyOfRange(arr, i_2, arr.length)))))).toArray()));
            }
            i_2 = i_2 + 5;
        }
        return median_of_medians(((int[])(medians)));
    }

    static int quick_select(int[] arr, int target) {
        if (target > arr.length) {
            return -1;
        }
        int x = median_of_medians(((int[])(arr)));
        int[] left = ((int[])(new int[]{}));
        int[] right = ((int[])(new int[]{}));
        boolean check = false;
        int i_3 = 0;
        while (i_3 < arr.length) {
            if (arr[i_3] < x) {
                left = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left), java.util.stream.IntStream.of(arr[i_3])).toArray()));
            } else             if (arr[i_3] > x) {
                right = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right), java.util.stream.IntStream.of(arr[i_3])).toArray()));
            } else             if (arr[i_3] == x) {
                if (!check) {
                    check = true;
                } else {
                    right = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right), java.util.stream.IntStream.of(arr[i_3])).toArray()));
                }
            } else {
                right = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right), java.util.stream.IntStream.of(arr[i_3])).toArray()));
            }
            i_3 = i_3 + 1;
        }
        int rank_x = left.length + 1;
        int answer = 0;
        if (rank_x == target) {
            answer = x;
        } else         if (rank_x > target) {
            answer = quick_select(((int[])(left)), target);
        } else {
            answer = quick_select(((int[])(right)), target - rank_x);
        }
        return answer;
    }

    static void main() {
        System.out.println(_p(median_of_five(((int[])(new int[]{5, 4, 3, 2})))));
        System.out.println(_p(quick_select(((int[])(new int[]{2, 4, 5, 7, 899, 54, 32})), 5)));
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
