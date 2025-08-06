public class Main {

    static int[] tail(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i = 1;
        while (i < xs.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static int[] rotate_left(int[] xs) {
        if (xs.length == 0) {
            return xs;
        }
        int[] res_1 = ((int[])(new int[]{}));
        int i_1 = 1;
        while (i_1 < xs.length) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(xs[0])).toArray()));
        return res_1;
    }

    static int[][] permute_recursive(int[] nums) {
        if (nums.length == 0) {
            int[][] base = ((int[][])(new int[][]{}));
            return appendObj(base, new int[]{});
        }
        int[][] result = ((int[][])(new int[][]{}));
        int[] current = ((int[])(nums));
        int count = 0;
        while (count < nums.length) {
            int n = current[0];
            int[] rest = ((int[])(tail(((int[])(current)))));
            int[][] perms = ((int[][])(permute_recursive(((int[])(rest)))));
            int j = 0;
            while (j < perms.length) {
                int[] perm = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(perms[j]), java.util.stream.IntStream.of(n)).toArray()));
                result = ((int[][])(appendObj(result, perm)));
                j = j + 1;
            }
            current = ((int[])(rotate_left(((int[])(current)))));
            count = count + 1;
        }
        return result;
    }

    static int[] swap(int[] xs, int i, int j) {
        int[] res_2 = ((int[])(new int[]{}));
        int k = 0;
        while (k < xs.length) {
            if (k == i) {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(xs[j])).toArray()));
            } else             if (k == j) {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(xs[i])).toArray()));
            } else {
                res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(xs[k])).toArray()));
            }
            k = k + 1;
        }
        return res_2;
    }

    static int[][] permute_backtrack_helper(int[] nums, int start, int[][] output) {
        if (start == nums.length - 1) {
            return appendObj(output, nums);
        }
        int i_2 = start;
        int[][] res_3 = ((int[][])(output));
        while (i_2 < nums.length) {
            int[] swapped = ((int[])(swap(((int[])(nums)), start, i_2)));
            res_3 = ((int[][])(permute_backtrack_helper(((int[])(swapped)), start + 1, ((int[][])(res_3)))));
            i_2 = i_2 + 1;
        }
        return res_3;
    }

    static int[][] permute_backtrack(int[] nums) {
        int[][] output = ((int[][])(new int[][]{}));
        return permute_backtrack_helper(((int[])(nums)), 0, ((int[][])(output)));
    }
    public static void main(String[] args) {
        System.out.println(_p(permute_recursive(((int[])(new int[]{1, 2, 3})))));
        System.out.println(_p(permute_backtrack(((int[])(new int[]{1, 2, 3})))));
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
