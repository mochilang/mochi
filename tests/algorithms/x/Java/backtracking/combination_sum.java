public class Main {

    static int[][] backtrack(int[] candidates, int start, int target, int[] path, int[][] result) {
        if (target == 0) {
            return appendObj(result, path);
        }
        int i = start;
        while (i < candidates.length) {
            int value = candidates[i];
            if (value <= target) {
                int[] new_path = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(path), java.util.stream.IntStream.of(value)).toArray()));
                result = ((int[][])(backtrack(((int[])(candidates)), i, target - value, ((int[])(new_path)), ((int[][])(result)))));
            }
            i = i + 1;
        }
        return result;
    }

    static int[][] combination_sum(int[] candidates, int target) {
        int[] path = ((int[])(new int[]{}));
        int[][] result = ((int[][])(new int[][]{}));
        return backtrack(((int[])(candidates)), 0, target, ((int[])(path)), ((int[][])(result)));
    }
    public static void main(String[] args) {
        System.out.println(_p(combination_sum(((int[])(new int[]{2, 3, 5})), 8)));
        System.out.println(_p(combination_sum(((int[])(new int[]{2, 3, 6, 7})), 7)));
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
