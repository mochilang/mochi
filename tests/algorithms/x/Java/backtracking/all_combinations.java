public class Main {

    static int[][] create_all_state(int increment, int total, int level, int[] current, int[][] result) {
        if (level == 0) {
            return appendObj(result, current);
        }
        int i = increment;
        while (i <= total - level + 1) {
            int[] next_current = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(current), java.util.stream.IntStream.of(i)).toArray()));
            result = ((int[][])(create_all_state(i + 1, total, level - 1, ((int[])(next_current)), ((int[][])(result)))));
            i = i + 1;
        }
        return result;
    }

    static int[][] generate_all_combinations(int n, int k) {
        if (k < 0 || n < 0) {
            return new int[][]{};
        }
        int[][] result = ((int[][])(new int[][]{}));
        return create_all_state(1, n, k, ((int[])(new int[]{})), ((int[][])(result)));
    }
    public static void main(String[] args) {
        System.out.println(_p(generate_all_combinations(4, 2)));
        System.out.println(_p(generate_all_combinations(3, 1)));
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
