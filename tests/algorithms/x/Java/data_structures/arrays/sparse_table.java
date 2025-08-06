public class Main {
    static int[][] st1;
    static int[][] st2;

    static int pow2(int n) {
        int result = 1;
        int i = 0;
        while (i < n) {
            result = result * 2;
            i = i + 1;
        }
        return result;
    }

    static int int_log2(int n) {
        int v = n;
        int res = 0;
        while (v > 1) {
            v = v / 2;
            res = res + 1;
        }
        return res;
    }

    static int[][] build_sparse_table(int[] number_list) {
        if (number_list.length == 0) {
            throw new RuntimeException(String.valueOf("empty number list not allowed"));
        }
        int length = number_list.length;
        int row = int_log2(length) + 1;
        int[][] sparse_table = ((int[][])(new int[][]{}));
        int j = 0;
        while (j < row) {
            int[] inner = ((int[])(new int[]{}));
            int i_1 = 0;
            while (i_1 < length) {
                inner = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(inner), java.util.stream.IntStream.of(0)).toArray()));
                i_1 = i_1 + 1;
            }
            sparse_table = ((int[][])(appendObj(sparse_table, inner)));
            j = j + 1;
        }
        int i_2 = 0;
        while (i_2 < length) {
sparse_table[0][i_2] = number_list[i_2];
            i_2 = i_2 + 1;
        }
        j = 1;
        while (pow2(j) <= length) {
            i_2 = 0;
            while (i_2 + pow2(j) - 1 < length) {
                int left = sparse_table[j - 1][i_2 + pow2(j - 1)];
                int right = sparse_table[j - 1][i_2];
                if (left < right) {
sparse_table[j][i_2] = left;
                } else {
sparse_table[j][i_2] = right;
                }
                i_2 = i_2 + 1;
            }
            j = j + 1;
        }
        return sparse_table;
    }

    static int query(int[][] sparse_table, int left_bound, int right_bound) {
        if (left_bound < 0 || right_bound >= sparse_table[0].length) {
            throw new RuntimeException(String.valueOf("list index out of range"));
        }
        int interval = right_bound - left_bound + 1;
        int j_1 = int_log2(interval);
        int val1 = sparse_table[j_1][right_bound - pow2(j_1) + 1];
        int val2 = sparse_table[j_1][left_bound];
        if (val1 < val2) {
            return val1;
        }
        return val2;
    }
    public static void main(String[] args) {
        st1 = ((int[][])(build_sparse_table(((int[])(new int[]{8, 1, 0, 3, 4, 9, 3})))));
        System.out.println(_p(st1));
        st2 = ((int[][])(build_sparse_table(((int[])(new int[]{3, 1, 9})))));
        System.out.println(_p(st2));
        System.out.println(_p(query(((int[][])(st1)), 0, 4)));
        System.out.println(_p(query(((int[][])(st1)), 4, 6)));
        System.out.println(_p(query(((int[][])(st2)), 2, 2)));
        System.out.println(_p(query(((int[][])(st2)), 0, 1)));
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
