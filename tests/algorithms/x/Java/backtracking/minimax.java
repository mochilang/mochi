public class Main {

    static int minimax(int depth, int node_index, boolean is_max, int[] scores, int height) {
        if (depth < 0) {
            throw new RuntimeException(String.valueOf("Depth cannot be less than 0"));
        }
        if (scores.length == 0) {
            throw new RuntimeException(String.valueOf("Scores cannot be empty"));
        }
        if (depth == height) {
            return scores[node_index];
        }
        if (((Boolean)(is_max))) {
            int left = minimax(depth + 1, node_index * 2, false, ((int[])(scores)), height);
            int right = minimax(depth + 1, node_index * 2 + 1, false, ((int[])(scores)), height);
            if (left > right) {
                return left;
            } else {
                return right;
            }
        }
        int left_1 = minimax(depth + 1, node_index * 2, true, ((int[])(scores)), height);
        int right_1 = minimax(depth + 1, node_index * 2 + 1, true, ((int[])(scores)), height);
        if (left_1 < right_1) {
            return left_1;
        } else {
            return right_1;
        }
    }

    static int tree_height(int n) {
        int h = 0;
        int v = n;
        while (v > 1) {
            v = v / 2;
            h = h + 1;
        }
        return h;
    }

    static void main() {
        int[] scores = ((int[])(new int[]{90, 23, 6, 33, 21, 65, 123, 34423}));
        int height = tree_height(scores.length);
        System.out.println("Optimal value : " + _p(minimax(0, 0, true, ((int[])(scores)), height)));
    }
    public static void main(String[] args) {
        main();
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
