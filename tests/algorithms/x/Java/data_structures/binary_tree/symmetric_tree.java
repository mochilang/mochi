public class Main {
    static int[][] symmetric_tree;
    static int[][] asymmetric_tree;

    static int[][] make_symmetric_tree() {
        return new int[][]{new int[]{1, 1, 2}, new int[]{2, 3, 4}, new int[]{2, 5, 6}, new int[]{3, -1, -1}, new int[]{4, -1, -1}, new int[]{4, -1, -1}, new int[]{3, -1, -1}};
    }

    static int[][] make_asymmetric_tree() {
        return new int[][]{new int[]{1, 1, 2}, new int[]{2, 3, 4}, new int[]{2, 5, 6}, new int[]{3, -1, -1}, new int[]{4, -1, -1}, new int[]{3, -1, -1}, new int[]{4, -1, -1}};
    }

    static boolean is_symmetric_tree(int[][] tree) {
        int[] stack = ((int[])(new int[]{tree[0][1], tree[0][2]}));
        while (stack.length >= 2) {
            int left = stack[stack.length - 2];
            int right = stack[stack.length - 1];
            stack = ((int[])(java.util.Arrays.copyOfRange(stack, 0, stack.length - 2)));
            if (left == (-1) && right == (-1)) {
                continue;
            }
            if (left == (-1) || right == (-1)) {
                return false;
            }
            int[] lnode = ((int[])(tree[left]));
            int[] rnode = ((int[])(tree[right]));
            if (lnode[0] != rnode[0]) {
                return false;
            }
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(lnode[1])).toArray()));
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(rnode[2])).toArray()));
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(lnode[2])).toArray()));
            stack = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(stack), java.util.stream.IntStream.of(rnode[1])).toArray()));
        }
        return true;
    }
    public static void main(String[] args) {
        symmetric_tree = ((int[][])(make_symmetric_tree()));
        asymmetric_tree = ((int[][])(make_asymmetric_tree()));
        System.out.println(_p(is_symmetric_tree(((int[][])(symmetric_tree)))));
        System.out.println(_p(is_symmetric_tree(((int[][])(asymmetric_tree)))));
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
