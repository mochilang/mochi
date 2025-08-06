public class Main {
    static int NONE;
    static class Tree {
        double[] data;
        int[] left;
        int[] right;
        Tree(double[] data, int[] left, int[] right) {
            this.data = data;
            this.left = left;
            this.right = right;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'left': %s, 'right': %s}", String.valueOf(data), String.valueOf(left), String.valueOf(right));
        }
    }

    static Tree tree1;
    static Tree tree2;
    static Tree tree3;

    static double[] inorder(Tree tree, int index) {
        double[] res = ((double[])(new double[]{}));
        if (index == NONE) {
            return res;
        }
        int left_idx = tree.left[index];
        if (left_idx != NONE) {
            res = ((double[])(concat(res, inorder(tree, left_idx))));
        }
        res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(tree.data[index])).toArray()));
        int right_idx = tree.right[index];
        if (right_idx != NONE) {
            res = ((double[])(concat(res, inorder(tree, right_idx))));
        }
        return res;
    }

    static boolean is_sorted(Tree tree, int index) {
        if (index == NONE) {
            return true;
        }
        int left_idx_1 = tree.left[index];
        if (left_idx_1 != NONE) {
            if (tree.data[index] < tree.data[left_idx_1]) {
                return false;
            }
            if (!(Boolean)is_sorted(tree, left_idx_1)) {
                return false;
            }
        }
        int right_idx_1 = tree.right[index];
        if (right_idx_1 != NONE) {
            if (tree.data[index] > tree.data[right_idx_1]) {
                return false;
            }
            if (!(Boolean)is_sorted(tree, right_idx_1)) {
                return false;
            }
        }
        return true;
    }
    public static void main(String[] args) {
        NONE = 0 - 1;
        tree1 = new Tree(new double[]{2.1, 2.0, 2.2}, new int[]{1, NONE, NONE}, new int[]{2, NONE, NONE});
        System.out.println("Tree " + _p(inorder(tree1, 0)) + " is sorted: " + _p(is_sorted(tree1, 0)));
        tree2 = new Tree(new double[]{2.1, 2.0, 2.0}, new int[]{1, NONE, NONE}, new int[]{2, NONE, NONE});
        System.out.println("Tree " + _p(inorder(tree2, 0)) + " is sorted: " + _p(is_sorted(tree2, 0)));
        tree3 = new Tree(new double[]{2.1, 2.0, 2.1}, new int[]{1, NONE, NONE}, new int[]{2, NONE, NONE});
        System.out.println("Tree " + _p(inorder(tree3, 0)) + " is sorted: " + _p(is_sorted(tree3, 0)));
    }

    static <T> T[] concat(T[] a, T[] b) {
        T[] out = java.util.Arrays.copyOf(a, a.length + b.length);
        System.arraycopy(b, 0, out, a.length, b.length);
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
