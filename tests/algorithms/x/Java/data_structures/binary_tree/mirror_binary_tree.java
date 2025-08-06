public class Main {
    static class Tree {
        int[] values;
        int[] left;
        int[] right;
        int root;
        Tree(int[] values, int[] left, int[] right, int root) {
            this.values = values;
            this.left = left;
            this.right = right;
            this.root = root;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'values': %s, 'left': %s, 'right': %s, 'root': %s}", String.valueOf(values), String.valueOf(left), String.valueOf(right), String.valueOf(root));
        }
    }


    static void mirror_node(int[] left, int[] right, int idx) {
        if (idx == (-1)) {
            return;
        }
        int temp = left[idx];
left[idx] = right[idx];
right[idx] = temp;
        mirror_node(((int[])(left)), ((int[])(right)), left[idx]);
        mirror_node(((int[])(left)), ((int[])(right)), right[idx]);
    }

    static Tree mirror(Tree tree) {
        mirror_node(((int[])(tree.left)), ((int[])(tree.right)), tree.root);
        return tree;
    }

    static int[] inorder(Tree tree, int idx) {
        if (idx == (-1)) {
            return new int[]{};
        }
        int[] left_vals = ((int[])(inorder(tree, tree.left[idx])));
        int[] right_vals = ((int[])(inorder(tree, tree.right[idx])));
        return concat(concat(left_vals, new int[]{tree.values[idx]}), right_vals);
    }

    static Tree make_tree_zero() {
        return new Tree(new int[]{0}, new int[]{-1}, new int[]{-1}, 0);
    }

    static Tree make_tree_seven() {
        return new Tree(new int[]{1, 2, 3, 4, 5, 6, 7}, new int[]{1, 3, 5, -1, -1, -1, -1}, new int[]{2, 4, 6, -1, -1, -1, -1}, 0);
    }

    static Tree make_tree_nine() {
        return new Tree(new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9}, new int[]{1, 3, -1, 6, -1, -1, -1, -1, -1}, new int[]{2, 4, 5, 7, 8, -1, -1, -1, -1}, 0);
    }

    static void main() {
        String[] names = ((String[])(new String[]{"zero", "seven", "nine"}));
        Tree[] trees = ((Tree[])(new Tree[]{make_tree_zero(), make_tree_seven(), make_tree_nine()}));
        int i = 0;
        while (i < trees.length) {
            Tree tree = trees[i];
            System.out.println("      The " + names[i] + " tree: " + _p(inorder(tree, tree.root)));
            Tree mirrored = mirror(tree);
            System.out.println("Mirror of " + names[i] + " tree: " + _p(inorder(mirrored, mirrored.root)));
            i = i + 1;
        }
    }
    public static void main(String[] args) {
        main();
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
