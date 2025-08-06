public class Main {
    static int NIL;
    static int[] node_values = new int[0];
    static double[] node_priors = new double[0];
    static int[] node_lefts = new int[0];
    static int[] node_rights = new int[0];
    static int seed = 0;
    static class SplitResult {
        int left;
        int right;
        SplitResult(int left, int right) {
            this.left = left;
            this.right = right;
        }
        SplitResult() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'right': %s}", String.valueOf(left), String.valueOf(right));
        }
    }


    static double random() {
        seed = Math.floorMod((seed * 13 + 7), 100);
        return (((Number)(seed)).doubleValue()) / 100.0;
    }

    static int new_node(int value) {
        node_values = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(node_values), java.util.stream.IntStream.of(value)).toArray()));
        node_priors = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(node_priors), java.util.stream.DoubleStream.of(random())).toArray()));
        node_lefts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(node_lefts), java.util.stream.IntStream.of(NIL)).toArray()));
        node_rights = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(node_rights), java.util.stream.IntStream.of(NIL)).toArray()));
        return node_values.length - 1;
    }

    static SplitResult split(int root, int value) {
        if (root == NIL) {
            return new SplitResult(NIL, NIL);
        }
        if (value < node_values[root]) {
            SplitResult res = node_lefts[root].split(java.util.regex.Pattern.quote(value));
node_lefts[root] = res.right;
            return new SplitResult(res.left, root);
        }
        SplitResult res_1 = node_rights[root].split(java.util.regex.Pattern.quote(value));
node_rights[root] = res_1.left;
        return new SplitResult(root, res_1.right);
    }

    static int merge(int left, int right) {
        if (left == NIL) {
            return right;
        }
        if (right == NIL) {
            return left;
        }
        if (node_priors[left] < node_priors[right]) {
node_rights[left] = merge(node_rights[left], right);
            return left;
        }
node_lefts[right] = merge(left, node_lefts[right]);
        return right;
    }

    static int insert(int root, int value) {
        int node = new_node(value);
        SplitResult res_2 = root.split(java.util.regex.Pattern.quote(value));
        return merge(merge(res_2.left, node), res_2.right);
    }

    static int erase(int root, int value) {
        SplitResult res1 = root.split(java.util.regex.Pattern.quote(value - 1));
        SplitResult res2 = res1.right.split(java.util.regex.Pattern.quote(value));
        return merge(res1.left, res2.right);
    }

    static int[] inorder(int i, int[] acc) {
        if (i == NIL) {
            return acc;
        }
        int[] left_acc = ((int[])(inorder(node_lefts[i], ((int[])(acc)))));
        int[] with_node = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left_acc), java.util.stream.IntStream.of(node_values[i])).toArray()));
        return inorder(node_rights[i], ((int[])(with_node)));
    }

    static void main() {
        int root = NIL;
        root = insert(root, 1);
        System.out.println(_p(inorder(root, ((int[])(new int[]{})))));
        root = insert(root, 3);
        root = insert(root, 5);
        root = insert(root, 17);
        root = insert(root, 19);
        root = insert(root, 2);
        root = insert(root, 16);
        root = insert(root, 4);
        root = insert(root, 0);
        System.out.println(_p(inorder(root, ((int[])(new int[]{})))));
        root = insert(root, 4);
        root = insert(root, 4);
        root = insert(root, 4);
        System.out.println(_p(inorder(root, ((int[])(new int[]{})))));
        root = erase(root, 0);
        System.out.println(_p(inorder(root, ((int[])(new int[]{})))));
        root = erase(root, 4);
        System.out.println(_p(inorder(root, ((int[])(new int[]{})))));
    }
    public static void main(String[] args) {
        NIL = 0 - 1;
        node_values = ((int[])(new int[]{}));
        node_priors = ((double[])(new double[]{}));
        node_lefts = ((int[])(new int[]{}));
        node_rights = ((int[])(new int[]{}));
        seed = 1;
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
