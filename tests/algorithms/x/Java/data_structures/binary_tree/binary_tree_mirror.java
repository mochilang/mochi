public class Main {

    static void binary_tree_mirror_dict(java.util.Map<Integer,int[]> tree, int root) {
        if ((root == 0) || (!(Boolean)(tree.containsKey(root)))) {
            return;
        }
        int[] children = (int[])(((int[])(tree).get(root)));
        int left = children[0];
        int right = children[1];
tree.put(root, ((int[])(new int[]{right, left})));
        binary_tree_mirror_dict(tree, left);
        binary_tree_mirror_dict(tree, right);
    }

    static java.util.Map<Integer,int[]> binary_tree_mirror(java.util.Map<Integer,int[]> binary_tree, int root) {
        if (binary_tree.size() == 0) {
            throw new RuntimeException(String.valueOf("binary tree cannot be empty"));
        }
        if (!(Boolean)(binary_tree.containsKey(root))) {
            throw new RuntimeException(String.valueOf("root " + _p(root) + " is not present in the binary_tree"));
        }
        java.util.Map<Integer,int[]> tree_copy = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
        for (int k : binary_tree.keySet()) {
tree_copy.put(k, (int[])(((int[])(binary_tree).get(k))));
        }
        binary_tree_mirror_dict(tree_copy, root);
        return tree_copy;
    }

    static void main() {
        java.util.Map<Integer,int[]> binary_tree = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(1, ((int[])(new int[]{2, 3}))), java.util.Map.entry(2, ((int[])(new int[]{4, 5}))), java.util.Map.entry(3, ((int[])(new int[]{6, 7}))), java.util.Map.entry(7, ((int[])(new int[]{8, 9})))))));
        System.out.println("Binary tree: " + _p(binary_tree));
        java.util.Map<Integer,int[]> mirrored = binary_tree_mirror(binary_tree, 1);
        System.out.println("Binary tree mirror: " + _p(mirrored));
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
