public class Main {
    static int[] node_data = new int[0];
    static int[] left_child = new int[0];
    static int[] right_child = new int[0];
    static int root_1;
    static int[] vals;

    static int new_node(int value) {
        node_data = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(node_data), java.util.stream.IntStream.of(value)).toArray()));
        left_child = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left_child), java.util.stream.IntStream.of(0)).toArray()));
        right_child = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(right_child), java.util.stream.IntStream.of(0)).toArray()));
        return node_data.length - 1;
    }

    static int build_tree() {
        int root = new_node(1);
        int n2 = new_node(2);
        int n5 = new_node(5);
        int n3 = new_node(3);
        int n4 = new_node(4);
        int n6 = new_node(6);
left_child[root] = n2;
right_child[root] = n5;
left_child[n2] = n3;
right_child[n2] = n4;
right_child[n5] = n6;
        return root;
    }

    static int[] flatten(int root) {
        if (root == 0) {
            return new int[]{};
        }
        int[] res = ((int[])(((int[])(new int[]{node_data[root]}))));
        int[] left_vals = ((int[])(flatten(left_child[root])));
        int[] right_vals = ((int[])(flatten(right_child[root])));
        int i = 0;
        while (i < left_vals.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(left_vals[i])).toArray()));
            i = i + 1;
        }
        i = 0;
        while (i < right_vals.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(right_vals[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static void display(int[] values) {
        String s = "";
        int i_1 = 0;
        while (i_1 < values.length) {
            if (i_1 == 0) {
                s = _p(_geti(values, i_1));
            } else {
                s = s + " " + _p(_geti(values, i_1));
            }
            i_1 = i_1 + 1;
        }
        System.out.println(s);
    }
    public static void main(String[] args) {
        node_data = ((int[])(((int[])(new int[]{0}))));
        left_child = ((int[])(((int[])(new int[]{0}))));
        right_child = ((int[])(((int[])(new int[]{0}))));
        System.out.println("Flattened Linked List:");
        root_1 = build_tree();
        vals = ((int[])(flatten(root_1)));
        display(((int[])(vals)));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
