public class Main {

    static Object[] create_node(int value) {
        return new int[]{value, null, null};
    }

    static Object[] insert(Object[] node, int value) {
        if ((node == null)) {
            return create_node(value);
        }
        if (value < ((Number)(node[0])).intValue()) {
node[1] = insert(((Object[])(node[1])), value);
        } else         if (value > ((Number)(node[0])).intValue()) {
node[2] = insert(((Object[])(node[2])), value);
        }
        return node;
    }

    static boolean search(Object[] node, int value) {
        if ((node == null)) {
            return false;
        }
        if (value == ((Number)(node[0])).intValue()) {
            return true;
        }
        if (value < ((Number)(node[0])).intValue()) {
            return search(((Object[])(node[1])), value);
        }
        return search(((Object[])(node[2])), value);
    }

    static int[] inorder(Object[] node, int[] acc) {
        if ((node == null)) {
            return acc;
        }
        int[] left_acc = ((int[])(inorder(((Object[])(node[1])), ((int[])(acc)))));
        Object[] with_node = ((Object[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(left_acc), java.util.stream.IntStream.of(((Number)(node[0])).intValue())).toArray()));
        return inorder(((Object[])(node[2])), ((int[])(with_node)));
    }

    static int find_min(Object[] node) {
        Object current = node;
        while (!(current[1] == null)) {
            current = current[1];
        }
        return current[0];
    }

    static int find_max(Object[] node) {
        Object current_1 = node;
        while (!(current_1[2] == null)) {
            current_1 = current_1[2];
        }
        return current_1[0];
    }

    static Object[] delete(Object[] node, int value) {
        if ((node == null)) {
            return null;
        }
        if (value < ((Number)(node[0])).intValue()) {
node[1] = delete(((Object[])(node[1])), value);
        } else         if (value > ((Number)(node[0])).intValue()) {
node[2] = delete(((Object[])(node[2])), value);
        } else {
            if ((node[1] == null)) {
                return node[2];
            }
            if ((node[2] == null)) {
                return node[1];
            }
            int min_val = find_min(((Object[])(node[2])));
node[0] = min_val;
node[2] = delete(((Object[])(node[2])), min_val);
        }
        return node;
    }

    static void main() {
        Object[] root = ((Object[])(null));
        int[] nums = ((int[])(new int[]{8, 3, 6, 1, 10, 14, 13, 4, 7}));
        for (int v : nums) {
            root = ((Object[])(insert(((Object[])(root)), v)));
        }
        System.out.println(_p(inorder(((Object[])(root)), ((int[])(new int[]{})))));
        System.out.println(search(((Object[])(root)), 6));
        System.out.println(search(((Object[])(root)), 20));
        System.out.println(find_min(((Object[])(root))));
        System.out.println(find_max(((Object[])(root))));
        root = ((Object[])(delete(((Object[])(root)), 6)));
        System.out.println(_p(inorder(((Object[])(root)), ((int[])(new int[]{})))));
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
