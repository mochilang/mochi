public class Main {
    static class Node {
        int key;
        int left;
        int right;
        Node(int key, int left, int right) {
            this.key = key;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'key': %s, 'left': %s, 'right': %s}", String.valueOf(key), String.valueOf(left), String.valueOf(right));
        }
    }

    static Node[] tree;

    static int[] inorder(Node[] nodes, int idx) {
        if (idx == (-1)) {
            return new int[]{};
        }
        Node node = nodes[idx];
        int[] result = ((int[])(inorder(((Node[])(nodes)), node.left)));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(node.key)).toArray()));
        result = ((int[])(concat(result, inorder(((Node[])(nodes)), node.right))));
        return result;
    }

    static int[] floor_ceiling(Node[] nodes, int idx, int key) {
        int floor_val = ((Number)(null)).intValue();
        int ceiling_val = ((Number)(null)).intValue();
        int current = idx;
        while (current != (-1)) {
            Node node_1 = nodes[current];
            if (node_1.key == key) {
                floor_val = node_1.key;
                ceiling_val = node_1.key;
                break;
            }
            if (key < node_1.key) {
                ceiling_val = node_1.key;
                current = node_1.left;
            } else {
                floor_val = node_1.key;
                current = node_1.right;
            }
        }
        return new int[]{floor_val, ceiling_val};
    }
    public static void main(String[] args) {
        tree = ((Node[])(new Node[]{new Node(10, 1, 2), new Node(5, 3, 4), new Node(20, 5, 6), new Node(3, -1, -1), new Node(7, -1, -1), new Node(15, -1, -1), new Node(25, -1, -1)}));
        System.out.println(_p(inorder(((Node[])(tree)), 0)));
        System.out.println(_p(floor_ceiling(((Node[])(tree)), 0, 8)));
        System.out.println(_p(floor_ceiling(((Node[])(tree)), 0, 14)));
        System.out.println(_p(floor_ceiling(((Node[])(tree)), 0, -1)));
        System.out.println(_p(floor_ceiling(((Node[])(tree)), 0, 30)));
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
