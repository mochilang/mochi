public class Main {
    static class Node {
        int value;
        int left;
        int right;
        Node(int value, int left, int right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'left': %s, 'right': %s}", String.valueOf(value), String.valueOf(left), String.valueOf(right));
        }
    }

    static class TreeState {
        Node[] nodes;
        int root;
        TreeState(Node[] nodes, int root) {
            this.nodes = nodes;
            this.root = root;
        }
        TreeState() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'root': %s}", String.valueOf(nodes), String.valueOf(root));
        }
    }


    static int new_node(TreeState state, int value) {
state.nodes = java.util.stream.Stream.concat(java.util.Arrays.stream(state.nodes), java.util.stream.Stream.of(new Node(value, (-1), (-1)))).toArray(Node[]::new);
        return state.nodes.length - 1;
    }

    static void insert(TreeState state, int value) {
        if (state.root == (-1)) {
state.root = new_node(state, value);
            return;
        }
        int current = state.root;
        Node[] nodes = ((Node[])(state.nodes));
        while (true) {
            Node node = nodes[current];
            if (value < node.value) {
                if (node.left == (-1)) {
                    int idx = new_node(state, value);
                    nodes = ((Node[])(state.nodes));
node.left = idx;
nodes[current] = node;
state.nodes = nodes;
                    return;
                }
                current = node.left;
            } else             if (value > node.value) {
                if (node.right == (-1)) {
                    int idx_1 = new_node(state, value);
                    nodes = ((Node[])(state.nodes));
node.right = idx_1;
nodes[current] = node;
state.nodes = nodes;
                    return;
                }
                current = node.right;
            } else {
                return;
            }
        }
    }

    static int[] inorder(TreeState state, int idx) {
        if (idx == (-1)) {
            return new int[]{};
        }
        Node node_1 = state.nodes[idx];
        int[] result = ((int[])(inorder(state, node_1.left)));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(node_1.value)).toArray()));
        int[] right_part = ((int[])(inorder(state, node_1.right)));
        int i = 0;
        while (i < right_part.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(right_part[i])).toArray()));
            i = i + 1;
        }
        return result;
    }

    static int[] tree_sort(int[] arr) {
        TreeState state = new TreeState(new Node[]{}, (-1));
        int i_1 = 0;
        while (i_1 < arr.length) {
            insert(state, arr[i_1]);
            i_1 = i_1 + 1;
        }
        if (state.root == (-1)) {
            return new int[]{};
        }
        return inorder(state, state.root);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(tree_sort(((int[])(new int[]{})))));
            System.out.println(_p(tree_sort(((int[])(new int[]{1})))));
            System.out.println(_p(tree_sort(((int[])(new int[]{1, 2})))));
            System.out.println(_p(tree_sort(((int[])(new int[]{5, 2, 7})))));
            System.out.println(_p(tree_sort(((int[])(new int[]{5, -4, 9, 2, 7})))));
            System.out.println(_p(tree_sort(((int[])(new int[]{5, 6, 1, -1, 4, 37, 2, 7})))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
