public class Main {
    static class Node {
        int start;
        int end;
        int val;
        int mid;
        int left;
        int right;
        Node(int start, int end, int val, int mid, int left, int right) {
            this.start = start;
            this.end = end;
            this.val = val;
            this.mid = mid;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'start': %s, 'end': %s, 'val': %s, 'mid': %s, 'left': %s, 'right': %s}", String.valueOf(start), String.valueOf(end), String.valueOf(val), String.valueOf(mid), String.valueOf(left), String.valueOf(right));
        }
    }

    static class BuildResult {
        Node[] nodes;
        int idx;
        BuildResult(Node[] nodes, int idx) {
            this.nodes = nodes;
            this.idx = idx;
        }
        BuildResult() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'idx': %s}", String.valueOf(nodes), String.valueOf(idx));
        }
    }

    static class SegmentTree {
        int[] arr;
        int op;
        SegmentTree(int[] arr, int op) {
            this.arr = arr;
            this.op = op;
        }
        SegmentTree() {}
        @Override public String toString() {
            return String.format("{'arr': %s, 'op': %s}", String.valueOf(arr), String.valueOf(op));
        }
    }

    static int[] arr;

    static int combine(int a, int b, int op) {
        if (op == 0) {
            return a + b;
        }
        if (op == 1) {
            if (a > b) {
                return a;
            }
            return b;
        }
        if (a < b) {
            return a;
        }
        return b;
    }

    static BuildResult build_tree(Node[] nodes, int[] arr, int start, int end, int op) {
        if (start == end) {
            Node node = new Node(start, end, arr[start], start, -1, -1);
            Node[] new_nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(node)).toArray(Node[]::new)));
            return new BuildResult(new_nodes, new_nodes.length - 1);
        }
        int mid = (start + end) / 2;
        BuildResult left_res = build_tree(((Node[])(nodes)), ((int[])(arr)), start, mid, op);
        BuildResult right_res = build_tree(((Node[])(left_res.nodes)), ((int[])(arr)), mid + 1, end, op);
        Node left_node = right_res.nodes[left_res.idx];
        Node right_node = right_res.nodes[right_res.idx];
        int val = combine(left_node.val, right_node.val, op);
        Node parent = new Node(start, end, val, mid, left_res.idx, right_res.idx);
        Node[] new_nodes_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(right_res.nodes), java.util.stream.Stream.of(parent)).toArray(Node[]::new)));
        return new BuildResult(new_nodes_1, new_nodes_1.length - 1);
    }

    static SegmentTree new_segment_tree(int[] collection, int op) {
        return new SegmentTree(collection, op);
    }

    static SegmentTree update(SegmentTree tree, int i, int val) {
        int[] new_arr = ((int[])(new int[]{}));
        int idx = 0;
        while (idx < tree.arr.length) {
            if (idx == i) {
                new_arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_arr), java.util.stream.IntStream.of(val)).toArray()));
            } else {
                new_arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_arr), java.util.stream.IntStream.of(tree.arr[idx])).toArray()));
            }
            idx = idx + 1;
        }
        return new SegmentTree(new_arr, tree.op);
    }

    static int query_range(SegmentTree tree, int i, int j) {
        int result = tree.arr[i];
        int idx_1 = i + 1;
        while (idx_1 <= j) {
            result = combine(result, tree.arr[idx_1], tree.op);
            idx_1 = idx_1 + 1;
        }
        return result;
    }

    static Node[] traverse(SegmentTree tree) {
        if (tree.arr.length == 0) {
            return new Node[]{};
        }
        BuildResult res = build_tree(((Node[])(new Node[]{})), ((int[])(tree.arr)), 0, tree.arr.length - 1, tree.op);
        return res.nodes;
    }

    static String node_to_string(Node node) {
        return "SegmentTreeNode(start=" + _p(node.start) + ", end=" + _p(node.end) + ", val=" + _p(node.val) + ")";
    }

    static void print_traverse(SegmentTree tree) {
        Node[] nodes = ((Node[])(traverse(tree)));
        int i = 0;
        while (i < nodes.length) {
            System.out.println(node_to_string(nodes[i]));
            i = i + 1;
        }
        System.out.println("");
    }
    public static void main(String[] args) {
        arr = ((int[])(new int[]{2, 1, 5, 3, 4}));
        for (int op : new int[]{0, 1, 2}) {
            System.out.println("**************************************************");
            SegmentTree tree = new_segment_tree(((int[])(arr)), op);
            print_traverse(tree);
            tree = update(tree, 1, 5);
            print_traverse(tree);
            System.out.println(query_range(tree, 3, 4));
            System.out.println(query_range(tree, 2, 2));
            System.out.println(query_range(tree, 1, 3));
            System.out.println("");
        }
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
