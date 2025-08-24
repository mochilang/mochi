public class Main {
    static class Node {
        java.math.BigInteger start;
        java.math.BigInteger end;
        java.math.BigInteger val;
        java.math.BigInteger mid;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger start, java.math.BigInteger end, java.math.BigInteger val, java.math.BigInteger mid, java.math.BigInteger left, java.math.BigInteger right) {
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
        java.math.BigInteger idx;
        BuildResult(Node[] nodes, java.math.BigInteger idx) {
            this.nodes = nodes;
            this.idx = idx;
        }
        BuildResult() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'idx': %s}", String.valueOf(nodes), String.valueOf(idx));
        }
    }

    static class SegmentTree {
        java.math.BigInteger[] arr;
        java.math.BigInteger op;
        SegmentTree(java.math.BigInteger[] arr, java.math.BigInteger op) {
            this.arr = arr;
            this.op = op;
        }
        SegmentTree() {}
        @Override public String toString() {
            return String.format("{'arr': %s, 'op': %s}", String.valueOf(arr), String.valueOf(op));
        }
    }

    static java.math.BigInteger[] arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)}));

    static java.math.BigInteger combine(java.math.BigInteger a, java.math.BigInteger b, java.math.BigInteger op) {
        if (op.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return a.add(b);
        }
        if (op.compareTo(java.math.BigInteger.valueOf(1)) == 0) {
            if (a.compareTo(b) > 0) {
                return a;
            }
            return b;
        }
        if (a.compareTo(b) < 0) {
            return a;
        }
        return b;
    }

    static BuildResult build_tree(Node[] nodes, java.math.BigInteger[] arr, java.math.BigInteger start, java.math.BigInteger end, java.math.BigInteger op) {
        if (start.compareTo(end) == 0) {
            Node node = new Node(start, end, arr[(int)(((java.math.BigInteger)(start)).longValue())], start, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate());
            Node[] new_nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(node)).toArray(Node[]::new)));
            return new BuildResult(new_nodes, new java.math.BigInteger(String.valueOf(new_nodes.length)).subtract(java.math.BigInteger.valueOf(1)));
        }
        java.math.BigInteger mid_1 = new java.math.BigInteger(String.valueOf((start.add(end)).divide(java.math.BigInteger.valueOf(2))));
        BuildResult left_res_1 = build_tree(((Node[])(nodes)), ((java.math.BigInteger[])(arr)), new java.math.BigInteger(String.valueOf(start)), new java.math.BigInteger(String.valueOf(mid_1)), new java.math.BigInteger(String.valueOf(op)));
        BuildResult right_res_1 = build_tree(((Node[])(left_res_1.nodes)), ((java.math.BigInteger[])(arr)), new java.math.BigInteger(String.valueOf(mid_1.add(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(end)), new java.math.BigInteger(String.valueOf(op)));
        Node left_node_1 = right_res_1.nodes[(int)(((java.math.BigInteger)(left_res_1.idx)).longValue())];
        Node right_node_1 = right_res_1.nodes[(int)(((java.math.BigInteger)(right_res_1.idx)).longValue())];
        java.math.BigInteger val_1 = new java.math.BigInteger(String.valueOf(combine(new java.math.BigInteger(String.valueOf(left_node_1.val)), new java.math.BigInteger(String.valueOf(right_node_1.val)), new java.math.BigInteger(String.valueOf(op)))));
        Node parent_1 = new Node(start, end, val_1, mid_1, left_res_1.idx, right_res_1.idx);
        Node[] new_nodes_2 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(right_res_1.nodes), java.util.stream.Stream.of(parent_1)).toArray(Node[]::new)));
        return new BuildResult(new_nodes_2, new java.math.BigInteger(String.valueOf(new_nodes_2.length)).subtract(java.math.BigInteger.valueOf(1)));
    }

    static SegmentTree new_segment_tree(java.math.BigInteger[] collection, java.math.BigInteger op) {
        return new SegmentTree(collection, op);
    }

    static SegmentTree update(SegmentTree tree, java.math.BigInteger i, java.math.BigInteger val) {
        java.math.BigInteger[] new_arr = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger idx_1 = java.math.BigInteger.valueOf(0);
        while (idx_1.compareTo(new java.math.BigInteger(String.valueOf(tree.arr.length))) < 0) {
            if (idx_1.compareTo(i) == 0) {
                new_arr = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_arr), java.util.stream.Stream.of(val)).toArray(java.math.BigInteger[]::new)));
            } else {
                new_arr = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_arr), java.util.stream.Stream.of(tree.arr[(int)(((java.math.BigInteger)(idx_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            }
            idx_1 = new java.math.BigInteger(String.valueOf(idx_1.add(java.math.BigInteger.valueOf(1))));
        }
        return new SegmentTree(new_arr, tree.op);
    }

    static java.math.BigInteger query_range(SegmentTree tree, java.math.BigInteger i, java.math.BigInteger j) {
        java.math.BigInteger result = new java.math.BigInteger(String.valueOf(tree.arr[(int)(((java.math.BigInteger)(i)).longValue())]));
        java.math.BigInteger idx_3 = new java.math.BigInteger(String.valueOf(i.add(java.math.BigInteger.valueOf(1))));
        while (idx_3.compareTo(j) <= 0) {
            result = new java.math.BigInteger(String.valueOf(combine(new java.math.BigInteger(String.valueOf(result)), new java.math.BigInteger(String.valueOf(tree.arr[(int)(((java.math.BigInteger)(idx_3)).longValue())])), new java.math.BigInteger(String.valueOf(tree.op)))));
            idx_3 = new java.math.BigInteger(String.valueOf(idx_3.add(java.math.BigInteger.valueOf(1))));
        }
        return result;
    }

    static Node[] traverse(SegmentTree tree) {
        if (new java.math.BigInteger(String.valueOf(tree.arr.length)).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return new Node[]{};
        }
        BuildResult res_1 = build_tree(((Node[])(new Node[]{})), ((java.math.BigInteger[])(tree.arr)), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf(new java.math.BigInteger(String.valueOf(tree.arr.length)).subtract(java.math.BigInteger.valueOf(1)))), new java.math.BigInteger(String.valueOf(tree.op)));
        return res_1.nodes;
    }

    static String node_to_string(Node node) {
        return "SegmentTreeNode(start=" + _p(node.start) + ", end=" + _p(node.end) + ", val=" + _p(node.val) + ")";
    }

    static void print_traverse(SegmentTree tree) {
        Node[] nodes = ((Node[])(traverse(tree)));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(nodes.length))) < 0) {
            System.out.println(node_to_string(nodes[(int)(((java.math.BigInteger)(i_1)).longValue())]));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println("");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            for (java.math.BigInteger op : new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2)}) {
                System.out.println("**************************************************");
                SegmentTree tree = new_segment_tree(((java.math.BigInteger[])(arr)), new java.math.BigInteger(String.valueOf(op)));
                print_traverse(tree);
                tree = update(tree, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(5));
                print_traverse(tree);
                System.out.println(query_range(tree, java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)));
                System.out.println(query_range(tree, java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(2)));
                System.out.println(query_range(tree, java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3)));
                System.out.println("");
            }
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
