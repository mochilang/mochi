public class Main {
    static class Node {
        java.math.BigInteger data;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger data, java.math.BigInteger left, java.math.BigInteger right) {
            this.data = data;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'left': %s, 'right': %s}", String.valueOf(data), String.valueOf(left), String.valueOf(right));
        }
    }

    static class TreeState {
        Node[] nodes;
        java.math.BigInteger root;
        TreeState(Node[] nodes, java.math.BigInteger root) {
            this.nodes = nodes;
            this.root = root;
        }
        TreeState() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'root': %s}", String.valueOf(nodes), String.valueOf(root));
        }
    }


    static java.math.BigInteger new_node(TreeState state, java.math.BigInteger value) {
state.nodes = java.util.stream.Stream.concat(java.util.Arrays.stream(state.nodes), java.util.stream.Stream.of(new Node(value, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate())))).toArray(Node[]::new);
        return new java.math.BigInteger(String.valueOf(state.nodes.length)).subtract(java.math.BigInteger.valueOf(1));
    }

    static void insert(TreeState state, java.math.BigInteger value) {
        if (state.root.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
state.root = new_node(state, new java.math.BigInteger(String.valueOf(value)));
            return;
        }
        java.math.BigInteger current_1 = new java.math.BigInteger(String.valueOf(state.root));
        Node[] nodes_1 = ((Node[])(state.nodes));
        while (true) {
            Node node_1 = nodes_1[(int)(((java.math.BigInteger)(current_1)).longValue())];
            if (value.compareTo(node_1.data) < 0) {
                if (node_1.left.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
node_1.left = new_node(state, new java.math.BigInteger(String.valueOf(value)));
nodes_1[(int)(((java.math.BigInteger)(current_1)).longValue())] = node_1;
state.nodes = nodes_1;
                    return;
                }
                current_1 = new java.math.BigInteger(String.valueOf(node_1.left));
            } else {
                if (node_1.right.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
node_1.right = new_node(state, new java.math.BigInteger(String.valueOf(value)));
nodes_1[(int)(((java.math.BigInteger)(current_1)).longValue())] = node_1;
state.nodes = nodes_1;
                    return;
                }
                current_1 = new java.math.BigInteger(String.valueOf(node_1.right));
            }
        }
    }

    static java.math.BigInteger[] inorder(TreeState state, java.math.BigInteger idx) {
        if (idx.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return new java.math.BigInteger[]{};
        }
        Node node_3 = state.nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(inorder(state, new java.math.BigInteger(String.valueOf(node_3.left)))));
        result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(node_3.data)).toArray(java.math.BigInteger[]::new)));
        java.math.BigInteger[] right_part_1 = ((java.math.BigInteger[])(inorder(state, new java.math.BigInteger(String.valueOf(node_3.right)))));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(right_part_1.length))) < 0) {
            result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(right_part_1[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return result_1;
    }

    static TreeState make_tree() {
        TreeState state = new TreeState(new Node[]{}, ((java.math.BigInteger.valueOf(1)).negate()));
        insert(state, java.math.BigInteger.valueOf(15));
        insert(state, java.math.BigInteger.valueOf(10));
        insert(state, java.math.BigInteger.valueOf(25));
        insert(state, java.math.BigInteger.valueOf(6));
        insert(state, java.math.BigInteger.valueOf(14));
        insert(state, java.math.BigInteger.valueOf(20));
        insert(state, java.math.BigInteger.valueOf(60));
        return state;
    }

    static void main() {
        TreeState state_1 = make_tree();
        System.out.println("Printing values of binary search tree in Inorder Traversal.");
        System.out.println(java.util.Arrays.toString(inorder(state_1, new java.math.BigInteger(String.valueOf(state_1.root)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
}
