public class Main {
    static class Node {
        java.math.BigInteger value;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger value, java.math.BigInteger left, java.math.BigInteger right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'left': %s, 'right': %s}", String.valueOf(value), String.valueOf(left), String.valueOf(right));
        }
    }


    static java.math.BigInteger tree_sum(Node[] nodes, java.math.BigInteger idx) {
        if (idx.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return 0;
        }
        Node node_1 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        return node_1.value.add(tree_sum(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.left)))).add(tree_sum(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.right))));
    }

    static boolean is_sum_node(Node[] nodes, java.math.BigInteger idx) {
        Node node_2 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        if (node_2.left.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0 && node_2.right.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return true;
        }
        java.math.BigInteger left_sum_1 = new java.math.BigInteger(String.valueOf(tree_sum(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_2.left)))));
        java.math.BigInteger right_sum_1 = new java.math.BigInteger(String.valueOf(tree_sum(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_2.right)))));
        if (node_2.value.compareTo(left_sum_1.add(right_sum_1)) != 0) {
            return false;
        }
        boolean left_ok_1 = true;
        if (node_2.left.compareTo(((java.math.BigInteger.valueOf(1)).negate())) != 0) {
            left_ok_1 = is_sum_node(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_2.left)));
        }
        boolean right_ok_1 = true;
        if (node_2.right.compareTo(((java.math.BigInteger.valueOf(1)).negate())) != 0) {
            right_ok_1 = is_sum_node(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_2.right)));
        }
        return left_ok_1 && right_ok_1;
    }

    static Node[] build_a_tree() {
        return new Node[]{new Node(11, 1, 2), new Node(2, 3, 4), new Node(29, 5, 6), new Node(1, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate())), new Node(7, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate())), new Node(15, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate())), new Node(40, 7, ((java.math.BigInteger.valueOf(1)).negate())), new Node(35, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate()))};
    }

    static Node[] build_a_sum_tree() {
        return new Node[]{new Node(26, 1, 2), new Node(10, 3, 4), new Node(3, ((java.math.BigInteger.valueOf(1)).negate()), 5), new Node(4, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate())), new Node(6, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate())), new Node(3, ((java.math.BigInteger.valueOf(1)).negate()), ((java.math.BigInteger.valueOf(1)).negate()))};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
