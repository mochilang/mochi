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

    static Node[] small;
    static Node[] medium;

    static java.math.BigInteger[] inorder(Node[] nodes, java.math.BigInteger index, java.math.BigInteger[] acc) {
        if (index.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return acc;
        }
        Node node_1 = nodes[(int)(((java.math.BigInteger)(index)).longValue())];
        java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(inorder(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.left)), ((java.math.BigInteger[])(acc)))));
        res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(node_1.data)).toArray(java.math.BigInteger[]::new)));
        res_1 = ((java.math.BigInteger[])(inorder(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.right)), ((java.math.BigInteger[])(res_1)))));
        return res_1;
    }

    static java.math.BigInteger size(Node[] nodes, java.math.BigInteger index) {
        if (index.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return 0;
        }
        Node node_3 = nodes[(int)(((java.math.BigInteger)(index)).longValue())];
        return java.math.BigInteger.valueOf(1).add(size(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_3.left)))).add(size(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_3.right))));
    }

    static java.math.BigInteger depth(Node[] nodes, java.math.BigInteger index) {
        if (index.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return 0;
        }
        Node node_5 = nodes[(int)(((java.math.BigInteger)(index)).longValue())];
        java.math.BigInteger left_depth_1 = new java.math.BigInteger(String.valueOf(depth(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_5.left)))));
        java.math.BigInteger right_depth_1 = new java.math.BigInteger(String.valueOf(depth(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_5.right)))));
        if (left_depth_1.compareTo(right_depth_1) > 0) {
            return left_depth_1.add(java.math.BigInteger.valueOf(1));
        }
        return right_depth_1.add(java.math.BigInteger.valueOf(1));
    }

    static boolean is_full(Node[] nodes, java.math.BigInteger index) {
        if (index.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return true;
        }
        Node node_7 = nodes[(int)(((java.math.BigInteger)(index)).longValue())];
        if (node_7.left.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0 && node_7.right.compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
            return true;
        }
        if (node_7.left.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0 && node_7.right.compareTo((java.math.BigInteger.valueOf(1)).negate()) != 0) {
            return is_full(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_7.left))) && is_full(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_7.right)));
        }
        return false;
    }

    static Node[] small_tree() {
        Node[] arr = ((Node[])(new Node[]{}));
        arr = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new Node(2, 1, 2))).toArray(Node[]::new)));
        arr = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new Node(1, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()))).toArray(Node[]::new)));
        arr = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new Node(3, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()))).toArray(Node[]::new)));
        return arr;
    }

    static Node[] medium_tree() {
        Node[] arr_1 = ((Node[])(new Node[]{}));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(4, 1, 4))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(2, 2, 3))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(1, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(3, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(5, (java.math.BigInteger.valueOf(1)).negate(), 5))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(6, (java.math.BigInteger.valueOf(1)).negate(), 6))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(7, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()))).toArray(Node[]::new)));
        return arr_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            small = ((Node[])(small_tree()));
            System.out.println(size(((Node[])(small)), java.math.BigInteger.valueOf(0)));
            System.out.println(java.util.Arrays.toString(inorder(((Node[])(small)), java.math.BigInteger.valueOf(0), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
            System.out.println(depth(((Node[])(small)), java.math.BigInteger.valueOf(0)));
            System.out.println(is_full(((Node[])(small)), java.math.BigInteger.valueOf(0)));
            medium = ((Node[])(medium_tree()));
            System.out.println(size(((Node[])(medium)), java.math.BigInteger.valueOf(0)));
            System.out.println(java.util.Arrays.toString(inorder(((Node[])(medium)), java.math.BigInteger.valueOf(0), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
            System.out.println(depth(((Node[])(medium)), java.math.BigInteger.valueOf(0)));
            System.out.println(is_full(((Node[])(medium)), java.math.BigInteger.valueOf(0)));
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
