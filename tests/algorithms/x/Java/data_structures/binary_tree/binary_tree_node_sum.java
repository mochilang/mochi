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

    static Node[] example;

    static java.math.BigInteger node_sum(Node[] tree, java.math.BigInteger index) {
        if (index.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return 0;
        }
        Node node_1 = tree[(int)(((java.math.BigInteger)(index)).longValue())];
        return node_1.value.add(node_sum(((Node[])(tree)), new java.math.BigInteger(String.valueOf(node_1.left)))).add(node_sum(((Node[])(tree)), new java.math.BigInteger(String.valueOf(node_1.right))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            example = ((Node[])(new Node[]{new Node(10, 1, 2), new Node(5, 3, (java.math.BigInteger.valueOf(1)).negate()), new Node((java.math.BigInteger.valueOf(3)).negate(), 4, 5), new Node(12, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(8, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(0, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate())}));
            System.out.println(node_sum(((Node[])(example)), java.math.BigInteger.valueOf(0)));
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
