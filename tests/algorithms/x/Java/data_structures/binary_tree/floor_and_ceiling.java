public class Main {
    static class Node {
        java.math.BigInteger key;
        java.math.BigInteger left;
        java.math.BigInteger right;
        Node(java.math.BigInteger key, java.math.BigInteger left, java.math.BigInteger right) {
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

    static java.math.BigInteger[] inorder(Node[] nodes, java.math.BigInteger idx) {
        if (idx.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return new java.math.BigInteger[]{};
        }
        Node node_1 = nodes[(int)(((java.math.BigInteger)(idx)).longValue())];
        java.math.BigInteger[] result_1 = ((java.math.BigInteger[])(inorder(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.left)))));
        result_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(node_1.key)).toArray(java.math.BigInteger[]::new)));
        result_1 = ((java.math.BigInteger[])(concat(result_1, inorder(((Node[])(nodes)), new java.math.BigInteger(String.valueOf(node_1.right))))));
        return result_1;
    }

    static java.math.BigInteger[] floor_ceiling(Node[] nodes, java.math.BigInteger idx, java.math.BigInteger key) {
        java.math.BigInteger floor_val = new java.math.BigInteger(String.valueOf(null));
        java.math.BigInteger ceiling_val_1 = new java.math.BigInteger(String.valueOf(null));
        java.math.BigInteger current_1 = new java.math.BigInteger(String.valueOf(idx));
        while (current_1.compareTo(((java.math.BigInteger.valueOf(1)).negate())) != 0) {
            Node node_3 = nodes[(int)(((java.math.BigInteger)(current_1)).longValue())];
            if (node_3.key.compareTo(key) == 0) {
                floor_val = new java.math.BigInteger(String.valueOf(node_3.key));
                ceiling_val_1 = new java.math.BigInteger(String.valueOf(node_3.key));
                break;
            }
            if (key.compareTo(node_3.key) < 0) {
                ceiling_val_1 = new java.math.BigInteger(String.valueOf(node_3.key));
                current_1 = new java.math.BigInteger(String.valueOf(node_3.left));
            } else {
                floor_val = new java.math.BigInteger(String.valueOf(node_3.key));
                current_1 = new java.math.BigInteger(String.valueOf(node_3.right));
            }
        }
        return new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(floor_val)), new java.math.BigInteger(String.valueOf(ceiling_val_1))};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            tree = ((Node[])(new Node[]{new Node(10, 1, 2), new Node(5, 3, 4), new Node(20, 5, 6), new Node(3, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(7, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(15, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate()), new Node(25, (java.math.BigInteger.valueOf(1)).negate(), (java.math.BigInteger.valueOf(1)).negate())}));
            System.out.println(_p(inorder(((Node[])(tree)), java.math.BigInteger.valueOf(0))));
            System.out.println(_p(floor_ceiling(((Node[])(tree)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(8))));
            System.out.println(_p(floor_ceiling(((Node[])(tree)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(14))));
            System.out.println(_p(floor_ceiling(((Node[])(tree)), java.math.BigInteger.valueOf(0), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())))));
            System.out.println(_p(floor_ceiling(((Node[])(tree)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(30))));
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

    static Object concat(Object a, Object b) {
        int len1 = java.lang.reflect.Array.getLength(a);
        int len2 = java.lang.reflect.Array.getLength(b);
        Object out = java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), len1 + len2);
        System.arraycopy(a, 0, out, 0, len1);
        System.arraycopy(b, 0, out, len1, len2);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
