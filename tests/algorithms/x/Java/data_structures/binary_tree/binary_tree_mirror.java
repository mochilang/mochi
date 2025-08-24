public class Main {

    static void binary_tree_mirror_dict(java.util.Map<java.math.BigInteger,java.math.BigInteger[]> tree, java.math.BigInteger root) {
        if ((root.compareTo(java.math.BigInteger.valueOf(0)) == 0) || (!(tree.containsKey(root)))) {
            return;
        }
        java.math.BigInteger[] children_1 = (java.math.BigInteger[])(((java.math.BigInteger[])(tree).get(root)));
        java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(children_1[(int)(0L)]));
        java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(children_1[(int)(1L)]));
tree.put(root, ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(right_1)), new java.math.BigInteger(String.valueOf(left_1))})));
        binary_tree_mirror_dict(tree, new java.math.BigInteger(String.valueOf(left_1)));
        binary_tree_mirror_dict(tree, new java.math.BigInteger(String.valueOf(right_1)));
    }

    static java.util.Map<java.math.BigInteger,java.math.BigInteger[]> binary_tree_mirror(java.util.Map<java.math.BigInteger,java.math.BigInteger[]> binary_tree, java.math.BigInteger root) {
        if (new java.math.BigInteger(String.valueOf(binary_tree.size())).compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            throw new RuntimeException(String.valueOf("binary tree cannot be empty"));
        }
        if (!(binary_tree.containsKey(root))) {
            throw new RuntimeException(String.valueOf("root " + _p(root) + " is not present in the binary_tree"));
        }
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> tree_copy_1 = ((java.util.Map<java.math.BigInteger,java.math.BigInteger[]>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger[]>()));
        for (java.math.BigInteger k : binary_tree.keySet()) {
tree_copy_1.put(k, (java.math.BigInteger[])(((java.math.BigInteger[])(binary_tree).get(k))));
        }
        binary_tree_mirror_dict(tree_copy_1, new java.math.BigInteger(String.valueOf(root)));
        return tree_copy_1;
    }

    static void main() {
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> binary_tree = ((java.util.Map<java.math.BigInteger,java.math.BigInteger[]>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger[]>(java.util.Map.ofEntries(java.util.Map.entry(java.math.BigInteger.valueOf(1), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3)}))), java.util.Map.entry(java.math.BigInteger.valueOf(2), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5)}))), java.util.Map.entry(java.math.BigInteger.valueOf(3), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7)}))), java.util.Map.entry(java.math.BigInteger.valueOf(7), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9)})))))));
        System.out.println("Binary tree: " + _p(binary_tree));
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> mirrored_1 = binary_tree_mirror(binary_tree, java.math.BigInteger.valueOf(1));
        System.out.println("Binary tree mirror: " + _p(mirrored_1));
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
