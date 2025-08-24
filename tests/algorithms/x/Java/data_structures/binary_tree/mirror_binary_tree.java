public class Main {
    static class Tree {
        java.math.BigInteger[] values;
        java.math.BigInteger[] left;
        java.math.BigInteger[] right;
        java.math.BigInteger root;
        Tree(java.math.BigInteger[] values, java.math.BigInteger[] left, java.math.BigInteger[] right, java.math.BigInteger root) {
            this.values = values;
            this.left = left;
            this.right = right;
            this.root = root;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'values': %s, 'left': %s, 'right': %s, 'root': %s}", String.valueOf(values), String.valueOf(left), String.valueOf(right), String.valueOf(root));
        }
    }


    static void mirror_node(java.math.BigInteger[] left, java.math.BigInteger[] right, java.math.BigInteger idx) {
        if (idx.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return;
        }
        java.math.BigInteger temp_1 = new java.math.BigInteger(String.valueOf(left[(int)(((java.math.BigInteger)(idx)).longValue())]));
left[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(right[(int)(((java.math.BigInteger)(idx)).longValue())]));
right[(int)(((java.math.BigInteger)(idx)).longValue())] = new java.math.BigInteger(String.valueOf(temp_1));
        mirror_node(((java.math.BigInteger[])(left)), ((java.math.BigInteger[])(right)), new java.math.BigInteger(String.valueOf(left[(int)(((java.math.BigInteger)(idx)).longValue())])));
        mirror_node(((java.math.BigInteger[])(left)), ((java.math.BigInteger[])(right)), new java.math.BigInteger(String.valueOf(right[(int)(((java.math.BigInteger)(idx)).longValue())])));
    }

    static Tree mirror(Tree tree) {
        mirror_node(((java.math.BigInteger[])(tree.left)), ((java.math.BigInteger[])(tree.right)), new java.math.BigInteger(String.valueOf(tree.root)));
        return tree;
    }

    static java.math.BigInteger[] inorder(Tree tree, java.math.BigInteger idx) {
        if (idx.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
            return new java.math.BigInteger[]{};
        }
        java.math.BigInteger[] left_vals_1 = ((java.math.BigInteger[])(inorder(tree, new java.math.BigInteger(String.valueOf(tree.left[(int)(((java.math.BigInteger)(idx)).longValue())])))));
        java.math.BigInteger[] right_vals_1 = ((java.math.BigInteger[])(inorder(tree, new java.math.BigInteger(String.valueOf(tree.right[(int)(((java.math.BigInteger)(idx)).longValue())])))));
        return ((java.math.BigInteger[])(concat(concat(left_vals_1, new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(tree.values[(int)(((java.math.BigInteger)(idx)).longValue())]))}), right_vals_1)));
    }

    static Tree make_tree_zero() {
        return new Tree(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}, new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}, new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}, 0);
    }

    static Tree make_tree_seven() {
        return new Tree(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(5), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(6), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}, 0);
    }

    static Tree make_tree_nine() {
        return new Tree(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(9)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), java.math.BigInteger.valueOf(6), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(8), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}, 0);
    }

    static void main() {
        String[] names = ((String[])(new String[]{"zero", "seven", "nine"}));
        Tree[] trees_1 = ((Tree[])(new Tree[]{make_tree_zero(), make_tree_seven(), make_tree_nine()}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(trees_1.length))) < 0) {
            Tree tree_1 = trees_1[(int)(((java.math.BigInteger)(i_1)).longValue())];
            System.out.println("      The " + names[(int)(((java.math.BigInteger)(i_1)).longValue())] + " tree: " + _p(inorder(tree_1, new java.math.BigInteger(String.valueOf(tree_1.root)))));
            Tree mirrored_1 = mirror(tree_1);
            System.out.println("Mirror of " + names[(int)(((java.math.BigInteger)(i_1)).longValue())] + " tree: " + _p(inorder(mirrored_1, new java.math.BigInteger(String.valueOf(mirrored_1.root)))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
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
