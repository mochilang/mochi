public class Main {
    static java.math.BigInteger NONE;
    static class Tree {
        double[] data;
        java.math.BigInteger[] left;
        java.math.BigInteger[] right;
        Tree(double[] data, java.math.BigInteger[] left, java.math.BigInteger[] right) {
            this.data = data;
            this.left = left;
            this.right = right;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'left': %s, 'right': %s}", String.valueOf(data), String.valueOf(left), String.valueOf(right));
        }
    }

    static Tree tree1;
    static Tree tree2;
    static Tree tree3;

    static double[] inorder(Tree tree, java.math.BigInteger index) {
        double[] res = ((double[])(new double[]{}));
        if (index.compareTo(NONE) == 0) {
            return res;
        }
        java.math.BigInteger left_idx_1 = new java.math.BigInteger(String.valueOf(tree.left[(int)(((java.math.BigInteger)(index)).longValue())]));
        if (left_idx_1.compareTo(NONE) != 0) {
            res = ((double[])(concat(res, inorder(tree, new java.math.BigInteger(String.valueOf(left_idx_1))))));
        }
        res = ((double[])(appendDouble(res, (double)(tree.data[(int)(((java.math.BigInteger)(index)).longValue())]))));
        java.math.BigInteger right_idx_1 = new java.math.BigInteger(String.valueOf(tree.right[(int)(((java.math.BigInteger)(index)).longValue())]));
        if (right_idx_1.compareTo(NONE) != 0) {
            res = ((double[])(concat(res, inorder(tree, new java.math.BigInteger(String.valueOf(right_idx_1))))));
        }
        return res;
    }

    static boolean is_sorted(Tree tree, java.math.BigInteger index) {
        if (index.compareTo(NONE) == 0) {
            return true;
        }
        java.math.BigInteger left_idx_3 = new java.math.BigInteger(String.valueOf(tree.left[(int)(((java.math.BigInteger)(index)).longValue())]));
        if (left_idx_3.compareTo(NONE) != 0) {
            if ((double)(tree.data[(int)(((java.math.BigInteger)(index)).longValue())]) < (double)(tree.data[(int)(((java.math.BigInteger)(left_idx_3)).longValue())])) {
                return false;
            }
            if (!(Boolean)is_sorted(tree, new java.math.BigInteger(String.valueOf(left_idx_3)))) {
                return false;
            }
        }
        java.math.BigInteger right_idx_3 = new java.math.BigInteger(String.valueOf(tree.right[(int)(((java.math.BigInteger)(index)).longValue())]));
        if (right_idx_3.compareTo(NONE) != 0) {
            if ((double)(tree.data[(int)(((java.math.BigInteger)(index)).longValue())]) > (double)(tree.data[(int)(((java.math.BigInteger)(right_idx_3)).longValue())])) {
                return false;
            }
            if (!(Boolean)is_sorted(tree, new java.math.BigInteger(String.valueOf(right_idx_3)))) {
                return false;
            }
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NONE = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
            tree1 = new Tree(new double[]{(double)(2.1), (double)(2.0), (double)(2.2)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(NONE)), new java.math.BigInteger(String.valueOf(NONE))}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf(NONE)), new java.math.BigInteger(String.valueOf(NONE))});
            System.out.println("Tree " + _p(inorder(tree1, java.math.BigInteger.valueOf(0))) + " is sorted: " + _p(is_sorted(tree1, java.math.BigInteger.valueOf(0))));
            tree2 = new Tree(new double[]{(double)(2.1), (double)(2.0), (double)(2.0)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(NONE)), new java.math.BigInteger(String.valueOf(NONE))}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf(NONE)), new java.math.BigInteger(String.valueOf(NONE))});
            System.out.println("Tree " + _p(inorder(tree2, java.math.BigInteger.valueOf(0))) + " is sorted: " + _p(is_sorted(tree2, java.math.BigInteger.valueOf(0))));
            tree3 = new Tree(new double[]{(double)(2.1), (double)(2.0), (double)(2.1)}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), new java.math.BigInteger(String.valueOf(NONE)), new java.math.BigInteger(String.valueOf(NONE))}, new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), new java.math.BigInteger(String.valueOf(NONE)), new java.math.BigInteger(String.valueOf(NONE))});
            System.out.println("Tree " + _p(inorder(tree3, java.math.BigInteger.valueOf(0))) + " is sorted: " + _p(is_sorted(tree3, java.math.BigInteger.valueOf(0))));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
