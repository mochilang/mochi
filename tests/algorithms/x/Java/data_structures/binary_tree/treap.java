public class Main {
    static java.math.BigInteger NIL;
    static java.math.BigInteger[] node_values = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
    static double[] node_priors = ((double[])(new double[]{}));
    static java.math.BigInteger[] node_lefts = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
    static java.math.BigInteger[] node_rights = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
    static java.math.BigInteger seed = java.math.BigInteger.valueOf(1);
    static class SplitResult {
        java.math.BigInteger left;
        java.math.BigInteger right;
        SplitResult(java.math.BigInteger left, java.math.BigInteger right) {
            this.left = left;
            this.right = right;
        }
        SplitResult() {}
        @Override public String toString() {
            return String.format("{'left': %s, 'right': %s}", String.valueOf(left), String.valueOf(right));
        }
    }


    static double random() {
        seed = new java.math.BigInteger(String.valueOf((seed.multiply(java.math.BigInteger.valueOf(13)).add(java.math.BigInteger.valueOf(7))).remainder(java.math.BigInteger.valueOf(100))));
        return (double)((((Number)(seed)).doubleValue())) / (double)(100.0);
    }

    static java.math.BigInteger new_node(java.math.BigInteger value) {
        node_values = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_values), java.util.stream.Stream.of(value)).toArray(java.math.BigInteger[]::new)));
        node_priors = ((double[])(appendDouble(node_priors, (double)(random()))));
        node_lefts = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_lefts), java.util.stream.Stream.of(NIL)).toArray(java.math.BigInteger[]::new)));
        node_rights = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_rights), java.util.stream.Stream.of(NIL)).toArray(java.math.BigInteger[]::new)));
        return new java.math.BigInteger(String.valueOf(node_values.length)).subtract(java.math.BigInteger.valueOf(1));
    }

    static SplitResult split(java.math.BigInteger root, java.math.BigInteger value) {
        if (root.compareTo(NIL) == 0) {
            return new SplitResult(NIL, NIL);
        }
        if (value.compareTo(node_values[(int)(((java.math.BigInteger)(root)).longValue())]) < 0) {
            SplitResult res_1 = node_lefts[(int)(((java.math.BigInteger)(root)).longValue())].split(java.util.regex.Pattern.quote(value));
node_lefts[(int)(((java.math.BigInteger)(root)).longValue())] = new java.math.BigInteger(String.valueOf(res_1.right));
            return new SplitResult(res_1.left, root);
        }
        SplitResult res_3 = node_rights[(int)(((java.math.BigInteger)(root)).longValue())].split(java.util.regex.Pattern.quote(value));
node_rights[(int)(((java.math.BigInteger)(root)).longValue())] = new java.math.BigInteger(String.valueOf(res_3.left));
        return new SplitResult(root, res_3.right);
    }

    static java.math.BigInteger merge(java.math.BigInteger left, java.math.BigInteger right) {
        if (left.compareTo(NIL) == 0) {
            return right;
        }
        if (right.compareTo(NIL) == 0) {
            return left;
        }
        if ((double)(node_priors[(int)(((java.math.BigInteger)(left)).longValue())]) < (double)(node_priors[(int)(((java.math.BigInteger)(right)).longValue())])) {
node_rights[(int)(((java.math.BigInteger)(left)).longValue())] = new java.math.BigInteger(String.valueOf(merge(new java.math.BigInteger(String.valueOf(node_rights[(int)(((java.math.BigInteger)(left)).longValue())])), new java.math.BigInteger(String.valueOf(right)))));
            return left;
        }
node_lefts[(int)(((java.math.BigInteger)(right)).longValue())] = new java.math.BigInteger(String.valueOf(merge(new java.math.BigInteger(String.valueOf(left)), new java.math.BigInteger(String.valueOf(node_lefts[(int)(((java.math.BigInteger)(right)).longValue())])))));
        return right;
    }

    static java.math.BigInteger insert(java.math.BigInteger root, java.math.BigInteger value) {
        java.math.BigInteger node = new java.math.BigInteger(String.valueOf(new_node(new java.math.BigInteger(String.valueOf(value)))));
        SplitResult res_5 = root.split(java.util.regex.Pattern.quote(value));
        return merge(new java.math.BigInteger(String.valueOf(merge(new java.math.BigInteger(String.valueOf(res_5.left)), new java.math.BigInteger(String.valueOf(node))))), new java.math.BigInteger(String.valueOf(res_5.right)));
    }

    static java.math.BigInteger erase(java.math.BigInteger root, java.math.BigInteger value) {
        SplitResult res1 = root.split(java.util.regex.Pattern.quote(value.subtract(java.math.BigInteger.valueOf(1))));
        SplitResult res2_1 = res1.right.split(java.util.regex.Pattern.quote(value));
        return merge(new java.math.BigInteger(String.valueOf(res1.left)), new java.math.BigInteger(String.valueOf(res2_1.right)));
    }

    static java.math.BigInteger[] inorder(java.math.BigInteger i, java.math.BigInteger[] acc) {
        if (i.compareTo(NIL) == 0) {
            return acc;
        }
        java.math.BigInteger[] left_acc_1 = ((java.math.BigInteger[])(inorder(new java.math.BigInteger(String.valueOf(node_lefts[(int)(((java.math.BigInteger)(i)).longValue())])), ((java.math.BigInteger[])(acc)))));
        java.math.BigInteger[] with_node_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(left_acc_1), java.util.stream.Stream.of(node_values[(int)(((java.math.BigInteger)(i)).longValue())])).toArray(java.math.BigInteger[]::new)));
        return inorder(new java.math.BigInteger(String.valueOf(node_rights[(int)(((java.math.BigInteger)(i)).longValue())])), ((java.math.BigInteger[])(with_node_1)));
    }

    static void main() {
        java.math.BigInteger root = new java.math.BigInteger(String.valueOf(NIL));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(1))));
        System.out.println(_p(inorder(new java.math.BigInteger(String.valueOf(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(3))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(5))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(17))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(19))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(2))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(16))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(4))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(0))));
        System.out.println(_p(inorder(new java.math.BigInteger(String.valueOf(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(4))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(4))));
        root = new java.math.BigInteger(String.valueOf(insert(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(4))));
        System.out.println(_p(inorder(new java.math.BigInteger(String.valueOf(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
        root = new java.math.BigInteger(String.valueOf(erase(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(0))));
        System.out.println(_p(inorder(new java.math.BigInteger(String.valueOf(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
        root = new java.math.BigInteger(String.valueOf(erase(new java.math.BigInteger(String.valueOf(root)), java.math.BigInteger.valueOf(4))));
        System.out.println(_p(inorder(new java.math.BigInteger(String.valueOf(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NIL = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
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
