public class Main {

    static Object[] create_node(java.math.BigInteger value) {
        return _toObjectArray(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(value)), new java.math.BigInteger(String.valueOf(null)), new java.math.BigInteger(String.valueOf(null))});
    }

    static Object[] insert(Object[] node, java.math.BigInteger value) {
        if (java.util.Arrays.equals(node, null)) {
            return create_node(new java.math.BigInteger(String.valueOf(value)));
        }
        if (value.compareTo(new java.math.BigInteger(String.valueOf(node[(int)(0L)]))) < 0) {
node[(int)(1L)] = insert(((Object[])(node[(int)(1L)])), new java.math.BigInteger(String.valueOf(value)));
        } else         if (value.compareTo(new java.math.BigInteger(String.valueOf(node[(int)(0L)]))) > 0) {
node[(int)(2L)] = insert(((Object[])(node[(int)(2L)])), new java.math.BigInteger(String.valueOf(value)));
        }
        return node;
    }

    static boolean search(Object[] node, java.math.BigInteger value) {
        if (java.util.Arrays.equals(node, null)) {
            return false;
        }
        if (value.compareTo(new java.math.BigInteger(String.valueOf(node[(int)(0L)]))) == 0) {
            return true;
        }
        if (value.compareTo(new java.math.BigInteger(String.valueOf(node[(int)(0L)]))) < 0) {
            return search(((Object[])(node[(int)(1L)])), new java.math.BigInteger(String.valueOf(value)));
        }
        return search(((Object[])(node[(int)(2L)])), new java.math.BigInteger(String.valueOf(value)));
    }

    static java.math.BigInteger[] inorder(Object[] node, java.math.BigInteger[] acc) {
        if (java.util.Arrays.equals(node, null)) {
            return acc;
        }
        java.math.BigInteger[] left_acc_1 = ((java.math.BigInteger[])(inorder(((Object[])(node[(int)(1L)])), ((java.math.BigInteger[])(acc)))));
        java.math.BigInteger[] with_node_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(left_acc_1), java.util.stream.Stream.of(node[(int)(0L)])).toArray(java.math.BigInteger[]::new)));
        return inorder(((Object[])(node[(int)(2L)])), ((java.math.BigInteger[])(with_node_1)));
    }

    static java.math.BigInteger find_min(Object[] node) {
        Object[] current = ((Object[])(node));
        while (!(current[(int)(1L)] == null)) {
            current = ((Object[])(current[(int)(1L)]));
        }
        return new java.math.BigInteger(String.valueOf(current[(int)(0L)]));
    }

    static java.math.BigInteger find_max(Object[] node) {
        Object[] current_1 = ((Object[])(node));
        while (!(current_1[(int)(2L)] == null)) {
            current_1 = ((Object[])(current_1[(int)(2L)]));
        }
        return new java.math.BigInteger(String.valueOf(current_1[(int)(0L)]));
    }

    static Object[] delete(Object[] node, java.math.BigInteger value) {
        if (java.util.Arrays.equals(node, null)) {
            return _toObjectArray(null);
        }
        if (value.compareTo(new java.math.BigInteger(String.valueOf(node[(int)(0L)]))) < 0) {
node[(int)(1L)] = delete(((Object[])(node[(int)(1L)])), new java.math.BigInteger(String.valueOf(value)));
        } else         if (value.compareTo(new java.math.BigInteger(String.valueOf(node[(int)(0L)]))) > 0) {
node[(int)(2L)] = delete(((Object[])(node[(int)(2L)])), new java.math.BigInteger(String.valueOf(value)));
        } else {
            if ((node[(int)(1L)] == null)) {
                return _toObjectArray(node[(int)(2L)]);
            }
            if ((node[(int)(2L)] == null)) {
                return _toObjectArray(node[(int)(1L)]);
            }
            java.math.BigInteger min_val_1 = new java.math.BigInteger(String.valueOf(find_min(((Object[])(node[(int)(2L)])))));
node[(int)(0L)] = min_val_1;
node[(int)(2L)] = delete(((Object[])(node[(int)(2L)])), new java.math.BigInteger(String.valueOf(min_val_1)));
        }
        return node;
    }

    static void main() {
        Object[] root = ((Object[])(null));
        java.math.BigInteger[] nums_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(14), java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(7)}));
        for (java.math.BigInteger v : nums_1) {
            root = ((Object[])(insert(((Object[])(root)), new java.math.BigInteger(String.valueOf(v)))));
        }
        System.out.println(_p(inorder(((Object[])(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
        System.out.println(search(((Object[])(root)), java.math.BigInteger.valueOf(6)));
        System.out.println(search(((Object[])(root)), java.math.BigInteger.valueOf(20)));
        System.out.println(find_min(((Object[])(root))));
        System.out.println(find_max(((Object[])(root))));
        root = ((Object[])(delete(((Object[])(root)), java.math.BigInteger.valueOf(6))));
        System.out.println(_p(inorder(((Object[])(root)), ((java.math.BigInteger[])(new java.math.BigInteger[]{})))));
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

    static Object[] _toObjectArray(Object v) {
        if (v instanceof Object[]) return (Object[]) v;
        if (v instanceof int[]) return java.util.Arrays.stream((int[]) v).boxed().toArray();
        if (v instanceof double[]) return java.util.Arrays.stream((double[]) v).boxed().toArray();
        if (v instanceof long[]) return java.util.Arrays.stream((long[]) v).boxed().toArray();
        if (v instanceof boolean[]) { boolean[] a = (boolean[]) v; Object[] out = new Object[a.length]; for (int i = 0; i < a.length; i++) out[i] = a[i]; return out; }
        return (Object[]) v;
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
