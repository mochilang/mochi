public class Main {
    static java.math.BigInteger[][] symmetric_tree;
    static java.math.BigInteger[][] asymmetric_tree;

    static java.math.BigInteger[][] make_symmetric_tree() {
        return new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}))};
    }

    static java.math.BigInteger[][] make_asymmetric_tree() {
        return new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(2)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(3), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(4), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())), new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()))}))};
    }

    static boolean is_symmetric_tree(java.math.BigInteger[][] tree) {
        java.math.BigInteger[] stack = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(tree[(int)(0L)][(int)(1L)])), new java.math.BigInteger(String.valueOf(tree[(int)(0L)][(int)(2L)]))}));
        while (new java.math.BigInteger(String.valueOf(stack.length)).compareTo(java.math.BigInteger.valueOf(2)) >= 0) {
            java.math.BigInteger left_1 = new java.math.BigInteger(String.valueOf(stack[(int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(stack.length)).subtract(java.math.BigInteger.valueOf(2)))).longValue())]));
            java.math.BigInteger right_1 = new java.math.BigInteger(String.valueOf(stack[(int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(stack.length)).subtract(java.math.BigInteger.valueOf(1)))).longValue())]));
            stack = ((java.math.BigInteger[])(java.util.Arrays.copyOfRange(stack, (int)(0L), (int)(((java.math.BigInteger)(new java.math.BigInteger(String.valueOf(stack.length)).subtract(java.math.BigInteger.valueOf(2)))).longValue()))));
            if (left_1.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0 && right_1.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
                continue;
            }
            if (left_1.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0 || right_1.compareTo(((java.math.BigInteger.valueOf(1)).negate())) == 0) {
                return false;
            }
            java.math.BigInteger[] lnode_1 = ((java.math.BigInteger[])(tree[(int)(((java.math.BigInteger)(left_1)).longValue())]));
            java.math.BigInteger[] rnode_1 = ((java.math.BigInteger[])(tree[(int)(((java.math.BigInteger)(right_1)).longValue())]));
            if (lnode_1[(int)(0L)].compareTo(rnode_1[(int)(0L)]) != 0) {
                return false;
            }
            stack = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(lnode_1[(int)(1L)])).toArray(java.math.BigInteger[]::new)));
            stack = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(rnode_1[(int)(2L)])).toArray(java.math.BigInteger[]::new)));
            stack = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(lnode_1[(int)(2L)])).toArray(java.math.BigInteger[]::new)));
            stack = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(stack), java.util.stream.Stream.of(rnode_1[(int)(1L)])).toArray(java.math.BigInteger[]::new)));
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            symmetric_tree = ((java.math.BigInteger[][])(make_symmetric_tree()));
            asymmetric_tree = ((java.math.BigInteger[][])(make_asymmetric_tree()));
            System.out.println(_p(is_symmetric_tree(((java.math.BigInteger[][])(symmetric_tree)))));
            System.out.println(_p(is_symmetric_tree(((java.math.BigInteger[][])(asymmetric_tree)))));
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
