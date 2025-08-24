public class Main {
    static java.math.BigInteger[] node_data = new java.math.BigInteger[0];
    static java.math.BigInteger[] left_child = new java.math.BigInteger[0];
    static java.math.BigInteger[] right_child = new java.math.BigInteger[0];
    static java.math.BigInteger root_1;
    static java.math.BigInteger[] vals;

    static java.math.BigInteger new_node(java.math.BigInteger value) {
        node_data = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(node_data), java.util.stream.Stream.of(value)).toArray(java.math.BigInteger[]::new)));
        left_child = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(left_child), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
        right_child = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(right_child), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
        return new java.math.BigInteger(String.valueOf(node_data.length)).subtract(java.math.BigInteger.valueOf(1));
    }

    static java.math.BigInteger build_tree() {
        java.math.BigInteger root = new java.math.BigInteger(String.valueOf(new_node(java.math.BigInteger.valueOf(1))));
        java.math.BigInteger n2_1 = new java.math.BigInteger(String.valueOf(new_node(java.math.BigInteger.valueOf(2))));
        java.math.BigInteger n5_1 = new java.math.BigInteger(String.valueOf(new_node(java.math.BigInteger.valueOf(5))));
        java.math.BigInteger n3_1 = new java.math.BigInteger(String.valueOf(new_node(java.math.BigInteger.valueOf(3))));
        java.math.BigInteger n4_1 = new java.math.BigInteger(String.valueOf(new_node(java.math.BigInteger.valueOf(4))));
        java.math.BigInteger n6_1 = new java.math.BigInteger(String.valueOf(new_node(java.math.BigInteger.valueOf(6))));
left_child[(int)(((java.math.BigInteger)(root)).longValue())] = new java.math.BigInteger(String.valueOf(n2_1));
right_child[(int)(((java.math.BigInteger)(root)).longValue())] = new java.math.BigInteger(String.valueOf(n5_1));
left_child[(int)(((java.math.BigInteger)(n2_1)).longValue())] = new java.math.BigInteger(String.valueOf(n3_1));
right_child[(int)(((java.math.BigInteger)(n2_1)).longValue())] = new java.math.BigInteger(String.valueOf(n4_1));
right_child[(int)(((java.math.BigInteger)(n5_1)).longValue())] = new java.math.BigInteger(String.valueOf(n6_1));
        return root;
    }

    static java.math.BigInteger[] flatten(java.math.BigInteger root) {
        if (root.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
            return new java.math.BigInteger[]{};
        }
        java.math.BigInteger[] res_1 = ((java.math.BigInteger[])(((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(node_data[(int)(((java.math.BigInteger)(root)).longValue())]))}))));
        java.math.BigInteger[] left_vals_1 = ((java.math.BigInteger[])(flatten(new java.math.BigInteger(String.valueOf(left_child[(int)(((java.math.BigInteger)(root)).longValue())])))));
        java.math.BigInteger[] right_vals_1 = ((java.math.BigInteger[])(flatten(new java.math.BigInteger(String.valueOf(right_child[(int)(((java.math.BigInteger)(root)).longValue())])))));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(left_vals_1.length))) < 0) {
            res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(left_vals_1[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(right_vals_1.length))) < 0) {
            res_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(right_vals_1[(int)(((java.math.BigInteger)(i_1)).longValue())])).toArray(java.math.BigInteger[]::new)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res_1;
    }

    static void display(java.math.BigInteger[] values) {
        String s = "";
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(values.length))) < 0) {
            if (i_3.compareTo(java.math.BigInteger.valueOf(0)) == 0) {
                s = _p(_geto(values, ((Number)(i_3)).intValue()));
            } else {
                s = s + " " + _p(_geto(values, ((Number)(i_3)).intValue()));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        System.out.println(s);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            node_data = ((java.math.BigInteger[])(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}))));
            left_child = ((java.math.BigInteger[])(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}))));
            right_child = ((java.math.BigInteger[])(((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0)}))));
            System.out.println("Flattened Linked List:");
            root_1 = new java.math.BigInteger(String.valueOf(build_tree()));
            vals = ((java.math.BigInteger[])(flatten(new java.math.BigInteger(String.valueOf(root_1)))));
            display(((java.math.BigInteger[])(vals)));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
