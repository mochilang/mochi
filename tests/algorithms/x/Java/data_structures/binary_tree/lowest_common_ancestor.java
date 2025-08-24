public class Main {

    static java.math.BigInteger pow2(java.math.BigInteger exp) {
        java.math.BigInteger res = java.math.BigInteger.valueOf(1);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(exp) < 0) {
            res = new java.math.BigInteger(String.valueOf(res.multiply(java.math.BigInteger.valueOf(2))));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return res;
    }

    static java.math.BigInteger[][] create_sparse(java.math.BigInteger max_node, java.math.BigInteger[][] parent) {
        java.math.BigInteger j = java.math.BigInteger.valueOf(1);
        while (pow2(new java.math.BigInteger(String.valueOf(j))).compareTo(max_node) < 0) {
            java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
            while (i_3.compareTo(max_node) <= 0) {
parent[(int)(((java.math.BigInteger)(j)).longValue())][(int)(((java.math.BigInteger)(i_3)).longValue())] = new java.math.BigInteger(String.valueOf(parent[(int)(((java.math.BigInteger)(j.subtract(java.math.BigInteger.valueOf(1)))).longValue())][(int)(((java.math.BigInteger)(parent[(int)(((java.math.BigInteger)(j.subtract(java.math.BigInteger.valueOf(1)))).longValue())][(int)(((java.math.BigInteger)(i_3)).longValue())])).longValue())]));
                i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
            }
            j = new java.math.BigInteger(String.valueOf(j.add(java.math.BigInteger.valueOf(1))));
        }
        return parent;
    }

    static java.math.BigInteger lowest_common_ancestor(java.math.BigInteger u, java.math.BigInteger v, java.math.BigInteger[] level, java.math.BigInteger[][] parent) {
        if (level[(int)(((java.math.BigInteger)(u)).longValue())].compareTo(level[(int)(((java.math.BigInteger)(v)).longValue())]) < 0) {
            java.math.BigInteger temp = new java.math.BigInteger(String.valueOf(u));
            u = new java.math.BigInteger(String.valueOf(v));
            v = new java.math.BigInteger(String.valueOf(temp));
        }
        java.math.BigInteger i_5 = java.math.BigInteger.valueOf(18);
        while (i_5.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            if (level[(int)(((java.math.BigInteger)(u)).longValue())].subtract(pow2(new java.math.BigInteger(String.valueOf(i_5)))).compareTo(level[(int)(((java.math.BigInteger)(v)).longValue())]) >= 0) {
                u = new java.math.BigInteger(String.valueOf(parent[(int)(((java.math.BigInteger)(i_5)).longValue())][(int)(((java.math.BigInteger)(u)).longValue())]));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.subtract(java.math.BigInteger.valueOf(1))));
        }
        if (u.compareTo(v) == 0) {
            return u;
        }
        i_5 = java.math.BigInteger.valueOf(18);
        while (i_5.compareTo(java.math.BigInteger.valueOf(0)) >= 0) {
            java.math.BigInteger pu_1 = new java.math.BigInteger(String.valueOf(parent[(int)(((java.math.BigInteger)(i_5)).longValue())][(int)(((java.math.BigInteger)(u)).longValue())]));
            java.math.BigInteger pv_1 = new java.math.BigInteger(String.valueOf(parent[(int)(((java.math.BigInteger)(i_5)).longValue())][(int)(((java.math.BigInteger)(v)).longValue())]));
            if (pu_1.compareTo(java.math.BigInteger.valueOf(0)) != 0 && pu_1.compareTo(pv_1) != 0) {
                u = new java.math.BigInteger(String.valueOf(pu_1));
                v = new java.math.BigInteger(String.valueOf(pv_1));
            }
            i_5 = new java.math.BigInteger(String.valueOf(i_5.subtract(java.math.BigInteger.valueOf(1))));
        }
        return parent[(int)(0L)][(int)(((java.math.BigInteger)(u)).longValue())];
    }

    static void breadth_first_search(java.math.BigInteger[] level, java.math.BigInteger[][] parent, java.math.BigInteger max_node, java.util.Map<java.math.BigInteger,java.math.BigInteger[]> graph, java.math.BigInteger root) {
level[(int)(((java.math.BigInteger)(root)).longValue())] = java.math.BigInteger.valueOf(0);
        java.math.BigInteger[] q_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        q_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(q_1), java.util.stream.Stream.of(root)).toArray(java.math.BigInteger[]::new)));
        java.math.BigInteger head_1 = java.math.BigInteger.valueOf(0);
        while (head_1.compareTo(new java.math.BigInteger(String.valueOf(q_1.length))) < 0) {
            java.math.BigInteger u_1 = new java.math.BigInteger(String.valueOf(q_1[(int)(((java.math.BigInteger)(head_1)).longValue())]));
            head_1 = new java.math.BigInteger(String.valueOf(head_1.add(java.math.BigInteger.valueOf(1))));
            java.math.BigInteger[] adj_1 = (java.math.BigInteger[])(((java.math.BigInteger[])(graph).get(u_1)));
            java.math.BigInteger j_2 = java.math.BigInteger.valueOf(0);
            while (j_2.compareTo(new java.math.BigInteger(String.valueOf(adj_1.length))) < 0) {
                java.math.BigInteger v_1 = new java.math.BigInteger(String.valueOf(adj_1[(int)(((java.math.BigInteger)(j_2)).longValue())]));
                if (level[(int)(((java.math.BigInteger)(v_1)).longValue())].compareTo((java.math.BigInteger.valueOf(1)).negate()) == 0) {
level[(int)(((java.math.BigInteger)(v_1)).longValue())] = new java.math.BigInteger(String.valueOf(level[(int)(((java.math.BigInteger)(u_1)).longValue())].add(java.math.BigInteger.valueOf(1))));
parent[(int)(0L)][(int)(((java.math.BigInteger)(v_1)).longValue())] = new java.math.BigInteger(String.valueOf(u_1));
                    q_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(q_1), java.util.stream.Stream.of(v_1)).toArray(java.math.BigInteger[]::new)));
                }
                j_2 = new java.math.BigInteger(String.valueOf(j_2.add(java.math.BigInteger.valueOf(1))));
            }
        }
    }

    static void main() {
        java.math.BigInteger max_node = java.math.BigInteger.valueOf(13);
        java.math.BigInteger[][] parent_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        java.math.BigInteger i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(java.math.BigInteger.valueOf(20)) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_4 = java.math.BigInteger.valueOf(0);
            while (j_4.compareTo(max_node.add(java.math.BigInteger.valueOf(10))) < 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(0)).toArray(java.math.BigInteger[]::new)));
                j_4 = new java.math.BigInteger(String.valueOf(j_4.add(java.math.BigInteger.valueOf(1))));
            }
            parent_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(parent_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{row_1})).toArray(java.math.BigInteger[][]::new)));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] level_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        i_7 = java.math.BigInteger.valueOf(0);
        while (i_7.compareTo(max_node.add(java.math.BigInteger.valueOf(10))) < 0) {
            level_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(level_1), java.util.stream.Stream.of((java.math.BigInteger.valueOf(1)).negate())).toArray(java.math.BigInteger[]::new)));
            i_7 = new java.math.BigInteger(String.valueOf(i_7.add(java.math.BigInteger.valueOf(1))));
        }
        java.util.Map<java.math.BigInteger,java.math.BigInteger[]> graph_1 = ((java.util.Map<java.math.BigInteger,java.math.BigInteger[]>)(new java.util.LinkedHashMap<java.math.BigInteger, java.math.BigInteger[]>()));
graph_1.put(1, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(2), java.math.BigInteger.valueOf(3), java.math.BigInteger.valueOf(4)})));
graph_1.put(2, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(5)})));
graph_1.put(3, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7)})));
graph_1.put(4, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(8)})));
graph_1.put(5, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(10)})));
graph_1.put(6, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(11)})));
graph_1.put(7, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
graph_1.put(8, ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(13)})));
graph_1.put(9, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
graph_1.put(10, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
graph_1.put(11, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
graph_1.put(12, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
graph_1.put(13, ((java.math.BigInteger[])(new java.math.BigInteger[]{})));
        breadth_first_search(((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)), new java.math.BigInteger(String.valueOf(max_node)), graph_1, java.math.BigInteger.valueOf(1));
        parent_1 = ((java.math.BigInteger[][])(create_sparse(new java.math.BigInteger(String.valueOf(max_node)), ((java.math.BigInteger[][])(parent_1)))));
        System.out.println("LCA of node 1 and 3 is: " + _p(lowest_common_ancestor(java.math.BigInteger.valueOf(1), java.math.BigInteger.valueOf(3), ((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)))));
        System.out.println("LCA of node 5 and 6 is: " + _p(lowest_common_ancestor(java.math.BigInteger.valueOf(5), java.math.BigInteger.valueOf(6), ((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)))));
        System.out.println("LCA of node 7 and 11 is: " + _p(lowest_common_ancestor(java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(11), ((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)))));
        System.out.println("LCA of node 6 and 7 is: " + _p(lowest_common_ancestor(java.math.BigInteger.valueOf(6), java.math.BigInteger.valueOf(7), ((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)))));
        System.out.println("LCA of node 4 and 12 is: " + _p(lowest_common_ancestor(java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(12), ((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)))));
        System.out.println("LCA of node 8 and 8 is: " + _p(lowest_common_ancestor(java.math.BigInteger.valueOf(8), java.math.BigInteger.valueOf(8), ((java.math.BigInteger[])(level_1)), ((java.math.BigInteger[][])(parent_1)))));
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
