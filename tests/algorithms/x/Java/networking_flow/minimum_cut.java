public class Main {
    static java.math.BigInteger[][] test_graph = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(16), java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(14), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(20)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)}))}));
    static java.math.BigInteger[][] result;

    static boolean bfs(java.math.BigInteger[][] graph, java.math.BigInteger s, java.math.BigInteger t, java.math.BigInteger[] parent) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(graph.length))) < 0) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] queue_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(s))}));
        java.math.BigInteger head_1 = java.math.BigInteger.valueOf(0);
visited[(int)(((java.math.BigInteger)(s)).longValue())] = true;
        while (head_1.compareTo(new java.math.BigInteger(String.valueOf(queue_1.length))) < 0) {
            java.math.BigInteger u_1 = new java.math.BigInteger(String.valueOf(queue_1[_idx((queue_1).length, ((java.math.BigInteger)(head_1)).longValue())]));
            head_1 = new java.math.BigInteger(String.valueOf(head_1.add(java.math.BigInteger.valueOf(1))));
            java.math.BigInteger ind_1 = java.math.BigInteger.valueOf(0);
            while (ind_1.compareTo(new java.math.BigInteger(String.valueOf(graph[_idx((graph).length, ((java.math.BigInteger)(u_1)).longValue())].length))) < 0) {
                if ((visited[_idx((visited).length, ((java.math.BigInteger)(ind_1)).longValue())] == false) && graph[_idx((graph).length, ((java.math.BigInteger)(u_1)).longValue())][_idx((graph[_idx((graph).length, ((java.math.BigInteger)(u_1)).longValue())]).length, ((java.math.BigInteger)(ind_1)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    queue_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(ind_1)))).toArray(java.math.BigInteger[]::new)));
visited[(int)(((java.math.BigInteger)(ind_1)).longValue())] = true;
parent[(int)(((java.math.BigInteger)(ind_1)).longValue())] = new java.math.BigInteger(String.valueOf(u_1));
                }
                ind_1 = new java.math.BigInteger(String.valueOf(ind_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return visited[_idx((visited).length, ((java.math.BigInteger)(t)).longValue())];
    }

    static java.math.BigInteger[][] mincut(java.math.BigInteger[][] graph, java.math.BigInteger source, java.math.BigInteger sink) {
        java.math.BigInteger[][] g = ((java.math.BigInteger[][])(graph));
        java.math.BigInteger[] parent_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(g.length))) < 0) {
            parent_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parent_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())))).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[][] temp_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(g.length))) < 0) {
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(g[_idx((g).length, ((java.math.BigInteger)(i_3)).longValue())].length))) < 0) {
                row_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(g[_idx((g).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((g[_idx((g).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_1)).longValue())])))).toArray(java.math.BigInteger[]::new)));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
            temp_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(temp_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(row_1))})).toArray(java.math.BigInteger[][]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        while (bfs(((java.math.BigInteger[][])(g)), new java.math.BigInteger(String.valueOf(source)), new java.math.BigInteger(String.valueOf(sink)), ((java.math.BigInteger[])(parent_1)))) {
            java.math.BigInteger path_flow_1 = java.math.BigInteger.valueOf(1000000000);
            java.math.BigInteger s_1 = new java.math.BigInteger(String.valueOf(sink));
            while (s_1.compareTo(source) != 0) {
                java.math.BigInteger p_1 = new java.math.BigInteger(String.valueOf(parent_1[_idx((parent_1).length, ((java.math.BigInteger)(s_1)).longValue())]));
                java.math.BigInteger cap_1 = new java.math.BigInteger(String.valueOf(g[_idx((g).length, ((java.math.BigInteger)(p_1)).longValue())][_idx((g[_idx((g).length, ((java.math.BigInteger)(p_1)).longValue())]).length, ((java.math.BigInteger)(s_1)).longValue())]));
                if (cap_1.compareTo(path_flow_1) < 0) {
                    path_flow_1 = new java.math.BigInteger(String.valueOf(cap_1));
                }
                s_1 = new java.math.BigInteger(String.valueOf(p_1));
            }
            java.math.BigInteger v_1 = new java.math.BigInteger(String.valueOf(sink));
            while (v_1.compareTo(source) != 0) {
                java.math.BigInteger u_3 = new java.math.BigInteger(String.valueOf(parent_1[_idx((parent_1).length, ((java.math.BigInteger)(v_1)).longValue())]));
g[_idx((g).length, ((java.math.BigInteger)(u_3)).longValue())][(int)(((java.math.BigInteger)(v_1)).longValue())] = new java.math.BigInteger(String.valueOf(g[_idx((g).length, ((java.math.BigInteger)(u_3)).longValue())][_idx((g[_idx((g).length, ((java.math.BigInteger)(u_3)).longValue())]).length, ((java.math.BigInteger)(v_1)).longValue())].subtract(path_flow_1)));
g[_idx((g).length, ((java.math.BigInteger)(v_1)).longValue())][(int)(((java.math.BigInteger)(u_3)).longValue())] = new java.math.BigInteger(String.valueOf(g[_idx((g).length, ((java.math.BigInteger)(v_1)).longValue())][_idx((g[_idx((g).length, ((java.math.BigInteger)(v_1)).longValue())]).length, ((java.math.BigInteger)(u_3)).longValue())].add(path_flow_1)));
                v_1 = new java.math.BigInteger(String.valueOf(u_3));
            }
        }
        java.math.BigInteger[][] res_1 = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{}));
        i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(g.length))) < 0) {
            java.math.BigInteger j_3 = java.math.BigInteger.valueOf(0);
            while (j_3.compareTo(new java.math.BigInteger(String.valueOf(g[_idx((g).length, 0L)].length))) < 0) {
                if (g[_idx((g).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((g[_idx((g).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) == 0 && temp_1[_idx((temp_1).length, ((java.math.BigInteger)(i_3)).longValue())][_idx((temp_1[_idx((temp_1).length, ((java.math.BigInteger)(i_3)).longValue())]).length, ((java.math.BigInteger)(j_3)).longValue())].compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    res_1 = ((java.math.BigInteger[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.stream.Stream.of(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{new java.math.BigInteger(String.valueOf(i_3)), new java.math.BigInteger(String.valueOf(j_3))}))})).toArray(java.math.BigInteger[][]::new)));
                }
                j_3 = new java.math.BigInteger(String.valueOf(j_3.add(java.math.BigInteger.valueOf(1))));
            }
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return ((java.math.BigInteger[][])(res_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            result = ((java.math.BigInteger[][])(mincut(((java.math.BigInteger[][])(test_graph)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5))));
            System.out.println(_p(result));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof java.util.Map<?, ?>) {
            StringBuilder sb = new StringBuilder("{");
            boolean first = true;
            for (java.util.Map.Entry<?, ?> e : ((java.util.Map<?, ?>) v).entrySet()) {
                if (!first) sb.append(", ");
                sb.append(_p(e.getKey()));
                sb.append("=");
                sb.append(_p(e.getValue()));
                first = false;
            }
            sb.append("}");
            return sb.toString();
        }
        if (v instanceof java.util.List<?>) {
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Object e : (java.util.List<?>) v) {
                if (!first) sb.append(", ");
                sb.append(_p(e));
                first = false;
            }
            sb.append("]");
            return sb.toString();
        }
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
