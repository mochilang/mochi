public class Main {
    static java.math.BigInteger INF = java.math.BigInteger.valueOf(1000000000);
    static java.math.BigInteger[][] graph = ((java.math.BigInteger[][])(new java.math.BigInteger[][]{((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(16), java.math.BigInteger.valueOf(13), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(10), java.math.BigInteger.valueOf(12), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(4), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(14), java.math.BigInteger.valueOf(0)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(9), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(20)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(7), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(4)})), ((java.math.BigInteger[])(new java.math.BigInteger[]{java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(0)}))}));

    static boolean breadth_first_search(java.math.BigInteger[][] graph, java.math.BigInteger source, java.math.BigInteger sink, java.math.BigInteger[] parent) {
        boolean[] visited = ((boolean[])(new boolean[]{}));
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(0);
        while (i_1.compareTo(new java.math.BigInteger(String.valueOf(graph.length))) < 0) {
            visited = ((boolean[])(appendBool(visited, false)));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger[] queue_1 = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        queue_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(source)))).toArray(java.math.BigInteger[]::new)));
visited[(int)(((java.math.BigInteger)(source)).longValue())] = true;
        java.math.BigInteger head_1 = java.math.BigInteger.valueOf(0);
        while (head_1.compareTo(new java.math.BigInteger(String.valueOf(queue_1.length))) < 0) {
            java.math.BigInteger u_1 = new java.math.BigInteger(String.valueOf(queue_1[_idx((queue_1).length, ((java.math.BigInteger)(head_1)).longValue())]));
            head_1 = new java.math.BigInteger(String.valueOf(head_1.add(java.math.BigInteger.valueOf(1))));
            java.math.BigInteger[] row_1 = ((java.math.BigInteger[])(graph[_idx((graph).length, ((java.math.BigInteger)(u_1)).longValue())]));
            java.math.BigInteger ind_1 = java.math.BigInteger.valueOf(0);
            while (ind_1.compareTo(new java.math.BigInteger(String.valueOf(row_1.length))) < 0) {
                java.math.BigInteger capacity_1 = new java.math.BigInteger(String.valueOf(row_1[_idx((row_1).length, ((java.math.BigInteger)(ind_1)).longValue())]));
                if ((visited[_idx((visited).length, ((java.math.BigInteger)(ind_1)).longValue())] == false) && capacity_1.compareTo(java.math.BigInteger.valueOf(0)) > 0) {
                    queue_1 = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf(ind_1)))).toArray(java.math.BigInteger[]::new)));
visited[(int)(((java.math.BigInteger)(ind_1)).longValue())] = true;
parent[(int)(((java.math.BigInteger)(ind_1)).longValue())] = new java.math.BigInteger(String.valueOf(u_1));
                }
                ind_1 = new java.math.BigInteger(String.valueOf(ind_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return visited[_idx((visited).length, ((java.math.BigInteger)(sink)).longValue())];
    }

    static java.math.BigInteger ford_fulkerson(java.math.BigInteger[][] graph, java.math.BigInteger source, java.math.BigInteger sink) {
        java.math.BigInteger[] parent = ((java.math.BigInteger[])(new java.math.BigInteger[]{}));
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(0);
        while (i_3.compareTo(new java.math.BigInteger(String.valueOf(graph.length))) < 0) {
            parent = ((java.math.BigInteger[])(java.util.stream.Stream.concat(java.util.Arrays.stream(parent), java.util.stream.Stream.of(new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate())))).toArray(java.math.BigInteger[]::new)));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        java.math.BigInteger max_flow_1 = java.math.BigInteger.valueOf(0);
        while (breadth_first_search(((java.math.BigInteger[][])(graph)), new java.math.BigInteger(String.valueOf(source)), new java.math.BigInteger(String.valueOf(sink)), ((java.math.BigInteger[])(parent)))) {
            java.math.BigInteger path_flow_1 = new java.math.BigInteger(String.valueOf(INF));
            java.math.BigInteger s_1 = new java.math.BigInteger(String.valueOf(sink));
            while (s_1.compareTo(source) != 0) {
                java.math.BigInteger prev_1 = new java.math.BigInteger(String.valueOf(parent[_idx((parent).length, ((java.math.BigInteger)(s_1)).longValue())]));
                java.math.BigInteger cap_1 = new java.math.BigInteger(String.valueOf(graph[_idx((graph).length, ((java.math.BigInteger)(prev_1)).longValue())][_idx((graph[_idx((graph).length, ((java.math.BigInteger)(prev_1)).longValue())]).length, ((java.math.BigInteger)(s_1)).longValue())]));
                if (cap_1.compareTo(path_flow_1) < 0) {
                    path_flow_1 = new java.math.BigInteger(String.valueOf(cap_1));
                }
                s_1 = new java.math.BigInteger(String.valueOf(prev_1));
            }
            max_flow_1 = new java.math.BigInteger(String.valueOf(max_flow_1.add(path_flow_1)));
            java.math.BigInteger v_1 = new java.math.BigInteger(String.valueOf(sink));
            while (v_1.compareTo(source) != 0) {
                java.math.BigInteger u_3 = new java.math.BigInteger(String.valueOf(parent[_idx((parent).length, ((java.math.BigInteger)(v_1)).longValue())]));
graph[_idx((graph).length, ((java.math.BigInteger)(u_3)).longValue())][(int)(((java.math.BigInteger)(v_1)).longValue())] = new java.math.BigInteger(String.valueOf(graph[_idx((graph).length, ((java.math.BigInteger)(u_3)).longValue())][_idx((graph[_idx((graph).length, ((java.math.BigInteger)(u_3)).longValue())]).length, ((java.math.BigInteger)(v_1)).longValue())].subtract(path_flow_1)));
graph[_idx((graph).length, ((java.math.BigInteger)(v_1)).longValue())][(int)(((java.math.BigInteger)(u_3)).longValue())] = new java.math.BigInteger(String.valueOf(graph[_idx((graph).length, ((java.math.BigInteger)(v_1)).longValue())][_idx((graph[_idx((graph).length, ((java.math.BigInteger)(v_1)).longValue())]).length, ((java.math.BigInteger)(u_3)).longValue())].add(path_flow_1)));
                v_1 = new java.math.BigInteger(String.valueOf(u_3));
            }
            java.math.BigInteger j_1 = java.math.BigInteger.valueOf(0);
            while (j_1.compareTo(new java.math.BigInteger(String.valueOf(parent.length))) < 0) {
parent[(int)(((java.math.BigInteger)(j_1)).longValue())] = new java.math.BigInteger(String.valueOf((java.math.BigInteger.valueOf(1)).negate()));
                j_1 = new java.math.BigInteger(String.valueOf(j_1.add(java.math.BigInteger.valueOf(1))));
            }
        }
        return new java.math.BigInteger(String.valueOf(max_flow_1));
    }
    public static void main(String[] args) {
        System.out.println(_p(ford_fulkerson(((java.math.BigInteger[][])(graph)), java.math.BigInteger.valueOf(0), java.math.BigInteger.valueOf(5))));
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
