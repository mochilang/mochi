public class Main {
    static class Neighbor {
        long node;
        long cost;
        Neighbor(long node, long cost) {
            this.node = node;
            this.cost = cost;
        }
        Neighbor() {}
        @Override public String toString() {
            return String.format("{'node': %s, 'cost': %s}", String.valueOf(node), String.valueOf(cost));
        }
    }

    static class EdgePair {
        long u;
        long v;
        EdgePair(long u, long v) {
            this.u = u;
            this.v = v;
        }
        EdgePair() {}
        @Override public String toString() {
            return String.format("{'u': %s, 'v': %s}", String.valueOf(u), String.valueOf(v));
        }
    }


    static EdgePair[] prims_algorithm(java.util.Map<Long,Neighbor[]> adjacency) {
        java.util.Map<Long,Boolean> visited = ((java.util.Map<Long,Boolean>)(new java.util.LinkedHashMap<Long, Boolean>()));
visited.put(0, true);
        EdgePair[] mst_1 = ((EdgePair[])(new EdgePair[]{}));
        long count_1 = 1L;
        long total_1 = 0L;
        for (long k : adjacency.keySet()) {
            total_1 = total_1 + 1;
        }
        while (count_1 < total_1) {
            long best_u_1 = 0L;
            long best_v_1 = 0L;
            long best_cost_1 = 2147483647L;
            for (long u_str : adjacency.keySet()) {
                long u_1 = ((Number)(u_str)).intValue();
                if (((boolean)(visited).getOrDefault(u_1, false))) {
                    for (Neighbor n : ((Neighbor[])(adjacency).get(u_1))) {
                        if (!((boolean)(visited).getOrDefault(n.node, false)) && n.cost < best_cost_1) {
                            best_cost_1 = n.cost;
                            best_u_1 = u_1;
                            best_v_1 = n.node;
                        }
                    }
                }
            }
visited.put(best_v_1, true);
            mst_1 = ((EdgePair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst_1), java.util.stream.Stream.of(new EdgePair(best_u_1, best_v_1))).toArray(EdgePair[]::new)));
            count_1 = count_1 + 1;
        }
        return mst_1;
    }

    static boolean test_prim_successful_result() {
        long[][] edges = ((long[][])(new long[][]{new long[]{0, 1, 4}, new long[]{0, 7, 8}, new long[]{1, 2, 8}, new long[]{7, 8, 7}, new long[]{7, 6, 1}, new long[]{2, 8, 2}, new long[]{8, 6, 6}, new long[]{2, 3, 7}, new long[]{2, 5, 4}, new long[]{6, 5, 2}, new long[]{3, 5, 14}, new long[]{3, 4, 9}, new long[]{5, 4, 10}, new long[]{1, 7, 11}}));
        java.util.Map<Long,Neighbor[]> adjacency_1 = ((java.util.Map<Long,Neighbor[]>)(new java.util.LinkedHashMap<Long, Neighbor[]>()));
        for (long[] e : edges) {
            long u_3 = e[(int)((long)(0))];
            long v_1 = e[(int)((long)(1))];
            long w_1 = e[(int)((long)(2))];
            if (!(adjacency_1.containsKey(u_3))) {
adjacency_1.put(u_3, ((Neighbor[])(new Neighbor[]{})));
            }
            if (!(adjacency_1.containsKey(v_1))) {
adjacency_1.put(v_1, ((Neighbor[])(new Neighbor[]{})));
            }
adjacency_1.put(u_3, ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((Neighbor[])(adjacency_1).get(u_3))), java.util.stream.Stream.of(new Neighbor(v_1, w_1))).toArray(Neighbor[]::new))));
adjacency_1.put(v_1, ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((Neighbor[])(adjacency_1).get(v_1))), java.util.stream.Stream.of(new Neighbor(u_3, w_1))).toArray(Neighbor[]::new))));
        }
        EdgePair[] result_1 = ((EdgePair[])(prims_algorithm(adjacency_1)));
        java.util.Map<String,Boolean> seen_1 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (EdgePair e : result_1) {
            String key1_1 = _p(e.u) + "," + _p(e.v);
            String key2_1 = _p(e.v) + "," + _p(e.u);
seen_1.put(key1_1, true);
seen_1.put(key2_1, true);
        }
        long[][] expected_1 = ((long[][])(new long[][]{new long[]{7, 6, 1}, new long[]{2, 8, 2}, new long[]{6, 5, 2}, new long[]{0, 1, 4}, new long[]{2, 5, 4}, new long[]{2, 3, 7}, new long[]{0, 7, 8}, new long[]{3, 4, 9}}));
        for (long[] ans : expected_1) {
            String key_1 = _p(_geti(ans, ((Number)(0)).intValue())) + "," + _p(_geti(ans, ((Number)(1)).intValue()));
            if (!((boolean)(seen_1).getOrDefault(key_1, false))) {
                return false;
            }
        }
        return true;
    }
    public static void main(String[] args) {
        System.out.println(test_prim_successful_result());
        System.out.println(true ? "True" : "False");
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Long _geti(long[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
