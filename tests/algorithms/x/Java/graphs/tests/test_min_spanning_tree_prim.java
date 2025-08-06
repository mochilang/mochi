public class Main {
    static class Neighbor {
        int node;
        int cost;
        Neighbor(int node, int cost) {
            this.node = node;
            this.cost = cost;
        }
        Neighbor() {}
        @Override public String toString() {
            return String.format("{'node': %s, 'cost': %s}", String.valueOf(node), String.valueOf(cost));
        }
    }

    static class EdgePair {
        int u;
        int v;
        EdgePair(int u, int v) {
            this.u = u;
            this.v = v;
        }
        EdgePair() {}
        @Override public String toString() {
            return String.format("{'u': %s, 'v': %s}", String.valueOf(u), String.valueOf(v));
        }
    }


    static EdgePair[] prims_algorithm(java.util.Map<Integer,Neighbor[]> adjacency) {
        java.util.Map<Integer,Boolean> visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
visited.put(0, true);
        EdgePair[] mst = ((EdgePair[])(new EdgePair[]{}));
        int count = 1;
        int total = 0;
        for (int k : adjacency.keySet()) {
            total = total + 1;
        }
        while (count < total) {
            int best_u = 0;
            int best_v = 0;
            int best_cost = 2147483647;
            for (int u_str : adjacency.keySet()) {
                int u = ((Number)(u_str)).intValue();
                if (((boolean)(visited).getOrDefault(u, false))) {
                    for (Neighbor n : ((Neighbor[])(adjacency).get(u))) {
                        if (!((boolean)(visited).getOrDefault(n.node, false)) && n.cost < best_cost) {
                            best_cost = n.cost;
                            best_u = u;
                            best_v = n.node;
                        }
                    }
                }
            }
visited.put(best_v, true);
            mst = ((EdgePair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(mst), java.util.stream.Stream.of(new EdgePair(best_u, best_v))).toArray(EdgePair[]::new)));
            count = count + 1;
        }
        return mst;
    }

    static boolean test_prim_successful_result() {
        int[][] edges = ((int[][])(new int[][]{new int[]{0, 1, 4}, new int[]{0, 7, 8}, new int[]{1, 2, 8}, new int[]{7, 8, 7}, new int[]{7, 6, 1}, new int[]{2, 8, 2}, new int[]{8, 6, 6}, new int[]{2, 3, 7}, new int[]{2, 5, 4}, new int[]{6, 5, 2}, new int[]{3, 5, 14}, new int[]{3, 4, 9}, new int[]{5, 4, 10}, new int[]{1, 7, 11}}));
        java.util.Map<Integer,Neighbor[]> adjacency = ((java.util.Map<Integer,Neighbor[]>)(new java.util.LinkedHashMap<Integer, Neighbor[]>()));
        for (int[] e : edges) {
            int u_1 = e[0];
            int v = e[1];
            int w = e[2];
            if (!(Boolean)(adjacency.containsKey(u_1))) {
adjacency.put(u_1, ((Neighbor[])(new Neighbor[]{})));
            }
            if (!(Boolean)(adjacency.containsKey(v))) {
adjacency.put(v, ((Neighbor[])(new Neighbor[]{})));
            }
adjacency.put(u_1, ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((Neighbor[])(adjacency).get(u_1))), java.util.stream.Stream.of(new Neighbor(v, w))).toArray(Neighbor[]::new))));
adjacency.put(v, ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(((Neighbor[])(adjacency).get(v))), java.util.stream.Stream.of(new Neighbor(u_1, w))).toArray(Neighbor[]::new))));
        }
        EdgePair[] result = ((EdgePair[])(prims_algorithm(adjacency)));
        java.util.Map<String,Boolean> seen = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        for (EdgePair e : result) {
            String key1 = _p(e.u) + "," + _p(e.v);
            String key2 = _p(e.v) + "," + _p(e.u);
seen.put(key1, true);
seen.put(key2, true);
        }
        int[][] expected = ((int[][])(new int[][]{new int[]{7, 6, 1}, new int[]{2, 8, 2}, new int[]{6, 5, 2}, new int[]{0, 1, 4}, new int[]{2, 5, 4}, new int[]{2, 3, 7}, new int[]{0, 7, 8}, new int[]{3, 4, 9}}));
        for (int[] ans : expected) {
            String key = _p(_geti(ans, 0)) + "," + _p(_geti(ans, 1));
            if (!((boolean)(seen).getOrDefault(key, false))) {
                return false;
            }
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(test_prim_successful_result());
            System.out.println(true ? "True" : "False");
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
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
        return String.valueOf(v);
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
