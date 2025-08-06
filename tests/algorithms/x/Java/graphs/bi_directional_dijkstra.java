public class Main {
    static class Edge {
        String to;
        int cost;
        Edge(String to, int cost) {
            this.to = to;
            this.cost = cost;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'to': '%s', 'cost': %s}", String.valueOf(to), String.valueOf(cost));
        }
    }

    static class QItem {
        String node;
        int cost;
        QItem(String node, int cost) {
            this.node = node;
            this.cost = cost;
        }
        QItem() {}
        @Override public String toString() {
            return String.format("{'node': '%s', 'cost': %s}", String.valueOf(node), String.valueOf(cost));
        }
    }

    static class PassResult {
        QItem[] queue;
        int dist;
        PassResult(QItem[] queue, int dist) {
            this.queue = queue;
            this.dist = dist;
        }
        PassResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'dist': %s}", String.valueOf(queue), String.valueOf(dist));
        }
    }

    static java.util.Map<String,Edge[]> graph_fwd = null;
    static java.util.Map<String,Edge[]> graph_bwd = null;

    static int get_min_index(QItem[] q) {
        int idx = 0;
        int i = 1;
        while (i < q.length) {
            if (q[i].cost < q[idx].cost) {
                idx = i;
            }
            i = i + 1;
        }
        return idx;
    }

    static QItem[] remove_at(QItem[] q, int idx) {
        QItem[] res = ((QItem[])(new QItem[]{}));
        int i_1 = 0;
        while (i_1 < q.length) {
            if (i_1 != idx) {
                res = ((QItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(q[i_1])).toArray(QItem[]::new)));
            }
            i_1 = i_1 + 1;
        }
        return res;
    }

    static PassResult pass_and_relaxation(java.util.Map<String,Edge[]> graph, String v, java.util.Map<String,Boolean> visited_forward, java.util.Map<String,Boolean> visited_backward, java.util.Map<String,Integer> cst_fwd, java.util.Map<String,Integer> cst_bwd, QItem[] queue, java.util.Map<String,String> parent, int shortest_distance) {
        QItem[] q = ((QItem[])(queue));
        int sd = shortest_distance;
        for (Edge e : ((Edge[])(graph).get(v))) {
            String nxt = e.to;
            int d = e.cost;
            if (((Boolean)(visited_forward.containsKey(nxt)))) {
                continue;
            }
            int old_cost = cst_fwd.containsKey(nxt) ? ((int)(cst_fwd).getOrDefault(nxt, 0)) : 2147483647;
            int new_cost = (int)(((int)(cst_fwd).getOrDefault(v, 0))) + d;
            if (new_cost < old_cost) {
                q = ((QItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(q), java.util.stream.Stream.of(new QItem(nxt, new_cost))).toArray(QItem[]::new)));
cst_fwd.put(nxt, new_cost);
parent.put(nxt, v);
            }
            if (((Boolean)(visited_backward.containsKey(nxt)))) {
                int alt = (int)(((int)(cst_fwd).getOrDefault(v, 0))) + d + (int)(((int)(cst_bwd).getOrDefault(nxt, 0)));
                if (alt < sd) {
                    sd = alt;
                }
            }
        }
        return new PassResult(q, sd);
    }

    static int bidirectional_dij(String source, String destination, java.util.Map<String,Edge[]> graph_forward, java.util.Map<String,Edge[]> graph_backward) {
        int shortest_path_distance = -1;
        java.util.Map<String,Boolean> visited_forward = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        java.util.Map<String,Boolean> visited_backward = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        java.util.Map<String,Integer> cst_fwd = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
cst_fwd.put(source, 0);
        java.util.Map<String,Integer> cst_bwd = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
cst_bwd.put(destination, 0);
        java.util.Map<String,String> parent_forward = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
parent_forward.put(source, "");
        java.util.Map<String,String> parent_backward = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
parent_backward.put(destination, "");
        QItem[] queue_forward = ((QItem[])(new QItem[]{}));
        queue_forward = ((QItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_forward), java.util.stream.Stream.of(new QItem(source, 0))).toArray(QItem[]::new)));
        QItem[] queue_backward = ((QItem[])(new QItem[]{}));
        queue_backward = ((QItem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_backward), java.util.stream.Stream.of(new QItem(destination, 0))).toArray(QItem[]::new)));
        int shortest_distance = 2147483647;
        if ((source.equals(destination))) {
            return 0;
        }
        while (queue_forward.length > 0 && queue_backward.length > 0) {
            int idx_f = get_min_index(((QItem[])(queue_forward)));
            QItem item_f = queue_forward[idx_f];
            queue_forward = ((QItem[])(remove_at(((QItem[])(queue_forward)), idx_f)));
            String v_fwd = item_f.node;
visited_forward.put(v_fwd, true);
            int idx_b = get_min_index(((QItem[])(queue_backward)));
            QItem item_b = queue_backward[idx_b];
            queue_backward = ((QItem[])(remove_at(((QItem[])(queue_backward)), idx_b)));
            String v_bwd = item_b.node;
visited_backward.put(v_bwd, true);
            PassResult res_f = pass_and_relaxation(graph_forward, v_fwd, visited_forward, visited_backward, cst_fwd, cst_bwd, ((QItem[])(queue_forward)), parent_forward, shortest_distance);
            queue_forward = ((QItem[])(res_f.queue));
            shortest_distance = res_f.dist;
            PassResult res_b = pass_and_relaxation(graph_backward, v_bwd, visited_backward, visited_forward, cst_bwd, cst_fwd, ((QItem[])(queue_backward)), parent_backward, shortest_distance);
            queue_backward = ((QItem[])(res_b.queue));
            shortest_distance = res_b.dist;
            if ((int)(((int)(cst_fwd).getOrDefault(v_fwd, 0))) + (int)(((int)(cst_bwd).getOrDefault(v_bwd, 0))) >= shortest_distance) {
                break;
            }
        }
        if (shortest_distance != 2147483647) {
            shortest_path_distance = shortest_distance;
        }
        return shortest_path_distance;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph_fwd = ((java.util.Map<String,Edge[]>)(new java.util.LinkedHashMap<String, Edge[]>(java.util.Map.ofEntries(java.util.Map.entry("B", ((Edge[])(new Edge[]{new Edge("C", 1)}))), java.util.Map.entry("C", ((Edge[])(new Edge[]{new Edge("D", 1)}))), java.util.Map.entry("D", ((Edge[])(new Edge[]{new Edge("F", 1)}))), java.util.Map.entry("E", ((Edge[])(new Edge[]{new Edge("B", 1), new Edge("G", 2)}))), java.util.Map.entry("F", new Edge[]{}), java.util.Map.entry("G", ((Edge[])(new Edge[]{new Edge("F", 1)})))))));
            graph_bwd = ((java.util.Map<String,Edge[]>)(new java.util.LinkedHashMap<String, Edge[]>(java.util.Map.ofEntries(java.util.Map.entry("B", ((Edge[])(new Edge[]{new Edge("E", 1)}))), java.util.Map.entry("C", ((Edge[])(new Edge[]{new Edge("B", 1)}))), java.util.Map.entry("D", ((Edge[])(new Edge[]{new Edge("C", 1)}))), java.util.Map.entry("F", ((Edge[])(new Edge[]{new Edge("D", 1), new Edge("G", 1)}))), java.util.Map.entry("E", new Edge[]{}), java.util.Map.entry("G", ((Edge[])(new Edge[]{new Edge("E", 2)})))))));
            System.out.println(_p(bidirectional_dij("E", "F", graph_fwd, graph_bwd)));
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
}
