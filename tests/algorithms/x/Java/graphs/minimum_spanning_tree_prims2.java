public class Main {
    static class QueueNode {
        String node;
        long weight;
        QueueNode(String node, long weight) {
            this.node = node;
            this.weight = weight;
        }
        QueueNode() {}
        @Override public String toString() {
            return String.format("{'node': '%s', 'weight': %s}", String.valueOf(node), String.valueOf(weight));
        }
    }

    static class MSTResult {
        java.util.Map<String,Long> dist;
        java.util.Map<String,String> parent;
        MSTResult(java.util.Map<String,Long> dist, java.util.Map<String,String> parent) {
            this.dist = dist;
            this.parent = parent;
        }
        MSTResult() {}
        @Override public String toString() {
            return String.format("{'dist': %s, 'parent': %s}", String.valueOf(dist), String.valueOf(parent));
        }
    }

    static java.util.Map<String,java.util.Map<String,Long>> graph = null;
    static MSTResult res;
    static java.util.Map<String,Long> dist_2;

    static MSTResult prims_algo(java.util.Map<String,java.util.Map<String,Long>> graph) {
        long INF = 2147483647L;
        java.util.Map<String,Long> dist_1 = ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>()));
        java.util.Map<String,String> parent_1 = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        QueueNode[] queue_1 = ((QueueNode[])(new QueueNode[]{}));
        for (String node : graph.keySet()) {
dist_1.put(node, (long)(INF));
parent_1.put(node, "");
            queue_1 = ((QueueNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue_1), java.util.stream.Stream.of(new QueueNode(node, INF))).toArray(QueueNode[]::new)));
        }
        if ((long)(queue_1.length) == 0L) {
            return new MSTResult(dist_1, parent_1);
        }
        long min_idx_1 = 0L;
        long i_1 = 1L;
        while ((long)(i_1) < (long)(queue_1.length)) {
            if ((long)(queue_1[(int)((long)(i_1))].weight) < (long)(queue_1[(int)((long)(min_idx_1))].weight)) {
                min_idx_1 = (long)(i_1);
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        QueueNode start_node_1 = queue_1[(int)((long)(min_idx_1))];
        String start_1 = start_node_1.node;
        QueueNode[] new_q_1 = ((QueueNode[])(new QueueNode[]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(queue_1.length)) {
            if ((long)(j_1) != (long)(min_idx_1)) {
                new_q_1 = ((QueueNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_q_1), java.util.stream.Stream.of(queue_1[(int)((long)(j_1))])).toArray(QueueNode[]::new)));
            }
            j_1 = (long)((long)(j_1) + 1L);
        }
        queue_1 = ((QueueNode[])(new_q_1));
dist_1.put(start_1, 0L);
        for (String neighbour : ((java.util.Map<String,Long>)(graph).get(start_1)).keySet()) {
            long w_1 = (long)(((long)(((java.util.Map<String,Long>)(graph).get(start_1))).getOrDefault(neighbour, 0L)));
            if ((long)(((long)(dist_1).getOrDefault(neighbour, 0L))) > (long)((long)(((long)(dist_1).getOrDefault(start_1, 0L))) + (long)(w_1))) {
dist_1.put(neighbour, (long)((long)(((long)(dist_1).getOrDefault(start_1, 0L))) + (long)(w_1)));
parent_1.put(neighbour, start_1);
                long k_1 = 0L;
                while ((long)(k_1) < (long)(queue_1.length)) {
                    if ((queue_1[(int)((long)(k_1))].node.equals(neighbour))) {
queue_1[(int)((long)(k_1))].weight = ((long)(dist_1).getOrDefault(neighbour, 0L));
                        break;
                    }
                    k_1 = (long)((long)(k_1) + 1L);
                }
            }
        }
        while ((long)(queue_1.length) > 0L) {
            long best_idx_1 = 0L;
            long p_1 = 1L;
            while ((long)(p_1) < (long)(queue_1.length)) {
                if ((long)(queue_1[(int)((long)(p_1))].weight) < (long)(queue_1[(int)((long)(best_idx_1))].weight)) {
                    best_idx_1 = (long)(p_1);
                }
                p_1 = (long)((long)(p_1) + 1L);
            }
            QueueNode node_entry_1 = queue_1[(int)((long)(best_idx_1))];
            String node_1 = node_entry_1.node;
            QueueNode[] tmp_1 = ((QueueNode[])(new QueueNode[]{}));
            long q_1 = 0L;
            while ((long)(q_1) < (long)(queue_1.length)) {
                if ((long)(q_1) != (long)(best_idx_1)) {
                    tmp_1 = ((QueueNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tmp_1), java.util.stream.Stream.of(queue_1[(int)((long)(q_1))])).toArray(QueueNode[]::new)));
                }
                q_1 = (long)((long)(q_1) + 1L);
            }
            queue_1 = ((QueueNode[])(tmp_1));
            for (String neighbour : ((java.util.Map<String,Long>)(graph).get(node_1)).keySet()) {
                long w_3 = (long)(((long)(((java.util.Map<String,Long>)(graph).get(node_1))).getOrDefault(neighbour, 0L)));
                if ((long)(((long)(dist_1).getOrDefault(neighbour, 0L))) > (long)((long)(((long)(dist_1).getOrDefault(node_1, 0L))) + (long)(w_3))) {
dist_1.put(neighbour, (long)((long)(((long)(dist_1).getOrDefault(node_1, 0L))) + (long)(w_3)));
parent_1.put(neighbour, node_1);
                    long r_1 = 0L;
                    while ((long)(r_1) < (long)(queue_1.length)) {
                        if ((queue_1[(int)((long)(r_1))].node.equals(neighbour))) {
queue_1[(int)((long)(r_1))].weight = ((long)(dist_1).getOrDefault(neighbour, 0L));
                            break;
                        }
                        r_1 = (long)((long)(r_1) + 1L);
                    }
                }
            }
        }
        return new MSTResult(dist_1, parent_1);
    }

    static long iabs(long x) {
        if ((long)(x) < 0L) {
            return -x;
        }
        return x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<String,java.util.Map<String,Long>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Long>>()));
graph.put("a", (long)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("b", 3L), java.util.Map.entry("c", 15L)))));
graph.put("b", (long)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("a", 3L), java.util.Map.entry("c", 10L), java.util.Map.entry("d", 100L)))));
graph.put("c", (long)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("a", 15L), java.util.Map.entry("b", 10L), java.util.Map.entry("d", 5L)))));
graph.put("d", (long)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("b", 100L), java.util.Map.entry("c", 5L)))));
            res = prims_algo(graph);
            dist_2 = res.dist;
            System.out.println(_p(iabs((long)((long)(((long)(dist_2).getOrDefault("a", 0L))) - (long)(((long)(dist_2).getOrDefault("b", 0L)))))));
            System.out.println(_p(iabs((long)((long)(((long)(dist_2).getOrDefault("d", 0L))) - (long)(((long)(dist_2).getOrDefault("b", 0L)))))));
            System.out.println(_p(iabs((long)((long)(((long)(dist_2).getOrDefault("a", 0L))) - (long)(((long)(dist_2).getOrDefault("c", 0L)))))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
