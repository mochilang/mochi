public class Main {
    static class QueueNode {
        String node;
        int weight;
        QueueNode(String node, int weight) {
            this.node = node;
            this.weight = weight;
        }
        QueueNode() {}
        @Override public String toString() {
            return String.format("{'node': '%s', 'weight': %s}", String.valueOf(node), String.valueOf(weight));
        }
    }

    static class MSTResult {
        java.util.Map<String,Integer> dist;
        java.util.Map<String,String> parent;
        MSTResult(java.util.Map<String,Integer> dist, java.util.Map<String,String> parent) {
            this.dist = dist;
            this.parent = parent;
        }
        MSTResult() {}
        @Override public String toString() {
            return String.format("{'dist': %s, 'parent': %s}", String.valueOf(dist), String.valueOf(parent));
        }
    }

    static java.util.Map<String,java.util.Map<String,Integer>> graph = null;
    static MSTResult res;
    static java.util.Map<String,Integer> dist_1;

    static MSTResult prims_algo(java.util.Map<String,java.util.Map<String,Integer>> graph) {
        int INF = 2147483647;
        java.util.Map<String,Integer> dist = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>()));
        java.util.Map<String,String> parent = ((java.util.Map<String,String>)(new java.util.LinkedHashMap<String, String>()));
        QueueNode[] queue = ((QueueNode[])(new QueueNode[]{}));
        for (String node : graph.keySet()) {
dist.put(node, INF);
parent.put(node, "");
            queue = ((QueueNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(queue), java.util.stream.Stream.of(new QueueNode(node, INF))).toArray(QueueNode[]::new)));
        }
        if (queue.length == 0) {
            return new MSTResult(dist, parent);
        }
        int min_idx = 0;
        int i = 1;
        while (i < queue.length) {
            if (queue[i].weight < queue[min_idx].weight) {
                min_idx = i;
            }
            i = i + 1;
        }
        QueueNode start_node = queue[min_idx];
        String start = start_node.node;
        QueueNode[] new_q = ((QueueNode[])(new QueueNode[]{}));
        int j = 0;
        while (j < queue.length) {
            if (j != min_idx) {
                new_q = ((QueueNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_q), java.util.stream.Stream.of(queue[j])).toArray(QueueNode[]::new)));
            }
            j = j + 1;
        }
        queue = ((QueueNode[])(new_q));
dist.put(start, 0);
        for (String neighbour : ((java.util.Map<String,Integer>)(graph).get(start)).keySet()) {
            int w = (int)(((int)(((java.util.Map<String,Integer>)(graph).get(start))).getOrDefault(neighbour, 0)));
            if ((int)(((int)(dist).getOrDefault(neighbour, 0))) > (int)(((int)(dist).getOrDefault(start, 0))) + w) {
dist.put(neighbour, (int)(((int)(dist).getOrDefault(start, 0))) + w);
parent.put(neighbour, start);
                int k = 0;
                while (k < queue.length) {
                    if (queue[k].node == ((Number)(neighbour)).intValue()) {
                        break;
                    }
                    k = k + 1;
                }
            }
        }
        while (queue.length > 0) {
            int best_idx = 0;
            int p = 1;
            while (p < queue.length) {
                if (queue[p].weight < queue[best_idx].weight) {
                    best_idx = p;
                }
                p = p + 1;
            }
            QueueNode node_entry = queue[best_idx];
            String node = node_entry.node;
            QueueNode[] tmp = ((QueueNode[])(new QueueNode[]{}));
            int q = 0;
            while (q < queue.length) {
                if (q != best_idx) {
                    tmp = ((QueueNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tmp), java.util.stream.Stream.of(queue[q])).toArray(QueueNode[]::new)));
                }
                q = q + 1;
            }
            queue = ((QueueNode[])(tmp));
            for (String neighbour : ((java.util.Map<String,Integer>)(graph).get(node)).keySet()) {
                int w_1 = (int)(((int)(((java.util.Map<String,Integer>)(graph).get(node))).getOrDefault(neighbour, 0)));
                if ((int)(((int)(dist).getOrDefault(neighbour, 0))) > (int)(((int)(dist).getOrDefault(node, 0))) + w_1) {
dist.put(neighbour, (int)(((int)(dist).getOrDefault(node, 0))) + w_1);
parent.put(neighbour, node);
                    int r = 0;
                    while (r < queue.length) {
                        if ((queue[r].node.equals(neighbour))) {
                            break;
                        }
                        r = r + 1;
                    }
                }
            }
        }
        return new MSTResult(dist, parent);
    }

    static int iabs(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<String,java.util.Map<String,Integer>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Integer>>()));
graph.put("a", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("b", 3), java.util.Map.entry("c", 15))));
graph.put("b", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 3), java.util.Map.entry("c", 10), java.util.Map.entry("d", 100))));
graph.put("c", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 15), java.util.Map.entry("b", 10), java.util.Map.entry("d", 5))));
graph.put("d", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("b", 100), java.util.Map.entry("c", 5))));
            res = prims_algo(graph);
            dist_1 = res.dist;
            System.out.println(_p(iabs((int)(((int)(dist_1).getOrDefault("a", 0))) - (int)(((int)(dist_1).getOrDefault("b", 0))))));
            System.out.println(_p(iabs((int)(((int)(dist_1).getOrDefault("d", 0))) - (int)(((int)(dist_1).getOrDefault("b", 0))))));
            System.out.println(_p(iabs((int)(((int)(dist_1).getOrDefault("a", 0))) - (int)(((int)(dist_1).getOrDefault("c", 0))))));
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
