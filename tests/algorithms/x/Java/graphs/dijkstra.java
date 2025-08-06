public class Main {
    static class NodeCost {
        String node;
        int cost;
        NodeCost(String node, int cost) {
            this.node = node;
            this.cost = cost;
        }
        NodeCost() {}
        @Override public String toString() {
            return String.format("{'node': '%s', 'cost': %s}", String.valueOf(node), String.valueOf(cost));
        }
    }

    static java.util.Map<String,java.util.Map<String,Integer>> G;
    static NodeCost[] heap = new NodeCost[0];
    static java.util.Map<String,Boolean> visited = null;
    static int result = 0;
    static java.util.Map<String,Object> G2;
    static NodeCost[] heap2 = new NodeCost[0];
    static java.util.Map<String,Boolean> visited2 = null;
    static int result2 = 0;
    static java.util.Map<String,Object> G3;
    static NodeCost[] heap3 = new NodeCost[0];
    static java.util.Map<String,Boolean> visited3 = null;
    static int result3 = 0;

    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            G = ((java.util.Map<String,java.util.Map<String,Integer>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Integer>>(java.util.Map.ofEntries(java.util.Map.entry("A", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("B", 2), java.util.Map.entry("C", 5)))))), java.util.Map.entry("B", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("A", 2), java.util.Map.entry("D", 3), java.util.Map.entry("E", 1), java.util.Map.entry("F", 1)))))), java.util.Map.entry("C", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("A", 5), java.util.Map.entry("F", 3)))))), java.util.Map.entry("D", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("B", 3)))))), java.util.Map.entry("E", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("B", 4), java.util.Map.entry("F", 3)))))), java.util.Map.entry("F", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("C", 3), java.util.Map.entry("E", 3))))))))));
            heap = ((NodeCost[])(new NodeCost[]{new NodeCost("E", 0)}));
            visited = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
            result = -1;
            while (heap.length > 0) {
                int best_idx = 0;
                int i = 1;
                while (i < heap.length) {
                    if (heap[i].cost < heap[best_idx].cost) {
                        best_idx = i;
                    }
                    i = i + 1;
                }
                NodeCost best = heap[best_idx];
                NodeCost[] new_heap = ((NodeCost[])(new NodeCost[]{}));
                int j = 0;
                while (j < heap.length) {
                    if (j != best_idx) {
                        new_heap = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap), java.util.stream.Stream.of(heap[j])).toArray(NodeCost[]::new)));
                    }
                    j = j + 1;
                }
                heap = ((NodeCost[])(new_heap));
                String u = best.node;
                int cost = best.cost;
                if (((Boolean)(visited.containsKey(u)))) {
                    continue;
                }
visited.put(u, true);
                if ((u.equals("C"))) {
                    result = cost;
                    break;
                }
                for (String v : ((java.util.Map<String,Integer>)(G).get(u)).keySet()) {
                    if (((Boolean)(visited.containsKey(v)))) {
                        continue;
                    }
                    int next_cost = cost + (int)(((int)(((java.util.Map<String,Integer>)(G).get(u))).getOrDefault(v, 0)));
                    heap = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new NodeCost(v, next_cost))).toArray(NodeCost[]::new)));
                }
            }
            System.out.println(result);
            G2 = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("B", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("C", 1)))), java.util.Map.entry("C", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("D", 1)))), java.util.Map.entry("D", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("F", 1)))), java.util.Map.entry("E", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("B", 1), java.util.Map.entry("F", 3)))), java.util.Map.entry("F", new java.util.LinkedHashMap<String, Object>())))));
            heap2 = ((NodeCost[])(new NodeCost[]{new NodeCost("E", 0)}));
            visited2 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
            result2 = -1;
            while (heap2.length > 0) {
                int best2_idx = 0;
                int i2 = 1;
                while (i2 < heap2.length) {
                    if (heap2[i2].cost < heap2[best2_idx].cost) {
                        best2_idx = i2;
                    }
                    i2 = i2 + 1;
                }
                NodeCost best2 = heap2[best2_idx];
                NodeCost[] new_heap2 = ((NodeCost[])(new NodeCost[]{}));
                int j2 = 0;
                while (j2 < heap2.length) {
                    if (j2 != best2_idx) {
                        new_heap2 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap2), java.util.stream.Stream.of(heap2[j2])).toArray(NodeCost[]::new)));
                    }
                    j2 = j2 + 1;
                }
                heap2 = ((NodeCost[])(new_heap2));
                String u2 = best2.node;
                int cost2 = best2.cost;
                if (((Boolean)(visited2.containsKey(u2)))) {
                    continue;
                }
visited2.put(u2, true);
                if ((u2.equals("F"))) {
                    result2 = cost2;
                    break;
                }
                for (var v2 : ((Object)(G2).get(u2))) {
                    if (((Boolean)(visited2.containsKey(v2)))) {
                        continue;
                    }
                    int next_cost2 = cost2 + ((Number)(((Object)(((Object)(G2).get(u2))).get(v2)))).intValue();
                    heap2 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap2), java.util.stream.Stream.of(new NodeCost(v2, next_cost2))).toArray(NodeCost[]::new)));
                }
            }
            System.out.println(result2);
            G3 = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("B", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("C", 1)))), java.util.Map.entry("C", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("D", 1)))), java.util.Map.entry("D", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("F", 1)))), java.util.Map.entry("E", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("B", 1), java.util.Map.entry("G", 2)))), java.util.Map.entry("F", new java.util.LinkedHashMap<String, Object>()), java.util.Map.entry("G", new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("F", 1))))))));
            heap3 = ((NodeCost[])(new NodeCost[]{new NodeCost("E", 0)}));
            visited3 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
            result3 = -1;
            while (heap3.length > 0) {
                int best3_idx = 0;
                int i3 = 1;
                while (i3 < heap3.length) {
                    if (heap3[i3].cost < heap3[best3_idx].cost) {
                        best3_idx = i3;
                    }
                    i3 = i3 + 1;
                }
                NodeCost best3 = heap3[best3_idx];
                NodeCost[] new_heap3 = ((NodeCost[])(new NodeCost[]{}));
                int j3 = 0;
                while (j3 < heap3.length) {
                    if (j3 != best3_idx) {
                        new_heap3 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap3), java.util.stream.Stream.of(heap3[j3])).toArray(NodeCost[]::new)));
                    }
                    j3 = j3 + 1;
                }
                heap3 = ((NodeCost[])(new_heap3));
                String u3 = best3.node;
                int cost3 = best3.cost;
                if (((Boolean)(visited3.containsKey(u3)))) {
                    continue;
                }
visited3.put(u3, true);
                if ((u3.equals("F"))) {
                    result3 = cost3;
                    break;
                }
                for (var v3 : ((Object)(G3).get(u3))) {
                    if (((Boolean)(visited3.containsKey(v3)))) {
                        continue;
                    }
                    int next_cost3 = cost3 + ((Number)(((Object)(((Object)(G3).get(u3))).get(v3)))).intValue();
                    heap3 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap3), java.util.stream.Stream.of(new NodeCost(v3, next_cost3))).toArray(NodeCost[]::new)));
                }
            }
            System.out.println(result3);
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
}
