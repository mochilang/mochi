public class Main {
    static class NodeCost {
        String node;
        long cost;
        NodeCost(String node, long cost) {
            this.node = node;
            this.cost = cost;
        }
        NodeCost() {}
        @Override public String toString() {
            return String.format("{'node': '%s', 'cost': %s}", String.valueOf(node), String.valueOf(cost));
        }
    }

    static java.util.Map<String,java.util.Map<String,Long>> G;
    static NodeCost[] heap = new NodeCost[0];
    static java.util.Map<String,Boolean> visited = null;
    static long result = 0;
    static java.util.Map<String,Object> G2;
    static NodeCost[] heap2 = new NodeCost[0];
    static java.util.Map<String,Boolean> visited2 = null;
    static long result2 = 0;
    static java.util.Map<String,Object> G3;
    static NodeCost[] heap3 = new NodeCost[0];
    static java.util.Map<String,Boolean> visited3 = null;
    static long result3 = 0;

    public static void main(String[] args) {
        G = ((java.util.Map<String,java.util.Map<String,Long>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Long>>(java.util.Map.ofEntries(java.util.Map.entry("A", ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("B", 2L), java.util.Map.entry("C", 5L)))))), java.util.Map.entry("B", ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("A", 2L), java.util.Map.entry("D", 3L), java.util.Map.entry("E", 1L), java.util.Map.entry("F", 1L)))))), java.util.Map.entry("C", ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("A", 5L), java.util.Map.entry("F", 3L)))))), java.util.Map.entry("D", ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("B", 3L)))))), java.util.Map.entry("E", ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("B", 4L), java.util.Map.entry("F", 3L)))))), java.util.Map.entry("F", ((java.util.Map<String,Long>)(new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("C", 3L), java.util.Map.entry("E", 3L))))))))));
        heap = ((NodeCost[])(new NodeCost[]{new NodeCost("E", 0)}));
        visited = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        result = (long)(-1);
        while ((long)(heap.length) > 0L) {
            long best_idx = 0L;
            long i = 1L;
            while ((long)(i) < (long)(heap.length)) {
                if ((long)(heap[(int)((long)(i))].cost) < (long)(heap[(int)((long)(best_idx))].cost)) {
                    best_idx = (long)(i);
                }
                i = (long)((long)(i) + 1L);
            }
            NodeCost best = heap[(int)((long)(best_idx))];
            NodeCost[] new_heap = ((NodeCost[])(new NodeCost[]{}));
            long j = 0L;
            while ((long)(j) < (long)(heap.length)) {
                if ((long)(j) != (long)(best_idx)) {
                    new_heap = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap), java.util.stream.Stream.of(heap[(int)((long)(j))])).toArray(NodeCost[]::new)));
                }
                j = (long)((long)(j) + 1L);
            }
            heap = ((NodeCost[])(new_heap));
            String u = best.node;
            long cost = (long)(best.cost);
            if (visited.containsKey(u)) {
                continue;
            }
visited.put(u, true);
            if ((u.equals("C"))) {
                result = (long)(cost);
                break;
            }
            for (String v : ((java.util.Map<String,Long>)(G).get(u)).keySet()) {
                if (visited.containsKey(v)) {
                    continue;
                }
                long next_cost = (long)((long)(cost) + (long)(((long)(((java.util.Map<String,Long>)(G).get(u))).getOrDefault(v, 0L))));
                heap = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap), java.util.stream.Stream.of(new NodeCost(v, next_cost))).toArray(NodeCost[]::new)));
            }
        }
        System.out.println(result);
        G2 = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("B", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("C", 1L)))), java.util.Map.entry("C", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("D", 1L)))), java.util.Map.entry("D", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("F", 1L)))), java.util.Map.entry("E", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("B", 1L), java.util.Map.entry("F", 3L)))), java.util.Map.entry("F", new java.util.LinkedHashMap<String, Object>())))));
        heap2 = ((NodeCost[])(new NodeCost[]{new NodeCost("E", 0)}));
        visited2 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        result2 = (long)(-1);
        while ((long)(heap2.length) > 0L) {
            long best2_idx = 0L;
            long i2 = 1L;
            while ((long)(i2) < (long)(heap2.length)) {
                if ((long)(heap2[(int)((long)(i2))].cost) < (long)(heap2[(int)((long)(best2_idx))].cost)) {
                    best2_idx = (long)(i2);
                }
                i2 = (long)((long)(i2) + 1L);
            }
            NodeCost best2 = heap2[(int)((long)(best2_idx))];
            NodeCost[] new_heap2 = ((NodeCost[])(new NodeCost[]{}));
            long j2 = 0L;
            while ((long)(j2) < (long)(heap2.length)) {
                if ((long)(j2) != (long)(best2_idx)) {
                    new_heap2 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap2), java.util.stream.Stream.of(heap2[(int)((long)(j2))])).toArray(NodeCost[]::new)));
                }
                j2 = (long)((long)(j2) + 1L);
            }
            heap2 = ((NodeCost[])(new_heap2));
            String u2 = best2.node;
            long cost2 = (long)(best2.cost);
            if (visited2.containsKey(u2)) {
                continue;
            }
visited2.put(u2, true);
            if ((u2.equals("F"))) {
                result2 = (long)(cost2);
                break;
            }
            for (String v2 : ((java.util.Map<String, ?>)((Object)(G2).get(u2))).keySet()) {
                if (visited2.containsKey(v2)) {
                    continue;
                }
                long next_cost2 = (long)((long)(cost2) + ((Number)(((Object)(((java.util.Map)((Object)(G2).get(u2)))).get(v2)))).intValue());
                heap2 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap2), java.util.stream.Stream.of(new NodeCost(v2, next_cost2))).toArray(NodeCost[]::new)));
            }
        }
        System.out.println(result2);
        G3 = ((java.util.Map<String,Object>)(new java.util.LinkedHashMap<String, Object>(java.util.Map.ofEntries(java.util.Map.entry("B", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("C", 1L)))), java.util.Map.entry("C", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("D", 1L)))), java.util.Map.entry("D", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("F", 1L)))), java.util.Map.entry("E", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("B", 1L), java.util.Map.entry("G", 2L)))), java.util.Map.entry("F", new java.util.LinkedHashMap<String, Object>()), java.util.Map.entry("G", new java.util.LinkedHashMap<String, Long>(java.util.Map.ofEntries(java.util.Map.entry("F", 1L))))))));
        heap3 = ((NodeCost[])(new NodeCost[]{new NodeCost("E", 0)}));
        visited3 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        result3 = (long)(-1);
        while ((long)(heap3.length) > 0L) {
            long best3_idx = 0L;
            long i3 = 1L;
            while ((long)(i3) < (long)(heap3.length)) {
                if ((long)(heap3[(int)((long)(i3))].cost) < (long)(heap3[(int)((long)(best3_idx))].cost)) {
                    best3_idx = (long)(i3);
                }
                i3 = (long)((long)(i3) + 1L);
            }
            NodeCost best3 = heap3[(int)((long)(best3_idx))];
            NodeCost[] new_heap3 = ((NodeCost[])(new NodeCost[]{}));
            long j3 = 0L;
            while ((long)(j3) < (long)(heap3.length)) {
                if ((long)(j3) != (long)(best3_idx)) {
                    new_heap3 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_heap3), java.util.stream.Stream.of(heap3[(int)((long)(j3))])).toArray(NodeCost[]::new)));
                }
                j3 = (long)((long)(j3) + 1L);
            }
            heap3 = ((NodeCost[])(new_heap3));
            String u3 = best3.node;
            long cost3 = (long)(best3.cost);
            if (visited3.containsKey(u3)) {
                continue;
            }
visited3.put(u3, true);
            if ((u3.equals("F"))) {
                result3 = (long)(cost3);
                break;
            }
            for (String v3 : ((java.util.Map<String, ?>)((Object)(G3).get(u3))).keySet()) {
                if (visited3.containsKey(v3)) {
                    continue;
                }
                long next_cost3 = (long)((long)(cost3) + ((Number)(((Object)(((java.util.Map)((Object)(G3).get(u3)))).get(v3)))).intValue());
                heap3 = ((NodeCost[])(java.util.stream.Stream.concat(java.util.Arrays.stream(heap3), java.util.stream.Stream.of(new NodeCost(v3, next_cost3))).toArray(NodeCost[]::new)));
            }
        }
        System.out.println(result3);
    }
}
