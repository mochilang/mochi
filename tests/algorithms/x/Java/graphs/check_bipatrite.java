public class Main {
    static java.util.Map<Long,long[]> graph;

    static boolean is_bipartite_bfs(java.util.Map<Long,long[]> graph) {
        java.util.Map<Long,Long> visited = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        for (long node : graph.keySet()) {
            if (!(visited.containsKey(node))) {
                long[] queue_1 = ((long[])(new long[]{}));
                queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(node))).toArray()));
visited.put(node, 0L);
                while ((long)(queue_1.length) > 0L) {
                    long curr_1 = (long)(queue_1[(int)((long)(0))]);
                    queue_1 = ((long[])(java.util.Arrays.copyOfRange(queue_1, (int)((long)(1)), (int)((long)(queue_1.length)))));
                    for (long neighbor : ((long[])(graph).get(curr_1))) {
                        if (!(visited.containsKey(neighbor))) {
visited.put(neighbor, (long)(1L - (long)(((long)(visited).getOrDefault(curr_1, 0L)))));
                            queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(neighbor))).toArray()));
                        } else                         if ((long)(((long)(visited).getOrDefault(neighbor, 0L))) == (long)(((long)(visited).getOrDefault(curr_1, 0L)))) {
                            return false;
                        }
                    }
                }
            }
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(1L, ((long[])(new long[]{0, 2}))), java.util.Map.entry(2L, ((long[])(new long[]{1, 3}))), java.util.Map.entry(3L, ((long[])(new long[]{0, 2})))))));
            System.out.println(_p(is_bipartite_bfs(graph)));
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
