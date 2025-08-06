public class Main {
    static java.util.Map<Integer,int[]> graph;

    static boolean is_bipartite_bfs(java.util.Map<Integer,int[]> graph) {
        java.util.Map<Integer,Integer> visited = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
        for (int node : graph.keySet()) {
            if (!(Boolean)(visited.containsKey(node))) {
                int[] queue = ((int[])(new int[]{}));
                queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(((Number)(node)).intValue())).toArray()));
visited.put(node, 0);
                while (queue.length > 0) {
                    int curr = queue[0];
                    queue = ((int[])(java.util.Arrays.copyOfRange(queue, 1, queue.length)));
                    for (int neighbor : ((int[])(graph).get(curr))) {
                        if (!(Boolean)(visited.containsKey(neighbor))) {
visited.put(neighbor, 1 - (int)(((int)(visited).getOrDefault(curr, 0))));
                            queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(neighbor)).toArray()));
                        } else                         if ((int)(((int)(visited).getOrDefault(neighbor, 0))) == (int)(((int)(visited).getOrDefault(curr, 0)))) {
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
            graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 3}))), java.util.Map.entry(1, ((int[])(new int[]{0, 2}))), java.util.Map.entry(2, ((int[])(new int[]{1, 3}))), java.util.Map.entry(3, ((int[])(new int[]{0, 2})))))));
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
        return String.valueOf(v);
    }
}
