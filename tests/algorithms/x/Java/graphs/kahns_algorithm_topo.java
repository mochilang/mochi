public class Main {

    static int[] topological_sort(java.util.Map<Integer,int[]> graph) {
        int[] indegree = ((int[])(new int[]{}));
        int i = 0;
        while (i < graph.size()) {
            indegree = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(indegree), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        for (var edges : new java.util.ArrayList<>(graph.values())) {
            int j = 0;
            while (j < String.valueOf(edges).length()) {
                Object v = edges[j];
indegree[v] = indegree[v] + 1;
                j = j + 1;
            }
        }
        int[] queue = ((int[])(new int[]{}));
        i = 0;
        while (i < indegree.length) {
            if (indegree[i] == 0) {
                queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(i)).toArray()));
            }
            i = i + 1;
        }
        int[] order = ((int[])(new int[]{}));
        int head = 0;
        int processed = 0;
        while (head < queue.length) {
            int v_1 = queue[head];
            head = head + 1;
            processed = processed + 1;
            order = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(order), java.util.stream.IntStream.of(v_1)).toArray()));
            int[] neighbors = (int[])(((int[])(graph).get(v_1)));
            int k = 0;
            while (k < neighbors.length) {
                int nb = neighbors[k];
indegree[nb] = indegree[nb] - 1;
                if (indegree[nb] == 0) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(nb)).toArray()));
                }
                k = k + 1;
            }
        }
        if (processed != graph.size()) {
            return null;
        }
        return order;
    }

    static void main() {
        java.util.Map<Integer,int[]> graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 2}))), java.util.Map.entry(1, ((int[])(new int[]{3}))), java.util.Map.entry(2, ((int[])(new int[]{3}))), java.util.Map.entry(3, ((int[])(new int[]{4, 5}))), java.util.Map.entry(4, new int[]{}), java.util.Map.entry(5, new int[]{})))));
        System.out.println(topological_sort(graph));
        java.util.Map<Integer,int[]> cyclic = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1}))), java.util.Map.entry(1, ((int[])(new int[]{2}))), java.util.Map.entry(2, ((int[])(new int[]{0})))))));
        System.out.println(topological_sort(cyclic));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
