public class Main {

    static long[] topological_sort(java.util.Map<Long,long[]> graph) {
        long[] indegree = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(graph.size())) {
            indegree = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(indegree), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        for (var edges : new java.util.ArrayList<>(graph.values())) {
            long j_1 = 0L;
            while ((long)(j_1) < (long)(String.valueOf(edges).length())) {
                Object v_1 = edges[j_1];
indegree[(int)((long)(v_1))] = (long)((long)(indegree[(int)((long)(v_1))]) + 1L);
                j_1 = (long)((long)(j_1) + 1L);
            }
        }
        long[] queue_1 = ((long[])(new long[]{}));
        i_1 = 0L;
        while ((long)(i_1) < (long)(indegree.length)) {
            if ((long)(indegree[(int)((long)(i_1))]) == 0L) {
                queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(i_1))).toArray()));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] order_1 = ((long[])(new long[]{}));
        long head_1 = 0L;
        long processed_1 = 0L;
        while ((long)(head_1) < (long)(queue_1.length)) {
            long v_3 = (long)(queue_1[(int)((long)(head_1))]);
            head_1 = (long)((long)(head_1) + 1L);
            processed_1 = (long)((long)(processed_1) + 1L);
            order_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(order_1), java.util.stream.LongStream.of((long)(v_3))).toArray()));
            long[] neighbors_1 = (long[])(((long[])(graph).get(v_3)));
            long k_1 = 0L;
            while ((long)(k_1) < (long)(neighbors_1.length)) {
                long nb_1 = (long)(neighbors_1[(int)((long)(k_1))]);
indegree[(int)((long)(nb_1))] = (long)((long)(indegree[(int)((long)(nb_1))]) - 1L);
                if ((long)(indegree[(int)((long)(nb_1))]) == 0L) {
                    queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(nb_1))).toArray()));
                }
                k_1 = (long)((long)(k_1) + 1L);
            }
        }
        if ((long)(processed_1) != (long)(graph.size())) {
            return ((long[])(null));
        }
        return order_1;
    }

    static void main() {
        java.util.Map<Long,long[]> graph = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1, 2}))), java.util.Map.entry(1L, ((long[])(new long[]{3}))), java.util.Map.entry(2L, ((long[])(new long[]{3}))), java.util.Map.entry(3L, ((long[])(new long[]{4, 5}))), java.util.Map.entry(4L, ((long[])(new long[]{}))), java.util.Map.entry(5L, ((long[])(new long[]{})))))));
        System.out.println(topological_sort(graph));
        java.util.Map<Long,long[]> cyclic_1 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>(java.util.Map.ofEntries(java.util.Map.entry(0L, ((long[])(new long[]{1}))), java.util.Map.entry(1L, ((long[])(new long[]{2}))), java.util.Map.entry(2L, ((long[])(new long[]{0})))))));
        System.out.println(topological_sort(cyclic_1));
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
