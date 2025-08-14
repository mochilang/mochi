public class Main {
    static long seed = 1L;

    static long rand() {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return seed;
    }

    static double random() {
        return (double)(((double)(1.0) * (double)(rand()))) / (double)(2147483648.0);
    }

    static java.util.Map<Long,long[]> complete_graph(long vertices_number) {
        java.util.Map<Long,long[]> graph = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(vertices_number)) {
            long[] neighbors_1 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(vertices_number)) {
                if ((long)(j_1) != (long)(i_1)) {
                    neighbors_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(neighbors_1), java.util.stream.LongStream.of((long)(j_1))).toArray()));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
graph.put(i_1, ((long[])(neighbors_1)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return graph;
    }

    static java.util.Map<Long,long[]> random_graph(long vertices_number, double probability, boolean directed) {
        java.util.Map<Long,long[]> graph_1 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(vertices_number)) {
graph_1.put(i_3, ((long[])(new long[]{})));
            i_3 = (long)((long)(i_3) + 1L);
        }
        if ((double)(probability) >= (double)(1.0)) {
            return complete_graph((long)(vertices_number));
        }
        if ((double)(probability) <= (double)(0.0)) {
            return graph_1;
        }
        i_3 = 0L;
        while ((long)(i_3) < (long)(vertices_number)) {
            long j_3 = (long)((long)(i_3) + 1L);
            while ((long)(j_3) < (long)(vertices_number)) {
                if ((double)(random()) < (double)(probability)) {
graph_1.put(i_3, ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(((long[])(graph_1).get(i_3))), java.util.stream.LongStream.of((long)(j_3))).toArray())));
                    if (!(Boolean)directed) {
graph_1.put(j_3, ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(((long[])(graph_1).get(j_3))), java.util.stream.LongStream.of((long)(i_3))).toArray())));
                    }
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return graph_1;
    }

    static void main() {
        seed = 1L;
        java.util.Map<Long,long[]> g1_1 = random_graph(4L, (double)(0.5), false);
        System.out.println(g1_1);
        seed = 1L;
        java.util.Map<Long,long[]> g2_1 = random_graph(4L, (double)(0.5), true);
        System.out.println(g2_1);
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
