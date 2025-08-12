public class Main {
    static long seed = 0;

    static long rand() {
        seed = ((long)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static java.util.Map<Long,long[]> complete_graph(long vertices_number) {
        java.util.Map<Long,long[]> graph = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()));
        long i_1 = 0L;
        while (i_1 < vertices_number) {
            long[] neighbors_1 = ((long[])(new long[]{}));
            long j_1 = 0L;
            while (j_1 < vertices_number) {
                if (j_1 != i_1) {
                    neighbors_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(neighbors_1), java.util.stream.LongStream.of(j_1)).toArray()));
                }
                j_1 = j_1 + 1;
            }
graph.put(i_1, ((long[])(neighbors_1)));
            i_1 = i_1 + 1;
        }
        return graph;
    }

    static java.util.Map<Long,long[]> random_graph(long vertices_number, double probability, boolean directed) {
        java.util.Map<Long,long[]> graph_1 = ((java.util.Map<Long,long[]>)(new java.util.LinkedHashMap<Long, long[]>()));
        long i_3 = 0L;
        while (i_3 < vertices_number) {
graph_1.put(i_3, ((long[])(new long[]{})));
            i_3 = i_3 + 1;
        }
        if (probability >= 1.0) {
            return complete_graph(vertices_number);
        }
        if (probability <= 0.0) {
            return graph_1;
        }
        i_3 = 0;
        while (i_3 < vertices_number) {
            long j_3 = i_3 + 1;
            while (j_3 < vertices_number) {
                if (random() < probability) {
graph_1.put(i_3, ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(((long[])(graph_1).get(i_3))), java.util.stream.LongStream.of(j_3)).toArray())));
                    if (!(Boolean)directed) {
graph_1.put(j_3, ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(((long[])(graph_1).get(j_3))), java.util.stream.LongStream.of(i_3)).toArray())));
                    }
                }
                j_3 = j_3 + 1;
            }
            i_3 = i_3 + 1;
        }
        return graph_1;
    }

    static void main() {
        seed = 1;
        java.util.Map<Long,long[]> g1_1 = random_graph(4L, 0.5, false);
        System.out.println(g1_1);
        seed = 1;
        java.util.Map<Long,long[]> g2_1 = random_graph(4L, 0.5, true);
        System.out.println(g2_1);
    }
    public static void main(String[] args) {
        seed = 1;
        main();
    }
}
